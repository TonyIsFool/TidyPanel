#' Robust Parsing and Extraction of Messy Excel Panel Data
#'
#' @description
#' `read_messy_panel()` is an industrial-grade parser designed to extract clean, standardized data frames 
#' from heavily malformed, human-readable Excel reports (e.g., financial statements, ERP exports). 
#' It automatically bypasses decoy rows, stitches N-dimensional hierarchical headers, extracts structural 
#' indentation hierarchies (parent-child relationships), amputates embedded subtotals, and standardizes 
#' financial/scientific numbers.
#'
#' @param file_path Character string. Path to the Excel file.
#' @param sheet Optional sheet name or index. If `NULL` (the default), it auto-discovers the first valid data panel across all sheets. If `"ALL"`, parses and merges all sheets.
#' @param na_strings Character vector. Strings to interpret as missing values. Supports complex missing-value lexicons.
#' @param clean_vars Logical. If `TRUE` (default), standardizes variable names to snake_case using `clean_variable_names()`.
#' @param auto_pivot Logical. If `TRUE`, attempts to reshape wide temporal columns (e.g., FY2021, Q1_2022) into a long format (`time_period`, `value`).
#' @param extract_all_blocks Logical. If `TRUE`, extracts all disjoint data tables on a sheet as a list of data frames. Default is `FALSE` (extracts only the largest block).
#' @param return_audit Logical. If `TRUE`, returns a list containing `$data` (the cleaned data frame) and `$audit` (a detailed log of all algorithmic modifications made).
#'
#' @return If `return_audit = FALSE`, a cleaned and standardized `data.frame`. 
#' If `return_audit = TRUE`, a named list containing:
#' \item{data}{The cleaned `data.frame`.}
#' \item{audit}{A `data.frame` detailing exactly what transformations, truncations, or imputations were applied.}
#'
#' @examples
#' # Toy example: create a small in-memory Excel file and parse it
#' tmp <- tempfile(fileext = ".xlsx")
#' df_raw <- data.frame(
#'   Category = c("Revenue", "Cost", "Total"),
#'   `2022` = c("1.2M", "800k", "2.0M"),
#'   `2023` = c("1.5M", "900k", "2.4M"),
#'   check.names = FALSE
#' )
#' writexl::write_xlsx(df_raw, tmp)
#' result <- read_messy_panel(tmp, auto_pivot = TRUE)
#' head(result)
#' unlink(tmp)
#'
#' @export
#' @importFrom readxl read_excel excel_sheets
#' @importFrom stringr str_trim str_replace str_remove_all str_squish str_extract
read_messy_panel <- function(file_path, sheet = NULL, na_strings = c("", "NA", "#N/A", "NULL", "S", "D", "ND", "N/A", "*", "**", "***", ".", "x", "c", "s", "z", "#VALUE!", "#REF!", "#DIV/0!", "#NUM!", "#NAME?", "none", "NR", "--", "---", "n.a.", "N.A.", "n/a", "Not Applicable"), clean_vars = TRUE, auto_pivot = FALSE, return_audit = FALSE, extract_all_blocks = FALSE) {
  
  audit_log <- list()
  
  is_df <- is.data.frame(file_path)
  is_csv <- FALSE
  if (!is_df && is.character(file_path)) {
      is_csv <- tolower(tools::file_ext(file_path)) %in% c("csv", "tsv", "txt", "rdb")
  }
  is_yaml <- !is_df && is.character(file_path) && length(file_path) == 1 &&
      tolower(tools::file_ext(file_path)) %in% c("yml", "yaml")

  if (is_yaml) {
      yaml <- parse_yaml_records_file(file_path, clean_vars = clean_vars)
      audit_df <- data.frame(
          Operation = names(yaml$audit),
          Count = as.character(unlist(yaml$audit, use.names = FALSE)),
          stringsAsFactors = FALSE
      )
      audit_df <- audit_df[audit_df$Count != "0", , drop = FALSE]
      rownames(audit_df) <- NULL
      if (return_audit) {
          return(list(data = yaml$data, audit = audit_df))
      }
      return(yaml$data)
  }

  if (is.character(sheet) && length(sheet) == 1 && toupper(sheet) == "ALL") {
      if (is_df || is_csv) {
          sheets <- c("Data1")
      } else {
          sheets <- readxl::excel_sheets(file_path)
      }
      all_data <- list()
      all_audits <- list()
      
      for (s in sheets) {
          res <- tryCatch({
              read_messy_panel(file_path, sheet = s, na_strings = na_strings, clean_vars = clean_vars, auto_pivot = auto_pivot, return_audit = TRUE, extract_all_blocks = extract_all_blocks)
          }, error = function(e) list(error = e$message))
          
          if ("data" %in% names(res)) {
              df_s <- res$data
              if (nrow(df_s) > 0) {
                  df_s$source_sheet_name <- s
                  df_s <- df_s[, c("source_sheet_name", setdiff(colnames(df_s), "source_sheet_name")), drop = FALSE]
              }
              all_data[[s]] <- df_s
              all_audits[[s]] <- res$audit
          } else {
              all_audits[[s]] <- list(error = res$error)
          }
      }
      
      if (length(all_data) == 0) {
          stop("Could not parse any sheet in the workbook.")
      }
      
      if (extract_all_blocks) {
          master_df <- unlist(all_data, recursive = FALSE)
          names(master_df) <- make.unique(names(master_df))
      } else {
          master_df <- dplyr::bind_rows(all_data)
      }
      
      if (return_audit) {
          return(list(success = TRUE, data = master_df, audit = all_audits))
      } else {
          return(master_df)
      }
  }
  
  if (is_df || is_csv) {
      sheets <- c("Data1")
  } else {
      sheets <- readxl::excel_sheets(file_path)
  }
  sheets_to_try <- if (!is.null(sheet) && !is_df && !is_csv) sheet else sheets
  
  is_numeric_like <- function(x) {
    if (is.na(x) || x %in% na_strings) return(TRUE) 
    clean_x <- stringr::str_remove_all(x, intToUtf8(160))
    if (grepl("(?i)^\\s*(week|wk|day|month|mo|visit|cycle|period|baseline|follow\\s*up|follow-up)\\s*[-_: ]*\\d+\\s*$", clean_x, perl = TRUE)) return(FALSE)
    if (grepl("(?i)^\\s*(jan|feb|mar|apr|may|jun|jul|aug|sep|sept|oct|nov|dec|january|february|march|april|june|july|august|september|october|november|december)\\.?\\s+\\d{1,2},?\\s+(19|20)\\d{2}\\s*$", clean_x, perl = TRUE)) return(FALSE)
    if (grepl("^\\s*(?:\\d{4}[-/]\\d{1,2}[-/]\\d{1,2}|\\d{1,2}[-/]\\d{1,2}[-/]\\d{2,4})\\s*$", clean_x, perl = TRUE)) return(FALSE)
    if (stringr::str_trim(clean_x) %in% c("-", "\u2013", "\u2014")) return(TRUE)
    if (grepl("^[-\u2014\u2013\u2014]+$", stringr::str_trim(clean_x))) return(TRUE)
    if (grepl("(?i)^(Q[1-4]|H[1-2]|FY[0-9]+)$", stringr::str_trim(clean_x))) return(FALSE)
    
    # Phase 5: Non-standard scientific notation
    clean_x <- stringr::str_replace_all(clean_x, "(?i)\\s*[x\\*]\\s*10\\^([\\-\\+]?[0-9]+)", "E\\1")
    clean_x <- stringr::str_replace(clean_x, "^\\s*\\((.*)\\)\\s*$", "-\\1")
    clean_x <- stringr::str_replace(clean_x, "^-\\s+", "-")
    clean_x <- stringr::str_replace(clean_x, "^\\s*([0-9.,\\s]+?)\\s*-$", "-\\1")
    clean_x <- stringr::str_remove_all(clean_x, "[\\$\\u20ac\\u00a3\\u00a5%\\u5143]")
    
    clean_x <- stringr::str_replace_all(clean_x, "(?<=\\d)[\\s\\u00A0'](?=\\d)", "")
    has_euro_decimal <- grepl(",[0-9]{1,2}[^0-9]*$", clean_x)
    if (!is.na(has_euro_decimal) && has_euro_decimal) {
        clean_x <- stringr::str_replace(stringr::str_remove_all(clean_x, "\\."), ",", ".")
    }
    clean_x <- stringr::str_remove_all(clean_x, ",")
    
    clean_x <- stringr::str_replace(clean_x, "\\s*\\*+\\s*$", "")
    clean_x <- stringr::str_replace(clean_x, "\\s*[\\(\\[].*?[\\)\\]]\\s*$", "")
    clean_x <- stringr::str_replace(clean_x, "\\s*[A-Za-z\u4e00-\u9fa5]+\\s*$", "")
    clean_x <- stringr::str_replace(clean_x, "^\\s*[A-Za-z\u4e00-\u9fa5]+\\s*", "")
    
    !is.na(suppressWarnings(as.numeric(clean_x)))
  }
  
  last_error <- "Could not parse any sheet."
  
  for (s in sheets_to_try) {
    res <- tryCatch({
      preamble_rows_dropped <- 0
      rdb_field_type_rows_dropped <- 0
      if (is_df) {
          raw_data <- as.data.frame(file_path, stringsAsFactors = FALSE)
          if (!all(grepl("^(V|X|Col)[0-9A-Za-z]*$|^\\.\\.\\.[0-9]+$", colnames(raw_data)))) {
              raw_data <- rbind(colnames(raw_data), raw_data)
          }
          colnames(raw_data) <- NULL
      } else if (is_csv) {
          ext <- tolower(tools::file_ext(file_path))
          sep <- if (ext %in% c("tsv", "rdb")) "\t" else ","
          raw_data <- suppressMessages(read_delimited_text_file(file_path, sep))
          attr_preamble <- attr(raw_data, "preamble_rows_dropped", exact = TRUE)
          if (!is.null(attr_preamble)) {
              preamble_rows_dropped <- as.integer(attr_preamble)
          }
      } else {
          raw_data <- suppressMessages(readxl::read_excel(file_path, sheet = s, col_names = FALSE, .name_repair = "minimal", trim_ws = FALSE))
      }
      if (nrow(raw_data) == 0) stop("Empty sheet")
      preamble_strip <- strip_explicit_delimited_preamble(raw_data)
      raw_data <- preamble_strip$data
      preamble_rows_dropped <- preamble_rows_dropped + preamble_strip$count
      if (nrow(raw_data) == 0) stop("Empty sheet")
      rdb_type_strip <- strip_rdb_field_type_row(raw_data)
      raw_data <- rdb_type_strip$data
      rdb_field_type_rows_dropped <- rdb_type_strip$count
      if (nrow(raw_data) == 0) stop("Empty sheet")
      
      raw_mat <- as.matrix(raw_data)
      
      if (extract_all_blocks) {
          empty_cols <- apply(raw_mat, 2, function(col) {
              valid <- col[!is.na(col) & stringr::str_trim(col) != ""]
              length(valid) == 0
          })
          if (any(empty_cols)) {
              col_indices <- seq_len(ncol(raw_mat))
              non_empty_indices <- col_indices[!empty_cols]
              if (length(non_empty_indices) > 0) {
                  gaps <- diff(non_empty_indices)
                  split_points <- which(gaps > 1)
                  if (length(split_points) > 0) {
                      h_blocks <- list()
                      start_idx <- 1
                      for (sp in split_points) {
                          h_blocks <- append(h_blocks, list(raw_mat[, non_empty_indices[start_idx:sp], drop = FALSE]))
                          start_idx <- sp + 1
                      }
                      h_blocks <- append(h_blocks, list(raw_mat[, non_empty_indices[start_idx:length(non_empty_indices)], drop = FALSE]))
                      
                      all_h_data <- list()
                      all_h_audits <- list()
                      for (hb in h_blocks) {
                          hb_df <- as.data.frame(hb, stringsAsFactors = FALSE)
                          hb_res <- tryCatch({
                              read_messy_panel(hb_df, sheet = s, na_strings = na_strings, clean_vars = clean_vars, auto_pivot = auto_pivot, return_audit = TRUE, extract_all_blocks = TRUE)
                          }, error = function(e) list(error = e$message))
                          if ("data" %in% names(hb_res)) {
                              if (is.data.frame(hb_res$data)) {
                                  all_h_data <- append(all_h_data, list(hb_res$data))
                                  all_h_audits <- append(all_h_audits, list(hb_res$audit))
                              } else {
                                  all_h_data <- append(all_h_data, hb_res$data)
                                  all_h_audits <- append(all_h_audits, hb_res$audit)
                              }
                          }
                      }
                      if (length(all_h_data) > 0) {
                          names(all_h_data) <- paste0("HBlock_", seq_along(all_h_data))
                          names(all_h_audits) <- paste0("HBlock_", seq_along(all_h_audits))
                          return(list(success = TRUE, data = all_h_data, audit = all_h_audits))
                      }
                  }
              }
          }
      }
      
      num_counts <- apply(raw_mat, 1, function(row) {
        sum(vapply(row, is_numeric_like, logical(1)) & !is.na(row) & row != "")
      })
      temporal_counts <- apply(raw_mat, 1, function(row) {
        sum(vapply(row, is_temporal_data_signal_value, logical(1)) & !is.na(row) & stringr::str_trim(row) != "")
      })
      data_signal_counts <- num_counts + temporal_counts
      
      is_data_row <- expand_temporal_data_rows(num_counts >= 1, temporal_counts >= 1)
      true_runs_indices <- which(is_data_row == TRUE)
      
      if (length(true_runs_indices) == 0) {
        stop("Could not detect any numeric panel data block.")
      }
      
      gaps <- diff(true_runs_indices)
      block_boundaries <- which(gaps > 5)
      
      if (length(block_boundaries) == 0) {
        blocks <- list(true_runs_indices)
      } else {
        blocks <- list()
        start_idx <- 1
        for (b in block_boundaries) {
          blocks <- append(blocks, list(true_runs_indices[start_idx:b]))
          start_idx <- b + 1
        }
        blocks <- append(blocks, list(true_runs_indices[start_idx:length(true_runs_indices)]))
      }
      
      if (!extract_all_blocks) {
        block_lengths <- vapply(blocks, length, integer(1))
        blocks_to_process <- list(blocks[[which.max(block_lengths)]])
      } else {
        blocks_to_process <- blocks
      }
      
      block_results <- lapply(blocks_to_process, function(current_block) {
        withCallingHandlers({
        audit_log <- list()
        if (preamble_rows_dropped > 0) {
            audit_log[["Delimited Preamble Rows Dropped"]] <- preamble_rows_dropped
        }
        if (rdb_field_type_rows_dropped > 0) {
            audit_log[["RDB Field Type Rows Dropped"]] <- rdb_field_type_rows_dropped
        }
        start_data_row <- min(current_block)
        end_data_row <- max(current_block)
      
      main_block_counts <- data_signal_counts[start_data_row:end_data_row]
      mode_count <- as.numeric(names(sort(table(main_block_counts), decreasing = TRUE)[1]))
      
      # We know header must be above true_start. We walk up looking for a header boundary.
      true_start <- start_data_row
      max_walk_up <- 2
      walked <- 0
      while (true_start > 1 && walked < max_walk_up) {
          non_empty <- sum(!is.na(raw_mat[true_start - 1, ]) & stringr::str_trim(raw_mat[true_start - 1, ]) != "")
          looks_like_header <- FALSE
          if (non_empty > 0) {
              looks_character <- stringr::str_trim(raw_mat[true_start - 1, !is.na(raw_mat[true_start - 1, ])]) != "" &
                                 !vapply(raw_mat[true_start - 1, !is.na(raw_mat[true_start - 1, ])], is_numeric_like, logical(1))
              if (sum(looks_character) >= (length(looks_character) * 0.5)) {
                  looks_like_header <- TRUE
              }
          }
          if (non_empty > 0 && data_signal_counts[true_start - 1] == 0 && !looks_like_header) {
              true_start <- true_start - 1
              walked <- walked + 1
          } else {
              break
          }
      }
      
      density_threshold <- max(1, floor(mode_count * 0.2))
      
      valid_in_block <- which(main_block_counts >= density_threshold)
      if (length(valid_in_block) == 0) {
          true_start <- start_data_row
          true_end <- end_data_row
      } else {
          true_start <- start_data_row
          true_end <- start_data_row + max(valid_in_block) - 1
      }
      
      while (true_start <= end_data_row) {
          if (data_signal_counts[true_start] < density_threshold) {
              true_start <- true_start + 1
              next
          }
          
          row_vals <- raw_mat[true_start, ]
          num_vals <- suppressWarnings(as.numeric(row_vals[!is.na(row_vals) & row_vals != ""]))
          valid_nums <- num_vals[!is.na(num_vals)]
          is_year_header <- length(valid_nums) >= 2 && all(valid_nums >= 1900 & valid_nums <= 2100)
          
          if (is_year_header) {
              true_start <- true_start + 1
              next
          }
          
          non_empty_current <- sum(!is.na(row_vals) & row_vals != "")
          non_empty_above <- if (true_start > 1) sum(!is.na(raw_mat[true_start-1, ]) & raw_mat[true_start-1, ] != "") else 0
          
          if (true_start > 1 && non_empty_above > 0 && (non_empty_above < non_empty_current * 0.3)) {
              true_start <- true_start + 1
              next
          }
          
          break
      }
      
      if (true_start > end_data_row) true_start <- end_data_row
      
      header_row_index <- max(1, true_start - 1)
      audit_log[["Decoy Rows Bypassed"]] <- header_row_index - 1
      
      extracted_metadata <- list()
      if (header_row_index > 1) {
          decoy_mat <- raw_mat[1:(header_row_index - 1), , drop = FALSE]
          for (r in seq_len(nrow(decoy_mat))) {
              row_vals <- decoy_mat[r, ]
              valid_vals <- row_vals[!is.na(row_vals) & stringr::str_trim(row_vals) != ""]
              
              if (length(valid_vals) == 1) {
                  val <- valid_vals[1]
                  if (grepl(":", val)) {
                      parts <- strsplit(val, ":")[[1]]
                      if (length(parts) == 2) {
                          key <- stringr::str_trim(parts[1])
                          val <- stringr::str_trim(parts[2])
                          extracted_metadata[[key]] <- val
                      }
                  }
              } else if (length(valid_vals) == 2) {
                  key <- stringr::str_trim(valid_vals[1])
                  val <- stringr::str_trim(valid_vals[2])
                  if (nchar(key) < 50) {
                      key <- stringr::str_replace(key, ":\\s*$", "")
                      extracted_metadata[[key]] <- val
                  }
              }
          }
      }
      
      if (header_row_index == true_start) {
          true_start <- true_start + 1
      }
      # Helper: detect rows that are pure noise (random-looking uppercase/lowercase strings
      # with no semantic value, e.g. "xdasdad", "WEDEWADAW"). Such rows should NOT be
      # concatenated onto real column names.
      is_noise_header_row <- function(row_vals) {
          non_empty <- row_vals[!is.na(row_vals) & stringr::str_trim(row_vals) != ""]
          if (length(non_empty) == 0) return(FALSE)
          
          # NEW: If any cell contains a URL, email, or long prose sentence -> metadata row
          has_url_or_email <- any(vapply(non_empty, function(x) {
              grepl("https?://|www\\.|@[a-zA-Z0-9]+\\.[a-zA-Z]{2,}|[A-Za-z]{10,}\\s[A-Za-z]{6,}\\s[A-Za-z]{4,}", x)
          }, logical(1)))
          if (has_url_or_email) return(TRUE)
          
          looks_random <- vapply(non_empty, function(x) {
              x <- stringr::str_trim(x)
              if (!grepl("^[A-Za-z]+$", x)) return(FALSE)
              if (nchar(x) <= 4) return(FALSE)
              if (grepl("^[A-Z][a-z]", x)) return(FALSE)
              TRUE
          }, logical(1))
          all(looks_random)
      }

      # Determine how far back to search for headers for this block
      search_start <- header_row_index
      while (search_start > 1) {
          non_empty <- sum(!is.na(raw_mat[search_start - 1, ]) & stringr::str_trim(raw_mat[search_start - 1, ]) != "")
          if (non_empty > 0) {
              search_start <- search_start - 1
          } else {
              break # Stop at empty row
          }
      }
      
      header_rows_list <- list()
      for (r in search_start:header_row_index) {
          row_vals <- raw_mat[r, ]
          non_empty_count <- sum(!is.na(row_vals) & stringr::str_trim(row_vals) != "")
          if (non_empty_count > 1 || r == header_row_index) {
              if (r != header_row_index && is_noise_header_row(row_vals)) {
                  audit_log[["Noise Header Rows Discarded"]] <-
                      c(audit_log[["Noise Header Rows Discarded"]], r)
                  next
              }
              header_rows_list <- append(header_rows_list, list(row_vals))
          }
      }
      
      if (length(header_rows_list) > 0) {
          for (i in seq_along(header_rows_list)) {
              h_row <- header_rows_list[[i]]
              if (length(unique(h_row[!is.na(h_row) & stringr::str_trim(h_row) != ""])) >= 1) {
                  for (j in 2:length(h_row)) {
                      if ((is.na(h_row[j]) || stringr::str_trim(h_row[j]) == "") && 
                          !is.na(h_row[j-1]) && stringr::str_trim(h_row[j-1]) != "") {
                          h_row[j] <- h_row[j-1]
                      }
                  }
                  header_rows_list[[i]] <- h_row
              }
          }
          
          headers <- rep("", length(header_rows_list[[1]]))
          for (i in seq_along(header_rows_list)) {
              h_row <- header_rows_list[[i]]
              valid_mask <- !is.na(h_row) & stringr::str_trim(h_row) != ""
              headers <- ifelse(valid_mask,
                                ifelse(is.na(headers) | headers == "", h_row, paste0(headers, "_", h_row)),
                                headers)
          }
          headers <- ifelse(is.na(headers) | headers == "", NA, headers)
          
          # Guard: if any header name exceeds 80 chars, it contains stitched metadata.
          # Recover by using only the LAST segment (the actual column label) after splitting on "_".
          max_col_name_len <- 80
          headers <- vapply(headers, function(h) {
              if (is.na(h)) return(NA_character_)
              if (nchar(h) > max_col_name_len) {
                  parts <- strsplit(h, "_")[[1]]
                  # Walk back from end to find a segment that is a plausible column name (<= 50 chars)
                  for (k in rev(seq_along(parts))) {
                      candidate <- paste(parts[k:length(parts)], collapse = "_")
                      if (nchar(candidate) <= max_col_name_len && nchar(stringr::str_trim(candidate)) > 0) {
                          return(candidate)
                      }
                  }
                  # Last resort: truncate to 80 chars
                  return(substr(h, nchar(h) - max_col_name_len + 1, nchar(h)))
              }
              as.character(h)
          }, character(1))
      } else {
          headers <- raw_mat[header_row_index, ]
      }
      
      data_block <- raw_mat[true_start:true_end, , drop = FALSE]
      
      block_counts <- data_signal_counts[true_start:true_end]
      internal_valid_rows <- (block_counts >= density_threshold)
      
      # Hierarchical Section Header Propagation
      section_categories <- rep(NA, nrow(data_block))
      has_sections <- FALSE
      
      for (i in seq_len(nrow(data_block))) {
          if (block_counts[i] == 0) {
              row_vals <- data_block[i, ]
              non_empty <- which(!is.na(row_vals) & stringr::str_trim(row_vals) != "")
              if (length(non_empty) == 1 && non_empty[1] <= 3) {
                  section_categories[i] <- stringr::str_trim(row_vals[non_empty[1]])
                  has_sections <- TRUE
              }
          }
      }
      
      if (has_sections) {
          current_section <- NA
          for (i in seq_len(length(section_categories))) {
              if (!is.na(section_categories[i])) {
                  current_section <- section_categories[i]
              } else {
                  section_categories[i] <- current_section
              }
          }
      }
      
      if (has_sections) {
          valid_sections <- section_categories[internal_valid_rows]
      }
      
      data_block <- data_block[internal_valid_rows, , drop = FALSE]
      
      empty_cols <- apply(data_block, 2, function(col) all(is.na(col) | col == "" | col %in% na_strings)) & 
                    (is.na(headers) | headers == "")
                    
      headers <- headers[!empty_cols]
      data_block <- data_block[, !empty_cols, drop = FALSE]
      
      # Intercept and remove repeating page headers
      is_repeated_header <- apply(data_block, 1, function(row) {
          match_count <- sum(stringr::str_trim(row) == stringr::str_trim(headers), na.rm = TRUE)
          match_count >= max(1, length(headers) - 1)
      })
      
      if (any(is_repeated_header)) {
          data_block <- data_block[!is_repeated_header, , drop = FALSE]
          if (has_sections) {
              valid_sections <- valid_sections[!is_repeated_header]
          }
      }
      
      df <- as.data.frame(data_block, stringsAsFactors = FALSE)
      
      if (has_sections) {
          df$section_category <- valid_sections
          headers <- c(headers, "section_category")
      }
      
      # Phase 16: Indentation Hierarchy Extraction
      first_col <- df[[1]]
      if (is.character(first_col)) {
          valid_idx <- which(!is.na(first_col) & first_col != "")
          if (length(valid_idx) > 0) {
              valid_vals <- first_col[valid_idx]
              num_leading_spaces <- nchar(valid_vals) - nchar(stringr::str_trim(valid_vals, side = "left"))
              
              if (max(num_leading_spaces) > 0 && min(num_leading_spaces) == 0 && length(unique(num_leading_spaces)) > 1) {
                  parent_category <- rep(NA, nrow(df))
                  current_parent <- NA
                  
                  for (r in seq_len(nrow(df))) {
                      val <- first_col[r]
                      if (!is.na(val) && val != "") {
                          spaces <- nchar(val) - nchar(stringr::str_trim(val, side = "left"))
                          if (spaces == 0) {
                              current_parent <- stringr::str_trim(val)
                          }
                          parent_category[r] <- current_parent
                      } else {
                          parent_category[r] <- current_parent
                      }
                  }
                  
                  extracted_count <- sum(!is.na(parent_category) & parent_category != stringr::str_trim(first_col), na.rm = TRUE)
                  if (extracted_count > 0) {
                      df$parent_category <- parent_category
                      headers <- c(headers, "parent_category")
                      audit_log[["Indentation Hierarchy Extracted"]] <- extracted_count
                  }
              }
          }
      }
      
      # Phase 6: Forward-fill leading character columns (Staircase Ledgers)
      for (c in 1:min(2, ncol(df))) {
          if (is_geographic_subdivision_column(colnames(df)[c])) next
          col_vals <- as.character(df[[c]])
          valid_vals <- col_vals[!is.na(col_vals) & stringr::str_trim(col_vals) != ""]
          if (length(valid_vals) > 0) {
              num_count <- sum(vapply(valid_vals, is_numeric_like, logical(1)))
              if (num_count < length(valid_vals) * 0.5) {
                  filled_col <- col_vals
                  last_val <- NA
                  for (r in seq_along(filled_col)) {
                      if (!is.na(filled_col[r]) && stringr::str_trim(filled_col[r]) != "") {
                          last_val <- filled_col[r]
                      } else if (!is.na(last_val)) {
                          filled_col[r] <- last_val
                      }
                  }
                  df[[c]] <- filled_col
              }
          }
      }
      
      # Phase 5: Amputate Mid-Table Subtotals
      subtotal_keywords <- c("subtotal", "\u5c0f\u8ba1", "total:", "sum:", "gesamt", "summe", "somme", "promedio")
      first_col_lower <- stringr::str_trim(tolower(as.character(df[[1]])))
      subtotal_idx <- vapply(first_col_lower, function(val) {
          if (is.na(val)) return(FALSE)
          any(vapply(subtotal_keywords, function(k) grepl(k, val, fixed = TRUE), logical(1)))
      }, logical(1))
      if (any(subtotal_idx)) {
          audit_log[["Mid-Table Subtotals Amputated"]] <- sum(subtotal_idx)
          df <- df[!subtotal_idx, , drop = FALSE]
          # Recalculate first_col_lower for the next steps
          first_col_lower <- stringr::str_trim(tolower(as.character(df[[1]])))
      }
      
      # Phase 6: Footnote Amputator (Trailing long-string rows)
      tail_n <- min(5, nrow(df))
      footnotes_dropped <- 0
      if (tail_n > 0) {
          rows_to_keep <- rep(TRUE, nrow(df))
          for (r in seq(nrow(df) - tail_n + 1, nrow(df))) {
              val1 <- as.character(df[r, 1])
              if (!is.na(val1) && nchar(stringr::str_trim(val1)) > 15) {
                  other_cols_empty <- TRUE
                  if (ncol(df) > 1) {
                      other_vals <- as.character(df[r, 2:ncol(df)])
                      if (any(!is.na(other_vals) & stringr::str_trim(other_vals) != "")) {
                          other_cols_empty <- FALSE
                      }
                  }
                  if (other_cols_empty) {
                      rows_to_keep[r] <- FALSE
                      footnotes_dropped <- footnotes_dropped + 1
                  }
              }
          }
          df <- df[rows_to_keep, , drop = FALSE]
      }
      audit_log[["Footnotes Dropped"]] <- footnotes_dropped
      
      # Phase 4: Amputate Trailing Aggregation Rows (Ghost Bottoms)
      tail_n <- min(5, nrow(df))
      if (tail_n > 0) {
          agg_keywords <- c("total", "sum", "average", "avg", "\u5408\u8ba1", "\u603b\u8ba1", "\u5e73\u5747", "mean", "gesamt", "summe", "durchschnitt", "moyenne", "somme", "promedio")
          for (r in seq(nrow(df) - tail_n + 1, nrow(df))) {
              if (!is.na(first_col_lower[r]) && any(vapply(agg_keywords, function(k) grepl(paste0("^", k), first_col_lower[r]), logical(1)))) {
                  audit_log[["Ghost Bottom Rows Dropped"]] <- nrow(df) - r + 1
                  if (r > 1) {
                      df <- df[1:(r-1), , drop = FALSE]
                  } else {
                      df <- df[0, , drop = FALSE]
                  }
                  break
              }
          }
      }
      
      # Phase 7: Sanitize raw headers (remove \n and \r)
      headers <- vapply(headers, function(x) {
          if (!is.na(x)) {
              x <- gsub("[\r\n]+", " ", x)
              x <- stringr::str_squish(x)
          }
          x
      }, character(1))
      
      # Phase 11: Orphaned Header Re-Alignment
      if (length(headers) > 0 && (is.na(headers[1]) || headers[1] == "")) {
          headers[1] <- "Category"
      }
      
      colnames(df) <- make.unique(stringr::str_trim(headers), sep = "_")
      
      # Phase 7: Amputate ALL Aggregation Columns (Embedded Subtotals)
      col_agg_keywords <- c("total", "sum", "subtotal", "ytd", "\u5408\u8ba1", "\u603b\u8ba1", "\u5c0f\u8ba1", "average", "avg", "gesamt", "summe", "durchschnitt", "moyenne", "somme", "promedio")
      cols_to_keep <- rep(TRUE, ncol(df))
      subtotal_cols_dropped <- c()
      for (c in seq_len(ncol(df))) {
          col_name <- tolower(colnames(df)[c])
          if (is_aggregation_summary_column(col_name, col_agg_keywords)) {
              if (c > 1) { # Protect the first column
                  cols_to_keep[c] <- FALSE
                  subtotal_cols_dropped <- c(subtotal_cols_dropped, col_name)
              }
          }
      }
      audit_log[["Subtotal Columns Amputated"]] <- length(subtotal_cols_dropped)
      df <- df[, cols_to_keep, drop = FALSE]
      
      # Phase 11: Phantom Column Purge (Information Density ~ 0)
      phantom_cols <- vapply(df, function(col) {
          valid_vals <- col[!is.na(col) & stringr::str_trim(col) != "" & !(col %in% na_strings)]
          length(valid_vals) == 0
      }, logical(1))
      no_header <- is.na(colnames(df)) | grepl("^(na|x|\\.\\.\\.)[_0-9]*$|^$", tolower(colnames(df)))
      if (any(phantom_cols & no_header)) {
          audit_log[["Phantom Columns Purged"]] <- sum(phantom_cols & no_header)
          df <- df[, !(phantom_cols & no_header), drop = FALSE]
      }
      
      # (section_category assigned earlier)
      
      # Forward-fill NAs in leading character columns (Handling Merged Cells)
      for (j in seq_len(ncol(df))) {
          if (is_geographic_subdivision_column(colnames(df)[j])) next
          col_vals <- df[[j]]
          valid_idx <- which(!is.na(col_vals) & col_vals != "")
          valid_count <- length(valid_idx)
          
          if (valid_count > 0) {
              num_likes <- sum(vapply(col_vals[valid_idx], is_numeric_like, logical(1)))
              if ((num_likes / valid_count) < 0.5) {
                  if (sum(is.na(col_vals) | col_vals == "") > 0) {
                      for (r in 2:nrow(df)) {
                          if (is.na(df[r, j]) || df[r, j] == "") {
                              df[r, j] <- df[r - 1, j]
                          }
                      }
                  }
              } else {
                  break
              }
          } else {
              break
          }
      }
      
      df[] <- lapply(df, function(x) {
        x[x %in% na_strings] <- NA
        x
      })
      
      df[] <- lapply(seq_along(df), function(i) {
        x <- df[[i]]
        cn <- colnames(df)[i]
        if (is_time_of_day_column(cn) && is_mostly_time_of_day(x)) {
            cleaned_time <- stringr::str_trim(as.character(x))
            cleaned_time[cleaned_time == ""] <- NA_character_
            return(cleaned_time)
        }
        if (is_compact_time_code_column(cn, x)) {
            cleaned_code <- stringr::str_trim(as.character(x))
            cleaned_code[cleaned_code == ""] <- NA_character_
            return(cleaned_code)
        }
        if (is_postal_code_column(cn)) {
            cleaned_postal <- stringr::str_trim(as.character(x))
            cleaned_postal[cleaned_postal == ""] <- NA_character_
            return(cleaned_postal)
        }
        clean_x <- stringr::str_remove_all(x, intToUtf8(160))
        
        # Phase 4: Convert Accounting Zeros to 0
        x_trimmed <- stringr::str_trim(clean_x)
        dash_idx <- x_trimmed %in% c("-", "\u2013", "\u2014")
        if (any(dash_idx, na.rm = TRUE)) {
            clean_x[which(dash_idx)] <- "0"
        }
        
        # Phase 5: Non-standard scientific notation
        clean_x <- stringr::str_replace_all(clean_x, "(?i)\\s*[x\\*]\\s*10\\^([\\-\\+]?[0-9]+)", "E\\1")
        
        clean_x <- stringr::str_replace(clean_x, "^\\s*\\((.*)\\)\\s*$", "-\\1")
        clean_x <- stringr::str_replace(clean_x, "^-\\s+", "-")
        clean_x <- stringr::str_replace(clean_x, "^\\s*([0-9.,\\s]+?)\\s*-$", "-\\1")
        is_pct <- grepl("%\\s*$", clean_x) & !is.na(clean_x)
        
        clean_x <- stringr::str_remove_all(clean_x, "[\\$\\u20ac\\u00a3\\u00a5%\\u5143]")
        clean_x <- stringr::str_trim(clean_x)
        
        clean_x <- stringr::str_replace_all(clean_x, "(?<=\\d)[\\s\\u00A0'](?=\\d)", "")
        has_euro_decimal <- grepl(",[0-9]{1,2}[^0-9]*$", clean_x)
        clean_x <- ifelse(has_euro_decimal & !is.na(has_euro_decimal), 
                          stringr::str_replace(stringr::str_remove_all(clean_x, "\\."), ",", "."), 
                          clean_x)
        clean_x <- stringr::str_remove_all(clean_x, ",")
        
        # Phase 11: Semantic Multiplier Engine
        stat_flag_idx <- grepl("(?i)(?<=[0-9])\\s+[bcdefnprsuwz]{1,3}\\s*$", clean_x, perl = TRUE) & !is.na(clean_x)
        clean_x[stat_flag_idx] <- stringr::str_replace(clean_x[stat_flag_idx], "(?i)(?<=[0-9])\\s+[bcdefnprsuwz]{1,3}\\s*$", "")
        multiplier <- rep(1, length(clean_x))
        k_idx <- grepl("(?i)[-0-9.]+\\s*(k|\u5343)$", clean_x) & !is.na(clean_x)
        wan_idx <- grepl("(?i)[-0-9.]+\\s*(w|wan|\u4e07)$", clean_x) & !is.na(clean_x)
        m_idx <- grepl("(?i)[-0-9.]+\\s*(m|mil|million)$", clean_x) & !is.na(clean_x)
        yi_idx <- grepl("(?i)[-0-9.]+\\s*(y|yi|\u4ebf)$", clean_x) & !is.na(clean_x)
        b_idx <- grepl("(?i)[-0-9.]+\\s*(b|bn|billion)$", clean_x) & !is.na(clean_x)
        t_idx <- grepl("(?i)[-0-9.]+\\s*(t|tn|trillion)$", clean_x) & !is.na(clean_x)
        
        multiplier[k_idx] <- 1000
        multiplier[wan_idx] <- 10000
        multiplier[m_idx] <- 1000000
        multiplier[yi_idx] <- 100000000
        multiplier[b_idx] <- 1000000000
        multiplier[t_idx] <- 1000000000000
        
        clean_x <- stringr::str_replace(clean_x, "(?i)\\s*(k|\u5343|w|wan|\u4e07|m|mil|million|y|yi|\u4ebf|b|bn|billion|t|tn|trillion)$", "")
        
        clean_x <- stringr::str_replace(clean_x, "\\s*\\*+\\s*$", "")
        clean_x <- stringr::str_replace(clean_x, "\\s*[\\(\\[].*?[\\)\\]]\\s*$", "")
        
        # Exclude quarters from being stripped and converted
        is_quarter <- grepl("(?i)^(Q[1-4]|H[1-2]|FY[0-9]+)$", stringr::str_trim(x))
        is_month_name_date <- grepl("(?i)^\\s*(jan|feb|mar|apr|may|jun|jul|aug|sep|sept|oct|nov|dec|january|february|march|april|june|july|august|september|october|november|december)\\.?\\s+\\d{1,2},?\\s+(19|20)\\d{2}\\s*$", stringr::str_trim(x))
        
        strip_alpha_idx <- !is_quarter & !is_month_name_date
        clean_x[strip_alpha_idx] <- stringr::str_replace(clean_x[strip_alpha_idx], "\\s*[A-Za-z\u4e00-\u9fa5]+\\s*$", "")
        clean_x[strip_alpha_idx] <- stringr::str_replace(clean_x[strip_alpha_idx], "^\\s*[A-Za-z\u4e00-\u9fa5]+\\s*", "")
        
        num_x <- suppressWarnings(as.numeric(clean_x))
        
        if(sum(is.na(num_x)) == sum(is.na(x))) {
            num_x[is_pct & !is.na(num_x)] <- num_x[is_pct & !is.na(num_x)] / 100
            valid_num_idx <- !is.na(num_x)
            num_x[valid_num_idx] <- num_x[valid_num_idx] * multiplier[valid_num_idx]
            return(num_x)
        } 
        
        final_str <- stringr::str_replace(x, "^\\s*[\\.-]+\\s*", "")
        return(stringr::str_trim(final_str))
      })
      
      if (clean_vars) {
        df <- clean_variable_names(df)
      } else {
        colnames(df) <- tolower(colnames(df))
      }
      geo_repair <- repair_spurious_geographic_subdivision_fill(df)
      df <- geo_repair$data
      if (geo_repair$count > 0) {
          audit_log[["Geographic Subdivision Fill Repaired"]] <- geo_repair$count
      }
      index_drop <- drop_export_index_column(df)
      df <- index_drop$data
      if (!is.na(index_drop$name)) {
          audit_log[["Export Index Column Dropped"]] <- index_drop$name
      }
      
      # Phase 15: Common Prefix Stripping
      # If >=75% of columns share a long common prefix (>=15 chars), strip it.
      # This handles WB-style repeated dataset labels in headers.
      cnames_for_prefix <- colnames(df)
      if (length(cnames_for_prefix) >= 3) {
          # Find longest common prefix among all column names
          find_common_prefix <- function(strs) {
              if (length(strs) == 0) return("")
              ref <- strsplit(strs[1], "")[[1]]
              for (s in strs[-1]) {
                  chars <- strsplit(s, "")[[1]]
                  common_len <- min(length(ref), length(chars))
                  mismatch <- which(ref[1:common_len] != chars[1:common_len])
                  if (length(mismatch) > 0) {
                      ref <- ref[1:(mismatch[1] - 1)]
                  } else {
                      ref <- ref[1:common_len]
                  }
              }
              paste(ref, collapse = "")
          }
          common_pfx <- find_common_prefix(cnames_for_prefix)
          # Only strip if prefix is meaningfully long (>=15 chars) and ends on a word boundary
          if (nchar(common_pfx) >= 15) {
              common_pfx <- sub("_+$", "", common_pfx)  # trim trailing underscores
              common_pfx <- paste0(common_pfx, "_")     # re-add one separator
              stripped <- sub(paste0("^", common_pfx), "", cnames_for_prefix)
              # Only apply if all stripped names are non-empty
              if (all(nchar(stripped) > 0)) {
                  colnames(df) <- stripped
                  audit_log[["Common Column Prefix Stripped"]] <- common_pfx
              }
          }
      }
      
      if (auto_pivot && nrow(df) > 0) {
          cnames <- colnames(df)
          temporal_pattern <- "^(19|20)[0-9]{2}(_q[1-4]|_h[1-2]|_[0-1]?[0-9]|_[a-z]{3})?$|^(q[1-4]|h[1-2]|fy[0-9]+)$|^[a-z]{3}_([0-9]{2}|(19|20)[0-9]{2})$|^[0-9]{1,2}_[0-9]{1,2}_[0-9]{2,4}$"
          is_temporal <- grepl(temporal_pattern, cnames)
          
          if (sum(is_temporal) >= 2) {
              id_cols <- cnames[!is_temporal]
              temporal_cols <- cnames[is_temporal]
              
              # Base R melt implementation
              long_list <- lapply(temporal_cols, function(tc) {
                  sub_df <- df[, id_cols, drop = FALSE]
                  sub_df$time_period <- tc
                  sub_df$value <- df[[tc]]
                  return(sub_df)
              })
              
              df <- do.call(rbind, long_list)
              rownames(df) <- NULL
              audit_log[["Auto-Pivot Wide to Long"]] <- sum(is_temporal)
          }
      }
      
      if (length(extracted_metadata) > 0) {
          attr(df, "metadata") <- extracted_metadata
          audit_log[["Metadata Keys Extracted"]] <- length(extracted_metadata)
      }
      
      list(success = TRUE, data = df, audit = audit_log)
      }, error = function(e) {
          print(sys.calls())
          stop(e)
      }) # end withCallingHandlers
      }) # End lapply over blocks
      
      if (!extract_all_blocks) {
          res_final <- block_results[[1]]
      } else {
          data_list <- lapply(block_results, `[[`, "data")
          audit_list <- lapply(block_results, `[[`, "audit")
          names(data_list) <- paste0("Block_", seq_along(data_list))
          names(audit_list) <- paste0("Block_", seq_along(audit_list))
          res_final <- list(success = TRUE, data = data_list, audit = audit_list)
      }
      res_final
    }, error = function(e) {
      list(success = FALSE, error = e$message)
    })
    
    if (res$success) {
      if (return_audit) {
          if (extract_all_blocks) {
              audit_dfs <- lapply(res$audit, function(aud) {
                  df <- data.frame(Operation = names(aud), Count = as.character(unlist(aud, use.names = FALSE)), stringsAsFactors = FALSE)
                  df[df$Count != "0", , drop = FALSE]
              })
              return(list(data = res$data, audit = audit_dfs))
          } else {
              audit_df <- data.frame(
                  Operation = names(res$audit),
                  Count = as.character(unlist(res$audit, use.names = FALSE)),
                  stringsAsFactors = FALSE
              )
              audit_df <- audit_df[audit_df$Count != "0", , drop = FALSE]
              rownames(audit_df) <- NULL
              return(list(data = res$data, audit = audit_df))
          }
      }
      return(res$data)
    } else {
      last_error <- res$error
    }
  }
  
  stop(paste("Failed to parse any valid panel from file.", last_error))
}

is_geographic_subdivision_column <- function(name) {
  if (is.na(name) || name == "") return(FALSE)
  grepl("(^|[/_ .-])(province|state|county|admin)([/_ .-]|$)", name, ignore.case = TRUE)
}

strip_explicit_delimited_preamble <- function(raw_data) {
  if (!is.data.frame(raw_data) || nrow(raw_data) < 2 || ncol(raw_data) < 1) {
      return(list(data = raw_data, count = 0))
  }
  first_col <- stringr::str_trim(as.character(raw_data[[1]]))
  marker_idx <- which(grepl("^-+\\s*end\\s+header\\s*-+$", first_col, ignore.case = TRUE, perl = TRUE))
  if (length(marker_idx) == 0) {
      return(list(data = raw_data, count = 0))
  }
  idx <- marker_idx[1]
  if (idx >= nrow(raw_data)) {
      return(list(data = raw_data, count = 0))
  }
  if (ncol(raw_data) == 1) {
      remaining_lines <- as.character(raw_data[[1]][(idx + 1):nrow(raw_data)])
      remaining_lines <- remaining_lines[!is.na(remaining_lines) & remaining_lines != ""]
      if (length(remaining_lines) == 0) {
          return(list(data = raw_data, count = 0))
      }
      sep <- detect_text_separator(remaining_lines)
      parsed <- utils::read.csv(
          text = paste(remaining_lines, collapse = "\n"),
          header = FALSE, sep = sep, stringsAsFactors = FALSE,
          na.strings = NULL, colClasses = "character", strip.white = FALSE,
          fill = TRUE, blank.lines.skip = FALSE
      )
      return(list(data = parsed, count = idx))
  }
  next_vals <- stringr::str_trim(as.character(raw_data[idx + 1, , drop = TRUE]))
  next_vals <- next_vals[!is.na(next_vals) & next_vals != ""]
  if (length(next_vals) < 2) {
      return(list(data = raw_data, count = 0))
  }
  list(data = raw_data[(idx + 1):nrow(raw_data), , drop = FALSE], count = idx)
}

read_delimited_text_file <- function(path, sep_hint = ",") {
  lines <- readLines(path, warn = FALSE)
  comment_strip <- strip_hash_comment_lines(lines)
  lines <- comment_strip$lines
  stripped <- strip_explicit_preamble_lines(lines)
  lines <- stripped$lines
  sep <- if (stripped$count > 0) detect_text_separator(lines) else sep_hint
  parsed <- utils::read.csv(
      text = paste(lines, collapse = "\n"),
      header = FALSE, sep = sep, stringsAsFactors = FALSE,
      na.strings = NULL, colClasses = "character", strip.white = FALSE,
      fill = TRUE, blank.lines.skip = FALSE
  )
  attr(parsed, "preamble_rows_dropped") <- comment_strip$count + stripped$count
  parsed
}

strip_hash_comment_lines <- function(lines) {
  if (length(lines) == 0) {
      return(list(lines = lines, count = 0))
  }
  is_comment <- grepl("^\\s*#", lines)
  keep <- !is_comment
  list(lines = lines[keep], count = sum(is_comment))
}

strip_explicit_preamble_lines <- function(lines) {
  if (length(lines) < 2) {
      return(list(lines = lines, count = 0))
  }
  marker_idx <- which(grepl("^-+\\s*end\\s+header\\s*-+$", stringr::str_trim(lines), ignore.case = TRUE, perl = TRUE))
  if (length(marker_idx) > 0 && marker_idx[1] < length(lines)) {
      remaining <- lines[(marker_idx[1] + 1):length(lines)]
      if (length(remaining) > 0 && max(vapply(gregexpr(",", remaining, fixed = TRUE), function(m) {
          if (length(m) == 1 && m[1] == -1) 0L else length(m)
      }, integer(1))) > 0) {
          return(list(lines = remaining, count = marker_idx[1]))
      }
  }

  metadata_strip <- strip_blank_delimited_metadata_preamble_lines(lines)
  if (metadata_strip$count > 0) {
      return(metadata_strip)
  }

  list(lines = lines, count = 0)
}

strip_blank_delimited_metadata_preamble_lines <- function(lines) {
  if (length(lines) < 4) {
      return(list(lines = lines, count = 0))
  }
  trimmed <- stringr::str_trim(lines)
  blank_idx <- which(trimmed == "")
  blank_idx <- blank_idx[blank_idx < length(lines)]
  if (length(blank_idx) == 0) {
      return(list(lines = lines, count = 0))
  }

  for (idx in blank_idx) {
      candidate_start <- idx + 1
      while (candidate_start <= length(lines) && trimmed[candidate_start] == "") {
          candidate_start <- candidate_start + 1
      }
      if (candidate_start >= length(lines)) next

      leading_nonblank <- sum(trimmed[seq_len(candidate_start - 1)] != "")
      if (leading_nonblank == 0 || leading_nonblank > 10) next

      remaining <- lines[candidate_start:length(lines)]
      remaining_nonblank <- remaining[stringr::str_trim(remaining) != ""]
      if (length(remaining_nonblank) < 2) next

      sep <- detect_text_separator(remaining_nonblank)
      header <- stringr::str_trim(split_delimited_fields(remaining_nonblank[1], sep))
      header <- header[header != ""]
      if (length(header) < 2) next

      data_lines <- remaining_nonblank[-1]
      check_n <- min(5, length(data_lines))
      data_fields <- lapply(data_lines[seq_len(check_n)], split_delimited_fields, sep = sep)
      width_ok <- mean(vapply(data_fields, function(fields) {
          length(fields) >= max(2, length(header) - 1)
      }, logical(1))) >= 0.8
      values <- stringr::str_trim(unlist(data_fields, use.names = FALSE))
      has_data_signal <- any(vapply(values, function(value) {
          is_delimited_data_signal_value(value)
      }, logical(1)))
      has_temporal_header <- any(grepl("(^time$|date|timestamp|^year$|^month$|^day$)", tolower(header)))

      if (width_ok && has_data_signal && (has_temporal_header || length(data_lines) >= leading_nonblank)) {
          return(list(lines = remaining, count = candidate_start - 1))
      }
  }

  list(lines = lines, count = 0)
}

split_delimited_fields <- function(line, sep) {
  if (length(line) == 0 || is.na(line)) {
      return(character(0))
  }
  parsed <- tryCatch(
      utils::read.csv(
          text = line, header = FALSE, sep = sep, stringsAsFactors = FALSE,
          na.strings = NULL, colClasses = "character", strip.white = FALSE,
          fill = TRUE, blank.lines.skip = FALSE, comment.char = ""
      ),
      error = function(e) NULL
  )
  if (is.null(parsed) || nrow(parsed) == 0) {
      return(strsplit(line, sep, fixed = TRUE)[[1]])
  }
  as.character(parsed[1, , drop = TRUE])
}

is_delimited_data_signal_value <- function(value) {
  if (length(value) == 0 || is.na(value)) {
      return(FALSE)
  }
  value <- stringr::str_trim(as.character(value))
  if (value == "") {
      return(FALSE)
  }
  if (grepl("^(19|20)[0-9]{2}-[0-9]{2}-[0-9]{2}", value)) {
      return(TRUE)
  }
  numeric_value <- suppressWarnings(as.numeric(gsub(",", "", value, fixed = TRUE)))
  !is.na(numeric_value)
}

is_temporal_data_signal_value <- function(value) {
  if (length(value) == 0 || is.na(value)) {
      return(FALSE)
  }
  value <- stringr::str_trim(as.character(value))
  if (value == "") {
      return(FALSE)
  }
  grepl("^(19|20)[0-9]{2}[-/][0-9]{1,2}[-/][0-9]{1,2}$", value, perl = TRUE)
}

expand_temporal_data_rows <- function(numeric_rows, temporal_rows) {
  if (length(numeric_rows) == 0) {
      return(logical(0))
  }
  included <- as.logical(numeric_rows)
  temporal_rows <- as.logical(temporal_rows)
  repeat {
      prev_included <- c(FALSE, included[-length(included)])
      next_included <- c(included[-1], FALSE)
      expanded <- included | (temporal_rows & (prev_included | next_included))
      if (identical(expanded, included)) {
          break
      }
      included <- expanded
  }
  included
}

detect_text_separator <- function(lines) {
  candidates <- c("," = ",", ";" = ";", "\t" = "\t", "|" = "|")
  counts <- vapply(candidates, function(sep) {
      sum(vapply(gregexpr(sep, lines, fixed = TRUE), function(m) {
          if (length(m) == 1 && m[1] == -1) 0L else length(m)
      }, integer(1)))
  }, integer(1))
  if (max(counts) > 0) candidates[[which.max(counts)]] else ","
}

parse_yaml_records_file <- function(path, clean_vars = TRUE) {
  obj <- yaml::read_yaml(path, eval.expr = FALSE)
  records <- yaml_record_rows(obj)
  rows <- records$rows
  if (length(rows) == 0) {
      stop("YAML input does not contain record-like data.")
  }

  all_cols <- unique(unlist(lapply(rows, names), use.names = FALSE))
  df <- as.data.frame(
      stats::setNames(lapply(all_cols, function(col) yaml_column_vector(rows, col)), all_cols),
      stringsAsFactors = FALSE
  )

  if (clean_vars && ncol(df) > 0) {
      df <- clean_variable_names(df)
  }

  audit <- list(
      "YAML Records Parsed" = nrow(df),
      "YAML Fields Parsed" = ncol(df)
  )
  if (!is.null(records$name_column)) {
      audit[["YAML Record Name Column"]] <- records$name_column
  }
  if (records$collapsed_fields > 0) {
      audit[["YAML Vector Fields Collapsed"]] <- records$collapsed_fields
  }

  list(data = df, audit = audit)
}

yaml_record_rows <- function(obj) {
  if (!is.list(obj) || length(obj) == 0) {
      stop("YAML input does not contain a list or mapping.")
  }

  names_obj <- names(obj)
  has_record_names <- !is.null(names_obj) &&
      length(names_obj) == length(obj) &&
      all(!is.na(names_obj) & names_obj != "")
  record_like <- vapply(obj, is_yaml_record_object, logical(1))

  if (has_record_names && any(record_like)) {
      rows <- Map(function(record_name, record) {
          c(list(record_name = record_name), flatten_yaml_record(record))
      }, names_obj[record_like], obj[record_like])
      collapsed <- sum(vapply(obj[record_like], count_yaml_collapsed_fields, integer(1)))
      return(list(rows = rows, name_column = "record_name", collapsed_fields = collapsed))
  }

  if (!has_record_names && all(record_like)) {
      rows <- lapply(obj, flatten_yaml_record)
      collapsed <- sum(vapply(obj, count_yaml_collapsed_fields, integer(1)))
      return(list(rows = rows, name_column = NULL, collapsed_fields = collapsed))
  }

  if (has_record_names) {
      rows <- Map(function(key, value) {
          list(record_name = key, value = scalarize_yaml_value(value))
      }, names_obj, obj)
      collapsed <- sum(vapply(obj, count_yaml_collapsed_fields, integer(1)))
      return(list(rows = rows, name_column = "record_name", collapsed_fields = collapsed))
  }

  stop("YAML input does not contain record-like data.")
}

is_yaml_record_object <- function(value) {
  is.list(value) && length(value) > 0 && !is.null(names(value)) &&
      any(!is.na(names(value)) & names(value) != "")
}

flatten_yaml_record <- function(record, prefix = NULL) {
  if (!is_yaml_record_object(record)) {
      key <- if (is.null(prefix)) "value" else prefix
      out <- list(scalarize_yaml_value(record))
      names(out) <- key
      return(out)
  }

  out <- list()
  for (nm in names(record)) {
      value <- record[[nm]]
      key <- if (is.null(prefix)) nm else paste(prefix, nm, sep = "_")
      if (is_yaml_record_object(value)) {
          out <- c(out, flatten_yaml_record(value, prefix = key))
      } else {
          out[[key]] <- scalarize_yaml_value(value)
      }
  }
  out
}

scalarize_yaml_value <- function(value) {
  if (is.null(value)) {
      return(NA_character_)
  }
  if (is.atomic(value)) {
      if (length(value) == 0) {
          return(NA_character_)
      }
      if (length(value) == 1) {
          return(value)
      }
      return(paste(as.character(value), collapse = ", "))
  }
  flattened <- unlist(value, recursive = TRUE, use.names = FALSE)
  if (length(flattened) == 0) NA_character_ else paste(as.character(flattened), collapse = ", ")
}

yaml_column_vector <- function(rows, col) {
  values <- lapply(rows, function(row) {
      value <- row[[col]]
      if (is.null(value)) NA else value
  })
  is_missing <- vapply(values, function(value) length(value) == 1 && is.na(value), logical(1))
  non_missing <- values[!is_missing]

  if (length(non_missing) > 0 &&
      all(vapply(non_missing, function(value) is.numeric(value) || is.integer(value), logical(1)))) {
      return(as.numeric(unlist(values, use.names = FALSE)))
  }
  if (length(non_missing) > 0 &&
      all(vapply(non_missing, is.logical, logical(1)))) {
      return(as.logical(unlist(values, use.names = FALSE)))
  }

  out <- as.character(unlist(values, use.names = FALSE))
  out[is_missing] <- NA_character_
  out
}

count_yaml_collapsed_fields <- function(value) {
  if (is.null(value)) {
      return(0L)
  }
  if (is.atomic(value)) {
      return(as.integer(length(value) > 1))
  }
  if (is_yaml_record_object(value)) {
      return(sum(vapply(value, count_yaml_collapsed_fields, integer(1))))
  }
  as.integer(length(value) > 1)
}

strip_rdb_field_type_row <- function(raw_data) {
  if (!is.data.frame(raw_data) || nrow(raw_data) < 3) {
      return(list(data = raw_data, count = 0))
  }
  vals <- stringr::str_trim(as.character(raw_data[2, , drop = TRUE]))
  vals <- vals[!is.na(vals) & vals != ""]
  if (length(vals) == 0 || !all(grepl("^[0-9]+[A-Za-z]$", vals))) {
      return(list(data = raw_data, count = 0))
  }
  stripped <- raw_data[-2, , drop = FALSE]
  rownames(stripped) <- NULL
  list(data = stripped, count = 1)
}

repair_spurious_geographic_subdivision_fill <- function(df) {
  country_cols <- which(grepl("(^|[/_ .-])country([/_ .-]|$)|country_region|country.region", colnames(df), ignore.case = TRUE))
  sub_cols <- which(vapply(colnames(df), is_geographic_subdivision_column, logical(1)))
  if (length(country_cols) == 0 || length(sub_cols) == 0 || nrow(df) < 2) {
      return(list(data = df, count = 0))
  }

  country <- as.character(df[[country_cols[1]]])
  repaired <- 0
  for (sc in sub_cols) {
      vals <- as.character(df[[sc]])
      for (r in 2:nrow(df)) {
          current_val <- stringr::str_trim(vals[r])
          previous_val <- stringr::str_trim(vals[r - 1])
          current_country <- stringr::str_trim(country[r])
          previous_country <- stringr::str_trim(country[r - 1])
          if (
              !is.na(current_val) && current_val != "" &&
              !is.na(previous_val) && current_val == previous_val &&
              !is.na(current_country) && current_country != "" &&
              !is.na(previous_country) && current_country != previous_country
          ) {
              vals[r] <- NA_character_
              repaired <- repaired + 1
          }
      }
      df[[sc]] <- vals
  }

  list(data = df, count = repaired)
}

drop_export_index_column <- function(df) {
  if (ncol(df) < 2 || nrow(df) == 0) {
      return(list(data = df, name = NA_character_))
  }

  first_name <- tolower(colnames(df)[1])
  if (!first_name %in% c("rownames", "row_names", "rowname", "row_name", "unnamed_0", "index")) {
      return(list(data = df, name = NA_character_))
  }

  vals <- suppressWarnings(as.numeric(as.character(df[[1]])))
  if (any(is.na(vals)) || any(!is.finite(vals)) || any(vals != floor(vals)) || anyDuplicated(vals)) {
      return(list(data = df, name = NA_character_))
  }

  is_sequential <- identical(vals, as.numeric(seq_len(nrow(df))))
  is_monotone_positive <- all(vals > 0) && all(diff(vals) > 0)
  if (!is_sequential && !is_monotone_positive) {
      return(list(data = df, name = NA_character_))
  }

  list(data = df[, -1, drop = FALSE], name = colnames(df)[1])
}

is_aggregation_summary_column <- function(col_name, keywords = NULL) {
  if (is.na(col_name) || col_name == "") return(FALSE)
  compact_codes <- grepl("^[a-z]{2,6}[0-9]*$", col_name)
  if (compact_codes) return(FALSE)

  word_pattern <- "(^|[ _./-])(total|subtotal|ytd|average|avg|gesamt|summe|durchschnitt|moyenne|somme|promedio)s?($|[ _./-])"
  if (grepl(word_pattern, col_name, perl = TRUE)) return(TRUE)
  if (grepl("^(sum|sums)$|^sum[ _./-]|[ _./-]sum[ _./-]", col_name, perl = TRUE)) return(TRUE)

  any(vapply(c("\u5408\u8ba1", "\u603b\u8ba1", "\u5c0f\u8ba1"), function(k) {
      grepl(k, col_name, fixed = TRUE)
  }, logical(1)))
}

is_time_of_day_column <- function(name) {
  if (is.na(name) || name == "") return(FALSE)
  grepl("(^|_)time($|_)", tolower(name), perl = TRUE)
}

is_mostly_time_of_day <- function(x) {
  vals <- stringr::str_trim(as.character(x))
  vals <- vals[!is.na(vals) & vals != ""]
  if (length(vals) == 0) return(FALSE)
  mean(grepl("^([01]?\\d|2[0-3]):[0-5]\\d(:[0-5]\\d)?$", vals, perl = TRUE)) >= 0.95
}

is_compact_time_code_column <- function(name, x) {
  if (!is_time_code_column(name)) return(FALSE)
  vals <- stringr::str_trim(as.character(x))
  vals <- vals[!is.na(vals) & vals != ""]
  if (length(vals) == 0) return(FALSE)
  compact <- grepl("^(?:[A-Za-z]{1,3})?0?[0-9]{1,4}$", vals, perl = TRUE)
  iso_duration <- grepl("^P[0-9]+[DWMY]$", vals, ignore.case = TRUE, perl = TRUE)
  compact <- compact | iso_duration
  has_code_signal <- grepl("^[A-Za-z]{1,3}0?[0-9]{1,4}$|^0[0-9]+$", vals, perl = TRUE) | iso_duration
  mean(compact) >= 0.95 && any(has_code_signal)
}

is_time_code_column <- function(name) {
  if (is.na(name) || name == "") return(FALSE)
  normalized <- gsub("[^a-z0-9]+", "_", tolower(name))
  grepl("(^|_)(period|period_code|month|month_code|quarter|quarter_code|cycle|cycle_code|visit|visit_code|time_format)($|_)", normalized, perl = TRUE)
}

is_postal_code_column <- function(name) {
  if (is.na(name) || name == "") return(FALSE)
  normalized <- gsub("[^a-z0-9]+", "_", tolower(name))
  grepl("(^|_)(zip|zipcode|postal|postcode|fips|geoid|geo_id|county_fips|state_fips|census_tract|tract|site_no|site_number|station|station_id)($|_)", normalized, perl = TRUE)
}
