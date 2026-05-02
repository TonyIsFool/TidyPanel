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
#' @param return_audit Logical. If `TRUE`, returns a list containing `$data` (the cleaned data frame) and `$audit` (a detailed log of all algorithmic modifications made).
#'
#' @return If `return_audit = FALSE`, a cleaned and standardized `data.frame`. 
#' If `return_audit = TRUE`, a named list containing:
#' \item{data}{The cleaned `data.frame`.}
#' \item{audit}{A `data.frame` detailing exactly what transformations, truncations, or imputations were applied.}
#'
#' @examples
#' \dontrun{
#' # Basic usage
#' df <- read_messy_panel("data_raw/financial_report.xlsx")
#' 
#' # Return data along with the algorithmic audit log
#' result <- read_messy_panel("data_raw/complex_hierarchy.xlsx", return_audit = TRUE)
#' head(result$data)
#' print(result$audit)
#' }
#'
#' @export
#' @importFrom readxl read_excel excel_sheets
#' @importFrom stringr str_trim str_replace str_remove_all str_squish str_extract
read_messy_panel <- function(file_path, sheet = NULL, na_strings = c("", "NA", "#N/A", "NULL", "\u65e0", "S", "D", "ND", "N/A", "*", "**", "***", ".", "x", "c", "s", "z", "#VALUE!", "#REF!", "#DIV/0!", "#NUM!", "#NAME?", "none", "NR", "--", "---", "n.a.", "N.A.", "n/a", "Not Applicable"), clean_vars = TRUE, auto_pivot = FALSE, return_audit = FALSE) {
  
  audit_log <- list()
  
  if (is.character(sheet) && length(sheet) == 1 && toupper(sheet) == "ALL") {
      sheets <- readxl::excel_sheets(file_path)
      all_data <- list()
      all_audits <- list()
      
      for (s in sheets) {
          res <- tryCatch({
              read_messy_panel(file_path, sheet = s, na_strings = na_strings, clean_vars = clean_vars, auto_pivot = auto_pivot, return_audit = TRUE)
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
      
      master_df <- dplyr::bind_rows(all_data)
      
      if (return_audit) {
          return(list(success = TRUE, data = master_df, audit = all_audits))
      } else {
          return(master_df)
      }
  }
  
  sheets <- readxl::excel_sheets(file_path)
  sheets_to_try <- if (!is.null(sheet)) sheet else sheets
  
  is_numeric_like <- function(x) {
    if (is.na(x) || x %in% na_strings) return(TRUE) 
    clean_x <- stringr::str_remove_all(x, intToUtf8(160))
    if (stringr::str_trim(clean_x) %in% c("-", "\u2013", "\u2014")) return(TRUE)
    if (grepl("^[-—\u2013\u2014]+$", stringr::str_trim(clean_x))) return(TRUE)
    if (grepl("(?i)^(Q[1-4]|H[1-2]|FY[0-9]+)$", stringr::str_trim(clean_x))) return(FALSE)
    
    # Phase 5: Non-standard scientific notation
    clean_x <- stringr::str_replace_all(clean_x, "(?i)\\s*[x\\*]\\s*10\\^([\\-\\+]?[0-9]+)", "E\\1")
    clean_x <- stringr::str_replace(clean_x, "^\\s*\\((.*)\\)\\s*$", "-\\1")
    clean_x <- stringr::str_replace(clean_x, "^-\\s+", "-")
    clean_x <- stringr::str_replace(clean_x, "^\\s*([0-9.,\\s]+?)\\s*-$", "-\\1")
    clean_x <- stringr::str_remove_all(clean_x, "[\\$€£¥%万亿元千]")
    
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
      raw_data <- suppressMessages(readxl::read_excel(file_path, sheet = s, col_names = FALSE, .name_repair = "minimal", trim_ws = FALSE))
      if (nrow(raw_data) == 0) stop("Empty sheet")
      
      raw_mat <- as.matrix(raw_data)
      
      num_counts <- apply(raw_mat, 1, function(row) {
        sum(vapply(row, is_numeric_like, logical(1)) & !is.na(row) & row != "")
      })
      
      is_data_row <- num_counts >= 1
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
      
      block_lengths <- vapply(blocks, length, integer(1))
      main_block <- blocks[[which.max(block_lengths)]]
      
      start_data_row <- min(main_block)
      end_data_row <- max(main_block)
      
      main_block_counts <- num_counts[start_data_row:end_data_row]
      mode_count <- as.numeric(names(sort(table(main_block_counts), decreasing = TRUE)[1]))
      
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
          if (num_counts[true_start] < density_threshold) {
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
      
      if (header_row_index == true_start) {
          true_start <- true_start + 1
      }
      header_rows_list <- list()
      for (r in 1:header_row_index) {
          row_vals <- raw_mat[r, ]
          non_empty_count <- sum(!is.na(row_vals) & stringr::str_trim(row_vals) != "")
          if (non_empty_count > 1 || r == header_row_index) {
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
                                ifelse(headers == "", h_row, paste0(headers, "_", h_row)),
                                headers)
          }
          headers <- ifelse(headers == "", NA, headers)
      } else {
          headers <- raw_mat[header_row_index, ]
      }
      
      data_block <- raw_mat[true_start:true_end, , drop = FALSE]
      
      block_counts <- num_counts[true_start:true_end]
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
      subtotal_keywords <- c("subtotal", "小计", "total:", "sum:")
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
          agg_keywords <- c("total", "sum", "average", "avg", "合计", "总计", "平均", "mean")
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
      col_agg_keywords <- c("total", "sum", "subtotal", "ytd", "合计", "总计", "小计", "average", "avg")
      cols_to_keep <- rep(TRUE, ncol(df))
      subtotal_cols_dropped <- c()
      for (c in seq_len(ncol(df))) {
          col_name <- tolower(colnames(df)[c])
          if (any(vapply(col_agg_keywords, function(k) grepl(k, col_name, fixed = TRUE), logical(1)))) {
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
      
      df[] <- lapply(df, function(x) {
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
        
        clean_x <- stringr::str_remove_all(clean_x, "[\\$€£¥%万亿元千]")
        clean_x <- stringr::str_trim(clean_x)
        
        clean_x <- stringr::str_replace_all(clean_x, "(?<=\\d)[\\s\\u00A0'](?=\\d)", "")
        has_euro_decimal <- grepl(",[0-9]{1,2}[^0-9]*$", clean_x)
        clean_x <- ifelse(has_euro_decimal & !is.na(has_euro_decimal), 
                          stringr::str_replace(stringr::str_remove_all(clean_x, "\\."), ",", "."), 
                          clean_x)
        clean_x <- stringr::str_remove_all(clean_x, ",")
        
        # Phase 11: Semantic Multiplier Engine
        multiplier <- rep(1, length(clean_x))
        k_idx <- grepl("(?i)[0-9.]+\\s*k$", clean_x) & !is.na(clean_x)
        m_idx <- grepl("(?i)[0-9.]+\\s*(m|mil|million)$", clean_x) & !is.na(clean_x)
        b_idx <- grepl("(?i)[0-9.]+\\s*(b|bn|billion)$", clean_x) & !is.na(clean_x)
        
        multiplier[k_idx] <- 1000
        multiplier[m_idx] <- 1000000
        multiplier[b_idx] <- 1000000000
        
        clean_x <- stringr::str_replace(clean_x, "(?i)\\s*(k|m|mil|million|b|bn|billion)$", "")
        
        clean_x <- stringr::str_replace(clean_x, "\\s*\\*+\\s*$", "")
        clean_x <- stringr::str_replace(clean_x, "\\s*[\\(\\[].*?[\\)\\]]\\s*$", "")
        
        # Exclude quarters from being stripped and converted
        is_quarter <- grepl("(?i)^(Q[1-4]|H[1-2]|FY[0-9]+)$", stringr::str_trim(x))
        
        clean_x[!is_quarter] <- stringr::str_replace(clean_x[!is_quarter], "\\s*[A-Za-z\u4e00-\u9fa5]+\\s*$", "")
        clean_x[!is_quarter] <- stringr::str_replace(clean_x[!is_quarter], "^\\s*[A-Za-z\u4e00-\u9fa5]+\\s*", "")
        
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
      
      colnames(df) <- tolower(colnames(df)) 
      if (clean_vars) {
        df <- clean_variable_names(df)
      }
      
      # Phase 14: Auto-Pivot Engine (Wide to Long)
      if (auto_pivot && nrow(df) > 0) {
          cnames <- colnames(df)
          temporal_pattern <- "^(19|20)[0-9]{2}(_q[1-4]|_h[1-2])?$|^(q[1-4]|h[1-2]|fy[0-9]+)$"
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
      
      list(success = TRUE, data = df, audit = audit_log)
    }, error = function(e) {
      list(success = FALSE, error = e$message)
    })
    
    if (res$success) {
      if (return_audit) {
          audit_df <- data.frame(
              Operation = names(res$audit),
              Count = as.character(unlist(res$audit, use.names = FALSE)),
              stringsAsFactors = FALSE
          )
          audit_df <- audit_df[audit_df$Count != "0", , drop = FALSE]
          rownames(audit_df) <- NULL
          return(list(data = res$data, audit = audit_df))
      }
      return(res$data)
    } else {
      last_error <- res$error
    }
  }
  
  stop(paste("Failed to parse any valid panel from file.", last_error))
}
