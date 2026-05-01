#' Smartly Read Messy Panel Data from Excel
#'
#' @param file_path Path to the excel file
#' @param sheet Optional sheet name or index to read. If NULL, auto-discovers the first valid panel across all sheets.
#' @param na_strings Character vector of strings to interpret as missing values.
#' @param clean_vars Logical, whether to standardize variable names.
#' @return A cleaned and standardized dataframe
#' @export
#' @importFrom readxl read_excel excel_sheets
#' @importFrom stringr str_trim str_replace str_remove_all
#'
read_messy_panel <- function(file_path, sheet = NULL, na_strings = c("", "NA", "--", "#N/A", "NULL", "无", "S", "D", "ND", "N/A", "*", "**", "***", "-", ".", "x", "c", "s", "z", "#VALUE!", "#REF!", "#DIV/0!", "#NUM!", "#NAME?", "none", "NR"), clean_vars = TRUE) {
  
  sheets <- readxl::excel_sheets(file_path)
  sheets_to_try <- if (!is.null(sheet)) sheet else sheets
  
  is_numeric_like <- function(x) {
    if (is.na(x) || x %in% na_strings) return(TRUE) 
    clean_x <- stringr::str_replace(x, "^\\s*\\((.*)\\)\\s*$", "-\\1")
    clean_x <- stringr::str_replace(clean_x, "^-\\s+", "-")
    clean_x <- stringr::str_replace(clean_x, "^\\s*([0-9.,\\s]+?)\\s*-$", "-\\1")
    clean_x <- stringr::str_remove_all(clean_x, "[\\$€£¥%]")
    
    clean_x <- stringr::str_replace_all(clean_x, "(?<=\\d)[\\s'](?=\\d)", "")
    has_euro_decimal <- grepl(",[0-9]{1,2}[^0-9]*$", clean_x)
    if (!is.na(has_euro_decimal) && has_euro_decimal) {
        clean_x <- stringr::str_replace(stringr::str_remove_all(clean_x, "\\."), ",", ".")
    }
    clean_x <- stringr::str_remove_all(clean_x, ",")
    
    clean_x <- stringr::str_replace(clean_x, "\\s*\\*+\\s*$", "")
    clean_x <- stringr::str_replace(clean_x, "\\s*[\\(\\[].*?[\\)\\]]\\s*$", "")
    clean_x <- stringr::str_replace(clean_x, "\\s*[A-Za-z]+\\s*$", "")
    
    !is.na(suppressWarnings(as.numeric(clean_x)))
  }
  
  last_error <- "Could not parse any sheet."
  
  for (s in sheets_to_try) {
    res <- tryCatch({
      raw_data <- suppressMessages(readxl::read_excel(file_path, sheet = s, col_names = FALSE, .name_repair = "minimal"))
      if (nrow(raw_data) == 0) stop("Empty sheet")
      
      raw_mat <- as.matrix(raw_data)
      
      num_counts <- apply(raw_mat, 1, function(row) {
        sum(sapply(row, is_numeric_like) & !is.na(row) & row != "")
      })
      
      is_data_row <- num_counts >= 2
      true_runs_indices <- which(is_data_row == TRUE)
      
      if (length(true_runs_indices) == 0) {
        stop("Could not detect any numeric panel data block.")
      }
      
      gaps <- diff(true_runs_indices)
      block_boundaries <- which(gaps > 3)
      
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
      
      block_lengths <- sapply(blocks, length)
      main_block <- blocks[[which.max(block_lengths)]]
      
      start_data_row <- min(main_block)
      end_data_row <- max(main_block)
      
      main_block_counts <- num_counts[start_data_row:end_data_row]
      mode_count <- as.numeric(names(sort(table(main_block_counts), decreasing = TRUE)[1]))
      
      density_threshold <- max(2, floor(mode_count * 0.2))
      
      valid_in_block <- which(main_block_counts >= density_threshold)
      if (length(valid_in_block) == 0) {
          true_start <- start_data_row
          true_end <- end_data_row
      } else {
          true_start <- start_data_row + min(valid_in_block) - 1
          true_end <- start_data_row + max(valid_in_block) - 1
      }
      
      if (true_start > 1) {
          non_empty_above <- sum(!is.na(raw_mat[true_start - 1, ]) & raw_mat[true_start - 1, ] != "")
          non_empty_current <- sum(!is.na(raw_mat[true_start, ]) & raw_mat[true_start, ] != "")
          
          is_year_header <- FALSE
          row_vals <- raw_mat[true_start, ]
          num_vals <- suppressWarnings(as.numeric(row_vals[!is.na(row_vals) & row_vals != ""]))
          valid_nums <- num_vals[!is.na(num_vals)]
          
          if (length(valid_nums) >= 2 && all(valid_nums >= 1900 & valid_nums <= 2100)) {
              is_year_header <- TRUE
          }
          
          if (is_year_header || (non_empty_above < non_empty_current * 0.3)) {
              true_start <- true_start + 1
          }
      }
      
      header_row_index <- max(1, true_start - 1)
      if (header_row_index == true_start) {
          true_start <- true_start + 1
      }
      headers <- raw_mat[header_row_index, ]
      
      if (header_row_index > 1) {
          h1 <- raw_mat[header_row_index - 1, ]
          h2 <- raw_mat[header_row_index, ]
          
          unique_h1 <- unique(h1[!is.na(h1) & h1 != ""])
          if (length(unique_h1) > 1) {
              for (j in 2:length(h1)) {
                  if ((is.na(h1[j]) || h1[j] == "") && !is.na(h1[j-1]) && h1[j-1] != "") {
                      h1[j] <- h1[j-1]
                  }
              }
              headers <- ifelse(is.na(h1) | h1 == "", h2, 
                                ifelse(is.na(h2) | h2 == "", h1, paste0(h1, " ", h2)))
          }
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
      
      df <- as.data.frame(data_block, stringsAsFactors = FALSE)
      colnames(df) <- stringr::str_trim(headers)
      
      if (has_sections) {
          df$section_category <- valid_sections
      }
      
      df[] <- lapply(df, function(x) {
        x[x %in% na_strings] <- NA
        x
      })
      
      df[] <- lapply(df, function(x) {
        clean_x <- stringr::str_replace(x, "^\\s*\\((.*)\\)\\s*$", "-\\1")
        clean_x <- stringr::str_replace(clean_x, "^-\\s+", "-")
        clean_x <- stringr::str_replace(clean_x, "^\\s*([0-9.,\\s]+?)\\s*-$", "-\\1")
        is_pct <- grepl("%\\s*$", clean_x) & !is.na(clean_x)
        
        clean_x <- stringr::str_remove_all(clean_x, "[\\$€£¥%]")
        clean_x <- stringr::str_trim(clean_x)
        
        clean_x <- stringr::str_replace_all(clean_x, "(?<=\\d)[\\s'](?=\\d)", "")
        has_euro_decimal <- grepl(",[0-9]{1,2}[^0-9]*$", clean_x)
        clean_x <- ifelse(has_euro_decimal & !is.na(has_euro_decimal), 
                          stringr::str_replace(stringr::str_remove_all(clean_x, "\\."), ",", "."), 
                          clean_x)
        clean_x <- stringr::str_remove_all(clean_x, ",")
        
        clean_x <- stringr::str_replace(clean_x, "\\s*\\*+\\s*$", "")
        clean_x <- stringr::str_replace(clean_x, "\\s*[\\(\\[].*?[\\)\\]]\\s*$", "")
        clean_x <- stringr::str_replace(clean_x, "\\s*[A-Za-z]+\\s*$", "")
        
        num_x <- suppressWarnings(as.numeric(clean_x))
        
        if(sum(is.na(num_x)) == sum(is.na(x))) {
            num_x[is_pct & !is.na(num_x)] <- num_x[is_pct & !is.na(num_x)] / 100
            return(num_x)
        } 
        
        final_str <- stringr::str_replace(x, "^\\s*[\\.-]+\\s*", "")
        return(stringr::str_trim(final_str))
      })
      
      colnames(df) <- tolower(colnames(df)) 
      if (clean_vars) {
        df <- clean_variable_names(df)
      }
      
      if (nrow(df) > 0) {
          char_cols <- which(sapply(df, is.character))
          if (length(char_cols) > 0) {
              first_char_col <- char_cols[1]
              agg_pattern <- "(?i)(total|average|mean|median|sub-?total)"
              is_aggregate <- grepl(agg_pattern, df[[first_char_col]])
              df <- df[!is_aggregate, , drop = FALSE]
          }
      }
      
      list(success = TRUE, data = df)
    }, error = function(e) {
      list(success = FALSE, error = e$message)
    })
    
    if (res$success) {
      return(res$data)
    } else {
      last_error <- res$error
    }
  }
  
  stop(paste("Failed to parse any valid panel from file.", last_error))
}
