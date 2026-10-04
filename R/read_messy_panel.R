#' Robust Parsing and Extraction of Messy Excel Panel Data
#'
#' @description
#' `read_messy_panel()` is an industrial-grade parser designed to extract clean, standardized data frames 
#' from heavily malformed, human-readable Excel reports (e.g., financial statements, ERP exports). 
#' It automatically bypasses decoy rows, stitches N-dimensional hierarchical headers, extracts structural 
#' indentation hierarchies (parent-child relationships), reshapes series-oriented workbooks with
#' metadata rows and a date axis, reads RDB plus gzip-compressed CSV, TSV, TXT, and RDB files, decodes UTF-16 BOM text plus CP932/Shift-JIS, CP949, Big5, Windows-1250, and Windows-1251 monthly text tables, rejects HTML responses disguised as CSV files, retains stable headerless daily records, detects official whitespace-delimited text tables, amputates embedded subtotals,
#' and standardizes financial/scientific numbers.
#'
#' @param file_path Character string. A path to an Excel, CSV, TSV, TXT, RDB, ZIP, or gzip-compressed CSV, TSV, TXT, or RDB file.
#' @param archive_member Optional member name for a ZIP archive containing multiple tabular files. ZIP archives with exactly one supported tabular member are read automatically.
#' @param sheet Optional sheet name or index. If `NULL` (the default), it selects the largest usable data panel across all sheets. If `"ALL"`, parses and merges all sheets.
#' @param na_strings Character vector. Strings to interpret as missing values. Supports complex missing-value lexicons. Literal `NA` is retained in explicit two-letter ISO code fields when observed values match the code schema.
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
#' example_path <- system.file(
#'   "extdata", "demo_01_numbers.csv", package = "TidyPanel", mustWork = TRUE
#' )
#' result <- read_messy_panel(example_path)
#' head(result)
#'
#' @export
#' @importFrom readxl read_excel excel_sheets
#' @importFrom stringi stri_trim_both stri_trim_left
#' @importFrom stringr str_replace str_remove_all str_squish str_extract
#' @importFrom stats quantile
#' @importFrom utils head read.csv read.table tail unzip
read_messy_panel <- function(file_path, sheet = NULL, na_strings = c("", ":", "NA", "#N/A", "NULL", "null", "S", "D", "ND", "N/A", "?", "*", "**", "***", ".", "x", "c", "s", "z", "#VALUE!", "#REF!", "#DIV/0!", "#NUM!", "#NAME?", "none", "NR", "--", "---", "-999", "n.a.", "N.A.", "n/a", "Not Applicable", "\\N", "\u2026"), clean_vars = TRUE, auto_pivot = FALSE, return_audit = FALSE, extract_all_blocks = FALSE, archive_member = NULL) {
  audit_log <- list()
  is_df <- is.data.frame(file_path)
  is_csv <- FALSE
  is_gzip <- FALSE
  is_zip <- FALSE
  zip_detected_by_signature <- FALSE
  is_ndbc_historical_input <- FALSE
  if (!is_df) {
      if (!is.character(file_path) || length(file_path) != 1L || !file.exists(file_path) || dir.exists(file_path)) {
          stop("file_path must be a data.frame or an existing Excel, CSV, TSV, or TXT file.")
      }
      file_name <- tolower(basename(file_path))
      file_extension <- tools::file_ext(file_name)
      if (!file_extension %in% c("zip", "xlsx", "xls")) {
          signature_connection <- file(file_path, open = "rb")
          signature <- tryCatch(
              readBin(signature_connection, what = "raw", n = 4L),
              finally = close(signature_connection)
          )
          zip_detected_by_signature <- length(signature) == 4L &&
              identical(as.integer(signature[1:2]), c(80L, 75L)) &&
              any(vapply(
                  list(c(3L, 4L), c(5L, 6L), c(7L, 8L)),
                  function(marker) identical(as.integer(signature[3:4]), marker),
                  logical(1)
              ))
      }
      if (file_extension == "zip" || zip_detected_by_signature) {
          archive_files <- utils::unzip(file_path, list = TRUE)$Name
          candidates <- archive_files[grepl("\\.(csv|tsv|txt|rdb)(\\.gz)?$", archive_files, ignore.case = TRUE)]
          if (is.null(archive_member)) {
              if (length(candidates) != 1L) {
                  stop("ZIP archives with multiple tabular members require archive_member to name the file to clean.")
              }
              archive_member <- candidates[[1L]]
          }
          if (!is.character(archive_member) || length(archive_member) != 1L || !archive_member %in% candidates) {
              stop("archive_member must name a supported CSV, TSV, TXT, or RDB member in the ZIP archive.")
          }
          extract_dir <- tempfile("tidypanel-zip-")
          dir.create(extract_dir)
          utils::unzip(file_path, files = archive_member, exdir = extract_dir)
          file_path <- file.path(extract_dir, archive_member)
          on.exit(unlink(extract_dir, recursive = TRUE), add = TRUE)
          file_name <- tolower(basename(file_path))
          is_zip <- TRUE
      }
      is_gzip <- grepl("\\.gz$", file_name)
      ext <- tolower(tools::file_ext(if (is_gzip) sub("\\.gz$", "", file_name) else file_name))
      if (!ext %in% c("xlsx", "xls", "csv", "tsv", "txt", "rdb") ||
          (is_gzip && !ext %in% c("csv", "tsv", "txt", "rdb"))) {
          stop("TidyPanel core supports data.frames and Excel, CSV, TSV, or TXT tabular files only (with RDB and gzip-compressed CSV, TSV, TXT, and RDB files also supported).")
      }
      is_csv <- ext %in% c("csv", "tsv", "txt", "rdb")
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

      has_series_observations <- any(vapply(all_audits, function(audit) {
          is.data.frame(audit) &&
              "Series-Oriented Workbook Reshaped" %in% audit$Operation
      }, logical(1)))
      series_catalog_sheets <- vapply(all_data, function(data) {
          is.data.frame(data) && nrow(data) > 0L &&
              !"date" %in% names(data) &&
              all(c("data_item_description", "series_type", "series_id") %in% names(data))
      }, logical(1))
      if (has_series_observations && any(series_catalog_sheets)) {
          excluded_sheets <- names(all_data)[series_catalog_sheets]
          all_data <- all_data[!series_catalog_sheets]
          for (excluded_sheet in excluded_sheets) {
              all_audits[[excluded_sheet]] <- data.frame(
                  Operation = "Series Catalog Sheet Excluded",
                  Count = 1L,
                  stringsAsFactors = FALSE
              )
          }
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
  input_audit <- list()
  if (is_gzip) {
      input_audit[["Gzip Text Input Decompressed"]] <- 1L
  }
  if (is_zip) {
      input_audit[["ZIP Archive Member Extracted"]] <- archive_member
  }
  if (zip_detected_by_signature) {
      input_audit[["ZIP Archive Detected by Signature"]] <- 1L
  }
  cyrillic_pattern <- paste0("[", intToUtf8(0x0400), "-", intToUtf8(0x052F), "]")
  
  is_numeric_like <- function(x) {
    fast_x <- trimws(x)
    if (is.na(x) || fast_x %in% na_strings || grepl("^\\*+$", fast_x)) return(TRUE) 
    if (grepl("^:\\s*(?:@[A-Za-z0-9_]+|[A-Za-z][A-Za-z0-9_]*)?\\s*$", fast_x, perl = TRUE)) return(TRUE)
    if (grepl("^[+-]?[0-9]+(?:\\.[0-9]+)?(?:[eE][+-]?[0-9]+)?$", fast_x, perl = TRUE)) return(TRUE)
    if (grepl("^[0-9]{4}[-/][0-9]{1,2}(?:[-/][0-9]{1,2})?$", fast_x)) return(FALSE)
    if (grepl("^[0-9]{1,2}[-/][0-9]{1,2}[-/](?:19|20)[0-9]{2}$", fast_x)) return(FALSE)
    if (grepl("^[0-9]{4}[-/][0-9]{1,2}[-/][0-9]{1,2}[T ]?[0-9]{1,2}:[0-9]{2}(?::[0-9]{2}(?:\\.[0-9]+)?)?(?:Z|[+-][0-9]{2}:?[0-9]{2})?$", fast_x)) return(FALSE)
    if (grepl("^[A-Za-z]+[0-9]+[A-Za-z0-9_-]*$", fast_x) ||
        grepl("^[0-9]+[A-Za-z]+[0-9]+[A-Za-z0-9_-]*$", fast_x)) return(FALSE)
    if (grepl("^[[:alpha:]][[:alpha:] _-]*[[:space:]][0-9]+(?:[[:space:]][[:alpha:]][[:alpha:] _-]*)?$", fast_x)) return(FALSE)
    if (!grepl("[0-9]", fast_x) && grepl("[[:alpha:]]", fast_x)) return(FALSE)
    if (grepl("[[:alpha:]]{2,}.*[[:space:]].*[[:alpha:]]{2,}", fast_x)) return(FALSE)
    clean_x <- stringr::str_remove_all(x, intToUtf8(160))
    if (grepl("^\\((NA|N/A|NULL|ND)\\)$", stringi::stri_trim_both(clean_x), ignore.case = TRUE)) return(TRUE)
    if (stringi::stri_trim_both(clean_x) %in% c("-", "\u2013", "\u2014")) return(TRUE)
    if (grepl("^[-\u2014\u2013\u2014]+$", stringi::stri_trim_both(clean_x))) return(TRUE)
    if (grepl("(?i)^(Q[1-4]|H[1-2]|FY[0-9]+)$", stringi::stri_trim_both(clean_x))) return(FALSE)
    
    # Phase 5: Non-standard scientific notation
    clean_x <- stringr::str_replace_all(clean_x, "(?i)\\s*[x\\*]\\s*10\\^([\\-\\+]?[0-9]+)", "E\\1")
    clean_x <- stringr::str_replace(clean_x, "^\\s*\\((.*)\\)\\s*$", "-\\1")
    clean_x <- stringr::str_replace(clean_x, "^-\\s+", "-")
    clean_x <- stringr::str_replace(clean_x, "^\\s*([0-9.,\\s]+?)\\s*-$", "-\\1")
    clean_x <- stringr::str_remove_all(clean_x, "[\\$\\u20ac\\u00a3\\u00a5%\\u5143]")
    
    clean_x <- stringr::str_replace_all(clean_x, "(?<=\\d)[\\s\\u00A0'](?=\\d)", "")
    has_euro_decimal <- grepl(",[0-9]{1,2}[^0-9]*$|,[0-9]{4,}[^0-9]*$", clean_x)
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

  is_numeric_like_vector_uncached <- function(values) {
    values <- as.character(values)
    result <- rep(FALSE, length(values))
    fast_values <- trimws(values)
    missing_values <- is.na(values) | fast_values %in% na_strings |
      grepl("^\\*+$", fast_values)
    result[missing_values] <- TRUE

    active <- !missing_values
    cyrillic_text <- active & grepl(cyrillic_pattern, fast_values, perl = TRUE)
    active <- active & !cyrillic_text
    colon_status_missing <- active & grepl(
        "^:\\s*(?:@[A-Za-z0-9_]+|[A-Za-z][A-Za-z0-9_]*)?\\s*$",
        fast_values,
        perl = TRUE
    )
    result[colon_status_missing] <- TRUE
    active <- active & !colon_status_missing
    simple_numeric <- active &
        grepl("^[+-]?[0-9]+(?:\\.[0-9]+)?(?:[eE][+-]?[0-9]+)?$", fast_values, perl = TRUE)
    result[simple_numeric] <- TRUE

    date_like <- active & !simple_numeric &
        (grepl("^[0-9]{4}[-/][0-9]{1,2}(?:[-/][0-9]{1,2})?$", fast_values) |
            grepl("^[0-9]{1,2}[-/][0-9]{1,2}[-/](?:19|20)[0-9]{2}$", fast_values) |
            grepl("^[0-9]{4}[-/][0-9]{1,2}[-/][0-9]{1,2}[T ]?[0-9]{1,2}:[0-9]{2}(?::[0-9]{2}(?:\\.[0-9]+)?)?(?:Z|[+-][0-9]{2}:?[0-9]{2})?$", fast_values))
    code_like <- active & !simple_numeric & !date_like &
        (grepl("^[A-Za-z]+[0-9]+[A-Za-z0-9_-]*$", fast_values) |
            grepl("^[0-9]+[A-Za-z]+[0-9]+[A-Za-z0-9_-]*$", fast_values))
    descriptive_numeric_label <- active & !simple_numeric & !date_like & !code_like &
        grepl("^[[:alpha:]][[:alpha:] _-]*[[:space:]][0-9]+(?:[[:space:]][[:alpha:]][[:alpha:] _-]*)?$", fast_values)
    text_only <- active & !simple_numeric & !date_like & !code_like & !descriptive_numeric_label &
        !grepl("[0-9]", fast_values) & grepl("[[:alpha:]]", fast_values)
    descriptive_text <- active & !simple_numeric & !date_like & !code_like & !descriptive_numeric_label & !text_only &
        grepl("[[:alpha:]]{2,}.*[[:space:]].*[[:alpha:]]{2,}", fast_values)
    complex_values <- active & !simple_numeric & !date_like & !code_like & !descriptive_numeric_label & !text_only & !descriptive_text
    formatted_numeric <- complex_values & grepl(
        "^\\s*[+-]?\\s*\\(?\\s*\\$?\\s*[0-9][0-9.,\\s']*(?:%|\\s*(?:k|m|b|t|wan|mil|million|billion|trillion))?\\s*\\)?\\s*$",
        fast_values,
        ignore.case = TRUE,
        perl = TRUE
    )
    result[formatted_numeric] <- TRUE
    complex_values <- complex_values & !formatted_numeric
    if (any(complex_values)) {
        # Process the remaining decorated numeric candidates in one vectorized
        # pass. Wide scientific exports commonly contain thousands of display
        # strings, where scalar stringr calls dominate parsing time.
        complex_x <- values[complex_values]
        clean_x <- stringr::str_remove_all(complex_x, intToUtf8(160))
        trimmed_x <- stringi::stri_trim_both(clean_x)
        explicitly_numeric <-
            grepl("^\\((NA|N/A|NULL|ND)\\)$", trimmed_x, ignore.case = TRUE) |
            trimmed_x %in% c("-", "\u2013", "\u2014") |
            grepl("^[-\u2014\u2013\u2014]+$", trimmed_x)
        quarter_or_half <- grepl("(?i)^(Q[1-4]|H[1-2]|FY[0-9]+)$", trimmed_x)

        clean_x <- stringr::str_replace_all(clean_x, "(?i)\\s*[x\\*]\\s*10\\^([\\-\\+]?[0-9]+)", "E\\1")
        clean_x <- stringr::str_replace(clean_x, "^\\s*\\((.*)\\)\\s*$", "-\\1")
        clean_x <- stringr::str_replace(clean_x, "^-\\s+", "-")
        clean_x <- stringr::str_replace(clean_x, "^\\s*([0-9.,\\s]+?)\\s*-$", "-\\1")
        clean_x <- stringr::str_remove_all(clean_x, "[\\$\\u20ac\\u00a3\\u00a5%\\u5143]")
        clean_x <- stringr::str_replace_all(clean_x, "(?<=\\d)[\\s\\u00A0'](?=\\d)", "")
        has_euro_decimal <- grepl(",[0-9]{1,2}[^0-9]*$|,[0-9]{4,}[^0-9]*$", clean_x)
        clean_x <- ifelse(
            has_euro_decimal & !is.na(has_euro_decimal),
            stringr::str_replace(stringr::str_remove_all(clean_x, "\\."), ",", "."),
            clean_x
        )
        clean_x <- stringr::str_remove_all(clean_x, ",")
        clean_x <- stringr::str_replace(clean_x, "\\s*\\*+\\s*$", "")
        clean_x <- stringr::str_replace(clean_x, "\\s*[\\(\\[].*?[\\)\\]]\\s*$", "")
        clean_x <- stringr::str_replace(clean_x, "\\s*[A-Za-z\u4e00-\u9fa5]+\\s*$", "")
        clean_x <- stringr::str_replace(clean_x, "^\\s*[A-Za-z\u4e00-\u9fa5]+\\s*", "")

        result[complex_values] <- explicitly_numeric |
            (!quarter_or_half & !is.na(suppressWarnings(as.numeric(clean_x))))
    }

    result
  }

  is_numeric_like_vector <- function(values) {
    values <- as.character(values)
    if (length(values) >= 100000L) {
      unique_values <- unique(values)
      if (length(unique_values) <= length(values) * 0.25) {
        unique_result <- is_numeric_like_vector_uncached(unique_values)
        return(unique_result[match(values, unique_values)])
      }
    }
    is_numeric_like_vector_uncached(values)
  }

  # Some official codes, including the Eurostat unit code NR, overlap with
  # missing-value markers. Treat them as missing only in measurement columns.
  ambiguous_na_strings <- intersect(na_strings, c("S", "D", "x", "c", "s", "z", "NR"))
  is_numeric_context <- function(values, column_name) {
    values <- as.character(values)
    trimmed <- stringi::stri_trim_both(values)
    evidence <- trimmed[!is.na(trimmed) & trimmed != "" & !(trimmed %in% na_strings)]

    if (length(evidence) > 0) {
      return(mean(is_numeric_like_vector(evidence)) >= 0.5)
    }

    normalized_name <- tolower(gsub("[^A-Za-z0-9]+", "_", column_name))
    grepl("(^|_)(value|amount|count|number|total|rate|ratio|percent|percentage|index|estimate|population|balance|price|score|quantity|volume|obs_value)(_|$)", normalized_name)
  }

  ndbc_historical_missing_marker <- function(values, column_name) {
    if (!is_ndbc_historical_input) return(rep(FALSE, length(values)))

    ndbc_missing_patterns <- c(
      wdir = "^999(?:\\.0+)?$",
      wspd = "^99(?:\\.0+)?$",
      gst = "^99(?:\\.0+)?$",
      wvht = "^99(?:\\.0+)?$",
      dpd = "^99(?:\\.0+)?$",
      apd = "^99(?:\\.0+)?$",
      mwd = "^999(?:\\.0+)?$",
      pres = "^9999(?:\\.0+)?$",
      atmp = "^999(?:\\.0+)?$",
      wtmp = "^999(?:\\.0+)?$",
      dewp = "^999(?:\\.0+)?$",
      vis = "^99(?:\\.0+)?$",
      tide = "^99(?:\\.0+)?$"
    )
    normalized_name <- tolower(gsub("[^A-Za-z0-9]+", "_", column_name))
    pattern <- unname(ndbc_missing_patterns[normalized_name])
    if (length(pattern) == 0L || is.na(pattern)) return(rep(FALSE, length(values)))

    trimmed <- stringi::stri_trim_both(as.character(values))
    !is.na(trimmed) & grepl(pattern, trimmed)
  }

  is_missing_marker <- function(values, column_name) {
    trimmed <- stringi::stri_trim_both(as.character(values))
    wrapped_missing <- !is.na(trimmed) & grepl("^\\((NA|N/A|NULL|ND)\\)$", trimmed, ignore.case = TRUE)
    colon_status_missing <- !is.na(trimmed) & grepl(
      "^:\\s*(?:@[A-Za-z0-9_]+|[A-Za-z][A-Za-z0-9_]*)?\\s*$",
      trimmed,
      perl = TRUE
    )
    unambiguous_na_strings <- setdiff(na_strings, ambiguous_na_strings)
    asterisk_missing <- !is.na(trimmed) & grepl("^\\*+$", trimmed)
    missing <- is.na(trimmed) | trimmed %in% unambiguous_na_strings |
      wrapped_missing | colon_status_missing | asterisk_missing |
      ndbc_historical_missing_marker(values, column_name)

    if (any(trimmed == "NA", na.rm = TRUE) &&
        .is_iso_alpha2_field(trimmed, column_name, na_strings)) {
      missing[!is.na(trimmed) & trimmed == "NA"] <- FALSE
    }

    has_ambiguous_marker <- length(ambiguous_na_strings) > 0 &&
      any(trimmed %in% ambiguous_na_strings, na.rm = TRUE)
    if (has_ambiguous_marker && is_numeric_context(values, column_name)) {
      missing <- missing | trimmed %in% ambiguous_na_strings
    }

    missing
  }

  apply_excel_date_stage <- function(df, audit_log) {
    list(data = df, audit = audit_log)
  }

  parse_series_oriented_sheet <- function(raw_mat) {
    if (nrow(raw_mat) < 4L || ncol(raw_mat) < 3L) return(NULL)

    first_column <- stringi::stri_trim_both(as.character(raw_mat[, 1]))
    normalized_labels <- tolower(gsub("[^A-Za-z0-9]+", "_", first_column))
    normalized_labels <- gsub("^_|_$", "", normalized_labels)
    series_id_rows <- which(normalized_labels == "series_id")
    if (length(series_id_rows) != 1L || series_id_rows >= nrow(raw_mat) - 1L) {
      return(NULL)
    }

    series_id_row <- series_id_rows[[1]]
    time_rows <- seq.int(series_id_row + 1L, nrow(raw_mat))
    raw_dates <- stringi::stri_trim_both(as.character(raw_mat[time_rows, 1]))
    serial_dates <- suppressWarnings(as.numeric(raw_dates))
    valid_serial_dates <- !is.na(serial_dates) & serial_dates >= 20000 & serial_dates <= 70000
    text_dates <- suppressWarnings(as.Date(raw_dates, format = "%d-%b-%Y"))
    valid_text_dates <- !is.na(text_dates)
    if (sum(valid_serial_dates) >= 2L && mean(valid_serial_dates) >= 0.8) {
      valid_time_rows <- valid_serial_dates
      parsed_dates <- serial_dates
    } else if (sum(valid_text_dates) >= 2L && mean(valid_text_dates) >= 0.8) {
      valid_time_rows <- valid_text_dates
      parsed_dates <- text_dates
    } else {
      return(NULL)
    }

    raw_series_ids <- stringi::stri_trim_both(as.character(raw_mat[series_id_row, -1]))
    series_columns <- which(!is.na(raw_series_ids) & raw_series_ids != "") + 1L
    if (length(series_columns) < 2L) return(NULL)

    raw_values <- as.character(raw_mat[time_rows[valid_time_rows], series_columns, drop = FALSE])
    missing_values <- is_missing_marker(as.vector(raw_values), "value")
    numeric_values <- suppressWarnings(as.numeric(as.vector(raw_values)))
    observed_values <- !is.na(as.vector(raw_values)) &
      stringi::stri_trim_both(as.vector(raw_values)) != "" & !missing_values
    if (sum(observed_values) == 0L ||
        mean(!is.na(numeric_values[observed_values])) < 0.8) {
      return(NULL)
    }
    numeric_values[missing_values] <- NA_real_

    metadata_rows <- seq_len(series_id_row)
    metadata_rows <- metadata_rows[vapply(metadata_rows, function(row_index) {
      values <- raw_mat[row_index, series_columns]
      any(!is.na(values) & stringi::stri_trim_both(as.character(values)) != "")
    }, logical(1))]
    metadata_names <- vapply(metadata_rows, function(row_index) {
      name <- normalized_labels[[row_index]]
      if (is.na(name) || name == "") {
        if (row_index == metadata_rows[[1]]) "data_item_description" else "metadata"
      } else {
        name
      }
    }, character(1))
    metadata_names <- make.unique(metadata_names, sep = "_")

    n_dates <- sum(valid_time_rows)
    cleaned <- list(date = parsed_dates[valid_time_rows])
    series_id_index <- match("series_id", metadata_names)
    cleaned$series_id <- rep(
      stringi::stri_trim_both(as.character(raw_mat[metadata_rows[[series_id_index]], series_columns])),
      each = n_dates
    )
    for (metadata_position in setdiff(seq_along(metadata_rows), series_id_index)) {
      row_index <- metadata_rows[[metadata_position]]
      metadata_name <- metadata_names[[metadata_position]]
      metadata_values <- stringi::stri_trim_both(
        as.character(raw_mat[row_index, series_columns])
      )
      observed_metadata <- !is.na(metadata_values) & metadata_values != ""
      numeric_metadata <- suppressWarnings(as.numeric(metadata_values))
      retain_as_text <- grepl(
        "(^|_)(id|unit|type|frequency|description|title|source|publication)(_|$)",
        metadata_name
      )
      if (!retain_as_text && any(observed_metadata) &&
          all(!is.na(numeric_metadata[observed_metadata]))) {
        metadata_values <- numeric_metadata
      }
      cleaned[[metadata_name]] <- rep(metadata_values, each = n_dates)
    }
    cleaned$value <- numeric_values
    df <- as.data.frame(cleaned, stringsAsFactors = FALSE, check.names = FALSE)
    if (clean_vars) df <- clean_variable_names(df)

    audit_log <- input_audit
    audit_log[["Series-Oriented Workbook Reshaped"]] <- length(series_columns)
    if (is_csv) audit_log[["Series-Oriented Flat File Reshaped"]] <- length(series_columns)
    date_stage <- apply_excel_date_stage(df, audit_log)
    list(success = TRUE, data = date_stage$data, audit = date_stage$audit)
  }
  
  last_error <- "Could not parse any sheet."
  auto_select_best_sheet <- !is_df && !is_csv && is.null(sheet) && length(sheets_to_try) > 1
  best_res <- NULL
  best_score <- -Inf
  audit_to_data_frame <- function(audit) {
      audit_values <- vapply(audit, function(value) {
          if (length(value) == 0) return("0")
          if (length(value) > 1) return(as.character(length(value)))
          as.character(value[[1]])
      }, character(1))
      data.frame(Operation = names(audit), Count = audit_values, stringsAsFactors = FALSE)
  }
  format_success_result <- function(res) {
      if (!return_audit) return(res$data)

      if (extract_all_blocks) {
          audit_dfs <- lapply(res$audit, function(aud) {
              df <- audit_to_data_frame(aud)
              df[df$Count != "0", , drop = FALSE]
          })
          return(list(data = res$data, audit = audit_dfs))
      }

      audit_df <- audit_to_data_frame(res$audit)
      audit_df <- audit_df[audit_df$Count != "0", , drop = FALSE]
      rownames(audit_df) <- NULL
      list(data = res$data, audit = audit_df)
  }
  
  for (s in sheets_to_try) {
    res <- tryCatch({
      if (is_df) {
          raw_data <- as.data.frame(file_path, stringsAsFactors = FALSE)
          if (!all(grepl("^(V|X|Col)[0-9A-Za-z]*$|^\\.\\.\\.[0-9]+$", colnames(raw_data)))) {
              raw_data <- rbind(colnames(raw_data), raw_data)
          }
          colnames(raw_data) <- NULL
      } else if (is_csv) {
          default_sep <- if (ext %in% c("tsv", "rdb")) "\t" else ","
          encoding_connection <- if (is_gzip) gzfile(file_path, open = "rb") else file(file_path, open = "rb")
          encoding_probe <- tryCatch(
              readBin(encoding_connection, what = "raw", n = 8L * 1024L * 1024L),
              finally = close(encoding_connection)
          )
          has_utf16le_bom <- length(encoding_probe) >= 2L && all(
              as.integer(encoding_probe[1:2]) == c(255L, 254L)
          )
          has_utf16be_bom <- length(encoding_probe) >= 2L && all(
              as.integer(encoding_probe[1:2]) == c(254L, 255L)
          )
          has_utf16_bom <- has_utf16le_bom || has_utf16be_bom
          non_ascii_positions <- which(as.integer(encoding_probe) > 127L)
          encoding_sample <- if (length(non_ascii_positions) == 0L) {
              utils::head(encoding_probe, 32L * 1024L)
          } else {
              # Scan a broad raw prefix cheaply, then convert only the first
              # non-ASCII neighborhood rather than every multi-megabyte byte.
              sample_start <- max(1L, non_ascii_positions[[1L]] - 16L)
              sample_end <- min(length(encoding_probe), sample_start + 32L * 1024L - 1L)
              encoding_probe[seq.int(sample_start, sample_end)]
          }
          raw_text <- if (has_utf16_bom) "" else rawToChar(encoding_sample)
          utf8_text <- if (has_utf16_bom) {
              NA_character_
          } else {
              suppressWarnings(iconv(raw_text, from = "UTF-8", to = "UTF-8", sub = NA))
          }
          cp932_text <- if (is.na(utf8_text)) {
              suppressWarnings(iconv(raw_text, from = "CP932", to = "UTF-8", sub = NA))
          } else {
              NA_character_
          }
          japanese_pattern <- paste0("[", intToUtf8(0x3040), "-", intToUtf8(0x30FF),
              intToUtf8(0x3400), "-", intToUtf8(0x9FFF), "]")
          japanese_characters <- if (!is.na(cp932_text)) {
              regmatches(cp932_text, gregexpr(japanese_pattern, cp932_text, perl = TRUE))[[1L]]
          } else {
              character()
          }
          japanese_non_whitespace <- if (!is.na(cp932_text)) {
              gsub("[[:space:]]", "", cp932_text)
          } else {
              ""
          }
          has_japanese_text <- !is.na(cp932_text) &&
              ((length(japanese_characters) >= 10L &&
                  length(japanese_characters) / max(nchar(japanese_non_whitespace), 1L) >= 0.01) ||
                  (length(japanese_characters) >= 2L &&
                      length(japanese_characters) / max(nchar(japanese_non_whitespace), 1L) >= 0.25))
          cp949_text <- if (is.na(utf8_text) && !has_japanese_text) {
              suppressWarnings(iconv(raw_text, from = "CP949", to = "UTF-8", sub = NA))
          } else {
              NA_character_
          }
          hangul_pattern <- paste0("[", intToUtf8(0xAC00), "-", intToUtf8(0xD7A3), "]")
          hangul_characters <- if (!is.na(cp949_text)) {
              regmatches(cp949_text, gregexpr(hangul_pattern, cp949_text, perl = TRUE))[[1L]]
          } else {
              character()
          }
          # Require multiple distinct Hangul characters to avoid reclassifying
          # unrelated East Asian legacy encodings as Korean text.
          has_korean_text <- !is.na(cp949_text) &&
              length(hangul_characters) >= 8L && length(unique(hangul_characters)) >= 5L
          big5_text <- if (is.na(utf8_text) && !has_japanese_text && !has_korean_text) {
              suppressWarnings(iconv(raw_text, from = "CP950", to = "UTF-8", sub = NA))
          } else {
              NA_character_
          }
          big5_han_characters <- if (!is.na(big5_text)) {
              gsub("[^\u3400-\u9fff]", "", big5_text)
          } else {
              ""
          }
          big5_non_whitespace <- if (!is.na(big5_text)) {
              gsub("[[:space:]]", "", big5_text)
          } else {
              ""
          }
          big5_han_count <- nchar(big5_han_characters)
          has_traditional_chinese_text <- !is.na(big5_text) &&
              (big5_han_count >= 5L ||
                  (big5_han_count >= 2L &&
                      big5_han_count / max(nchar(big5_non_whitespace), 1L) >= 0.25))
          windows1250_text <- if (is.na(utf8_text) && !has_japanese_text &&
              !has_traditional_chinese_text) {
              suppressWarnings(iconv(raw_text, from = "windows-1250", to = "UTF-8", sub = NA))
          } else {
              NA_character_
          }
          central_european_pattern <- paste0("[", intToUtf8(0x0100), "-", intToUtf8(0x017F), "]")
          central_european_characters <- if (!is.na(windows1250_text)) {
              regmatches(
                  windows1250_text,
                  gregexpr(central_european_pattern, windows1250_text, perl = TRUE)
              )[[1L]]
          } else {
              character()
          }
          # Multiple distinct letters avoid promoting ordinary western Latin-1 text.
          has_windows1250_text <- length(unique(central_european_characters)) >= 3L
          windows1251_text <- if (is.na(utf8_text) && !has_japanese_text &&
              !has_traditional_chinese_text) {
              suppressWarnings(iconv(raw_text, from = "windows-1251", to = "UTF-8", sub = NA))
          } else {
              NA_character_
          }
          cyrillic_characters <- if (!is.na(windows1251_text)) {
              regmatches(
                  windows1251_text,
                  gregexpr(cyrillic_pattern, windows1251_text, perl = TRUE)
              )[[1L]]
          } else {
              character()
          }
          cyrillic_non_whitespace <- if (!is.na(windows1251_text)) {
              gsub("[[:space:]]", "", windows1251_text)
          } else {
              ""
          }
          # Dense Cyrillic distinguishes CP1251 from CP1250 cross-decoding noise.
          has_windows1251_text <- length(unique(cyrillic_characters)) >= 5L &&
              length(cyrillic_characters) / max(nchar(cyrillic_non_whitespace), 1L) >= 0.20
          file_encoding <- if (has_utf16le_bom) {
              "UTF-16LE"
          } else if (has_utf16be_bom) {
              "UTF-16BE"
          } else if (!is.na(utf8_text)) {
              "UTF-8"
          } else if (has_japanese_text) {
              "CP932"
          } else if (has_korean_text) {
              "CP949"
          } else if (has_traditional_chinese_text) {
              "CP950"
          } else if (has_windows1251_text) {
              "windows-1251"
          } else if (has_windows1250_text) {
              "windows-1250"
          } else {
              "latin1"
          }
          decoded_utf16_text <- NULL
          if (file_encoding %in% c("UTF-16LE", "UTF-16BE")) {
              decoded_connection <- if (is_gzip) {
                  gzfile(file_path, open = "rb")
              } else {
                  file(file_path, open = "rb")
              }
              decoded_chunks <- tryCatch({
                  chunks <- list()
                  repeat {
                      chunk <- readBin(decoded_connection, what = "raw", n = 64L * 1024L)
                      if (length(chunk) == 0L) break
                      chunks[[length(chunks) + 1L]] <- chunk
                  }
                  chunks
              }, finally = close(decoded_connection))
              decoded_utf16_text <- suppressWarnings(stringi::stri_encode(
                  do.call(c, decoded_chunks),
                  from = file_encoding,
                  to = "UTF-8"
              ))
              if (is.na(decoded_utf16_text)) {
                  stop("UTF-16 text could not be decoded as UTF-8.")
              }
              decoded_utf16_text <- sub(
                  paste0("^", intToUtf8(0xFEFF)),
                  "",
                  decoded_utf16_text
              )
          }
          if (file_encoding == "UTF-16LE") {
              input_audit[["UTF-16LE Text Decoded"]] <- 1L
          }
          if (file_encoding == "UTF-16BE") {
              input_audit[["UTF-16BE Text Decoded"]] <- 1L
          }
          if (file_encoding == "CP932") {
              input_audit[["CP932 Text Decoded"]] <- 1L
          }
          if (file_encoding == "CP949") {
              input_audit[["CP949 Text Decoded"]] <- 1L
          }
          if (file_encoding == "CP950") {
              input_audit[["Big5 Text Decoded"]] <- 1L
          }
          if (file_encoding == "windows-1250") {
              input_audit[["Windows-1250 Text Decoded"]] <- 1L
          }
          if (file_encoding == "windows-1251") {
              input_audit[["Windows-1251 Text Decoded"]] <- 1L
          }
          if (!is.null(decoded_utf16_text)) {
              sample_lines <- strsplit(decoded_utf16_text, "\\r?\\n", perl = TRUE)[[1L]]
          } else if (is_gzip) {
              sample_connection <- gzfile(file_path, open = "rt", encoding = file_encoding)
              sample_lines <- tryCatch(
                  readLines(sample_connection, n = 1000L, warn = FALSE),
                  finally = close(sample_connection)
              )
          } else {
              sample_lines <- readLines(file_path, n = 1000L, warn = FALSE, encoding = file_encoding)
          }
          if (file_encoding %in% c("CP932", "CP949", "CP950", "windows-1250", "windows-1251") && !is_gzip) {
              sample_lines <- iconv(sample_lines, from = file_encoding, to = "UTF-8")
          }
          first_non_blank_line <- sample_lines[
              which(stringi::stri_trim_both(sample_lines) != "")[1]
          ]
          if (length(first_non_blank_line) > 0 && grepl(
              "^\\s*(<!doctype\\s+html|<html(?:\\s|>)|<head(?:\\s|>)|<body(?:\\s|>))",
              first_non_blank_line,
              ignore.case = TRUE
          )) {
              stop("Input appears to be an HTML document rather than a delimited table.")
          }
          if (length(first_non_blank_line) > 0 && grepl(
              "^\\s*[\\[{]",
              first_non_blank_line
          )) {
              stop("Input appears to be a JSON document rather than a delimited table.")
          }
          explicit_table_lines <- NULL
          table_begin_pattern <- "^\\s*[!#][[:alnum:]_ -]*table[_[:space:]]*begin\\s*$"
          table_end_pattern <- "^\\s*[!#][[:alnum:]_ -]*table[_[:space:]]*end\\s*$"
          sample_table_begin <- which(grepl(
              table_begin_pattern,
              sample_lines,
              ignore.case = TRUE
          ))
          if (length(sample_table_begin) > 0L) {
              boundary_connection <- if (!is.null(decoded_utf16_text)) {
                  textConnection(decoded_utf16_text, open = "r")
              } else if (is_gzip) {
                  gzfile(file_path, open = "rt", encoding = file_encoding)
              } else {
                  file(file_path, open = "rt", encoding = file_encoding)
              }
              boundary_lines <- tryCatch(
                  readLines(boundary_connection, warn = FALSE),
                  finally = close(boundary_connection)
              )
              table_begin <- which(grepl(
                  table_begin_pattern,
                  boundary_lines,
                  ignore.case = TRUE
              ))[[1]]
              table_end_candidates <- which(grepl(
                  table_end_pattern,
                  boundary_lines,
                  ignore.case = TRUE
              ))
              table_end_candidates <- table_end_candidates[table_end_candidates > table_begin]
              if (length(table_end_candidates) > 0L) {
                  table_end <- table_end_candidates[[1]]
                  explicit_table_lines <- boundary_lines[
                      seq.int(table_begin + 1L, table_end - 1L)
                  ]
                  if (length(explicit_table_lines) < 2L) {
                      stop("Explicit table boundary does not contain a complete table.")
                  }
                  sample_lines <- explicit_table_lines
                  input_audit[["Explicit Table Boundary Extracted"]] <- 1L
              }
          }
          leading_comment_count <- 0L
          for (line in sample_lines) {
              if (grepl("^\\s*#", line)) {
                  leading_comment_count <- leading_comment_count + 1L
              } else {
                  break
              }
          }
          delimiter_candidates <- unique(c(default_sep, ",", ";", "\t", "|"))
          delimiter_scores <- vapply(delimiter_candidates, function(candidate) {
              sum(vapply(sample_lines, function(line) {
                  length(strsplit(line, candidate, fixed = TRUE)[[1]]) - 1L
              }, integer(1)))
          }, integer(1))
          non_blank_lines <- sample_lines[stringi::stri_trim_both(sample_lines) != ""]
          fixed_width_lines <- non_blank_lines[
              !grepl("^\\s*#", non_blank_lines)
          ]
          has_delimited_fields <- any(vapply(
              fixed_width_lines,
              function(line) grepl("[,;\\t|]", line),
              logical(1)
          ))
          looks_like_fixed_width_records <- length(fixed_width_lines) >= 3L &&
              !has_delimited_fields &&
              mean(grepl("^[[:alnum:]][[:space:]]{2,}[^[:space:]]{12,}", fixed_width_lines)) >= 0.8
          if (looks_like_fixed_width_records) {
              stop(
                  "Input appears to be a fixed-width record file rather than a delimited table. ",
                  "Convert it to CSV or provide a field schema before cleaning."
              )
          }
          has_tabular_tabs <- length(non_blank_lines) > 0 &&
              mean(vapply(non_blank_lines, function(line) grepl("\t", line, fixed = TRUE), logical(1))) >= 0.5
          whitespace_header_index <- NA_integer_
          commented_whitespace_header_fields <- NULL
          whitespace_fields <- list()
          if (length(sample_lines) >= 2L) {
              whitespace_fields <- lapply(sample_lines, function(line) {
                  trimmed <- stringi::stri_trim_both(line)
                  if (trimmed == "") return(character(0))
                  strsplit(trimmed, "\\s+")[[1]]
              })
              whitespace_candidates <- vapply(seq_len(length(whitespace_fields) - 1L), function(i) {
                  header_fields <- whitespace_fields[[i]]
                  data_fields <- whitespace_fields[[i + 1L]]
                  if (length(header_fields) < 2L || length(header_fields) != length(data_fields)) {
                      return(FALSE)
                  }
                  header_like <- all(grepl("^[[:alpha:]_][[:alnum:]_.-]*$", header_fields))
                  numeric_count <- sum(!is.na(suppressWarnings(as.numeric(data_fields))))
                  header_like && numeric_count >= max(2L, ceiling(length(data_fields) * 0.3))
              }, logical(1))
              if (any(whitespace_candidates)) {
                  whitespace_header_index <- which(whitespace_candidates)[[1]]
              }
          }
          if (leading_comment_count > 0L &&
              length(whitespace_fields) > leading_comment_count) {
              data_fields <- whitespace_fields[[leading_comment_count + 1L]]
              commented_whitespace_candidates <- vapply(
                  seq_len(leading_comment_count),
                  function(i) {
                      header_fields <- strsplit(
                          sub("^\\s*#", "", sample_lines[[i]]),
                          "\\s+"
                      )[[1]]
                      if (length(header_fields) < 2L ||
                          length(header_fields) != length(data_fields)) {
                          return(FALSE)
                      }
                      header_like <- all(grepl(
                          "^[[:alpha:]_][[:alnum:]_.-]*$",
                          header_fields
                      ))
                      numeric_count <- sum(!is.na(suppressWarnings(as.numeric(data_fields))))
                      header_like &&
                          numeric_count >= max(2L, ceiling(length(data_fields) * 0.3))
                  },
                  logical(1)
              )
              if (any(commented_whitespace_candidates)) {
                  header_index <- which(commented_whitespace_candidates)[[1]]
                  commented_whitespace_header_fields <- strsplit(
                      sub("^\\s*#", "", sample_lines[[header_index]]),
                      "\\s+"
                  )[[1]]
                  ndbc_headers <- tolower(commented_whitespace_header_fields)
                  is_ndbc_historical_input <- all(
                      c("yy", "mm", "dd", "hh", "wdir", "wspd") %in% ndbc_headers
                  )
              }
          }
          is_whitespace_delimited <-
              !is.na(whitespace_header_index) ||
              !is.null(commented_whitespace_header_fields)
          sep <- if (ext == "tsv" && has_tabular_tabs) {
              "\t"
          } else if (max(delimiter_scores) > 0L) {
              delimiter_candidates[which.max(delimiter_scores)]
          } else {
              default_sep
          }
          if (sep == "|") {
              input_audit[["Pipe-Delimited Text Detected"]] <- 1L
          }
          commented_header <- FALSE
          if (!is_whitespace_delimited && leading_comment_count > 0L && length(sample_lines) > leading_comment_count) {
              header_line <- sample_lines[[leading_comment_count]]
              data_line <- sample_lines[[leading_comment_count + 1L]]
              header_fields <- strsplit(sub("^\\s*#", "", header_line), sep, fixed = TRUE)[[1]]
              data_fields <- strsplit(data_line, sep, fixed = TRUE)[[1]]
              matching_width <- length(header_fields) == length(data_fields) ||
                  (endsWith(data_line, sep) && length(data_fields) < length(header_fields))
              commented_header <- grepl("^\\s*#[[:alpha:]_]", header_line) &&
                  length(header_fields) >= 2L &&
                  matching_width &&
                  all(nzchar(stringi::stri_trim_both(header_fields)))
          }
          skip_lines <- if (!is.null(commented_whitespace_header_fields)) {
              leading_comment_count
          } else if (is_whitespace_delimited) {
              whitespace_header_index - 1L
          } else if (commented_header) {
              leading_comment_count - 1L
          } else {
              leading_comment_count
          }
          if (commented_header) {
              input_audit[["Commented Headers Retained"]] <- 1L
          }
          data_connection <- if (!is.null(explicit_table_lines)) {
              textConnection(explicit_table_lines, open = "r")
          } else if (!is.null(decoded_utf16_text)) {
              textConnection(decoded_utf16_text, open = "r")
          } else if (is_gzip) {
              gzfile(file_path, open = "rt", encoding = file_encoding)
          } else {
              file(file_path, open = "rt", encoding = file_encoding)
          }
          on.exit(close(data_connection), add = TRUE)
          if (is_whitespace_delimited) {
              input_audit[["Whitespace-Delimited Text Detected"]] <- 1L
              raw_data <- suppressMessages(read.table(
                  data_connection,
                  header = FALSE,
                  sep = "",
                  stringsAsFactors = FALSE,
                  na.strings = NULL,
                  colClasses = "character",
                  fill = TRUE,
                  quote = "",
                  comment.char = "",
                  skip = skip_lines,
                  strip.white = TRUE
              ))
              if (!is.null(commented_whitespace_header_fields)) {
                  raw_data <- as.data.frame(
                      rbind(commented_whitespace_header_fields, as.matrix(raw_data)),
                      stringsAsFactors = FALSE
                  )
                  input_audit[["Commented Whitespace Headers Retained"]] <- 1L
                  if (is_ndbc_historical_input) {
                      input_audit[["NDBC Historical Format Detected"]] <- 1L
                  }
              }
          } else {
              count_connection <- if (!is.null(explicit_table_lines)) {
                  textConnection(explicit_table_lines, open = "r")
              } else if (!is.null(decoded_utf16_text)) {
                  textConnection(decoded_utf16_text, open = "r")
              } else if (is_gzip) {
                  gzfile(file_path, open = "rt", encoding = file_encoding)
              } else {
                  file(file_path, open = "rt", encoding = file_encoding)
              }
              field_counts <- tryCatch(
                  utils::count.fields(
                      count_connection,
                      sep = sep,
                      quote = "\"",
                      skip = skip_lines,
                      blank.lines.skip = FALSE,
                      comment.char = if (commented_header) "" else "#"
                  ),
                  finally = close(count_connection)
              )
              field_counts <- field_counts[is.finite(field_counts) & field_counts > 0L]
              max_fields <- if (length(field_counts)) max(field_counts) else 1L
              raw_data <- suppressMessages(read.table(
                  data_connection,
                  header = FALSE,
                  sep = sep,
                  stringsAsFactors = FALSE,
                  na.strings = NULL,
                  colClasses = "character",
                  strip.white = FALSE,
                  blank.lines.skip = FALSE,
                  skip = skip_lines,
                  comment.char = if (commented_header) "" else "#",
                  quote = "\"",
                  fill = TRUE,
                  col.names = paste0("V", seq_len(max_fields))
              ))
          }

          # Eurostat-style TSV files store several dimensions as one comma-delimited
          # code tuple before the time columns. Expand it only when every row agrees.
          if (sep == "\t" && nrow(raw_data) >= 2 && ncol(raw_data) >= 2) {
              first_header <- as.character(raw_data[1, 1])
              dimension_part <- sub("\\\\TIME_PERIOD.*$", "", first_header)
              dimension_names <- trimws(strsplit(dimension_part, ",", fixed = TRUE)[[1]])
              code_tuples <- strsplit(as.character(raw_data[-1, 1]), ",", fixed = TRUE)
              consistent_tuples <- length(dimension_names) >= 2 &&
                  all(nzchar(dimension_names)) &&
                  all(vapply(code_tuples, length, integer(1)) == length(dimension_names))

              if (grepl("\\\\TIME_PERIOD", first_header) && consistent_tuples) {
                  dimension_values <- do.call(rbind, lapply(code_tuples, trimws))
                  value_headers <- trimws(as.character(raw_data[1, -1, drop = TRUE]))
                  value_data <- as.matrix(raw_data[-1, -1, drop = FALSE])
                  raw_data <- as.data.frame(
                      rbind(c(dimension_names, value_headers), cbind(dimension_values, value_data)),
                      stringsAsFactors = FALSE
                  )
                  input_audit[["Delimited Dimensions Expanded"]] <- length(dimension_names)
              }
          }
      } else {
          raw_data <- suppressMessages(readxl::read_excel(file_path, sheet = s, col_names = FALSE, .name_repair = "minimal", trim_ws = FALSE))
      }
      if (nrow(raw_data) == 0) stop("Empty sheet")
      
      raw_mat <- as.matrix(raw_data)
      if (nrow(raw_mat) >= 3L && ncol(raw_mat) >= 5L) {
          first_values <- stringi::stri_trim_both(as.character(raw_mat[1L, ]))
          second_values <- stringi::stri_trim_both(as.character(raw_mat[2L, ]))
          third_values <- stringi::stri_trim_both(as.character(raw_mat[3L, ]))
          first_non_empty <- first_values[!is.na(first_values) & first_values != ""]
          second_non_empty <- second_values[!is.na(second_values) & second_values != ""]
          third_non_empty <- third_values[!is.na(third_values) & third_values != ""]
          first_label <- if (length(first_non_empty)) tolower(first_non_empty[[1L]]) else ""
          japanese_download_label <- intToUtf8(c(
              0x30C0, 0x30A6, 0x30F3, 0x30ED, 0x30FC, 0x30C9
          ))
          metadata_label <- grepl(
              "^(?:download|updated|release|as[[:space:]]+(?:at|of)|date)",
              first_label,
              ignore.case = TRUE
          ) || startsWith(first_label, japanese_download_label)
          metadata_has_year <- any(grepl("(?:19|20)[0-9]{2}", first_non_empty))
          second_is_textual <- length(second_non_empty) > 0L &&
              mean(!is_numeric_like_vector(second_non_empty)) >= 0.75
          has_sparse_metadata_preamble <-
              length(first_non_empty) >= 2L &&
              length(second_non_empty) >= 5L &&
              length(first_non_empty) < length(second_non_empty) * 0.6 &&
              length(third_non_empty) >= length(second_non_empty) * 0.7 &&
              second_is_textual && (metadata_label || metadata_has_year)
          if (has_sparse_metadata_preamble) {
              raw_data <- raw_data[-1L, , drop = FALSE]
              rownames(raw_data) <- NULL
              raw_mat <- as.matrix(raw_data)
              input_audit[["Initial Metadata Preamble Bypassed"]] <- 1L
          }
      }

      series_oriented_result <- parse_series_oriented_sheet(raw_mat)
      if (!is.null(series_oriented_result)) {
          series_oriented_result
      } else {
      if (extract_all_blocks) {
          empty_cols <- apply(raw_mat, 2, function(col) {
              valid <- col[!is.na(col) & stringi::stri_trim_both(col) != ""]
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
      
      numeric_like_mat <- matrix(
          is_numeric_like_vector(as.character(raw_mat)),
          nrow = nrow(raw_mat),
          ncol = ncol(raw_mat)
      )
      valid_cells <- !is.na(raw_mat) & raw_mat != ""
      num_counts <- rowSums(numeric_like_mat & valid_cells)

      # Ordinal percentile labels can look numeric after annotation cleanup.
      ordinal_header_cells <- matrix(
          grepl("^[[:space:]]*(?:[0-9]{1,2}|100)(?:st|nd|rd|th)[[:space:]]*$",
              as.character(raw_mat), ignore.case = TRUE),
          nrow = nrow(raw_mat), ncol = ncol(raw_mat)
      )
      statistical_header_candidates <- which(
          rowSums(ordinal_header_cells) >= 2L &
              rowSums(numeric_like_mat & valid_cells & !ordinal_header_cells) == 0L
      )
      statistical_header_rows <- statistical_header_candidates[vapply(
          statistical_header_candidates, function(r) {
              labels <- tolower(stringi::stri_trim_both(as.character(raw_mat[r, ])))
              any(grepl("^(std[[:space:]_]+(dev|deviation)|standard[[:space:]_]+deviation)$", labels)) &&
                  any(grepl("^(average|avg|mean)([[:space:]_]|$)", labels))
          }, logical(1)
      )]
      if (length(statistical_header_rows) > 0L) {
          num_counts[statistical_header_rows] <- 0L
          input_audit[["Statistical Header Rows Recognized"]] <- length(statistical_header_rows)
      }

      temporal_value_pattern <- paste0(
          "^(?:(?:1[89]|20|21)[0-9]{2}(?:[-/](?:0[1-9]|1[0-2])(?:[-/](?:0[1-9]|[12][0-9]|3[01]))?)?",
          "|(?:0?[1-9]|1[0-2])/(?:0?[1-9]|[12][0-9]|3[01])/[0-9]{4}",
          "|(?:0?[1-9]|[12][0-9]|3[01])/(?:0?[1-9]|1[0-2])/[0-9]{4})(?:[ T].*)?$"
      )
      temporal_period_pattern <- "^(?:19|20)[0-9]{2}[[:space:]]+(?:Q[1-4]|M(?:0[1-9]|1[0-2])|[A-Za-z]{3,9})$"
      temporal_record_pattern <- paste0("(?:", temporal_value_pattern, "|", temporal_period_pattern, ")")
      first_column_values <- stringi::stri_trim_both(as.character(raw_mat[, 1L]))
      first_column_non_empty <- !is.na(first_column_values) & first_column_values != ""
      temporal_record_rows <- first_column_non_empty &
          grepl(temporal_record_pattern, first_column_values)
      first_header_is_temporal <- first_column_non_empty[1L] && grepl(
          "(^|_)(date|time|datetime|timestamp|period)(_|$)",
          tolower(gsub("[^A-Za-z0-9]+", "_", first_column_values[1L]))
      )
      first_header_is_identifier <- first_column_non_empty[1L] && grepl(
          "(^|_)(id|identifier|code|key|uuid|guid|dguid|unitid|isbn|issn|upc|barcode|vector|coordinate|fips|geofips|station|state|county|sumlev|region|division|regdep|lsoa)(_|$)",
          tolower(gsub("[^A-Za-z0-9]+", "_", first_column_values[1L]))
      )
      temporal_axis_in_input <- sum(temporal_record_rows) >= 3L && (
          first_header_is_temporal ||
          (!first_header_is_identifier &&
              sum(temporal_record_rows) / max(1L, sum(first_column_non_empty)) >= 0.5)
      )

      is_data_row <- num_counts >= 1 | (temporal_axis_in_input & temporal_record_rows)
      true_runs_indices <- which(is_data_row == TRUE)
      
      if (length(true_runs_indices) == 0) {
        non_empty_counts <- rowSums(valid_cells)
        candidate_rows <- which(non_empty_counts >= 2)
        if (length(candidate_rows) < 2) {
          stop("Could not detect any numeric panel data block.")
        }

        modal_width <- as.integer(names(sort(table(non_empty_counts[candidate_rows]), decreasing = TRUE)[1]))
        structural_rows <- which(non_empty_counts == modal_width)
        if (length(structural_rows) < 2) {
          stop("Could not detect any numeric panel data block.")
        }

        # A stable multi-column text table is still a valid panel even when it
        # contains only dimension or status fields and no numeric observations.
        num_counts[structural_rows] <- 1L
        is_data_row <- num_counts >= 1
        true_runs_indices <- structural_rows
        input_audit[["Categorical Table Fallback"]] <- 1L
      }
      
      gaps <- diff(true_runs_indices)
      header_after_blank <- vapply(seq_along(gaps), function(i) {
          if (gaps[i] <= 1) return(FALSE)

          between_rows <- seq.int(true_runs_indices[i] + 1, true_runs_indices[i + 1] - 1)
          non_empty_counts <- rowSums(valid_cells[between_rows, , drop = FALSE])
          any(non_empty_counts == 0) && any(non_empty_counts >= 2 & num_counts[between_rows] == 0)
      }, logical(1))
      block_boundaries <- which(gaps > 5 | header_after_blank)
      if (any(header_after_blank)) {
          input_audit[["Blank-Line Tables Separated"]] <- sum(header_after_blank)
      }
      
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
        audit_log <- input_audit
        start_data_row <- min(current_block)
        end_data_row <- max(current_block)
      
      main_block_counts <- num_counts[start_data_row:end_data_row]
      mode_count <- as.numeric(names(sort(table(main_block_counts), decreasing = TRUE)[1]))
      
      # We know header must be above true_start. We walk up looking for a header boundary.
      true_start <- start_data_row
      max_walk_up <- 2
      walked <- 0
      while (true_start > 1 && walked < max_walk_up) {
          non_empty <- sum(!is.na(raw_mat[true_start - 1, ]) & stringi::stri_trim_both(raw_mat[true_start - 1, ]) != "")
          looks_like_header <- FALSE
          if (non_empty > 0) {
              looks_character <- stringi::stri_trim_both(raw_mat[true_start - 1, !is.na(raw_mat[true_start - 1, ])]) != "" &
                                 !vapply(raw_mat[true_start - 1, !is.na(raw_mat[true_start - 1, ])], is_numeric_like, logical(1))
              if (sum(looks_character) >= (length(looks_character) * 0.5)) {
                  looks_like_header <- TRUE
              }
          }
          if (non_empty > 0 && num_counts[true_start - 1] == 0 && !looks_like_header) {
              true_start <- true_start - 1
              walked <- walked + 1
          } else {
              break
          }
      }
      
      density_threshold <- max(1, floor(mode_count * 0.2))
      
      # Keep the whole candidate block until headers are known. The later
      # identifier-aware filter can then retain legitimate sparse tail rows
      # while still excluding ordinary low-density noise.
      true_start <- start_data_row
      true_end <- end_data_row
      
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
          sparse_section_above <- true_start > 1 &&
              non_empty_above == 1L && num_counts[true_start - 1L] == 0L
          
          if (true_start > 1 && non_empty_above > 0 &&
              !sparse_section_above &&
              (non_empty_above < non_empty_current * 0.3)) {
              true_start <- true_start + 1
              next
          }
          
          break
      }
      
      if (true_start > end_data_row) true_start <- end_data_row
      
      initial_record_rows <- seq.int(start_data_row, min(end_data_row, start_data_row + 4L))
      initial_record_matrix <- raw_mat[initial_record_rows, , drop = FALSE]
      initial_non_empty_counts <- rowSums(
          !is.na(initial_record_matrix) & stringi::stri_trim_both(initial_record_matrix) != ""
      )
      initial_date_rows <- apply(initial_record_matrix, 1, function(row) {
          values <- stringi::stri_trim_both(as.character(row))
          any(grepl("^(?:19|20)[0-9]{2}(?:0[1-9]|1[0-2])(?:0[1-9]|[12][0-9]|3[01])$", values))
      })
      initial_code_values <- stringi::stri_trim_both(as.character(initial_record_matrix[, 1]))
      headerless_daily_panel <- start_data_row == 1L && length(initial_record_rows) >= 3L &&
          all(num_counts[initial_record_rows] >= 1L) &&
          diff(range(num_counts[initial_record_rows])) <= 1L &&
          diff(range(initial_non_empty_counts)) <= 1L &&
          all(initial_date_rows) &&
          all(grepl("^[[:alnum:]_.-]+$", initial_code_values))
      initial_numeric_counts <- apply(initial_record_matrix, 1, function(row) {
          sum(!is.na(suppressWarnings(as.numeric(row))))
      })
      headerless_numeric_panel <- start_data_row == 1L && length(initial_record_rows) >= 3L &&
          ncol(initial_record_matrix) >= 3L &&
          all(!is.na(suppressWarnings(as.numeric(initial_record_matrix[, 1])))) &&
          all(initial_numeric_counts >= 1L) &&
          diff(range(initial_non_empty_counts)) <= 1L
      headerless_coded_panel <- start_data_row == 1L &&
          length(initial_record_rows) >= 3L &&
          ncol(initial_record_matrix) >= 3L &&
          all(num_counts[initial_record_rows] >= 1L) &&
          diff(range(num_counts[initial_record_rows])) <= 1L &&
          diff(range(initial_non_empty_counts)) == 0L &&
          all(grepl(
              "^(?:[A-Za-z]+[0-9]+[A-Za-z0-9_.-]*|[0-9]+[A-Za-z]+[A-Za-z0-9_.-]*)$",
              initial_code_values
          )) &&
          !all(initial_date_rows)
      temporal_indices <- which(temporal_record_rows)
      temporal_runs <- if (length(temporal_indices) == 0L) {
          list()
      } else {
          split(temporal_indices, cumsum(c(TRUE, diff(temporal_indices) != 1L)))
      }
      temporal_run_lengths <- lengths(temporal_runs)
      dominant_temporal_run <- if (length(temporal_run_lengths) == 0L) {
          integer()
      } else {
          temporal_runs[[which.max(temporal_run_lengths)]]
      }
      temporal_series_start <- if (length(dominant_temporal_run) == 0L) {
          NA_integer_
      } else {
          dominant_temporal_run[[1L]]
      }
      temporal_series_share <- if (is.na(temporal_series_start)) {
          0
      } else {
          length(dominant_temporal_run) / max(1L, nrow(raw_mat) - temporal_series_start + 1L)
      }
      preceding_label <- if (is.na(temporal_series_start) || temporal_series_start <= 1L) {
          ""
      } else {
          stringi::stri_trim_both(as.character(raw_mat[temporal_series_start - 1L, 1L]))
      }
      normalized_preceding_label <- tolower(gsub(
          "[^A-Za-z0-9]+",
          "_",
          preceding_label
      ))
      has_explicit_temporal_header <- grepl(
          "(^|_)(date|time|datetime|timestamp|period|year)(_|$)",
          normalized_preceding_label
      )
      metadata_prefixed_temporal_panel <- !is_df && !is_csv &&
          !is.na(temporal_series_start) &&
          temporal_series_start > 1L &&
          ncol(raw_mat) >= 2L &&
          length(dominant_temporal_run) >= 3L &&
          temporal_series_share >= 0.8 &&
          !has_explicit_temporal_header
      metadata_prefixed_explicit_temporal_panel <-
          !is.na(temporal_series_start) &&
          temporal_series_start > start_data_row &&
          ncol(raw_mat) >= 2L &&
          length(dominant_temporal_run) >= 3L &&
          temporal_series_share >= 0.8 &&
          has_explicit_temporal_header
      headerless_panel <- headerless_daily_panel || headerless_numeric_panel ||
          headerless_coded_panel ||
          metadata_prefixed_temporal_panel
      if (headerless_panel) {
          true_start <- if (metadata_prefixed_temporal_panel) temporal_series_start else start_data_row
          audit_log[[if (headerless_daily_panel) {
              "Headerless Daily Records Detected"
          } else if (headerless_numeric_panel) {
              "Headerless Numeric Records Detected"
          } else if (headerless_coded_panel) {
              "Headerless Coded Records Detected"
          } else {
              "Preamble Headerless Temporal Series Detected"
          }]] <- 1L
      } else if (metadata_prefixed_explicit_temporal_panel) {
          true_start <- temporal_series_start
          audit_log[["Temporal Metadata Preamble Bypassed"]] <-
              temporal_series_start - 2L
      }

      header_row_index <- max(1, true_start - 1)
      audit_log[["Decoy Rows Bypassed"]] <- header_row_index - 1
      
      extracted_metadata <- list()
      if (header_row_index > 1) {
          decoy_mat <- raw_mat[1:(header_row_index - 1), , drop = FALSE]
          for (r in seq_len(nrow(decoy_mat))) {
              row_vals <- decoy_mat[r, ]
              valid_vals <- row_vals[!is.na(row_vals) & stringi::stri_trim_both(row_vals) != ""]
              
              if (length(valid_vals) == 1) {
                  val <- valid_vals[1]
                  if (grepl(":", val)) {
                      parts <- strsplit(val, ":")[[1]]
                      if (length(parts) == 2) {
                          key <- stringi::stri_trim_both(parts[1])
                          val <- stringi::stri_trim_both(parts[2])
                          extracted_metadata[[key]] <- val
                      }
                  }
              } else if (length(valid_vals) == 2) {
                  key <- stringi::stri_trim_both(valid_vals[1])
                  val <- stringi::stri_trim_both(valid_vals[2])
                  if (nchar(key) < 50) {
                      key <- stringr::str_replace(key, ":\\s*$", "")
                      extracted_metadata[[key]] <- val
                  }
              }
          }
      }
      
      if (!headerless_panel && header_row_index == true_start) {
          true_start <- true_start + 1
      }
      if (true_start <= end_data_row) {
          type_row_values <- raw_mat[true_start, ]
          type_row_values <- stringi::stri_trim_both(type_row_values[!is.na(type_row_values) & type_row_values != ""])
          is_type_descriptor_row <- length(type_row_values) > 1 &&
              all(grepl("^[0-9]+[A-Za-z]+$", type_row_values))
          if (is_type_descriptor_row) {
              true_start <- true_start + 1
              audit_log[["Type Descriptor Rows Dropped"]] <- 1L
          }
      }
      # Helper: detect rows that are pure noise (random-looking uppercase/lowercase strings
      # with no semantic value, e.g. "xdasdad", "WEDEWADAW"). Such rows should NOT be
      # concatenated onto real column names.
      is_noise_header_row <- function(row_vals) {
          non_empty <- row_vals[!is.na(row_vals) & stringi::stri_trim_both(row_vals) != ""]
          if (length(non_empty) == 0) return(FALSE)
          
          # NEW: If any cell contains a URL, email, or long prose sentence -> metadata row
          has_url_or_email <- any(vapply(non_empty, function(x) {
              grepl("https?://|www\\.|@[a-zA-Z0-9]+\\.[a-zA-Z]{2,}|[A-Za-z]{10,}\\s[A-Za-z]{6,}\\s[A-Za-z]{4,}", x)
          }, logical(1)))
          if (has_url_or_email) return(TRUE)

          sparse_metadata <- length(non_empty) < ceiling(ncol(raw_mat) * 0.5)
          has_contact_metadata <- any(grepl(
              "^(date of (next )?publication|inquiries?|telephone|email|contact)",
              stringi::stri_trim_both(non_empty),
              ignore.case = TRUE
          ))
          if (sparse_metadata && has_contact_metadata) return(TRUE)
          
          looks_random <- vapply(non_empty, function(x) {
              x <- stringi::stri_trim_both(x)
              if (!grepl("^[A-Za-z]+$", x)) return(FALSE)
              if (nchar(x) <= 4) return(FALSE)
              if (grepl("^[A-Z][a-z]", x)) return(FALSE)
              TRUE
          }, logical(1))
          all(looks_random)
      }

      # Determine how far back to search for headers for this block
      search_start <- header_row_index
      if (!metadata_prefixed_explicit_temporal_panel) {
          while (search_start > 1) {
              non_empty <- sum(!is.na(raw_mat[search_start - 1, ]) & stringi::stri_trim_both(raw_mat[search_start - 1, ]) != "")
              if (non_empty > 0) {
                  search_start <- search_start - 1
              } else {
                  break # Stop at empty row
              }
          }
      }

      header_row_values <- raw_mat[header_row_index, ]
      header_row_non_empty <- header_row_values[
          !is.na(header_row_values) & stringi::stri_trim_both(header_row_values) != ""
      ]
      header_row_label <- if (length(header_row_non_empty) == 1L) {
          tolower(stringi::stri_trim_both(as.character(header_row_non_empty[[1L]])))
      } else {
          ""
      }
      header_row_is_metadata <- grepl(
          "^(release date|next release|important notes?|notes?|last updated|updated|preunit|unit|einheit|dimension|stand vom)$",
          header_row_label
      )
      leading_section_row <- header_row_index > 1L && header_row_index < true_start &&
          num_counts[header_row_index] == 0L &&
          length(header_row_non_empty) == 1L && !header_row_is_metadata &&
          sum(!is.na(raw_mat[header_row_index - 1L, ]) &
              stringi::stri_trim_both(raw_mat[header_row_index - 1L, ]) != "") >= 2L
      if (leading_section_row) {
          audit_log[["Leading Section Row Retained"]] <- 1L
      }
      
      header_rows_list <- list()
      previous_header_was_descriptive <- FALSE
      for (r in search_start:header_row_index) {
          row_vals <- raw_mat[r, ]
          non_empty_vals <- row_vals[!is.na(row_vals) & stringi::stri_trim_both(row_vals) != ""]
          non_empty_count <- length(non_empty_vals)
          first_label <- if (non_empty_count > 0) tolower(stringi::stri_trim_both(row_vals[1])) else ""
          calendar_format_row <- r > 1L && ncol(raw_mat) >= 3L &&
              identical(tolower(stringi::stri_trim_both(as.character(raw_mat[r - 1L, 1:3]))),
                  c("year", "month", "day")) &&
              all(mapply(grepl, c("^Y{2,4}$", "^M{1,2}$", "^D{1,2}$"),
                  stringi::stri_trim_both(as.character(row_vals[1:3])),
                  MoreArgs = list(ignore.case = TRUE)))
          if (calendar_format_row) {
              extracted_metadata[["column_formats"]] <- stats::setNames(
                  stringi::stri_trim_both(as.character(row_vals)),
                  stringi::stri_trim_both(as.character(raw_mat[r - 1L, ]))
              )
              format_row_count <- audit_log[["Calendar Format Header Rows Discarded"]]
              audit_log[["Calendar Format Header Rows Discarded"]] <-
                  if (is.null(format_row_count)) 1L else format_row_count + 1L
              next
          }
          metadata_label <- grepl("^(release date|next release|important notes?|notes?|last updated|updated|preunit|unit|einheit|dimension|stand vom)$", first_label)
          navigation_label <- grepl("^back to (contents|index|menu|top)$", first_label)
          source_key_label <- grepl("^source\\s*key$", first_label)
          repeated_values <- non_empty_count >= max(2L, ceiling(ncol(raw_mat) * 0.5)) &&
              (max(table(non_empty_vals)) / non_empty_count) >= 0.5
          sparse_metadata <- metadata_label && non_empty_count < ceiling(ncol(raw_mat) * 0.5)
          compact_code_row <- non_empty_count > 1 &&
              all(grepl("^[A-Z][A-Z0-9_]{1,15}$", non_empty_vals))
          temporal_metadata <- grepl("^(release date|next release|last updated|updated|stand vom)$", first_label)
          skip_metadata <- navigation_label || source_key_label ||
              (metadata_label && (sparse_metadata || repeated_values || temporal_metadata))

          if ((non_empty_count > 1 || r == header_row_index) && !skip_metadata &&
              !(r == header_row_index && leading_section_row)) {
              if (r != header_row_index && is_noise_header_row(row_vals)) {
                  audit_log[["Noise Header Rows Discarded"]] <-
                      c(audit_log[["Noise Header Rows Discarded"]], r)
                  next
              }
              if (compact_code_row && previous_header_was_descriptive) {
                  audit_log[["Compact Code Header Rows Discarded"]] <-
                      c(audit_log[["Compact Code Header Rows Discarded"]], r)
                  next
              }
              header_rows_list <- append(header_rows_list, list(row_vals))
              previous_header_was_descriptive <- !compact_code_row
          } else if (skip_metadata) {
              audit_log[["Metadata Header Rows Discarded"]] <-
                  c(audit_log[["Metadata Header Rows Discarded"]], r)
          }
      }
      
      trailing_header_was_blank <- FALSE
      if (length(header_rows_list) > 0) {
          trailing_header_was_blank <- all(vapply(header_rows_list, function(h_row) {
              last_value <- h_row[[length(h_row)]]
              is.na(last_value) || stringi::stri_trim_both(as.character(last_value)) == ""
          }, logical(1)))
          for (i in seq_along(header_rows_list)) {
              h_row <- header_rows_list[[i]]
              if (length(unique(h_row[!is.na(h_row) & stringi::stri_trim_both(h_row) != ""])) >= 1) {
                  for (j in 2:length(h_row)) {
                      if ((is.na(h_row[j]) || stringi::stri_trim_both(h_row[j]) == "") && 
                          !is.na(h_row[j-1]) && stringi::stri_trim_both(h_row[j-1]) != "") {
                          h_row[j] <- h_row[j-1]
                      }
                  }
                  header_rows_list[[i]] <- h_row
              }
          }
          
          headers <- rep("", length(header_rows_list[[1]]))
          for (i in seq_along(header_rows_list)) {
              h_row <- header_rows_list[[i]]
              valid_mask <- !is.na(h_row) & stringi::stri_trim_both(h_row) != ""
              headers <- ifelse(valid_mask,
                                ifelse(is.na(headers) | headers == "", h_row, paste0(headers, "_", h_row)),
                                headers)
          }
          headers <- ifelse(is.na(headers) | headers == "", NA, headers)

          headers <- vapply(headers, function(h) {
              if (is.na(h)) return(NA_character_)
              h <- stringr::str_squish(gsub("[\r\n]+", " ", h))
              stringr::str_replace(h, "(?i)\\s*\\[note\\s+[0-9, ]+\\]\\s*$", "")
          }, character(1))
          
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
                      if (nchar(candidate) <= max_col_name_len && nchar(stringi::stri_trim_both(candidate)) > 0) {
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
      if (headerless_panel) {
          headers <- if (metadata_prefixed_temporal_panel) {
              c(
                  "time_period",
                  "value",
                  if (ncol(raw_mat) > 2L) paste0("column_", seq.int(3L, ncol(raw_mat))) else character()
              )
          } else {
              paste0("column_", seq_len(ncol(raw_mat)))
          }
      }
      
      data_block_start <- if (leading_section_row) header_row_index else true_start
      data_block <- raw_mat[data_block_start:true_end, , drop = FALSE]
      
      block_counts <- num_counts[data_block_start:true_end]
      first_column_values <- stringi::stri_trim_both(as.character(data_block[, 1]))
      monthly_period_rows <- grepl(
          "^(?:19|20)[0-9]{2}(?:(?:0[1-9]|1[0-2])|M(?:0[1-9]|1[0-2]))$",
          first_column_values
      )
      first_monthly_period <- which(monthly_period_rows)[1]
      remaining_period_share <- if (!is.na(first_monthly_period)) {
          mean(monthly_period_rows[first_monthly_period:length(monthly_period_rows)])
      } else {
          0
      }
      if (!is.na(first_monthly_period) && first_monthly_period > 1L &&
          remaining_period_share >= 0.8) {
          descriptor_count <- first_monthly_period - 1L
          data_block <- data_block[first_monthly_period:nrow(data_block), , drop = FALSE]
          block_counts <- block_counts[first_monthly_period:length(block_counts)]
          headers[[1]] <- "time_period"
          audit_log[["Leading Period Descriptor Rows Dropped"]] <- descriptor_count
      }
      end_of_record_columns <- vapply(seq_len(ncol(data_block)), function(column_index) {
          header_name <- stringi::stri_trim_both(as.character(headers[[column_index]]))
          if (is.na(header_name) || tolower(header_name) != "eor") return(FALSE)

          values <- stringi::stri_trim_both(as.character(data_block[, column_index]))
          observed <- !is.na(values) & values != ""
          any(observed) && all(tolower(values[observed]) == "eor")
      }, logical(1))
      if (any(end_of_record_columns)) {
          headers <- headers[!end_of_record_columns]
          data_block <- data_block[, !end_of_record_columns, drop = FALSE]
          audit_log[["End-of-Record Marker Columns Removed"]] <- sum(end_of_record_columns)
      }
      identifier_headers <- vapply(headers, function(name) {
          normalized_name <- tolower(gsub("[^A-Za-z0-9]+", "_", name))
          grepl("(^|_)(id|identifier|code|key|uuid|guid|dguid|unitid|isbn|issn|upc|barcode|vector|coordinate|fips|geofips|station|state|county|sumlev|region|division|regdep|lsoa)(_|$)", normalized_name)
      }, logical(1))
      sparse_identifier_rows <- rep(FALSE, nrow(data_block))
      if (any(identifier_headers)) {
          sparse_identifier_rows <- rowSums(
              !is.na(data_block[, identifier_headers, drop = FALSE]) &
                  stringi::stri_trim_both(data_block[, identifier_headers, drop = FALSE]) != ""
          ) > 0
      }
      first_block_values <- stringi::stri_trim_both(as.character(data_block[, 1L]))
      temporal_sparse_rows <- temporal_axis_in_input &
          block_counts < density_threshold &
          !is.na(first_block_values) & first_block_values != "" &
          grepl(temporal_record_pattern, first_block_values)
      temporal_record_rows <- !is.na(first_block_values) &
          first_block_values != "" &
          grepl(temporal_record_pattern, first_block_values)
      temporal_record_share <- if (any(!is.na(first_block_values) & first_block_values != "")) {
          mean(temporal_record_rows[!is.na(first_block_values) & first_block_values != ""])
      } else {
          0
      }
      trailing_narrative_rows <- rep(FALSE, nrow(data_block))
      if (temporal_record_share >= 0.75 && any(temporal_record_rows)) {
          last_temporal_record <- max(which(temporal_record_rows))
          trailing_narrative_rows <- seq_len(nrow(data_block)) > last_temporal_record &
              !temporal_record_rows
      }
      internal_valid_rows <- (block_counts >= density_threshold) |
          sparse_identifier_rows | temporal_sparse_rows
      trailing_narrative_dropped <- sum(internal_valid_rows & trailing_narrative_rows)
      internal_valid_rows <- internal_valid_rows & !trailing_narrative_rows
      retained_sparse_rows <- sum(sparse_identifier_rows & block_counts < density_threshold)
      if (retained_sparse_rows > 0) {
          audit_log[["Sparse Identifier Rows Retained"]] <- retained_sparse_rows
      }
      if (sum(temporal_sparse_rows) > 0) {
          audit_log[["Sparse Temporal Rows Retained"]] <- sum(temporal_sparse_rows)
      }
      if (trailing_narrative_dropped > 0) {
          audit_log[["Trailing Narrative Rows Dropped"]] <- trailing_narrative_dropped
      }
      
      # Hierarchical Section Header Propagation
      section_categories <- rep(NA, nrow(data_block))
      has_sections <- FALSE
      
      for (i in seq_len(nrow(data_block))) {
          if (block_counts[i] == 0 && !temporal_sparse_rows[i]) {
              row_vals <- data_block[i, ]
              non_empty <- which(!is.na(row_vals) & stringi::stri_trim_both(row_vals) != "")
              if (length(non_empty) == 1 && non_empty[1] <= 3) {
                  section_categories[i] <- stringi::stri_trim_both(row_vals[non_empty[1]])
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
      
      empty_data_cols <- apply(data_block, 2, function(col) all(is.na(col) | col == "" | col %in% na_strings))
      repeated_header_columns <- rep(FALSE, length(headers))
      if (length(headers) > 1) {
          repeated_header_columns[2:length(headers)] <-
              !is.na(headers[2:length(headers)]) &
              !is.na(headers[1:(length(headers) - 1)]) &
              stringi::stri_trim_both(headers[2:length(headers)]) ==
                  stringi::stri_trim_both(headers[1:(length(headers) - 1)])
      }
      empty_cols <- empty_data_cols &
          (headerless_panel | is.na(headers) | stringi::stri_trim_both(headers) == "" | repeated_header_columns)
      if (any(empty_cols)) {
          audit_log[["Empty Data Columns Dropped"]] <- sum(empty_cols)
      }
                    
      headers <- headers[!empty_cols]
      data_block <- data_block[, !empty_cols, drop = FALSE]
      if (headerless_panel && !metadata_prefixed_temporal_panel) {
          headers <- paste0("column_", seq_len(length(headers)))
      }
      
      # Intercept and remove repeating page headers. A row that matches all but
      # one header must match at least one of the first two comparable headers,
      # so avoid full-row work for ordinary data rows in large flat files.
      is_repeated_header <- rep(FALSE, nrow(data_block))
      comparable_header_columns <- head(which(!is.na(headers)), 2L)
      if (length(comparable_header_columns) == 0L) {
          repeated_header_candidates <- seq_len(nrow(data_block))
      } else {
          candidate_rows <- rep(FALSE, nrow(data_block))
          trimmed_headers <- stringi::stri_trim_both(headers)
          for (column_index in comparable_header_columns) {
              column_values <- as.character(data_block[, column_index])
              candidate_rows <- candidate_rows |
                  (!is.na(column_values) &
                      stringi::stri_trim_both(column_values) == trimmed_headers[[column_index]])
          }
          repeated_header_candidates <- which(candidate_rows)
      }
      if (length(repeated_header_candidates) > 0L) {
          is_repeated_header[repeated_header_candidates] <- apply(
              data_block[repeated_header_candidates, , drop = FALSE],
              1,
              function(row) {
                  match_count <- sum(stringi::stri_trim_both(row) == stringi::stri_trim_both(headers), na.rm = TRUE)
                  match_count >= max(1, length(headers) - 1)
              }
          )
      }
      
      if (any(is_repeated_header)) {
          data_block <- data_block[!is_repeated_header, , drop = FALSE]
          if (has_sections) {
              valid_sections <- valid_sections[!is_repeated_header]
          }
      }
      
      df <- as.data.frame(data_block, stringsAsFactors = FALSE)

      # A final blank header paired with a grand-total row is an unlabeled total column,
      # not a duplicate of the preceding merged header.
      if (trailing_header_was_blank && ncol(df) >= 2L &&
          !is.na(headers[[ncol(df)]]) && !is.na(headers[[ncol(df) - 1L]]) &&
          stringi::stri_trim_both(headers[[ncol(df)]]) ==
              stringi::stri_trim_both(headers[[ncol(df) - 1L]])) {
          first_values <- stringi::stri_trim_both(as.character(df[[1L]]))
          last_values <- stringi::stri_trim_both(as.character(df[[ncol(df)]]))
          grand_total_rows <- !is.na(first_values) &
              grepl("^(?:grand[[:space:]]+)?total\\b", first_values, ignore.case = TRUE)
          if (any(grand_total_rows & is_numeric_like_vector(last_values), na.rm = TRUE)) {
              headers[[ncol(df)]] <- "Total"
              audit_log[["Trailing Total Header Inferred"]] <- 1L
          }
      }

      header_values <- stringi::stri_trim_both(as.character(df[1L, ]))
      day_columns <- if (ncol(df) > 3L) {
          grepl("^(?:0?[1-9]|[12][0-9]|3[01])$", header_values[4:ncol(df)])
      } else {
          logical()
      }
      is_calendar_grid <- nrow(df) > 1L && sum(day_columns) >= 7L &&
          grepl("(?i)(day|jour)", header_values[[2L]]) &&
          grepl("(?i)total", header_values[[3L]])
      if (is_calendar_grid) {
          day_numbers <- suppressWarnings(as.integer(header_values))
          day_names <- ifelse(
              grepl("^(?:0?[1-9]|[12][0-9]|3[01])$", header_values),
              paste0("day_", sprintf("%02d", day_numbers)),
              NA_character_
          )
          headers <- c("year", "month", "total_month", day_names[4:ncol(df)])
          df <- df[-1L, , drop = FALSE]
          years <- as.character(df[[1L]])
          for (row_index in seq_along(years)) {
              if ((is.na(years[[row_index]]) || years[[row_index]] == "") && row_index > 1L) {
                  years[[row_index]] <- years[[row_index - 1L]]
              }
          }
          df[[1L]] <- years
          audit_log[["Calendar Grid Headers Rebuilt"]] <- sum(day_columns)
      }
      
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
              num_leading_spaces <- nchar(valid_vals) - nchar(stringi::stri_trim_left(valid_vals))
              
              if (max(num_leading_spaces) > 0 && min(num_leading_spaces) == 0 && length(unique(num_leading_spaces)) > 1) {
                  parent_category <- rep(NA, nrow(df))
                  current_parent <- NA
                  
                  for (r in seq_len(nrow(df))) {
                      val <- first_col[r]
                      if (!is.na(val) && val != "") {
                          spaces <- nchar(val) - nchar(stringi::stri_trim_left(val))
                          if (spaces == 0) {
                              current_parent <- stringi::stri_trim_both(val)
                          }
                          parent_category[r] <- current_parent
                      } else {
                          parent_category[r] <- current_parent
                      }
                  }
                  
                  extracted_count <- sum(!is.na(parent_category) & parent_category != stringi::stri_trim_both(first_col), na.rm = TRUE)
                  if (extracted_count > 0) {
                      df$parent_category <- parent_category
                      headers <- c(headers, "parent_category")
                      audit_log[["Indentation Hierarchy Extracted"]] <- extracted_count
                  }
              }
          }
      }
      
      # Phase 6: Forward-fill leading character columns (Staircase Ledgers)
      # Delimited records encode blank values directly; only worksheet cells can be merged.
      if (!is_csv) {
      for (c in 1:min(2, ncol(df))) {
          col_vals <- as.character(df[[c]])
          valid_vals <- col_vals[!is.na(col_vals) & stringi::stri_trim_both(col_vals) != ""]
          column_label <- if (length(headers) >= c && !is.na(headers[[c]])) {
              headers[[c]]
          } else {
              colnames(df)[c]
          }
          normalized_col_name <- gsub(
              "^_|_$",
              "",
              gsub("[^a-z0-9]+", "_", tolower(column_label))
          )
          is_descriptive_text_column <- grepl(
              "(^|_)(description|history|comment|note|remarks?|narrative|detail)(_|$)",
              normalized_col_name
          )
          if (length(valid_vals) > 0) {
              num_count <- sum(is_numeric_like_vector(valid_vals))
              if (!is_descriptive_text_column && num_count < length(valid_vals) * 0.5) {
                  filled_col <- col_vals
                  last_val <- NA
                  for (r in seq_along(filled_col)) {
                      if (!is.na(filled_col[r]) && stringi::stri_trim_both(filled_col[r]) != "") {
                          last_val <- filled_col[r]
                      } else if (!is.na(last_val)) {
                          filled_col[r] <- last_val
                      }
                  }
                  df[[c]] <- filled_col
              }
          }
      }
      }
      
      # Phase 5: Amputate Mid-Table Subtotals
      subtotal_keywords <- c("subtotal", "\u5c0f\u8ba1", "total:", "sum:", "gesamt", "summe", "somme", "promedio")
      first_col_lower <- stringi::stri_trim_both(tolower(as.character(df[[1]])))
      total_rows <- rep(FALSE, nrow(df))
      if (ncol(df) <= 3L) {
          total_rows <- !is.na(first_col_lower) & grepl(
              "^total([[:space:]]|:|$)",
              first_col_lower
          )
      }
      subtotal_idx <- total_rows | vapply(first_col_lower, function(val) {
          if (is.na(val)) return(FALSE)
          any(vapply(subtotal_keywords, function(k) grepl(k, val, fixed = TRUE), logical(1)))
      }, logical(1))
      if (any(subtotal_idx)) {
          audit_log[["Mid-Table Subtotals Amputated"]] <- sum(subtotal_idx)
          df <- df[!subtotal_idx, , drop = FALSE]
          # Recalculate first_col_lower for the next steps
          first_col_lower <- stringi::stri_trim_both(tolower(as.character(df[[1]])))
      }
      
      # Phase 6: Footnote Amputator (Trailing long-string rows)
      tail_n <- min(5, nrow(df))
      footnotes_dropped <- 0
      if (tail_n > 0) {
          rows_to_keep <- rep(TRUE, nrow(df))
          for (r in seq(nrow(df) - tail_n + 1, nrow(df))) {
              val1 <- as.character(df[r, 1])
              if (!is.na(val1) && nchar(stringi::stri_trim_both(val1)) > 15) {
                  other_cols_empty <- TRUE
                  if (ncol(df) > 1) {
                      other_vals <- as.character(df[r, 2:ncol(df)])
                      if (any(!is.na(other_vals) & stringi::stri_trim_both(other_vals) != "")) {
                          other_cols_empty <- FALSE
                      }
                  }
                  is_temporal_record <- temporal_axis_in_input && grepl(
                      temporal_value_pattern,
                      stringi::stri_trim_both(val1)
                  )
                  if (other_cols_empty && !is_temporal_record) {
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
          agg_keywords <- c("grand total", "total", "sum", "average", "avg", "\u5408\u8ba1", "\u603b\u8ba1", "\u5e73\u5747", "mean", "gesamt", "summe", "durchschnitt", "moyenne", "somme", "promedio")
          is_aggregation_row <- function(value) {
              !is.na(value) && any(vapply(
                  agg_keywords,
                  function(keyword) grepl(paste0("^", keyword), value),
                  logical(1)
              ))
          }
          for (r in seq(nrow(df) - tail_n + 1, nrow(df))) {
              following_rows <- if (r < nrow(df)) first_col_lower[seq.int(r + 1L, nrow(df))] else character()
              has_later_detail <- any(vapply(following_rows, function(value) {
                  !is.na(value) && value != "" && !is_aggregation_row(value)
              }, logical(1)))
              if (is_aggregation_row(first_col_lower[r]) && !has_later_detail) {
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
          first_column <- stringi::stri_trim_both(as.character(df[[1]]))
          observed_first_column <- !is.na(first_column) & first_column != ""
           has_iso_date_axis <- any(observed_first_column) && all(grepl(
               "^(?:19|20)[0-9]{2}-(?:0?[1-9]|1[0-2])-(?:0?[1-9]|[12][0-9]|3[01])$",
               first_column[observed_first_column]
           ))
           has_monthly_period_axis <- any(observed_first_column) && all(grepl(
               "^(?:19|20)[0-9]{2}(?:(?:0[1-9]|1[0-2])|M(?:0[1-9]|1[0-2]))$",
               first_column[observed_first_column]
           ))
           rolling_period_values <- first_column[observed_first_column]
          has_rolling_period_axis <- length(rolling_period_values) >= 3L &&
              mean(grepl(
                  "^[A-Za-z]{3}[-/][A-Za-z]{3} (?:19|20)[0-9]{2}$",
                  rolling_period_values
              )) >= 0.95
           headers[1] <- if (has_iso_date_axis) {
               "Date"
           } else if (has_monthly_period_axis || has_rolling_period_axis) {
               "Time Period"
          } else {
              "Category"
          }
      }
      
      colnames(df) <- make.unique(stringi::stri_trim_both(headers), sep = "_")
      
      # Phase 7: Amputate ALL Aggregation Columns (Embedded Subtotals)
      col_agg_keywords <- c("total", "sum", "subtotal", "ytd", "\u5408\u8ba1", "\u603b\u8ba1", "\u5c0f\u8ba1", "average", "avg", "gesamt", "summe", "durchschnitt", "moyenne", "somme", "promedio")
      col_agg_prefix <- paste0("^(", paste(col_agg_keywords, collapse = "|"), ")(_|$)")
      normalized_headers <- gsub("^_|_$", "", gsub("[^a-z0-9]+", "_", tolower(headers)))
      has_seasonal_abbreviations <- all(c("win", "spr", "sum", "aut") %in% normalized_headers)
      has_distribution_headers <- sum(grepl(
          "^(?:[0-9]{1,2}|100)(?:st|nd|rd|th)$", normalized_headers
      )) >= 2L && any(grepl(
          "^(std_(dev|deviation)|standard_deviation)$", normalized_headers
      ))
      cols_to_keep <- rep(TRUE, ncol(df))
      subtotal_cols_dropped <- c()
      for (c in seq_len(ncol(df))) {
        col_name <- tolower(colnames(df)[c])
        normalized_col_name <- gsub("^_|_$", "", gsub("[^a-z0-9]+", "_", col_name))
        is_baseline_measure <- grepl("(^|_)from_(average|avg)$", normalized_col_name)
        is_seasonal_summer <- has_seasonal_abbreviations && normalized_col_name == "sum"
        is_distribution_mean <- has_distribution_headers &&
            grepl("^(average|avg|mean)(_|$)", normalized_col_name)
        is_aggregation_column <- !is_baseline_measure &&
              !is_distribution_mean &&
              !is_seasonal_summer && grepl(col_agg_prefix, normalized_col_name)
          if (is_aggregation_column) {
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
          valid_vals <- col[!is.na(col) & stringi::stri_trim_both(col) != "" & !(col %in% na_strings)]
          length(valid_vals) == 0
      }, logical(1))
      no_header <- is.na(colnames(df)) | grepl("^(na|x|\\.\\.\\.)[_0-9]*$|^$", tolower(colnames(df)))
      if (any(phantom_cols & no_header)) {
          audit_log[["Phantom Columns Purged"]] <- sum(phantom_cols & no_header)
          df <- df[, !(phantom_cols & no_header), drop = FALSE]
      }
      
      # (section_category assigned earlier)
      
      # Forward-fill NAs in leading character columns (Handling Merged Cells)
      if (!is_csv) {
      for (j in seq_len(ncol(df))) {
          col_vals <- df[[j]]
          valid_idx <- which(!is.na(col_vals) & col_vals != "")
          valid_count <- length(valid_idx)
          normalized_col_name <- gsub(
            "^_|_$",
            "",
            gsub("[^a-z0-9]+", "_", tolower(colnames(df)[j]))
          )
          is_descriptive_text_column <- grepl(
            "(^|_)(description|history|comment|note|remarks?|narrative|detail)(_|$)",
            normalized_col_name
          )

          if (is_descriptive_text_column) {
            next
          }
          if (valid_count == 0) {
            break
          }
          num_likes <- sum(is_numeric_like_vector(col_vals[valid_idx]))
          if ((num_likes / valid_count) >= 0.5) {
            break
          }
          if (sum(is.na(col_vals) | col_vals == "") > 0) {
            for (r in 2:nrow(df)) {
              if (is.na(df[r, j]) || df[r, j] == "") {
                df[r, j] <- df[r - 1, j]
              }
            }
          }
      }
      }
      
      ndbc_missing_total <- sum(vapply(seq_along(df), function(i) {
        sum(ndbc_historical_missing_marker(df[[i]], colnames(df)[i]))
      }, integer(1)))
      if (ndbc_missing_total > 0L) {
        audit_log[["NDBC Historical Missing Values Recognized"]] <- ndbc_missing_total
      }
      df[] <- lapply(seq_along(df), function(i) {
        x <- df[[i]]
        missing <- is_missing_marker(x, colnames(df)[i])
        x[missing] <- NA
        x
      })
      
      is_identifier_column <- function(name) {
        normalized_name <- tolower(gsub("[^A-Za-z0-9]+", "_", name))
        japanese_corporate_number <- intToUtf8(c(0x6CD5, 0x4EBA, 0x756A, 0x53F7))
        traditional_chinese_code <- intToUtf8(c(0x4EE3, 0x78BC))
        simplified_chinese_code <- intToUtf8(c(0x4EE3, 0x7801))
        grepl("(^|_)(id|identifier|objectid|object_id|fid|code|key|uuid|guid|dguid|unitid|isbn|issn|upc|barcode|vector|coordinate|fips|geofips|station|state|county|sumlev|region|division|regdep|lsoa|zip|zipcode|postal|postalcode|postcode|phone|telephone|mobile|fax)(_|$)", normalized_name) ||
          grepl(japanese_corporate_number, name, fixed = TRUE) ||
          grepl(traditional_chinese_code, name, fixed = TRUE) ||
          grepl(simplified_chinese_code, name, fixed = TRUE)
      }

      is_leading_zero_code <- function(x) {
        values <- stringi::stri_trim_both(x[!is.na(x)])
        values <- values[values != ""]
        length(values) > 0 &&
          all(grepl("^[0-9]+$", values)) &&
          any(grepl("^0[0-9]+$", values))
      }

      is_alphanumeric_code <- function(x) {
        values <- stringi::stri_trim_both(x[!is.na(x)])
        values <- values[values != ""]
        if (length(values) == 0) return(FALSE)

        is_scientific <- grepl("^[+-]?[0-9]+(?:\\.[0-9]+)?[eE][+-]?[0-9]+$", values, perl = TRUE)
        has_code_shape <- grepl("^[A-Za-z]+(?:[_-]?[0-9]+)[A-Za-z0-9_-]*$", values) |
          grepl("^[0-9]+[A-Za-z]+[0-9]+[A-Za-z0-9_-]*$", values)
        all(has_code_shape & !is_scientific)
      }

      df[] <- lapply(seq_along(df), function(i) {
        x <- df[[i]]
        column_name <- colnames(df)[i]
        normalized_column_name <- if (is.na(column_name)) "" else {
          tolower(gsub("[^A-Za-z0-9]+", "_", column_name))
        }
        date_values <- stringi::stri_trim_both(x)
        is_date_column <-
            normalized_column_name == "time_period" ||
            grepl(
                "(^|_)(date|data|datum|datetime|timestamp)(_|$)|(?:sample|analysis|collection|observation|report|issue|start|end)date$",
                normalized_column_name
            )
        is_portuguese_date_column <- grepl("(^|_)(data|vencimento)(_|$)", normalized_column_name)
        valid_dates <- !is.na(date_values) & date_values != ""
        is_yyyymm_period_column <-
            (normalized_column_name == "time_period" || grepl("^tlist(?:_|$)", normalized_column_name)) &&
            any(valid_dates) &&
            all(grepl("^(?:19|20)[0-9]{2}(?:0[1-9]|1[0-2])$", date_values[valid_dates]))
        if (is_yyyymm_period_column) {
            return(date_values)
        }
        is_descriptive_period_column <- grepl(
            "(^|_)(month|quarter|period|time)(_|$)",
            normalized_column_name
        )
        if (is_descriptive_period_column && any(valid_dates) &&
            any(grepl("[[:alpha:]]", date_values[valid_dates]))) {
            return(date_values)
        }
        if (is_date_column && any(valid_dates)) {
            date_format_patterns <- c(
                "%Y%m%d" = "^(?:19|20)[0-9]{2}(?:0[1-9]|1[0-2])(?:0[1-9]|[12][0-9]|3[01])$",
                "%Y-%m-%d" = "^[0-9]{4}-(?:0?[1-9]|1[0-2])-(?:0?[1-9]|[12][0-9]|3[01])$",
                "%Y/%m/%d" = "^[0-9]{4}/(?:0?[1-9]|1[0-2])/(?:0?[1-9]|[12][0-9]|3[01])$",
                "%d/%m/%Y" = "^(?:0[1-9]|[12][0-9]|3[01])/(?:0[1-9]|1[0-2])/[0-9]{4}$",
                "%m/%d/%Y" = "^(?:0[1-9]|1[0-2])/(?:0[1-9]|[12][0-9]|3[01])/[0-9]{4}$",
                "%d-%m-%Y" = "^(?:0[1-9]|[12][0-9]|3[01])-(?:0[1-9]|1[0-2])-[0-9]{4}$",
                "%d.%m.%Y" = "^(?:0[1-9]|[12][0-9]|3[01])\\.(?:0[1-9]|1[0-2])\\.[0-9]{4}$",
                "%d%b%Y" = "^(?:0?[1-9]|[12][0-9]|3[01])[A-Za-z]{3}(?:19|20)[0-9]{2}$",
                "%d %b %Y" = "^(?:0?[1-9]|[12][0-9]|3[01]) [A-Za-z]{3,9} (?:19|20)[0-9]{2}$"
            )
            dmy_slash_values <- grepl(date_format_patterns[["%d/%m/%Y"]], date_values[valid_dates])
            has_unambiguous_dmy_slash <- all(dmy_slash_values) && any(
                as.integer(sub("^([0-9]{2})/.*$", "\\1", date_values[valid_dates])) > 12L
            )
            mdy_slash_values <- grepl(date_format_patterns[["%m/%d/%Y"]], date_values[valid_dates])
            has_unambiguous_mdy_slash <- all(mdy_slash_values) && any(
                as.integer(sub("^.*/([0-9]{2})/.*$", "\\1", date_values[valid_dates])) > 12L
            )
            dmy_dot_values <- grepl(date_format_patterns[["%d.%m.%Y"]], date_values[valid_dates])
            has_dmy_dot_dates <- all(dmy_dot_values)
            date_formats <- c("%Y%m%d", "%Y-%m-%d", "%Y/%m/%d", "%d%b%Y", "%d %b %Y")
            if (is_portuguese_date_column || has_unambiguous_dmy_slash || has_dmy_dot_dates) {
                date_formats <- c("%d/%m/%Y", "%d-%m-%Y", "%d.%m.%Y", date_formats)
            } else if (has_unambiguous_mdy_slash) {
                date_formats <- c("%m/%d/%Y", date_formats)
            }
            for (date_format in date_formats) {
                if (!all(grepl(date_format_patterns[[date_format]], date_values[valid_dates]))) {
                    next
                }
                parsed_dates <- suppressWarnings(as.Date(date_values, format = date_format))
                if (all(!is.na(parsed_dates[valid_dates]))) {
                    return(parsed_dates)
                }
            }
        }

        is_opaque_code_header <- grepl(
            "^[a-z]+[0-9]+[a-z]+[0-9]+$",
            normalized_column_name
        )
        if ((headerless_coded_panel && i == 1L) ||
            is_identifier_column(colnames(df)[i]) || is_opaque_code_header ||
            is_leading_zero_code(x) || is_alphanumeric_code(x)) {
            identifier <- stringi::stri_trim_both(stringr::str_remove_all(x, intToUtf8(160)))
          missing <- is_missing_marker(identifier, colnames(df)[i])
          identifier[missing] <- NA
          return(identifier)
        }

        is_status_field <- grepl(
          "(^|_)(status|symbol|terminated)(_|$)",
          normalized_column_name
        )
        if (is_status_field) {
          return(stringi::stri_trim_both(x))
        }

        # Most large public extracts contain plain numeric columns. Avoid the
        # semantic-formatting pipeline when every observed value is already numeric.
        observed_values <- x[!is.na(x) & stringi::stri_trim_both(x) != ""]
        if (length(observed_values) > 0L && any(grepl(
          cyrillic_pattern, observed_values, perl = TRUE
        ))) {
          return(date_values)
        }
        if (length(observed_values) > 0L && all(
          !grepl("[0-9]", observed_values) & grepl("[[:alpha:]]", observed_values)
        )) {
          return(date_values)
        }
        if (length(observed_values) > 0L && all(grepl(
          "^[+-]?[0-9]+(?:\\.[0-9]+)?(?:[eE][+-]?[0-9]+)?$",
          stringi::stri_trim_both(observed_values),
          perl = TRUE
        ))) {
          return(suppressWarnings(as.numeric(date_values)))
        }

        clean_x <- stringr::str_remove_all(x, intToUtf8(160))
        
        # Phase 4: Convert Accounting Zeros to 0
        x_trimmed <- stringi::stri_trim_both(clean_x)
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
        clean_x <- stringi::stri_trim_both(clean_x)

        # Official statistical exports often append spaced b/e/p quality flags.
        # They are annotations, not magnitude suffixes such as 2.5M.
        has_spaced_quality_flag <- grepl(
            "[0-9][[:space:]]+(?:b|e|p){1,2}[[:space:]]*$",
            clean_x,
            ignore.case = TRUE
        ) & !is.na(clean_x)
        
        clean_x <- stringr::str_replace_all(clean_x, "(?<=\\d)[\\s\\u00A0'](?=\\d)", "")
        has_euro_decimal <- grepl(",[0-9]{1,2}[^0-9]*$|,[0-9]{4,}[^0-9]*$", clean_x)
        clean_x <- ifelse(has_euro_decimal & !is.na(has_euro_decimal), 
                          stringr::str_replace(stringr::str_remove_all(clean_x, "\\."), ",", "."), 
                          clean_x)
        clean_x <- stringr::str_remove_all(clean_x, ",")
        
        # Phase 11: Semantic Multiplier Engine
        multiplier <- rep(1, length(clean_x))
        k_idx <- grepl("(?i)[-0-9.]+\\s*(k|\u5343)$", clean_x) & !is.na(clean_x)
        wan_idx <- grepl("(?i)[-0-9.]+\\s*(w|wan|\u4e07)$", clean_x) & !is.na(clean_x)
        m_idx <- grepl("(?i)[-0-9.]+\\s*(m|mil|million)$", clean_x) & !is.na(clean_x)
        yi_idx <- grepl("(?i)[-0-9.]+\\s*(y|yi|\u4ebf)$", clean_x) & !is.na(clean_x)
        b_idx <- grepl("(?i)[-0-9.]+\\s*(b|bn|billion)$", clean_x) & !is.na(clean_x) & !has_spaced_quality_flag
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
        is_quarter <- grepl("(?i)^(Q[1-4]|H[1-2]|FY[0-9]+)$", stringi::stri_trim_both(x))
        descriptive_numeric_label <- !is.na(x) & grepl(
            "^[[:alpha:]][[:alpha:] _-]*[[:space:]][0-9]+(?:[[:space:]][[:alpha:]][[:alpha:] _-]*)?$",
            stringi::stri_trim_both(x)
        )
        
        clean_x[!is_quarter] <- stringr::str_replace(clean_x[!is_quarter], "\\s*[A-Za-z\u4e00-\u9fa5]+\\s*$", "")
        clean_x[!is_quarter] <- stringr::str_replace(clean_x[!is_quarter], "^\\s*[A-Za-z\u4e00-\u9fa5]+\\s*", "")
        
        num_x <- suppressWarnings(as.numeric(clean_x))
        num_x[descriptive_numeric_label] <- NA_real_
        
        if(sum(is.na(num_x)) == sum(is.na(x))) {
            num_x[is_pct & !is.na(num_x)] <- num_x[is_pct & !is.na(num_x)] / 100
            valid_num_idx <- !is.na(num_x)
            num_x[valid_num_idx] <- num_x[valid_num_idx] * multiplier[valid_num_idx]
            return(num_x)
        } 
        
        return(stringi::stri_trim_both(x))
      })
      
      colnames(df) <- tolower(colnames(df)) 
      if (clean_vars) {
        df <- clean_variable_names(df)
      }
      date_stage <- apply_excel_date_stage(df, audit_log)
      df <- date_stage$data
      audit_log <- date_stage$audit
      
      # Phase 15: Common Prefix Stripping
      # If >=75% of columns share a long common prefix (>=15 chars), strip it.
      # This handles WB-style repeated dataset labels in headers.
      cnames_for_prefix <- colnames(df)
      if (length(cnames_for_prefix) >= 3) {
          # Find longest common prefix among all column names
          find_common_prefix <- function(strs) {
              strs <- strs[!is.na(strs) & strs != ""]
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
          temporal_pattern <- "^(19|20)[0-9]{2}(_q[1-4]|_h[1-2]|_[0-1]?[0-9]|_[a-z]{3})?$|^[0-9]{4}-[0-9]{2}-[0-9]{2}$|^(q[1-4]|h[1-2]|fy[0-9]+|fy[0-9]{4}_[0-9]{2})$|^[a-z]{3}_([0-9]{2}|(19|20)[0-9]{2})$"
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
      }
    }, error = function(e) {
      list(success = FALSE, error = e$message)
    })
    
    if (res$success) {
      if (auto_select_best_sheet) {
          candidate_data <- res$data
          candidate_score <- if (is.data.frame(candidate_data)) {
              nrow(candidate_data) * ncol(candidate_data)
          } else if (is.list(candidate_data)) {
              sum(vapply(candidate_data, function(df) {
                  if (is.data.frame(df)) nrow(df) * ncol(df) else 0
              }, numeric(1)))
          } else {
              0
          }
          if (candidate_score > best_score) {
              best_res <- res
              best_score <- candidate_score
          }
      } else {
          return(format_success_result(res))
      }
    } else {
      last_error <- res$error
    }
  }

  if (!is.null(best_res)) {
      return(format_success_result(best_res))
  }
  
  stop(paste("Failed to parse any valid panel from file.", last_error))
}
