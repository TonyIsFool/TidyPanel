#' Diagnose the Structure of a Messy Excel Panel File
#'
#' @description
#' `detect_panel_structure()` performs a static, non-destructive analysis of a raw
#' Excel file and returns a structured report describing its "messiness". It detects
#' decoy metadata rows, multi-level headers, temporal wide columns, aggregation rows,
#' and phantom columns. It also generates a recommended `read_messy_panel()` call
#' based on the findings.
#'
#' @param path A character string. Path to the `.xlsx` or `.xls` file.
#' @param sheet A character string or integer. The sheet to analyse. Defaults to `1`.
#' @param verbose Logical. If `TRUE`, prints a human-readable summary to the console. Default is `TRUE`.
#'
#' @return A named list with the following elements:
#'   \describe{
#'     \item{`n_rows`}{Total rows in the raw sheet.}
#'     \item{`n_cols`}{Total columns in the raw sheet.}
#'     \item{`estimated_decoy_rows`}{Number of estimated metadata/noise rows at the top.}
#'     \item{`multi_level_header`}{Logical. Whether multi-level (merged) headers are detected.}
#'     \item{`has_temporal_cols`}{Logical. Whether wide-format temporal columns (years, quarters) are detected.}
#'     \item{`n_aggregation_rows`}{Number of suspected Total/Sum aggregation rows found.}
#'     \item{`n_phantom_cols`}{Number of fully-empty ghost columns.}
#'     \item{`n_data_blocks`}{Total number of disjoint data panels found in the sheet.}
#'     \item{`recommended_call`}{A character string with a suggested `read_messy_panel()` call.}
#'   }
#'
#' @examples
#' # Toy example: detect structure from a temporary Excel file
#' tmp <- tempfile(fileext = ".xlsx")
#' df <- data.frame(
#'   Category = c("Total", "Revenue", "Cost"),
#'   `FY2022` = c("3M", "2M", "1M"),
#'   `FY2023` = c("4M", "2.5M", "1.5M"),
#'   check.names = FALSE
#' )
#' writexl::write_xlsx(df, tmp)
#' report <- detect_panel_structure(tmp, verbose = FALSE)
#' str(report)
#' unlink(tmp)
#'
#' @export
#' @importFrom readxl read_excel excel_sheets
#' @importFrom stringr str_trim str_detect
detect_panel_structure <- function(path, sheet = 1, verbose = TRUE) {
    is_df <- is.data.frame(path)
    is_csv <- FALSE
    if (!is_df && is.character(path)) {
        if (!file.exists(path)) stop("File not found: ", path)
        is_csv <- tolower(tools::file_ext(path)) %in% c("csv", "tsv", "txt")
    }

    if (is_df) {
        raw <- as.data.frame(path, stringsAsFactors = FALSE)
        if (!all(grepl("^(V|X|Col)[0-9A-Za-z]*$|^\\.\\.\\.[0-9]+$", colnames(raw)))) {
            raw <- rbind(colnames(raw), raw)
        }
        colnames(raw) <- NULL
    } else if (is_csv) {
        ext <- tolower(tools::file_ext(path))
        sep <- if (ext == "tsv") "\t" else ","
        raw <- suppressMessages(suppressWarnings(
            read.csv(path, header = FALSE, sep = sep, stringsAsFactors = FALSE, na.strings = NULL, colClasses = "character", strip.white = FALSE)
        ))
    } else {
        raw <- suppressMessages(suppressWarnings(
            readxl::read_excel(path, sheet = sheet, col_names = FALSE,
                               col_types = "text", .name_repair = "minimal")
        ))
    }

    n_rows <- nrow(raw)
    n_cols <- ncol(raw)
    mat <- as.matrix(raw)

    # ---- 1. Count empty cells per row to find "density" ----
    row_density <- apply(mat, 1, function(r) sum(!is.na(r) & stringr::str_trim(r) != ""))

    # ---- 2. Estimate decoy rows at the top ----
    # Decoy rows typically have very few non-empty cells relative to the data block,
    # OR they are dense but consist entirely of random noise/metadata strings.
    is_noise_row <- apply(mat, 1, function(r) {
        non_empty <- r[!is.na(r) & stringr::str_trim(r) != ""]
        if (length(non_empty) == 0) return(TRUE) # Empty rows are treated as noise/decoy
        
        # If any cell contains a URL or long prose, it's metadata
        has_url <- any(vapply(non_empty, function(x) {
            grepl("https?://|www\\.|@[a-zA-Z0-9]+\\.[a-zA-Z]{2,}|[A-Za-z]{10,}\\s[A-Za-z]{6,}\\s[A-Za-z]{4,}", x)
        }, logical(1)))
        if (has_url) return(TRUE)
        
        # Check if all non-empty cells look like random noise strings
        looks_random <- vapply(non_empty, function(x) {
            x <- stringr::str_trim(x)
            if (!grepl("^[A-Za-z]+$", x)) return(FALSE)
            if (nchar(x) <= 4) return(FALSE)
            if (grepl("^[A-Z][a-z]", x)) return(FALSE) # Title case is usually valid
            TRUE
        }, logical(1))
        
        all(looks_random)
    })
    
    max_density <- max(row_density, na.rm = TRUE)
    threshold <- max(2, max_density * 0.5)
    
    estimated_decoy_rows <- 0
    for (i in seq_len(n_rows)) {
        if (is_noise_row[i]) {
            estimated_decoy_rows <- i
        } else if (row_density[i] >= threshold) {
            # We found a dense row that is NOT noise. This is likely the header/data.
            break
        } else {
            # Sparse row that is not explicit noise. Could still be a decoy or a subtitle.
            # We'll increment decoy rows if we haven't hit the data block yet.
            estimated_decoy_rows <- i
        }
    }

    # ---- 3. Detect multi-level headers ----
    # If there are 2+ consecutive rows near the top that are partially filled
    # (not fully empty, not fully dense), they may form a multi-level header.
    multi_level_header <- FALSE
    if (estimated_decoy_rows < n_rows - 2) {
        header_zone <- mat[seq(max(1, estimated_decoy_rows + 1), min(estimated_decoy_rows + 5, n_rows)), , drop = FALSE]
        partial_rows <- apply(header_zone, 1, function(r) {
            non_empty <- sum(!is.na(r) & stringr::str_trim(r) != "")
            non_empty > 0 && non_empty < n_cols * 0.8
        })
        multi_level_header <- sum(partial_rows) >= 2
    }

    # ---- 4. Detect temporal wide columns ----
    # Check all cells in the sheet for year-like (19xx/20xx) or quarter-like patterns.
    all_cells <- as.vector(mat)
    all_cells <- all_cells[!is.na(all_cells)]
    temporal_pattern <- "^(19|20)[0-9]{2}$|^[Qq][1-4]$|^[Hh][1-2]$|^[Ff][Yy][0-9]|^[A-Za-z]{3}[-_/][0-9]{2,4}$"
    n_temporal_like <- sum(stringr::str_detect(all_cells, temporal_pattern), na.rm = TRUE)
    has_temporal_cols <- n_temporal_like >= 3

    # ---- 5. Detect aggregation rows ----
    agg_keywords <- c("total", "sum", "average", "avg", "subtotal", "grand total",
                      "\u5408\u8ba1", "\u603b\u8ba1", "\u5c0f\u8ba1",
                      "gesamt", "summe", "moyenne", "somme", "promedio")
    first_col <- mat[, 1]
    n_aggregation_rows <- sum(vapply(first_col, function(v) {
        if (is.na(v) || stringr::str_trim(v) == "") return(FALSE)
        any(vapply(agg_keywords, function(k) grepl(k, tolower(v), fixed = TRUE), logical(1)))
    }, logical(1)), na.rm = TRUE)

    # ---- 6. Detect phantom columns ----
    col_density <- apply(mat, 2, function(col) sum(!is.na(col) & stringr::str_trim(col) != ""))
    n_phantom_cols <- sum(col_density == 0)

    # ---- 6.5 Detect Number of Blocks ----
    # Minimal logic mirroring read_messy_panel gap analysis
    is_numeric_like <- function(x) {
        if (is.na(x) || stringr::str_trim(x) == "") return(TRUE)
        !is.na(suppressWarnings(as.numeric(stringr::str_remove_all(x, "[,%$]"))))
    }
    num_counts <- apply(mat, 1, function(row) {
        sum(vapply(row, is_numeric_like, logical(1)) & !is.na(row) & row != "")
    })
    is_data_row <- num_counts >= 1
    true_runs_indices <- which(is_data_row == TRUE)
    n_data_blocks <- 1
    if (length(true_runs_indices) > 0) {
        gaps <- diff(true_runs_indices)
        n_data_blocks <- sum(gaps > 5) + 1
    }

    # ---- 7. Multi-sheet detection ----
    if (is_df || is_csv) {
        n_sheets <- 1
    } else {
        all_sheets <- suppressMessages(readxl::excel_sheets(path))
        n_sheets <- length(all_sheets)
    }

    # ---- 8. Build recommended call ----
    rec_parts <- c("read_messy_panel(")
    if (is_df) {
        rec_parts <- c(rec_parts, "  file_path = <data.frame>,")
    } else {
        rec_parts <- c(rec_parts, paste0('  file_path = "', basename(path), '",'))
    }
    if (n_sheets > 1) {
        rec_parts <- c(rec_parts, '  sheet = 1,  # or "ALL" for all sheets')
    }
    if (n_data_blocks > 1) {
        rec_parts <- c(rec_parts, "  extract_all_blocks = TRUE,")
    }
    if (has_temporal_cols) {
        rec_parts <- c(rec_parts, "  auto_pivot = TRUE,")
    }
    rec_parts <- c(rec_parts, "  clean_vars = TRUE,")
    rec_parts <- c(rec_parts, "  return_audit = TRUE")
    rec_parts <- c(rec_parts, ")")
    recommended_call <- paste(rec_parts, collapse = "\n")

    # ---- 9. Assemble report ----
    report <- list(
        n_rows               = n_rows,
        n_cols               = n_cols,
        n_sheets             = n_sheets,
        estimated_decoy_rows = estimated_decoy_rows,
        multi_level_header   = multi_level_header,
        has_temporal_cols    = has_temporal_cols,
        n_aggregation_rows   = n_aggregation_rows,
        n_phantom_cols       = n_phantom_cols,
        n_data_blocks        = n_data_blocks,
        recommended_call     = recommended_call
    )

    # ---- 10. Verbose console output ----
    if (verbose) {
        cat("=== TidyPanel Structure Report ===\n")
        if (is_df) {
            cat("  Source       : data.frame\n")
        } else {
            cat(sprintf("  File         : %s\n", basename(path)))
        }
        cat(sprintf("  Sheet        : %s  (%d total)\n", sheet, n_sheets))
        cat(sprintf("  Dimensions   : %d rows x %d cols\n", n_rows, n_cols))
        cat(sprintf("  Decoy rows   : %d  (estimated metadata at top)\n", estimated_decoy_rows))
        cat(sprintf("  Multi-header : %s\n", if (multi_level_header) "YES - multi-level headers detected" else "No"))
        cat(sprintf("  Temporal cols: %s\n", if (has_temporal_cols) "YES - consider auto_pivot = TRUE" else "No"))
        cat(sprintf("  Agg. rows    : %d  (Total/Sum rows found)\n", n_aggregation_rows))
        cat(sprintf("  Ghost cols   : %d  (fully empty columns)\n", n_phantom_cols))
        cat(sprintf("  Data blocks  : %d  %s\n", n_data_blocks, if (n_data_blocks > 1) "(Multiple disjoint panels found)" else ""))
        cat("\n-- Recommended call --\n")
        cat(recommended_call, "\n")
        cat("==================================\n")
    }

    invisible(report)
}
