#' Validate Data Quality of a Cleaned Panel Data Frame
#'
#' @description
#' `validate_panel()` performs a post-cleaning quality audit on a data frame.
#' It checks for NA rates, outliers (via IQR), duplicate rows, sparse columns,
#' columns that should be numeric but remain character, and optional time-series
#' gap detection. Returns a structured report and optionally prints a summary.
#'
#' @param data A `data.frame`. The cleaned panel data to validate.
#' @param time_col A character string. Optional. Name of the time/date column to
#'   check for temporal continuity. Default is `NULL` (skip time check).
#' @param na_warn_threshold Numeric between 0-1. Columns with NA rate above this
#'   are flagged as high-risk. Default is `0.5`.
#' @param sparse_warn_threshold Numeric between 0-1. Columns with valid value rate
#'   below this are flagged as sparse. Default is `0.1`.
#' @param verbose Logical. If `TRUE`, prints a human-readable report. Default is `TRUE`.
#'
#' @return A named list with quality metrics per column and summary flags.
#'
#' @examples
#' # Toy example: validate a simple data frame
#' df <- data.frame(
#'   id   = c(1, 2, 2, 4),
#'   val  = c(100, 200, 200, NA),
#'   flag = c("1.5", "2.0", "2.0", "N/A")
#' )
#' report <- validate_panel(df, verbose = FALSE)
#' report$n_duplicates  # 1 duplicate row
#' report$high_na_cols  # columns with > 50% NA
#'
#' @export
#' @importFrom stringr str_trim
validate_panel <- function(data,
                           time_col = NULL,
                           na_warn_threshold = 0.5,
                           sparse_warn_threshold = 0.1,
                           verbose = TRUE) {

    if (!is.data.frame(data)) stop("`data` must be a data.frame.")
    n_rows <- nrow(data)
    n_cols <- ncol(data)

    # ---- 1. Duplicate row detection ----
    n_duplicates <- sum(duplicated(data))

    # ---- 2. Per-column analysis ----
    col_reports <- lapply(seq_len(n_cols), function(i) {
        col <- data[[i]]
        col_name <- colnames(data)[i]
        n_na <- sum(is.na(col))
        na_rate <- if (n_rows > 0) n_na / n_rows else NA
        valid_vals <- col[!is.na(col)]
        n_valid <- length(valid_vals)
        valid_rate <- if (n_rows > 0) n_valid / n_rows else NA

        is_high_na <- !is.na(na_rate) && na_rate > na_warn_threshold
        is_sparse   <- !is.na(valid_rate) && valid_rate < sparse_warn_threshold

        # Numeric checks
        n_outliers <- 0L
        should_be_numeric <- FALSE
        if (is.numeric(col) && n_valid >= 4) {
            q1 <- quantile(col, 0.25, na.rm = TRUE)
            q3 <- quantile(col, 0.75, na.rm = TRUE)
            iqr_val <- q3 - q1
            if (iqr_val > 0) {
                lower <- q1 - 3.0 * iqr_val
                upper <- q3 + 3.0 * iqr_val
                n_outliers <- sum(col < lower | col > upper, na.rm = TRUE)
            }
        } else if (is.character(col) && n_valid >= 3) {
            # Check if this char column should have been converted to numeric
            num_attempt <- suppressWarnings(as.numeric(valid_vals))
            if (sum(!is.na(num_attempt)) / n_valid >= 0.9) {
                should_be_numeric <- TRUE
            }
        }

        list(
            col_name          = col_name,
            type              = class(col)[1],
            n_na              = n_na,
            na_rate           = round(na_rate, 3),
            n_valid           = n_valid,
            valid_rate        = round(valid_rate, 3),
            n_outliers        = n_outliers,
            is_high_na        = is_high_na,
            is_sparse         = is_sparse,
            should_be_numeric = should_be_numeric
        )
    })

    # ---- 3. Time series gap detection ----
    time_gaps <- NULL
    if (!is.null(time_col) && time_col %in% colnames(data)) {
        t_vals <- data[[time_col]]
        if (inherits(t_vals, "Date") || is.numeric(t_vals)) {
            t_sorted <- sort(unique(t_vals[!is.na(t_vals)]))
            if (length(t_sorted) >= 3) {
                diffs <- diff(t_sorted)
                expected_diff <- as.numeric(names(sort(table(diffs), decreasing = TRUE))[1])
                gap_idx <- which(diffs > expected_diff * 1.5)
                if (length(gap_idx) > 0) {
                    time_gaps <- data.frame(
                        gap_after = t_sorted[gap_idx],
                        gap_before = t_sorted[gap_idx + 1],
                        gap_size = diffs[gap_idx]
                    )
                }
            }
        }
    }

    # ---- 4. Summary flags ----
    high_na_cols     <- Filter(function(r) r$is_high_na, col_reports)
    sparse_cols      <- Filter(function(r) r$is_sparse && !r$is_high_na, col_reports)
    outlier_cols     <- Filter(function(r) r$n_outliers > 0, col_reports)
    mistyped_cols    <- Filter(function(r) r$should_be_numeric, col_reports)

    report <- list(
        n_rows          = n_rows,
        n_cols          = n_cols,
        n_duplicates    = n_duplicates,
        col_reports     = col_reports,
        high_na_cols    = vapply(high_na_cols, `[[`, character(1), "col_name"),
        sparse_cols     = vapply(sparse_cols, `[[`, character(1), "col_name"),
        outlier_cols    = vapply(outlier_cols, `[[`, character(1), "col_name"),
        mistyped_cols   = vapply(mistyped_cols, `[[`, character(1), "col_name"),
        time_gaps       = time_gaps
    )

    # ---- 5. Verbose output ----
    if (verbose) {
        cat("=== TidyPanel Validation Report ===\n")
        cat(sprintf("  Dimensions   : %d rows x %d cols\n", n_rows, n_cols))
        cat(sprintf("  Duplicates   : %d\n", n_duplicates))

        if (length(report$high_na_cols) > 0) {
            cat(sprintf("  [WARN] High NA (>%.0f%%): %s\n",
                na_warn_threshold * 100, paste(report$high_na_cols, collapse = ", ")))
        }
        if (length(report$sparse_cols) > 0) {
            cat(sprintf("  [WARN] Sparse cols (<%.0f%% valid): %s\n",
                sparse_warn_threshold * 100, paste(report$sparse_cols, collapse = ", ")))
        }
        if (length(report$outlier_cols) > 0) {
            cat(sprintf("  [INFO] Outliers detected: %s\n",
                paste(report$outlier_cols, collapse = ", ")))
        }
        if (length(report$mistyped_cols) > 0) {
            cat(sprintf("  [WARN] Should be numeric: %s\n",
                paste(report$mistyped_cols, collapse = ", ")))
        }
        if (!is.null(report$time_gaps) && nrow(report$time_gaps) > 0) {
            cat(sprintf("  [WARN] Time gaps detected: %d gap(s) in '%s'\n",
                nrow(report$time_gaps), time_col))
        }
        if (length(report$high_na_cols) == 0 &&
            length(report$sparse_cols) == 0 &&
            length(report$mistyped_cols) == 0 &&
            n_duplicates == 0) {
            cat("  [OK] No major quality issues detected.\n")
        }
        cat("===================================\n")
    }

    invisible(report)
}
