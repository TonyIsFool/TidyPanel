#' Normalize Numeric Columns Based on Header Unit Declarations
#' 
#' @description
#' `normalize_units()` scans the column names of a data frame for financial/scientific 
#' unit declarations (e.g., "Revenue (in millions)", "Assets ($k)", "Employees ('000)").
#' It automatically multiplies the numeric values in the corresponding columns by the 
#' detected multiplier (1,000, 1,000,000, etc.) and optionally strips the unit 
#' declaration from the column name.
#' 
#' @param data A `data.frame`. The data frame to be normalized.
#' @param strip_units Logical. If `TRUE`, removes the unit declarations from the column names. Default is `TRUE`.
#' @return A `data.frame` with the normalized data and updated column names.
#' 
#' @examples
#' # Scale columns declared in millions and thousands
#' df <- data.frame(
#'   `Revenue ($M)` = c(1.5, 2.0),
#'   `Cost (in thousands)` = c(500, 600),
#'   check.names = FALSE
#' )
#' result <- normalize_units(df)
#' result$Revenue  # c(1500000, 2000000)
#' result$Cost     # c(500000, 600000)
#'
#' @export
#' @importFrom stringr str_remove str_replace str_trim
normalize_units <- function(data, strip_units = TRUE) {
    cnames <- colnames(data)
    
    # Define regex patterns for different units
    p_k <- "(?i)(\\bin thousands\\b|\\bin 000s\\b|'000s|'000|\\$k\\b|\\$ k\\b|\\(\\s*k\\s*\\)|\\(\u5343\\)|\uff08\u5343\uff09)"
    p_m <- "(?i)(\\bin millions\\b|\\bin mln\\b|\\bin millionen\\b|\\ben millions\\b|\\$m\\b|\\$ m\\b|\\(\\s*m\\s*\\)|\\(\u767e\u4e07\\)|\uff08\u767e\u4e07\uff09)"
    p_b <- "(?i)(\\bin billions\\b|\\bin bln\\b|\\bin bn\\b|\\$b\\b|\\$ b\\b|\\(\\s*b\\s*\\)|\\(\u5341\u4ebf\\)|\uff08\u5341\u4ebf\uff09)"
    p_t <- "(?i)(\\bin trillions\\b|\\$t\\b|\\$ t\\b|\\(\\s*t\\s*\\)|\\(\u5146\\)|\uff08\u5146\uff09)"
    
    for (i in seq_along(cnames)) {
        col_name <- cnames[i]
        multiplier <- 1
        matched_pattern <- NULL
        
        if (grepl(p_t, col_name)) {
            multiplier <- 1e12
            matched_pattern <- p_t
        } else if (grepl(p_b, col_name)) {
            multiplier <- 1e9
            matched_pattern <- p_b
        } else if (grepl(p_m, col_name)) {
            multiplier <- 1e6
            matched_pattern <- p_m
        } else if (grepl(p_k, col_name)) {
            multiplier <- 1e3
            matched_pattern <- p_k
        }
        
        if (multiplier > 1) {
            if (is.numeric(data[[i]])) {
                data[[i]] <- data[[i]] * multiplier
            } else {
                num_vals <- suppressWarnings(as.numeric(as.character(data[[i]])))
                if (!all(is.na(num_vals[!is.na(data[[i]])]))) {
                     is_num_idx <- !is.na(num_vals)
                     # Convert character to numeric for rows where it's valid
                     data[[i]] <- as.character(data[[i]])
                     data[[i]][is_num_idx] <- as.character(num_vals[is_num_idx] * multiplier)
                }
            }
            
            if (strip_units) {
                new_name <- stringr::str_remove(col_name, matched_pattern)
                new_name <- stringr::str_replace(new_name, "\\(\\s*\\)|\uff08\\s*\uff09", "")
                new_name <- stringr::str_trim(new_name)
                cnames[i] <- new_name
            }
        }
    }
    
    if (strip_units) {
        colnames(data) <- cnames
    }
    
    return(data)
}
