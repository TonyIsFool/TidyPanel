#' Smart Type Coercion & NA Recognition
#' 
#' @description
#' `infer_data_types()` scans character columns in a data frame, identifies common
#' financial placeholders for missing data (e.g., "-", "N/A", "n.m."), safely replaces 
#' them with `NA`, and then coerces the column to `numeric` or `Date` if a high 
#' percentage of the remaining values match those types.
#' 
#' @param data A `data.frame`.
#' @param na_strings A character vector of strings to be interpreted as `NA`.
#' @param num_threshold Numeric between 0 and 1. The proportion of valid numbers required to convert a column to numeric. Default is `0.95`.
#' @return A `data.frame` with inferred data types.
#' 
#' @examples
#' # Clean financial placeholders and coerce to numeric
#' df <- data.frame(val = c("1.5", "-", "2.0", "N/A"), stringsAsFactors = FALSE)
#' df_clean <- infer_data_types(df)
#' df_clean$val  # numeric: c(1.5, NA, 2.0, NA)
#' is.numeric(df_clean$val)  # TRUE
#'
#' @export
#' @importFrom stringr str_trim
infer_data_types <- function(data, na_strings = c("-", "N/A", "n/a", "n.m.", "n.m", "NA", "null", "NULL", "."), num_threshold = 0.95) {
    
    for (i in seq_along(data)) {
        if (is.character(data[[i]])) {
            col_data <- stringr::str_trim(data[[i]])
            
            # Replace defined NA strings with actual NA
            is_na_string <- tolower(col_data) %in% tolower(na_strings)
            col_data[is_na_string] <- NA
            
            # Count valid elements
            valid_elements <- col_data[!is.na(col_data) & col_data != ""]
            
            if (length(valid_elements) > 0) {
                # Test for numeric
                num_vals <- suppressWarnings(as.numeric(valid_elements))
                num_ratio <- sum(!is.na(num_vals)) / length(valid_elements)
                
                if (num_ratio >= num_threshold) {
                    # Safe to convert to numeric
                    col_data[col_data == ""] <- NA
                    data[[i]] <- suppressWarnings(as.numeric(col_data))
                    next
                }
                
                # Test for Dates
                date_vals <- suppressWarnings(as.Date(valid_elements, format = "%Y-%m-%d"))
                if (sum(!is.na(date_vals)) / length(valid_elements) >= num_threshold) {
                    col_data[col_data == ""] <- NA
                    data[[i]] <- suppressWarnings(as.Date(col_data, format = "%Y-%m-%d"))
                    next
                }
                
                # Alternate date formats can be added here
                date_vals2 <- suppressWarnings(as.Date(valid_elements, format = "%Y/%m/%d"))
                if (sum(!is.na(date_vals2)) / length(valid_elements) >= num_threshold) {
                    col_data[col_data == ""] <- NA
                    data[[i]] <- suppressWarnings(as.Date(col_data, format = "%Y/%m/%d"))
                    next
                }
                
                # If neither numeric nor date ratio met the threshold, assign NA-cleaned vector
                data[[i]] <- col_data
            } else {
                # Entire column is NA or empty
                col_data[col_data == ""] <- NA
                # Convert fully empty character columns to logical NAs to match standard read_csv behavior
                data[[i]] <- as.logical(col_data) 
            }
        }
    }
    
    return(data)
}
