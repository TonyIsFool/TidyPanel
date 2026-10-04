#' Smart Type Coercion & NA Recognition
#' 
#' @description
#' `infer_data_types()` scans character columns in a data frame, identifies common
#' financial placeholders for missing data (e.g., "-", "N/A", "n.m."), safely replaces 
#' them with `NA`, retains identifier-like columns such as phone numbers as text, and
#' then coerces the remaining columns to `numeric` or `Date` if a high percentage of
#' their values match those types.
#' 
#' @param data A `data.frame`.
#' @param na_strings A character vector of strings to be interpreted as `NA`. Literal `NA` is retained in explicit two-letter ISO code fields when observed values match the code schema.
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
    is_identifier_column <- function(name) {
        normalized_name <- tolower(gsub("[^A-Za-z0-9]+", "_", name))
        grepl("(^|_)(id|identifier|objectid|object_id|fid|code|key|uuid|guid|dguid|isbn|issn|upc|barcode|vector|coordinate|fips|geofips|station|state|county|sumlev|region|division|phone|telephone|mobile|fax)(_|$)", normalized_name)
    }

    is_leading_zero_code <- function(x) {
        values <- stringr::str_trim(x[!is.na(x)])
        values <- values[values != ""]
        length(values) > 0 &&
            all(grepl("^[0-9]+$", values)) &&
            any(grepl("^0[0-9]+$", values))
    }

    
    for (i in seq_along(data)) {
        if (is.character(data[[i]])) {
            col_data <- stringr::str_trim(data[[i]])
            
            # Replace defined NA strings with actual NA
            wrapped_missing <- !is.na(col_data) & grepl("^\\((NA|N/A|NULL|ND)\\)$", col_data, ignore.case = TRUE)
            is_na_string <- tolower(col_data) %in% tolower(na_strings) | wrapped_missing
            if (any(col_data == "NA", na.rm = TRUE) &&
                .is_iso_alpha2_field(col_data, names(data)[i], na_strings)) {
                is_na_string[!is.na(col_data) & col_data == "NA"] <- FALSE
            }
            col_data[is_na_string] <- NA

            if (is_identifier_column(names(data)[i]) || is_leading_zero_code(col_data)) {
                data[[i]] <- col_data
                next
            }
            
            # Count valid elements
            valid_elements <- col_data[!is.na(col_data) & col_data != ""]
            
            if (length(valid_elements) > 0) {
                # Test for numeric
                num_vals <- suppressWarnings(as.numeric(valid_elements))
                num_ratio <- sum(!is.na(num_vals)) / length(valid_elements)
                
                if (num_ratio >= num_threshold) {
                    # Semantic Excel Serial Date Inference
                    col_name <- names(data)[i]
                    if (!is.null(col_name) && grepl("date|period|time|year|month", col_name, ignore.case = TRUE)) {
                        # Excel dates between 1982 and 2064 fall in [30000, 60000]
                        if (all(num_vals[!is.na(num_vals)] >= 30000 & num_vals[!is.na(num_vals)] <= 60000)) {
                            col_data[col_data == ""] <- NA
                            data[[i]] <- as.Date(suppressWarnings(as.numeric(col_data)), origin = "1899-12-30")
                            next
                        }
                    }
                    
                    # Safe to convert to numeric
                    col_data[col_data == ""] <- NA
                    data[[i]] <- suppressWarnings(as.numeric(col_data))
                    next
                }
                
                # Advanced Multi-Format Date Inference
                date_formats <- c("%Y-%m-%d", "%Y/%m/%d", "%m/%d/%Y", "%d/%m/%Y", "%d.%m.%Y", "%d%b%Y", "%d-%b-%Y", "%b %d, %Y")
                date_inferred <- FALSE
                
                for (fmt in date_formats) {
                    # as.Date can be slow if it fails on many strings, but valid_elements is usually small
                    date_vals <- suppressWarnings(as.Date(valid_elements, format = fmt))
                    if (sum(!is.na(date_vals)) / length(valid_elements) >= num_threshold) {
                        col_data[col_data == ""] <- NA
                        data[[i]] <- suppressWarnings(as.Date(col_data, format = fmt))
                        date_inferred <- TRUE
                        break
                    }
                }
                
                if (date_inferred) next
                
                # Phase 5: Logical/Boolean Coercion
                bool_true_strs <- c("true", "t", "yes", "y", "1", "是")
                bool_false_strs <- c("false", "f", "no", "n", "0", "否")
                
                lower_elements <- tolower(valid_elements)
                is_bool <- lower_elements %in% c(bool_true_strs, bool_false_strs)
                
                if (sum(is_bool) / length(valid_elements) >= num_threshold) {
                    logical_vec <- rep(NA, length(col_data))
                    lower_col <- tolower(col_data)
                    logical_vec[lower_col %in% bool_true_strs] <- TRUE
                    logical_vec[lower_col %in% bool_false_strs] <- FALSE
                    data[[i]] <- logical_vec
                    next
                }
                
                # If neither numeric, date, nor logical ratio met the threshold, assign NA-cleaned vector
                data[[i]] <- col_data
            } else {
                # Entire column is NA or empty
                col_data[col_data == ""] <- NA
                # Convert fully empty character columns to logical NAs to match standard read_csv behavior
                data[[i]] <- as.logical(col_data) 
            }
        } else if (is.numeric(data[[i]])) {
            num_vals <- data[[i]]
            col_name <- names(data)[i]
            if (!is.null(col_name) && grepl("date|period|time|year|month", col_name, ignore.case = TRUE)) {
                valid_num <- num_vals[!is.na(num_vals)]
                # Excel dates between 1982 and 2064 fall in [30000, 60000]
                if (length(valid_num) > 0 && all(valid_num >= 30000 & valid_num <= 60000)) {
                    data[[i]] <- as.Date(num_vals, origin = "1899-12-30")
                }
            }
        }
    }
    
    return(data)
}

# Two-letter ISO schemas give literal NA an unambiguous code meaning.
.is_iso_alpha2_field <- function(values, column_name, na_strings) {
    if (length(column_name) != 1L || is.na(column_name)) return(FALSE)
    normalized_name <- tolower(gsub("[^A-Za-z0-9]+", "_", column_name))
    code_header <- grepl(
        "(^|_)(iso(?:_?3166(?:_?1)?)?_?(?:alpha_?2|a2|2)|alpha_?2)(_|$)",
        normalized_name, perl = TRUE
    )
    if (!code_header) return(FALSE)

    placeholders <- tolower(values) %in% tolower(na_strings) |
        grepl("^\\((NA|N/A|NULL|ND)\\)$", values, ignore.case = TRUE) |
        grepl("^\\*+$|^:\\s*(?:@[A-Za-z0-9_]+|[A-Za-z][A-Za-z0-9_]*)?\\s*$", values, perl = TRUE)
    evidence <- values[!is.na(values) & values != "" & !placeholders]
    all(grepl("^[A-Z]{2}$", evidence))
}
