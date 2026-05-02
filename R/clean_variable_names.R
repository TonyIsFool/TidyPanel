#' Standardize and Clean Variable Names
#'
#' @description
#' `clean_variable_names()` standardizes column names in a messy data frame. It converts all names 
#' to snake_case, strips special characters (except `_`), translates Excel serial dates (e.g., `44197`) 
#' into ISO date strings (`2021-01-01`), and maps common financial/academic synonyms (e.g., `gvkey`, 
#' `permno`, `cusip`) to standard names (`id`, `ticker`).
#'
#' @param data A `data.frame`. The data frame with messy column names.
#' @return A `data.frame` with the same data but standardized column names.
#' 
#' @examples
#' \dontrun{
#' df <- data.frame(`Total Revenue ($)` = 1, `44197` = 2, `PERMNO` = 3, check.names = FALSE)
#' clean_df <- clean_variable_names(df)
#' colnames(clean_df)
#' # Returns: c("total_revenue", "2021-01-01", "id")
#' }
#' 
#' @export
#' @importFrom stringr str_remove_all str_trim str_to_lower
clean_variable_names <- function(data) {
  clean_names <- stringr::str_trim(colnames(data))
  clean_names <- stringr::str_to_lower(clean_names)
  
  # Check if the name is an Excel serial date (e.g. 44197 -> 2021-01-01)
  is_excel_date <- grepl("^[345][0-9]{4}$", clean_names)
  if (any(is_excel_date)) {
      clean_names[is_excel_date] <- as.character(as.Date(as.numeric(clean_names[is_excel_date]), origin = "1899-12-30"))
  }
  
  dict <- c(
    "gvkey" = "id",
    "permno" = "id",
    "global company key" = "id",
    "company id" = "id",
    "entity id" = "id",
    "datadate" = "date",
    "date" = "date",
    "data date" = "date",
    "fiscal year" = "date",
    "report date" = "date",
    "at" = "total_assets",
    "assets - total" = "total_assets",
    "assets total" = "total_assets",
    "lt" = "total_liabilities",
    "liabilities - total" = "total_liabilities",
    "liabilities total" = "total_liabilities",
    "sic" = "category",
    "standard industry classification code" = "category",
    "industry code" = "category",
    "sector" = "category",
    "conm" = "name",
    "company name" = "name",
    "ticker" = "name",
    "ticker symbol" = "name",
    "patient mrn" = "id",
    "provider id" = "id",
    "hospital name" = "name",
    "admission date" = "date",
    "icd-10 code" = "category",
    "billing amount" = "value",
    "insurance copay" = "value",
    "total charges" = "value",
    "net revenue" = "value",
    "employee no." = "id",
    "pay period" = "date",
    "posting date" = "date",
    "hourly rate" = "value",
    "amount (usd)" = "value",
    "department" = "category",
    "cost center" = "category",
    "g/l account" = "category",
    "document no." = "ref",
    "reference" = "ref",
    "tracking id" = "id",
    "dispatch date" = "date",
    "shipping cost" = "value",
    "destination" = "category",
    "state" = "category",
    "soc code" = "category",
    "mean hourly wage" = "value",
    "annual mean wage" = "value"
  )
  
  clean_names <- stringr::str_remove_all(clean_names, "\\s*[a-z]/\\s*$")
  clean_names <- stringr::str_remove_all(clean_names, "\\s*\\([^)]*\\)\\s*$")
  clean_names <- stringr::str_trim(clean_names)
  
  for (i in seq_along(clean_names)) {
    if (clean_names[i] %in% names(dict)) {
      clean_names[i] <- dict[[clean_names[i]]]
    }
  }
  
  # Convert to proper snake_case:
  # 1. Replace any non-alphanumeric character (spaces, dashes, dots, /) with underscore
  clean_names <- gsub("[^a-z0-9]+", "_", clean_names)
  # 2. Collapse multiple consecutive underscores into one
  clean_names <- gsub("_+", "_", clean_names)
  # 3. Strip leading and trailing underscores
  clean_names <- gsub("^_|_$", "", clean_names)
  # 4. Ensure no empty names (fallback to 'var')
  clean_names[clean_names == ""] <- "var"
  # 5. Make names unique if there are duplicates after cleaning
  clean_names <- make.unique(clean_names, sep = "_")
  
  colnames(data) <- clean_names
  return(data)
}
