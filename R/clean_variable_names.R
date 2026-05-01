#' Clean and standardize variable names
#'
#' @param data A dataframe containing messy data
#' @return A dataframe with standardized column names
#' @export
#' @importFrom stringr str_remove_all str_trim str_to_lower
#'
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
  
  colnames(data) <- clean_names
  return(data)
}
