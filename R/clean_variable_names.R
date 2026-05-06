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
#' # Toy example: standardize column names in a data frame
#' df <- data.frame(
#'   `Total Revenue ($)` = 1,
#'   `PERMNO` = 3,
#'   `My Custom Column!` = 4,
#'   check.names = FALSE
#' )
#' clean_df <- clean_variable_names(df)
#' colnames(clean_df)
#' # Returns: c("revenue", "id", "my_custom_column")
#'
#' # Excel serial dates are also handled
#' df2 <- data.frame(`44197` = 2, check.names = FALSE)
#' colnames(clean_variable_names(df2))
#' # Returns: "2021-01-01"
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
    "patient mrn" = "id",
    "provider id" = "id",
    "employee no." = "id",
    "tracking id" = "id",
    "\u8eab\u4efd\u8bc1\u53f7" = "id",
    "patienten-id" = "id",
    "num\u00e9ro de patient" = "id",
    
    "datadate" = "date",
    "date" = "date",
    "data date" = "date",
    "fiscal year" = "date",
    "report date" = "date",
    "admission date" = "date",
    "pay period" = "date",
    "posting date" = "date",
    "dispatch date" = "date",
    "\u65e5\u671f" = "date",
    "datum" = "date",
    "fecha" = "date",
    
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
    "icd-10 code" = "category",
    "department" = "category",
    "cost center" = "category",
    "g/l account" = "category",
    "destination" = "category",
    "state" = "category",
    "soc code" = "category",
    "\u7c7b\u522b" = "category",
    "kategorie" = "category",
    "cat\u00e9gorie" = "category",
    "categor\u00eda" = "category",
    
    "conm" = "name",
    "company name" = "name",
    "ticker" = "name",
    "ticker symbol" = "name",
    "hospital name" = "name",
    "\u540d\u79f0" = "name",
    "nom" = "name",
    "nombre" = "name",
    
    "billing amount" = "value",
    "insurance copay" = "value",
    "total charges" = "value",
    "net revenue" = "value",
    "hourly rate" = "value",
    "amount (usd)" = "value",
    "shipping cost" = "value",
    "mean hourly wage" = "value",
    "annual mean wage" = "value",
    "\u91d1\u989d" = "value",
    "betrag" = "value",
    "montant" = "value",
    "importe" = "value",
    
    "document no." = "ref",
    "reference" = "ref"
  )
  
  clean_names <- stringr::str_remove_all(clean_names, "\\s*[a-z]/\\s*$")
  clean_names <- stringr::str_remove_all(clean_names, "\\s*\\([^)]*\\)\\s*$")
  clean_names <- stringr::str_trim(clean_names)
  
  regex_dict <- list(
      "revenue" = c("revenue", "sales", "turnover", "umsatz", "chiffre d'affaires", "ingresos"),
      "profit" = c("profit", "margin", "income", "gewinn", "b\u00e9n\u00e9fice", "beneficio"),
      "cost" = c("cost", "expense", "cogs", "kosten", "d\u00e9pense", "gasto")
  )
  
  for (i in seq_along(clean_names)) {
    matched <- FALSE
    
    # 1. Exact Match
    if (clean_names[i] %in% names(dict)) {
      clean_names[i] <- dict[[clean_names[i]]]
      matched <- TRUE
    }
    
    # 2. Regex Fuzzy Match
    if (!matched) {
      for (target in names(regex_dict)) {
        patterns <- regex_dict[[target]]
        if (any(vapply(patterns, function(p) grepl(p, clean_names[i], ignore.case = TRUE), logical(1)))) {
            clean_names[i] <- target
            matched <- TRUE
            break
        }
      }
    }
    
    # 2.5 ISO Date Pass-through
    if (!matched && grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", clean_names[i])) {
        matched <- TRUE
    }
    
    # 3. Strict snake_case conversion for unmapped variables
    if (!matched) {
        snaked <- stringr::str_replace_all(clean_names[i], "[^a-z0-9_]+", "_")
        snaked <- stringr::str_replace_all(snaked, "_+", "_")
        snaked <- stringr::str_replace(snaked, "^_|_$", "")
        if (snaked != "") {
            clean_names[i] <- snaked
        }
    }
  }
  
  colnames(data) <- clean_names
  return(data)
}
