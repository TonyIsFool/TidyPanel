#' Standardize and Clean Variable Names
#'
#' @description
#' `clean_variable_names()` standardizes column names in a messy data frame. It converts all names 
#' to snake_case, normalizes Unicode numeric symbols, keeps percent and common
#' parenthetical time units, categorical suffixes, and fiscal-year headers
#' explicit, and strips other special characters (except `_`),
#' translates Excel serial dates (e.g., `44197`)
#' into ISO date strings (`2021-01-01`), and maps common financial/academic synonyms (e.g., `gvkey`, 
#' `permno`, `cusip`) to standard names (`id`, `ticker`).
#' Explicit identifier suffixes are protected from approximate financial mappings.
#' Qualified population income measures retain their meaning rather than becoming profit.
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
  percentage_units <- grepl("%", clean_names, fixed = TRUE)
  fiscal_year_matches <- regmatches(
    clean_names,
    regexec(
      "^(?:fy\\s*)?((?:19|20)[0-9]{2})\\s*[-/]\\s*((?:19|20)?[0-9]{2})$",
      clean_names,
      ignore.case = TRUE
    )
  )
  fiscal_year_labels <- vapply(fiscal_year_matches, function(parts) {
    if (length(parts) != 3L) {
      return(NA_character_)
    }
    start_year <- as.integer(parts[[2L]])
    end_text <- parts[[3L]]
    end_year <- if (nchar(end_text) == 2L) {
      candidate <- (start_year - (start_year %% 100L)) + as.integer(end_text)
      if (candidate <= start_year) candidate + 100L else candidate
    } else {
      as.integer(end_text)
    }
    if (is.na(end_year) || end_year != start_year + 1L) {
      return(NA_character_)
    }
    paste0("fy", start_year, "_", sprintf("%02d", end_year %% 100L))
  }, character(1))
  has_parenthetical_time_unit <- grepl(
    "\\((days?|weeks?|months?|years?|hours?|minutes?|seconds?)\\)\\s*$",
    clean_names,
    ignore.case = TRUE
  )
  parenthetical_time_units <- stringr::str_to_lower(sub(
    ".*\\((days?|weeks?|months?|years?|hours?|minutes?|seconds?)\\)\\s*$",
    "\\1",
    clean_names,
    ignore.case = TRUE
  ))
  clean_names <- normalize_unicode_numeric_symbols(clean_names)
  clean_names <- split_camel_case_names(clean_names)
  clean_names <- stringr::str_to_lower(clean_names)
  
  # Check if the name is an Excel serial date (e.g. 44197 -> 2021-01-01)
  is_excel_date <- grepl("^[345][0-9]{4}$", clean_names)
  if (any(is_excel_date)) {
      clean_names[is_excel_date] <- as.character(as.Date(as.numeric(clean_names[is_excel_date]), origin = "1899-12-30"))
  }
  
  is_slash_date <- grepl("^[0-9]{1,2}/[0-9]{1,2}/[0-9]{2,4}$", clean_names)
  if (any(is_slash_date)) {
      two_digit_year <- grepl("/[0-9]{2}$", clean_names[is_slash_date])
      parsed_dates <- rep(as.Date(NA), sum(is_slash_date))
      parsed_dates[two_digit_year] <- as.Date(clean_names[is_slash_date][two_digit_year], format = "%m/%d/%y")
      parsed_dates[!two_digit_year] <- as.Date(clean_names[is_slash_date][!two_digit_year], format = "%m/%d/%Y")
      clean_names[is_slash_date] <- as.character(parsed_dates)
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
    "entity" = "entity",
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
    "state" = "state",
    "status" = "status",
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
    "valeur" = "value",
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
      "cost" = c("cost", "expense", "cogs", "kosten", "d\u00e9pense", "gasto"),
      "total_assets" = c("assets?", "verm\u00f6gen", "actifs?", "activos?"),
      "total_liabilities" = c("liabilit(y|ies)", "verbindlichkeiten", "passifs?", "pasivos?"),
      "equity" = c("equity", "eigenkapital", "capitaux propres", "patrimonio"),
      "cash" = c("cash", "liquidity", "bargeld", "tr\u00e9sorerie", "efectivo"),
      "headcount" = c("^headcount$", "^employees?$", "^mitarbeiter$", "^effectif$", "^empleados?$"),
      "tax" = c("(^|[[:space:]_])tax(es)?($|[[:space:]_])", "steuer", "imp\u00f4t", "impuesto"),
      "ebitda" = c("ebitda", "oibda")
  )
  
  abbreviation_dict <- c(
      "stname" = "state_name",
      "ctyname" = "county_name"
  )
  
  for (i in seq_along(clean_names)) {
    if (is.na(clean_names[i]) || clean_names[i] == "") {
        clean_names[i] <- paste0("v", i)
        next
    }
    
    matched <- FALSE
    
    # 1. Exact Match
    if (clean_names[i] %in% names(abbreviation_dict)) {
      clean_names[i] <- abbreviation_dict[[clean_names[i]]]
      matched <- TRUE
    }
    if (!matched && clean_names[i] %in% names(dict)) {
      clean_names[i] <- dict[[clean_names[i]]]
      matched <- TRUE
    }
    
    # 2. Regex Fuzzy Match (skip generated measures and categorical dimensions)
    is_generated_suffix <- grepl("_(currency|value|units|remarks)$", clean_names[i])
    is_categorical_suffix <- grepl(
      "(^|[[:space:]_])(type|category|class|classification|group|code|status|flag)$",
      clean_names[i]
    )
    is_identifier_suffix <- grepl(
      "(^|[[:space:]_-])(id|identifier|code|key|uuid|guid)$", clean_names[i]
    )
    is_population_income <- grepl(
      "(^|[[:space:]_-])(income[[:space:]_-]+per[[:space:]_-]+(person|capita|household|worker|employee)|(per[[:space:]_-]+capita|household|personal|disposable|median)[[:space:]_-]+income)($|[[:space:]_-])",
      clean_names[i]
    )
    if (!matched && !is_generated_suffix && !is_categorical_suffix && !is_identifier_suffix && !is_population_income) {
      for (target in names(regex_dict)) {
        patterns <- regex_dict[[target]]
        if (any(vapply(patterns, function(p) grepl(p, clean_names[i], ignore.case = TRUE), logical(1)))) {
            clean_names[i] <- target
            matched <- TRUE
            break
        }
      }
    }
    # 2.3 Levenshtein Typo Tolerance (Phase 5)
    if (!matched && !is_generated_suffix && !is_identifier_suffix && !is_population_income && nchar(clean_names[i]) > 5) {
      all_targets <- unique(c(unname(dict), names(regex_dict)))
      distances <- adist(clean_names[i], all_targets, ignore.case = TRUE)[1, ]
      min_dist <- min(distances)
      transposed <- vapply(all_targets, function(target) {
          if (nchar(target) != nchar(clean_names[i])) return(FALSE)
          observed <- strsplit(clean_names[i], "", fixed = TRUE)[[1L]]
          expected <- strsplit(target, "", fixed = TRUE)[[1L]]
          changed <- which(observed != expected)
          length(changed) == 2L && diff(changed) == 1L &&
              identical(observed[changed], rev(expected[changed]))
      }, logical(1))
      # Two arbitrary edits can turn valid measurements into unrelated labels.
      if (min_dist <= 1) {
          clean_names[i] <- all_targets[which.min(distances)]
          matched <- TRUE
      } else if (any(transposed)) {
          clean_names[i] <- all_targets[which(transposed)[[1L]]]
          matched <- TRUE
      }
    }
    
    # 2.5 ISO Date Pass-through
    if (!matched && grepl("^[0-9]{4}-[0-9]{2}-[0-9]{2}$", clean_names[i])) {
        matched <- TRUE
    }
    
    # 3. Strict snake_case conversion for unmapped variables
    if (!matched) {
        transliterated_name <- suppressWarnings(iconv(clean_names[i], to = "ASCII//TRANSLIT"))
        name_for_snaking <- if (!is.na(transliterated_name) && !grepl("\\?\\?", transliterated_name)) {
            transliterated_name
        } else {
            clean_names[i]
        }
        snaked <- stringr::str_replace_all(name_for_snaking, "[^a-z0-9_]+", "_")
        snaked <- stringr::str_replace_all(snaked, "_+", "_")
        snaked <- stringr::str_replace(snaked, "^_|_$", "")
        if (snaked != "") {
            clean_names[i] <- snaked
        }
    }
  }

  for (i in which(percentage_units)) {
    if (!grepl("(^|_)(percent|percentage|pct)$", clean_names[i])) {
      clean_names[i] <- paste0(clean_names[i], "_percent")
    }
  }

  fiscal_year_columns <- which(!is.na(fiscal_year_labels))
  if (length(fiscal_year_columns) > 0L) {
    clean_names[fiscal_year_columns] <- fiscal_year_labels[fiscal_year_columns]
  }

  for (i in which(has_parenthetical_time_unit)) {
    if (!grepl(paste0("(^|_)", parenthetical_time_units[i], "$"), clean_names[i])) {
      clean_names[i] <- paste0(clean_names[i], "_", parenthetical_time_units[i])
    }
  }
  
  colnames(data) <- make.unique(clean_names, sep = "_")
  return(data)
}

normalize_unicode_numeric_symbols <- function(x) {
  x <- chartr(
    "\u2080\u2081\u2082\u2083\u2084\u2085\u2086\u2087\u2088\u2089",
    "0123456789",
    x
  )
  chartr(
    "\u2070\u00b9\u00b2\u00b3\u2074\u2075\u2076\u2077\u2078\u2079",
    "0123456789",
    x
  )
}

split_camel_case_names <- function(x) {
  x <- gsub("([A-Z]+)([A-Z][a-z])", "\\1 \\2", x, perl = TRUE)
  gsub("([a-z0-9])([A-Z])", "\\1 \\2", x, perl = TRUE)
}
