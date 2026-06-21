# TidyPanel

<!-- badges: start -->
<!-- badges: end -->

**TidyPanel** extracts clean, standardized data frames from messy spreadsheet-like tables. This public submission copy uses synthetic examples only and does not include internal datasets, customer files, debug traces, or development notes.

## Installation

You can install the development version of TidyPanel from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("TonyIsFool/TidyPanel")
```

## Why TidyPanel?

Spreadsheet exports are rarely tidy. They may contain multi-line headers, decoy rows, empty ghost columns, embedded subtotals, and multiple tables on the same sheet.

`TidyPanel` uses a multi-phase heuristic engine to:

1. **Bypass Decoy Rows**: Skips irrelevant metadata at the top of the sheet.
2. **Header Stitching**: Identifies multi-line headers and creates flat, readable column names.
3. **Smart Cleanup**: Removes decorative breaks, subtotal rows, and ghost columns.
4. **Auto Pivot**: Detects temporal columns such as Q1, 2021, or FY23 and pivots them into long format.
5. **Semantic Cleaners**: Normalizes accounting dashes, percentages, currencies, and common numeric formats.

## Synthetic Example

The example below creates a temporary workbook with toy data, reads it, and returns an audit trail.

``` r
library(TidyPanel)

tmp <- tempfile(fileext = ".xlsx")
toy <- data.frame(
  X1 = c("Demo Export", "Region", "North", "South", "Total"),
  X2 = c("", "Sales", "100", "250", "350"),
  stringsAsFactors = FALSE
)
writexl::write_xlsx(toy, tmp)

result <- read_messy_panel(tmp, return_audit = TRUE)

print(result$data)
print(result$audit)

unlink(tmp)
```

## Contact

Questions, feedback, and bug reports are welcome at xulunt123@gmail.com.

## License

MIT (c) TonyIsFool
