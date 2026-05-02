# TidyPanel <img src="man/figures/logo.png" align="right" height="139" />

<!-- badges: start -->
<!-- badges: end -->

**TidyPanel** is an industrial-grade parser designed to extract clean, standardized data frames from heavily malformed, human-readable Excel reports. If you have ever struggled to parse financial statements, ERP exports, or complex tables with N-dimensional headers, decoy rows, and embedded subtotals, `TidyPanel` is built for you.

## Installation

You can install the development version of TidyPanel from GitHub with:

``` r
# install.packages("devtools")
devtools::install_github("TonyL/TidyPanel")
```

## Why TidyPanel?

Real-world commercial data is rarely tidy. It contains multi-line merged headers, embedded subtotal rows, empty "ghost" columns, and categorical hierarchies defined purely by visual indentation (e.g., in a Profit & Loss statement).

`TidyPanel` uses a multi-phase heuristic engine to:
1. **Bypass Decoy Rows**: Skips irrelevant metadata at the top of the sheet.
2. **N-Dimensional Header Stitching**: Intelligently identifies multiline headers and forward-fills merged cells to create flat, readable column names.
3. **Indentation Hierarchy Extraction**: Uses leading spaces to identify parent-child categorical relationships and creates a `parent_category` column.
4. **Smart Data Amputation**: Automatically removes decorative page breaks, mid-table subtotals, and ghost aggregate rows.
5. **Auto Pivot**: Detects temporal columns (e.g., Q1, 2021, FY23) and pivots them into a tidy, long format.
6. **Semantic Cleaners**: Automatically detects and normalizes accounting dashes, non-standard scientific notation, and financial multipliers (e.g., "1.5M" -> 1500000).

## 100% Transparency: The Audit Log

Data parsing should never be a "black box". TidyPanel features a unique **Audit Log** that explicitly records every transformation applied to your data. By setting `return_audit = TRUE`, you get both the cleaned data and a precise ledger of what was modified.

``` r
library(TidyPanel)

# Read data and get an audit trail
result <- read_messy_panel("data_raw/financial_report.xlsx", return_audit = TRUE)

# View the audit trail
print(result$audit)
```
```text
=== Algorithm Modification Audit Log ===
                        Operation Count
1             Decoy Rows Bypassed     5
2 Indentation Hierarchy Extracted     3
3       Ghost Bottom Rows Dropped     4
```

## License

MIT © Tony Lu
