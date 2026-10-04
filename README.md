# TidyPanel

TidyPanel cleans spreadsheet-style tables from data frames, Excel, CSV, TSV,
TXT, RDB, ZIP, and gzip-compressed tabular files. It standardizes column names
and common numeric values, removes subtotal rows, infers types, and can return
an audit table or reshape wide period columns.

## Installation

```r
install.packages("remotes")
remotes::install_github("TonyIsFool/TidyPanel")
```

## Three Small Examples

The package includes exactly three fictional CSV examples, each with three
input rows. They contain no personal information and are not training,
validation, or regression datasets.

```r
library(TidyPanel)

# Numbers, missing values, and leading-zero record codes
numbers <- read_messy_panel(system.file(
  "extdata", "demo_01_numbers.csv", package = "TidyPanel", mustWork = TRUE
))
numbers

# Calendar dates and a missing reading
dates <- read_messy_panel(system.file(
  "extdata", "demo_02_dates.csv", package = "TidyPanel", mustWork = TRUE
))
dates

# A total row and two yearly columns
panel <- read_messy_panel(
  system.file(
    "extdata", "demo_03_subtotals.csv", package = "TidyPanel", mustWork = TRUE
  ),
  auto_pivot = TRUE,
  return_audit = TRUE
)
panel$data
panel$audit
```

To clean your own file, pass its path to `read_messy_panel()`.
The release does not include development datasets or regression fixtures.

## Contact

Tony Lu: xulunt123@gmail.com
