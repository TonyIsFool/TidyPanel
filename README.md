# TidyPanel

TidyPanel cleans spreadsheet-style tables from data frames, Excel, CSV, TSV,
TXT, RDB, ZIP, and gzip-compressed tabular files. It standardizes column names
and common numeric values, removes subtotal rows, infers types, and can return
an audit table or reshape wide period columns.

Stable categorical labels in headerless numeric tables and paired free/total
measurements are retained. For numeric text reports, provide the observation
section as a plain table or data frame rather than including report metadata.
Commented delimited headers retain their labels, including whitespace after
the comment marker and quoted separators in field names or records.
Explicit identifier suffixes remain distinct from financial measurement labels.
Sampled averages retain their values when time and integer observation-count
fields establish a measurement context. Sources with delimited metadata
preambles must be supplied as plain observation tables.
Headerless whitespace records with quoted labels must be converted to CSV or
supplied as an explicitly named data frame; unsupported mixed separators fail
clearly rather than silently combining fields.
Numeric-context `NaN` placeholders become missing values without changing pure
text labels. For scientific exports with a separate unit descriptor row, remove
that row before supplying a named observation table. Customize `na_strings`
when literal codes overlap with default missing markers.
Numeric missing markers can also overlap with genuine measurements. Choose
`na_strings` from the source's definitions; retain numeric markers that the
source uses as observations by excluding them from that list.
Hyphenated English month dates are converted only when every observed date is
valid, independently of the time locale. Explicit date identifier fields remain
text. Qualified population income measures retain their labels.
Sparse country-year records remain present even when their measurements are
missing. Explicitly qualified total-energy measurements are not treated as
subtotal columns. Missing values in explicitly labelled calendar fields are
not filled from neighboring records.
Lowercase `na` is recognized in numeric contexts while pure text labels and
recognized identifier fields remain literal. Exclude it from a custom
`na_strings` when the marker has a different meaning in your table.

Explicit mixed-case `ID` suffixes remain identifier text and use `_id` in
cleaned names. Sparse digit identifiers are not treated as prose footnotes.
Type inference retains literal `Trace` observations unless the caller explicitly
includes them in `na_strings`; it does not assign an invented exact measurement.
Clock-bearing values and date suffixes remain text during type inference;
fractional serial days are not reduced to dates. Explicit flag fields retain
their literal tokens. Unit-qualified total rain, snow, and precipitation fields
and their corresponding flags are retained as measurements.

## Installation

Install the packages listed under Imports in DESCRIPTION, then install the
downloaded and extracted source folder. Replace the example path with its
actual location:

```sh
R CMD INSTALL path/to/TidyPanel
```

Alternatively, install a locally built source archive:

```r
install.packages("TidyPanel_0.2.114.tar.gz", repos = NULL, type = "source")
```

## Ten Small Examples

The package includes exactly ten fictional CSV examples, each with three
input rows. They contain no personal information and are not training,
validation, or regression datasets.

| CSV | Example |
| --- | --- |
| `demo_01_numbers.csv` | Grouped amounts, negatives, and missing values |
| `demo_02_dates.csv` | Dates and a missing reading |
| `demo_03_subtotals.csv` | A total row and wide yearly columns |
| `demo_04_percentages.csv` | Percentages converted to proportions |
| `demo_05_currency.csv` | Currency symbols and an accounting negative |
| `demo_06_codes.csv` | Leading-zero product and shelf codes |
| `demo_07_whitespace.csv` | Spaces in labels, names, and numeric cells |
| `demo_08_missing.csv` | A caller-selected missing marker |
| `demo_09_repeated.csv` | Repeated records retained in order |
| `demo_10_clocks_flags.csv` | Literal clocks and quality flags |

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
The seven additional files use the same `system.file()` pattern. For the
custom missing-marker example, explicitly select its demonstration marker:

```r
missing <- read_messy_panel(
  system.file("extdata", "demo_08_missing.csv", package = "TidyPanel", mustWork = TRUE),
  na_strings = c("", "NA", "-777")
)
missing
```

The release does not include development datasets or regression fixtures.

## Contact

Tony Lu: xulunt123@gmail.com
