TidyPanel: Universal Messy Panel Data Cleaner for R

TidyPanel is a robust, domain-agnostic toolkit designed to automatically standardize and clean complex tabular data exported from commercial enterprise systems (SAP, ERPs), healthcare records, logistics software, and legacy HR databases.

It follows a fire-and-forget philosophy. You provide an Excel path; it intelligently clusters data blocks, resolves structural anomalies, and outputs regression-ready panels.

FEATURES

Multi-Format Numeric Standardization
Perfectly parses international number formats (Euro 50.000,00, Swiss 50'000.00, spaced 50 000), trailing minuses (1234.56-), spaced parentheses ( 123.45 ), and percentages 12.5%.

Excel Date Anti-Corruption
Automatically detects Excel serial timestamps inadvertently placed as column headers (e.g., 44197) and mathematically converts them back to valid YYYY-MM-DD strings.

Aggressive Error Annihilation
Seamlessly intercepts and neutralizes Excel formula crash tokens (#VALUE!, #REF!, #DIV/0!) and institutional missing sentinels (none, NR, *, -) into standard NA.

Aggregate Row Purge
Smart regex radars detect and strip contaminating subtotal and average rows to prevent statistical bias.

Density-Based Auto-Discovery
Uses a heuristic density radar to isolate the primary data block, cutting through multi-header structures, disjointed titles, and trailing footnotes.


INSTALLATION

Install directly from GitHub (ensure devtools is installed):

# install.packages("devtools")
devtools::install_github("your-username/TidyPanel")


QUICK START

Using TidyPanel is meant to be completely seamless:

library(TidyPanel)

# Just provide the path. The engine automatically handles block targeting, 
# ghost header stripping, and international numeric parsing.
clean_data <- read_messy_panel("data_raw/messy_export.xlsx")

# Start modeling immediately!
summary(clean_data)


ADVANCED USAGE

If your lab or organization uses proprietary sentinel values for missing data, simply inject them:

clean_data <- read_messy_panel(
  file_path = "data_raw/messy_export.xlsx",
  sheet = 1,
  na_strings = c(NA, "TBA", "Not Available", "Omitted"),
  clean_vars = TRUE
)


EXAMPLE USE CASES INCLUDED

We have included 2 safe sample datasets in the inst/extdata/ directory, along with their perfectly cleaned CSV outputs, to demonstrate the engine's capabilities:

1. raw_deep_junk.xlsx (and cleaned_deep_junk.csv): Demonstrates TidyPanel's density radar bypassing massive blocks of non-tabular metadata, random text paragraphs, and report headers at the top of an Excel file to find the true data block.
2. raw_messy_finance.xlsx (and cleaned_messy_finance.csv): Demonstrates TidyPanel's ability to handle trailing minuses, spaced parentheses, and aggressive Excel crash tokens (#VALUE!, #REF!).

You can test them immediately:

library(TidyPanel)
path <- system.file("extdata", "raw_deep_junk.xlsx", package = "TidyPanel")
clean_data <- read_messy_panel(path)
print(clean_data)

SUPPORT & CONTACT

If you encounter any specific edge cases, extreme datasets that fail to parse, or if you have any questions regarding the package, feel free to reach out via email:
Tony Lu: xulunt123@gmail.com


LICENSE

MIT License. Copyright 2026 Tony Lu
