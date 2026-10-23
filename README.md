<div align="center">
<h1>TidyPanel</h1>
<p><strong>The tabular cleaning step for AI workflows.</strong></p>
<p>Spend tokens on analysis, not spreadsheet noise.</p>
<p>
  <img src="https://img.shields.io/badge/R-4.1%2B-276DC3?style=flat-square" alt="R 4.1 or later">
  <a href="#validation-coverage"><img src="https://img.shields.io/badge/external_validation-100%2B_cases-167D68?style=flat-square" alt="100+ external-data validation cases"></a>
  <img src="https://img.shields.io/badge/license-MIT-555555?style=flat-square" alt="MIT license">
</p>
<p><a href="#download-and-install"><img src="https://img.shields.io/badge/Download_%26_Install-R_source-0969DA?style=for-the-badge" alt="Download and install the R source package"></a></p>
<p><a href="#quick-start">Quick Start</a> | <a href="#agent-workflow">Agent Workflow</a> | <a href="#three-small-examples">Examples</a> | <a href="#contact">Contact</a></p>
</div>

## What You Get

| Value | In practice |
| --- | --- |
| Less prompt noise | Keep full observations in files; send only the context needed for a question. |
| Reusable cleanup | Standardize column names, missing markers and common numeric formats. |
| Literal identifiers | Recognized codes retain their text and leading zeros. |
| Clear handoffs | Return a cleaned table and operation audit, with optional wide-to-long reshaping. |

Let TidyPanel handle supported tabular preparation; let your agent handle analysis.
Passing a compact summary instead of a complete raw report can reduce prompt
content. Token savings depend on the data, selected output and model tokenizer;
cleaning alone does not guarantee fewer tokens.

**Inputs:** data frames, Excel, CSV, TSV, TXT and RDB, including supported
ZIP and gzip-compressed tabular files.

## Validation Coverage

**100+ external-data validation cases** across five domains:

| Domain | Types of tables checked |
| --- | --- |
| Economics and public statistics | Prices, exchange rates and population series |
| Weather and environment | Station observations and climate tables |
| Science and health research | Measurements and aggregate indicators |
| Transport and geography | Counts, schedules and coded reference tables |
| Energy and industry | Consumption and production observations |

Cases include cleaning checks and clear rejection of unsupported layouts.
They are not counts of independent databases.

## Download And Install

**R 4.1 or later, with a writable R library.** No AI service is required.

1. On this GitHub repository, select **Code > Download ZIP**.
2. Extract the ZIP and set `source_dir` to the folder containing **DESCRIPTION**.
3. Run the code below in R. It does not open a file picker or mirror-selection menu.

```r
install.packages(
  c("dplyr", "stringr", "stringi", "readxl", "jsonlite"),
  repos = "https://cloud.r-project.org"
)
source_dir <- "path/to/extracted/TidyPanel"
install.packages(source_dir, repos = NULL, type = "source")
```

An agent can also install the source directory through `Rscript`, after the
dependencies above are installed. R must be available on its PATH:

```sh
Rscript --vanilla -e "install.packages(commandArgs(TRUE)[1], repos = NULL, type = 'source')" "path/to/extracted/TidyPanel"
```

<details>
<summary>Prefer a file picker for manual installation?</summary>

After installing the dependencies, run this in an interactive R session and
select **DESCRIPTION** in the extracted package folder:

```r
install.packages(dirname(file.choose()), repos = NULL, type = "source")
```

</details>

<details>
<summary>Already have a built R source archive?</summary>

```r
install.packages("TidyPanel_0.2.129.tar.gz", repos = NULL, type = "source")
```

A built source archive also includes the example guide:

```r
vignette("TidyPanel_introduction", package = "TidyPanel")
```

</details>

## Quick Start

Run a bundled example without preparing your own file:

```r
library(TidyPanel)

input <- system.file(
  "extdata", "demo_03_subtotals.csv",
  package = "TidyPanel", mustWork = TRUE
)
result <- read_messy_panel(input, auto_pivot = TRUE, return_audit = TRUE)

result$data
result$audit
```

The total row is removed and the two yearly columns become a four-row long
table. Replace `input` with your own file path; omit `auto_pivot = TRUE`
to keep wide columns.

## Agent Workflow

**One request -> one JSON response -> complete local result files.**

Install the matching **v0.2.129 public source package** first. The launcher
checks the installed edition and version before loading it. This is a local
process interface, not an MCP server or automatic global tool registration.

From the extracted source folder, an agent can inspect the interface and run a job:

```sh
Rscript --vanilla exec/tidypanel.R describe
Rscript --vanilla exec/tidypanel.R clean --request request.json
```

Send a UTF-8 JSON object in the request file, or use `clean --request -` and
send the same object on standard input, then close that input stream:

```json
{
  "input": "inst/extdata/demo_03_subtotals.csv",
  "output_dir": "../tidypanel-job-001",
  "na_strings": [""],
  "auto_pivot": true
}
```

This request uses one of the three fictional demos, not an additional dataset.
For another source, declare its own missing markers and choose its intended
Excel `sheet` or ZIP `archive_member`. All paths are relative to the process
working directory, including requests read from files.

| Interface | Contract |
| --- | --- |
| Input | One local tabular file, a new output directory and explicit `na_strings`. Unknown keys, URLs and network-share paths are rejected. |
| Discovery | `describe` returns the JSON request schema, limits, result fields and exit codes. |
| Output | Exactly one JSON response on stdout. Reader chatter goes to stderr; bounded warning messages are retained in the response. |
| Full results | `artifacts` names the cleaned RDS, convenience CSV, audit CSV and complete column metadata JSON. |
| Preview | Off by default. Opt in with `preview_rows`; limits are 10 rows, 20 columns and 160 characters per displayed label or value. |
| Failure | A nonzero exit code and `status: "error"`. Do not treat partial output directories as successful jobs. |

On success, `status` is `"ok"`, with `row_count`, `column_count`,
`artifacts`, `preview`, `audit_preview` and warning fields. Start with the
counts and artifact paths; read only the data needed for the question.
A preview, path, warning or audit can contain sensitive caller information.
Redact it or keep it local unless sharing with the selected model is authorized.

<details>
<summary>Installed entry point, limits and exit codes</summary>

To locate the launcher after installation, without relying on the source folder:

```r
system.file("exec", "tidypanel.R", package = "TidyPanel", mustWork = TRUE)
```

Pass that returned path to `Rscript --vanilla`. R callers can alternatively
use `TidyPanel::tidypanel_cli()`; it returns an exit status without terminating R.
The bundled launcher exits with that status.

| Exit code | Meaning |
| --- | --- |
| 0 | Completed successfully; review any warnings. |
| 2 | Invalid arguments, malformed JSON or invalid request fields. |
| 3 | Missing dependencies or a nonmatching installed edition/version. |
| 4 | Cleaning failed, an archive has unsafe paths, or output is not one table. |
| 5 | Output directory unavailable or artifact writing failed. |

Requests are limited to 65,536 bytes. Only options listed by `describe` are
accepted. `na_strings: []` declares no caller missing tokens. `sheet` selects
one name or positive index, not `"ALL"`. `auto_pivot` defaults to false.
`preview_rows` defaults to 0; `preview_columns` defaults to 8 and
`preview_chars` to 80. Preview cells are text, with null for missing positions;
their column indices map to the complete schema in the metadata artifact.

The output directory must be new, have an existing parent and stay outside
the installed package and launcher package folders. Existing files are never
overwritten by the job interface. A failed write may leave a partial new
directory; retain it for inspection and choose a new directory for a retry.
The interface inherits the caller's filesystem permissions and is not a sandbox.

The RDS artifact retains the full cleaned R table and audit for a compatible
trusted R environment. CSV is a convenience export with empty fields for
missing values; it cannot preserve every type or distinguish every literal
empty string from missingness. Consult column metadata before cross-language
reading. The audit records supported operations and may be empty; it is not a
complete cell-by-cell history. Preserve the source and review the result before
analysis. This interface adds no new cleaning rules, unit inference or analysis
model, and sends no files to an AI service.

</details>

## Three Small Examples

| CSV | Try it for |
| --- | --- |
| [Numbers](inst/extdata/demo_01_numbers.csv) | Grouped amounts, negatives and leading-zero codes |
| [Dates](inst/extdata/demo_02_dates.csv) | Calendar dates and missing readings |
| [Subtotals](inst/extdata/demo_03_subtotals.csv) | Total-row removal and wide-to-long reshaping |

Exactly three fictional CSVs, each with three input records. No personal
records, development datasets or regression fixtures are distributed.
See the [example guide](vignettes/TidyPanel_introduction.Rmd) for all three workflows.

<details>
<summary>Cleaning notes and supported-layout boundaries</summary>

Stable categorical labels in headerless numeric tables and paired free/total
measurements are retained. For numeric text reports, provide the observation
section as a plain table or data frame rather than including report metadata.
Commented delimited headers retain their labels, including whitespace after
the comment marker and quoted separators in field names or records.
Explicit TAB records remain tab-delimited when labels contain spaces.
Explicit two-column `field,value` tables retain every uniquely named record,
including blank trailing values and long text. Hash characters inside delimited
fields are treated as data rather than silently truncating the field.
Explicit identifier suffixes remain distinct from financial measurement labels.
Fields named exactly `site` preserve literal identifiers, including leading zeros.
Fields named `ident` or ending in `_ident` retain text codes without filling
missing positions. A qualified `iso_country` column retains literal `NA` only
with at least two distinct other uppercase two-letter code observations.
Coded reference catalogs require a separately prepared named observation table.
Sectioned observation exports must be supplied as separately prepared named tables.
Prefaced measurement exports require a separately prepared named observation table.
Prefaced monthly series exports require a separately prepared named observation table.
Qualified numeric total-uncertainty measurements are retained, while ordinary
subtotal columns remain removable.
Related measurement fields such as `site_count` remain eligible for numeric cleaning.
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
Slash-dated series exports require a separately prepared named observation
table; unsupported metadata layouts fail clearly instead of merging labels.
Mixed compact codes with zero-padded evidence stay text rather than losing
letters or padding. Scientific notation and formatted decimal amounts retain
numeric conversion. Unsupported name-first reference exports require a named
table with an identifier column first.
Sectioned control-record reports require a separately prepared named
observation table; the simplified public reader does not combine their sections.

Large-table subtotal checks retain the existing row-selection rules while
avoiding repeated per-record matching.

</details>

## Contact

**Tony Lu** | [xulunt123@gmail.com](mailto:xulunt123@gmail.com)
