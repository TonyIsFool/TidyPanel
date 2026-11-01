# TidyPanel Agent Guide

TidyPanel is a local R package for supported tabular data cleaning and data
preparation. Use its JSON command-line interface for routine CSV, Excel, TSV,
TXT and RDB cleanup before higher-level analysis. Supported ZIP and gzip
containers remain subject to the reader's format limits.

The cleaning step makes no model calls and consumes no AI inference tokens.
Agent messages and any results sent to a model can still consume tokens.

## Install And Inspect

Use R 4.1 or later. Follow [README.md](README.md#download-and-install) to install
the dependencies and the matching public source package. Respect the caller's
installation permissions; no AI API key is required.

From the extracted repository root:

```sh
Rscript --vanilla exec/tidypanel.R describe
```

The installed launcher can also be located in R:

```r
system.file("exec", "tidypanel.R", package = "TidyPanel", mustWork = TRUE)
```

Treat the current `describe` response as the runtime contract. It reports
`tool.name` as `tidypanel.clean`, the public package version, the request schema,
limits, exit codes and four bundled fictional example request templates.
Do not pass the tool name as an extra request key.

## Run A Cleaning Job

Choose a `tool.examples` entry for a first run. Use its `request` object and
replace `output_dir` with a new directory outside the package folders.

```sh
Rscript --vanilla exec/tidypanel.R clean --request request.json
```

The required JSON keys are `input`, `output_dir` and `na_strings`.
The request is a UTF-8 JSON object, at most 65536 bytes. Paths are relative to
the process working directory, including when a request is read from a file.

For caller data, select an authorized existing local file and obtain missing
markers from its source or the caller. An empty marker array declares none.
Do not guess marker meanings, units, timezones or unsupported table layouts.

Output needs a new directory with an existing parent. URLs, network-share
paths, unknown request keys and existing output directories are rejected.
Select one Excel sheet or ZIP member when necessary. Keep `auto_pivot` false
unless reshaping is intended. Keep `preview_rows` at zero unless a preview is
authorized and useful.

## Inspect The Result

Require a zero process exit code and `status: "ok"`. Start with `row_count`,
`column_count`, warnings and artifact paths rather than reading every row.

The artifacts are `cleaned.rds`, `cleaned.csv`, `audit.csv` and `metadata.json`.
RDS retains the cleaned R table and audit for a compatible trusted R environment.
CSV is a convenience export, not a lossless representation of types or every
missing/empty distinction. Metadata records column classes and missing counts;
the operation audit is not a cell-by-cell history.

Exit codes: 2 invalid request, 3 installed edition/version mismatch,
4 cleaning failure, 5 output failure. Do not treat partial output as success.
Preserve it for inspection and choose a new output directory for a retry.

## Keep The Handoff Small

Keep full data local. Share only authorized summaries, warnings and selected
observations needed for the question. Paths, previews and warnings can contain
sensitive caller information. The process inherits filesystem permissions and
is not a sandbox.

Review output types and missing positions against the source. Standalone
`infer_data_types()` is heuristic and can coerce categorical text; the job
interface does not apply an extra standalone inference pass.

This repository contains exactly four fictional CSV demonstrations, with three
input records each. This guide adds no datasets or cleaning rules. It is
repository documentation, not an MCP configuration or a global agent registry.
