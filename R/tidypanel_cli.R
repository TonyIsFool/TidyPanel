#' Run the Local JSON Cleaning Interface
#'
#' @description A process-friendly adapter for the public table cleaner. It reads
#' one bounded UTF-8 JSON request and writes one JSON response to standard output.
#' It does not evaluate request text, start a service, or upload data.
#' @param args Character arguments: `describe`, or `clean --request PATH`.
#' Use `-` as PATH for standard input. `clean` alone reads standard input too.
#' @param protected_roots Additional existing directories where output is forbidden.
#' The installed package directory is always protected. The launcher adds its own
#' package directory when run from an extracted source tree.
#' @return Invisibly, an integer exit status: 0 on success, 2 for an invalid
#' request, 3 for an edition mismatch, 4 for a cleaning failure, or 5 for an output
#' failure. The function does not terminate R; the bundled launcher does.
#' @details Run `describe` for the request schema. Select one local file and a new
#' output directory with an existing parent. Explicitly declare `na_strings` from
#' the source. Preview values are opt-in. CSV is a convenience export, not a
#' lossless type or missing-value interchange format. Failed writes can leave a
#' partial new output directory; it is not deleted or reused.
#' The description includes four fictional example request templates, with
#' installed input paths and explicit options. It does not read caller tables
#' or create output directories. Choose a new output directory before reuse.
#' @examples
#' \dontrun{
#' tidypanel_cli("describe")
#' }
#' @export
tidypanel_cli <- function(args = commandArgs(trailingOnly = TRUE),
                          protected_roots = character()) {
    warnings_seen <- character()
    warning_count <- 0L
    warnings_truncated <- FALSE
    phase <- "request"
    command <- NULL
    created_output <- NULL
    status <- 0L
    response <- tryCatch(withCallingHandlers({
        .tp_cli_check_edition()
        if (!is.character(args) || anyNA(args)) .tp_cli_abort("invalid_arguments", "Arguments must be strings.", 2L)
        if (!length(args) || (length(args) == 1L && args %in% c("describe", "--help", "-h"))) {
            command <- "describe"
            list(schema_version = "1.0", status = "ok", command = "describe", tool = .tp_cli_description())
        } else {
            if (length(args) && identical(args[[1L]], "clean")) command <- "clean"
            if (!identical(args, "clean") &&
                !(length(args) == 3L && identical(args[1:2], c("clean", "--request")))) {
                .tp_cli_abort("invalid_arguments", "Use describe, or clean --request PATH; use - for standard input.", 2L)
            }
            request_path <- if (length(args) == 1L) "-" else args[[3L]]
            request <- .tp_cli_validate_request(.tp_cli_read_request(request_path), protected_roots)
            phase <- "cleaning"
            result <- .tp_cli_read_table(request)
            if (!is.list(result) || !is.data.frame(result$data) ||
                !is.data.frame(result$audit) || !nrow(result$data) || !ncol(result$data)) {
                .tp_cli_abort("unexpected_result", "Expected one nonempty table and a tabular audit. Select one table explicitly.", 4L)
            }
            phase <- "output"
            if (!dir.create(request$output_dir, showWarnings = FALSE)) {
                .tp_cli_abort("output_unavailable", "Output directory already exists or cannot be created.", 5L)
            }
            created_output <- request$output_dir
            artifacts <- list(
                rds = file.path(created_output, "cleaned.rds"),
                csv = file.path(created_output, "cleaned.csv"),
                audit = file.path(created_output, "audit.csv"),
                metadata = file.path(created_output, "metadata.json")
            )
            saveRDS(list(data = result$data, audit = result$audit), artifacts$rds)
            utils::write.csv(result$data, artifacts$csv, row.names = FALSE, na = "", fileEncoding = "UTF-8")
            utils::write.csv(result$audit, artifacts$audit, row.names = FALSE, na = "", fileEncoding = "UTF-8")
            metadata <- list(
                schema_version = "1.0", package_version = "0.2.133",
                row_count = nrow(result$data), column_count = ncol(result$data),
                columns = lapply(seq_len(ncol(result$data)), function(i) list(
                    index = i, name = names(result$data)[[i]],
                    r_classes = as.list(class(result$data[[i]])),
                    missing_count = sum(is.na(result$data[[i]]))
                )),
                csv_missing_representation = "", csv_is_lossless = FALSE,
                warnings = as.list(warnings_seen), warning_count = warning_count,
                warnings_truncated = warnings_truncated
            )
            writeLines(.tp_cli_json(metadata), artifacts$metadata, useBytes = TRUE)
            list(
                schema_version = "1.0", status = "ok", command = "clean",
                package_version = "0.2.133", row_count = nrow(result$data),
                column_count = ncol(result$data), artifacts = artifacts,
                preview = if (request$preview_rows == 0L) NULL else .tp_cli_preview(result$data, request$preview_rows, request$preview_columns, request$preview_chars),
                audit_preview = .tp_cli_preview(result$audit, 8L, 2L, 160L)
            )
        }
    }, warning = function(w) {
        warning_count <<- warning_count + 1L
        message <- conditionMessage(w)
        if (length(warnings_seen) < 16L) warnings_seen <<- c(warnings_seen, substr(message, 1L, 512L))
        if (nchar(message) > 512L || warning_count > 16L) warnings_truncated <<- TRUE
        invokeRestart("muffleWarning")
    }), error = function(e) {
        known <- inherits(e, "tidypanel_cli_error")
        status <<- if (known) e$status else switch(phase, request = 2L, cleaning = 4L, output = 5L)
        list(
            schema_version = "1.0", status = "error", command = command,
            error = list(
                code = if (known) e$code else switch(phase, request = "invalid_request", cleaning = "cleaning_failed", output = "output_failed"),
                message = substr(conditionMessage(e), 1L, 2048L)
            ), output_dir = created_output
        )
    })
    response$warnings <- as.list(warnings_seen)
    response$warning_count <- warning_count
    response$warnings_truncated <- warnings_truncated
    cat(.tp_cli_json(response), "\n", sep = "", file = stdout())
    invisible(status)
}

.tp_cli_json <- function(x) {
    as.character(jsonlite::toJSON(x, auto_unbox = TRUE, null = "null", na = "null", digits = NA))
}

.tp_cli_abort <- function(code, message, status) {
    stop(structure(list(message = message, call = NULL, code = code, status = status),
                   class = c("tidypanel_cli_error", "error", "condition")))
}

.tp_cli_check_edition <- function() {
    path <- getNamespaceInfo(asNamespace("TidyPanel"), "path")
    metadata <- read.dcf(file.path(path, "DESCRIPTION"))
    if (!all(c("Version", "Config/TidyPanel/Edition") %in% colnames(metadata)) ||
        metadata[1L, "Version"] != "0.2.133" || metadata[1L, "Config/TidyPanel/Edition"] != "public") {
        .tp_cli_abort("edition_mismatch", "Install the matching public source edition before using this interface.", 3L)
    }
}

.tp_cli_description <- function() {
    scalar <- function(type, ...) c(list(type = type), list(...))
    examples <- lapply(list(
        list(id = "numbers", title = "Numbers and literal codes", file = "demo_01_numbers.csv", markers = list("NA"), pivot = FALSE),
        list(id = "dates", title = "Dates and missing readings", file = "demo_02_dates.csv", markers = list("NA"), pivot = FALSE),
        list(id = "subtotals", title = "Subtotals and yearly columns", file = "demo_03_subtotals.csv", markers = list(), pivot = TRUE),
        list(id = "missing_tokens", title = "Caller-declared missing tokens", file = "demo_04_missing_tokens.csv", markers = list("MISSING"), pivot = FALSE)
    ), function(example) list(
        id = example$id, title = example$title, file = example$file,
        fictional = TRUE, input_records = 3L,
        request = list(
            input = system.file("extdata", example$file, package = "TidyPanel", mustWork = TRUE),
            output_dir = paste0("../tidypanel-example-", example$id),
            na_strings = example$markers, auto_pivot = example$pivot, preview_rows = 0L
        )
    ))
    list(
        name = "tidypanel.clean", package_version = "0.2.133", edition = "public",
        description = "Clean one caller-selected local table with the public reader; keep full artifacts locally.",
        examples = examples,
        request_schema = list(
            `$schema` = "https://json-schema.org/draft/2020-12/schema",
            type = "object", additionalProperties = FALSE,
            required = list("input", "output_dir", "na_strings"),
            properties = list(
                input = scalar("string", minLength = 1L, maxLength = 4096L, description = "Existing local tabular file; no URLs or network-share paths."),
                output_dir = scalar("string", minLength = 1L, maxLength = 4096L, description = "New directory outside package folders, with an existing parent."),
                na_strings = list(type = "array", maxItems = 128L, items = scalar("string", maxLength = 256L), description = "Source-declared missing tokens; [] declares none."),
                sheet = list(anyOf = list(scalar("string", minLength = 1L, maxLength = 4096L, not = list(pattern = "^[Aa][Ll][Ll]$")), scalar("integer", minimum = 1L, maximum = .Machine$integer.max), scalar("null")), description = "One Excel sheet name or positive index; ALL is not supported."),
                archive_member = list(anyOf = list(scalar("string", minLength = 1L, maxLength = 4096L), scalar("null"))),
                auto_pivot = scalar("boolean", default = FALSE),
                preview_rows = scalar("integer", minimum = 0L, maximum = 10L, default = 0L),
                preview_columns = scalar("integer", minimum = 1L, maximum = 20L, default = 8L),
                preview_chars = scalar("integer", minimum = 1L, maximum = 160L, default = 80L)
            )
        ),
        request_limit_bytes = 65536L,
        path_base = "Process working directory, including requests read from files.",
        stdout = "Exactly one JSON response. Other reader output goes to stderr.",
        response_contract = list(
            success = list("schema_version", "status", "command", "package_version", "row_count", "column_count", "artifacts", "preview", "audit_preview", "warnings", "warning_count", "warnings_truncated"),
            error = list("schema_version", "status", "command", "error", "output_dir", "warnings", "warning_count", "warnings_truncated"),
            artifacts = list("rds", "csv", "audit", "metadata")
        ),
        exit_codes = list(success = 0L, invalid_request = 2L, edition_mismatch = 3L, cleaning_failed = 4L, output_failed = 5L),
        artifacts = list(rds = "Complete cleaned R data and audit for compatible trusted R use.", csv = "Convenience table export; consult metadata for types and missing representation.", audit = "Recorded operations, not a cell-by-cell change history.", metadata = "Complete column schema and bounded warning messages."),
        privacy = "Preview values are opt-in and may be sensitive. No data is uploaded. This process inherits the caller's filesystem permissions; it is not a sandbox."
    )
}

.tp_cli_text <- function(x, name, max_chars = 4096L) {
    if (!is.character(x) || length(x) != 1L || is.na(x) || !nzchar(x) || nchar(x) > max_chars) {
        .tp_cli_abort("invalid_request", paste0(name, " must be one nonempty bounded string."), 2L)
    }
    x
}

.tp_cli_local_path <- function(x, name) {
    x <- .tp_cli_text(x, name)
    if (grepl("^[A-Za-z][A-Za-z0-9+.-]*://|^[/\\\\]{2}", x, perl = TRUE)) {
        .tp_cli_abort("invalid_request", paste0(name, " must be a local path, not a URL or network-share path."), 2L)
    }
    path.expand(x)
}

.tp_cli_read_request <- function(path) {
    if (identical(path, "-")) {
        con <- file("stdin", open = "rb")
    } else {
        path <- .tp_cli_local_path(path, "Request path")
        if (!file.exists(path) || dir.exists(path)) .tp_cli_abort("invalid_request", "Request file does not exist.", 2L)
        con <- file(path, open = "rb")
    }
    on.exit(close(con))
    bytes <- readBin(con, "raw", n = 65537L)
    if (!length(bytes) || length(bytes) > 65536L || any(bytes == as.raw(0L))) {
        .tp_cli_abort("invalid_request", "Request must be nonempty UTF-8 JSON of at most 65536 bytes, without NUL bytes.", 2L)
    }
    if (length(bytes) >= 3L && identical(bytes[1:3], as.raw(c(239L, 187L, 191L)))) bytes <- bytes[-(1:3)]
    text <- iconv(rawToChar(bytes), from = "UTF-8", to = "UTF-8", sub = NA_character_)
    if (is.na(text) || !jsonlite::validate(text)) .tp_cli_abort("invalid_request", "Request is not valid UTF-8 JSON.", 2L)
    request <- jsonlite::fromJSON(text, simplifyVector = FALSE)
    if (!is.list(request) || is.null(names(request)) || anyDuplicated(names(request))) {
        .tp_cli_abort("invalid_request", "Request must be one JSON object with unique keys.", 2L)
    }
    request
}

.tp_cli_validate_request <- function(x, protected_roots) {
    supported <- c("input", "output_dir", "na_strings", "sheet", "archive_member", "auto_pivot", "preview_rows", "preview_columns", "preview_chars")
    if (any(!names(x) %in% supported) || !all(c("input", "output_dir", "na_strings") %in% names(x))) {
        .tp_cli_abort("invalid_request", "Required keys are input, output_dir and na_strings; unknown keys are rejected. Run describe for the schema.", 2L)
    }
    if (!is.list(x$na_strings) || !is.null(names(x$na_strings)) || length(x$na_strings) > 128L ||
        !all(vapply(x$na_strings, function(v) is.character(v) && length(v) == 1L && !is.na(v) && nchar(v) <= 256L, logical(1)))) {
        .tp_cli_abort("invalid_request", "na_strings must be an array of at most 128 strings, each at most 256 characters.", 2L)
    }
    x$na_strings <- as.character(unlist(x$na_strings, use.names = FALSE))
    if (!is.null(x$sheet)) {
        sheet_ok <- (is.character(x$sheet) && length(x$sheet) == 1L && !is.na(x$sheet) && nzchar(x$sheet) && nchar(x$sheet) <= 4096L && toupper(x$sheet) != "ALL") ||
            (is.numeric(x$sheet) && length(x$sheet) == 1L && is.finite(x$sheet) && x$sheet == floor(x$sheet) && x$sheet >= 1 && x$sheet <= .Machine$integer.max)
        if (!sheet_ok) .tp_cli_abort("invalid_request", "sheet must select one name or positive integer index; ALL is not supported.", 2L)
    }
    if (!is.null(x$archive_member)) x$archive_member <- .tp_cli_text(x$archive_member, "archive_member")
    if (!"auto_pivot" %in% names(x)) x$auto_pivot <- FALSE
    if (!is.logical(x$auto_pivot) || length(x$auto_pivot) != 1L || is.na(x$auto_pivot)) .tp_cli_abort("invalid_request", "auto_pivot must be boolean.", 2L)
    bounds <- list(preview_rows = c(0L, 10L, 0L), preview_columns = c(1L, 20L, 8L), preview_chars = c(1L, 160L, 80L))
    for (name in names(bounds)) {
        limit <- bounds[[name]]
        if (!name %in% names(x)) x[[name]] <- limit[[3L]]
        value <- x[[name]]
        if (!is.numeric(value) || length(value) != 1L || !is.finite(value) || value != floor(value) || value < limit[[1L]] || value > limit[[2L]]) {
            .tp_cli_abort("invalid_request", paste0(name, " must be an integer within the described limits."), 2L)
        }
        x[[name]] <- as.integer(value)
    }
    input <- .tp_cli_local_path(x$input, "input")
    if (!file.exists(input) || dir.exists(input)) .tp_cli_abort("invalid_request", "Input file does not exist.", 2L)
    x$input <- normalizePath(input, winslash = "/", mustWork = TRUE)
    output <- .tp_cli_local_path(x$output_dir, "output_dir")
    link <- Sys.readlink(output)
    if (file.exists(output) || (!is.na(link) && nzchar(link))) .tp_cli_abort("output_unavailable", "Output directory must not already exist.", 5L)
    if (!dir.exists(dirname(output)) || basename(output) %in% c(".", "..", "")) .tp_cli_abort("invalid_request", "Output directory needs an existing parent and a new name.", 2L)
    parent <- normalizePath(dirname(output), winslash = "/", mustWork = TRUE)
    output <- file.path(parent, basename(output))
    if (!is.character(protected_roots) || anyNA(protected_roots) || any(!dir.exists(protected_roots))) .tp_cli_abort("invalid_arguments", "Protected roots must be existing directories.", 2L)
    roots <- unique(c(getNamespaceInfo(asNamespace("TidyPanel"), "path"), protected_roots))
    roots <- normalizePath(roots, winslash = "/", mustWork = TRUE)
    comparable <- function(p) if (.Platform$OS.type == "windows") tolower(p) else p
    candidate <- comparable(output)
    if (any(vapply(comparable(roots), function(p) identical(candidate, p) || startsWith(candidate, paste0(sub("/+$", "", p), "/")), logical(1)))) {
        .tp_cli_abort("output_unavailable", "Output must be outside package directories.", 5L)
    }
    x$output_dir <- output
    x
}

.tp_cli_read_table <- function(request) {
    # Check archive paths before delegating extraction to the existing reader.
    con <- file(request$input, open = "rb")
    signature <- tryCatch(readBin(con, "raw", n = 4L), finally = close(con))
    zipped <- grepl("\\.zip$", request$input, ignore.case = TRUE) ||
        (!grepl("\\.xlsx?$", request$input, ignore.case = TRUE) && length(signature) == 4L &&
         identical(as.integer(signature[1:2]), c(80L, 75L)) && any(vapply(list(c(3L, 4L), c(5L, 6L), c(7L, 8L)), function(v) identical(as.integer(signature[3:4]), v), logical(1))))
    if (!is.null(request$sheet) && !grepl("\\.xlsx?$", request$input, ignore.case = TRUE)) {
        .tp_cli_abort("invalid_request", "sheet can only select an Excel worksheet.", 2L)
    }
    if (!is.null(request$archive_member) && !zipped) {
        .tp_cli_abort("invalid_request", "archive_member can only select a ZIP member.", 2L)
    }
    if (zipped) {
        members <- utils::unzip(request$input, list = TRUE)$Name
        unsafe <- grepl("(^|[/\\\\])\\.\\.([/\\\\]|$)|^[/\\\\]|:|\\\\", members, perl = TRUE)
        keys <- if (.Platform$OS.type == "windows") tolower(members) else members
        if (any(unsafe) || anyDuplicated(keys)) .tp_cli_abort("unsafe_archive", "Archive has unsafe or duplicate member paths.", 4L)
    }
    level <- sink.number(type = "output")
    sink(stderr(), type = "output")
    on.exit(if (sink.number(type = "output") > level) sink(type = "output"), add = TRUE)
    read_messy_panel(request$input, sheet = request$sheet, na_strings = request$na_strings,
                     auto_pivot = request$auto_pivot, return_audit = TRUE,
                     extract_all_blocks = FALSE, archive_member = request$archive_member)
}

.tp_cli_preview <- function(data, rows, columns, chars) {
    indices <- seq_len(min(ncol(data), columns))
    records <- seq_len(min(nrow(data), rows))
    list(
        columns = lapply(indices, function(i) list(index = i, name = substr(names(data)[[i]], 1L, chars), r_classes = as.list(class(data[[i]])))),
        rows = lapply(records, function(r) lapply(indices, function(i) {
            value <- as.character(data[[i]][r])
            if (is.na(value)) NULL else substr(value, 1L, chars)
        })),
        row_count = length(records), column_count = length(indices),
        truncated = nrow(data) > length(records) || ncol(data) > length(indices) ||
            any(nchar(names(data)[indices]) > chars) || any(vapply(data[indices], function(x) any(nchar(as.character(utils::head(x, rows))) > chars, na.rm = TRUE), logical(1)))
    )
}
