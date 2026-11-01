#!/usr/bin/env Rscript
local({
    fail <- function(code, message, status) {
        # Only fixed bootstrap strings reach this path, before dependencies load.
        cat(sprintf('{"schema_version":"1.0","status":"error","command":null,"error":{"code":"%s","message":"%s"},"output_dir":null,"warnings":[],"warning_count":0,"warnings_truncated":false}\n', code, message))
        quit(save = "no", status = status, runLast = FALSE)
    }
    args <- commandArgs(trailingOnly = FALSE)
    script <- sub("^--file=", "", args[startsWith(args, "--file=")])
    root <- if (length(script) == 1L) dirname(dirname(normalizePath(script, mustWork = TRUE))) else character()
    package_path <- tryCatch(find.package("TidyPanel", quiet = TRUE), error = function(e) character())
    metadata <- if (length(package_path) == 1L) tryCatch(read.dcf(file.path(package_path, "DESCRIPTION")), error = function(e) NULL) else NULL
    if (is.null(metadata) || !all(c("Version", "Config/TidyPanel/Edition") %in% colnames(metadata)) ||
        metadata[1L, "Version"] != "0.2.133" || metadata[1L, "Config/TidyPanel/Edition"] != "public") {
        fail("edition_mismatch", "Install the matching public source edition before using this launcher.", 3L)
    }
    loaded <- tryCatch(suppressWarnings(suppressPackageStartupMessages(requireNamespace("TidyPanel", quietly = TRUE))), error = function(e) FALSE)
    if (!loaded) fail("dependency_unavailable", "Install the public package and its declared dependencies.", 3L)
    status <- tryCatch(TidyPanel::tidypanel_cli(protected_roots = root), error = function(e) {
        fail("interface_failed", "The local interface could not complete the request.", 5L)
    })
    quit(save = "no", status = status, runLast = FALSE)
})
