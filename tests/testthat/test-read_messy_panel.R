library(testthat)
library(TidyPanel)

test_that("Engine throws informative error for empty sheets", {
    # Create an empty file
    tmp <- tempfile(fileext = ".xlsx")
    df_empty <- data.frame()
    
    # We use suppressWarnings because writexl might warn about empty dataframe
    suppressWarnings(writexl::write_xlsx(df_empty, tmp))
    
    expect_error(read_messy_panel(tmp), "Failed to parse any valid panel")
    unlink(tmp)
})

test_that("Engine throws error for non-numeric pure string sheets", {
    tmp <- tempfile(fileext = ".xlsx")
    df_strings <- data.frame(A = c("Hello", "World"), B = c("Foo", "Bar"))
    writexl::write_xlsx(df_strings, tmp)
    
    expect_error(read_messy_panel(tmp), "Failed to parse any valid panel")
    unlink(tmp)
})

test_that("Engine parses inst/extdata deep junk file correctly", {
    # In CRAN testing, inst/extdata becomes extdata
    file_path <- system.file("extdata", "raw_deep_junk.xlsx", package = "TidyPanel")
    
    # Skip if file doesn't exist (e.g. if inst/extdata isn't populated yet)
    skip_if(file_path == "")
    
    res <- read_messy_panel(file_path)
    
    expect_s3_class(res, "data.frame")
    expect_true(nrow(res) > 0)
})

test_that("Engine amputates embedded subtotals", {
    tmp <- tempfile(fileext = ".xlsx")
    df_subtotal <- data.frame(
        Category = c("A", "Subtotal", "B"),
        Value = c("10", "10", "20"),
        stringsAsFactors = FALSE
    )
    writexl::write_xlsx(df_subtotal, tmp)
    
    res <- read_messy_panel(tmp)
    expect_equal(nrow(res), 2)
    expect_equal(as.numeric(res$value), c(10, 20))
    unlink(tmp)
})

test_that("Engine converts scientific notation", {
    tmp <- tempfile(fileext = ".xlsx")
    df_sci <- data.frame(
        ID = c("1", "2"),
        Val = c("2.5 x 10^4", "3.1 * 10^-2"),
        stringsAsFactors = FALSE
    )
    writexl::write_xlsx(df_sci, tmp, col_names = FALSE)
    
    res <- read_messy_panel(tmp)
    expect_equal(as.numeric(res[[2]]), c(25000, 0.031))
    unlink(tmp)
})

test_that("Engine cleans variable names", {
    tmp <- tempfile(fileext = ".xlsx")
    df_names <- data.frame(
        `Company Name` = c("A"),
        `Fiscal Year` = c("2020"),
        `Assets Total` = c("100"),
        check.names = FALSE
    )
    writexl::write_xlsx(df_names, tmp)
    
    res <- read_messy_panel(tmp)
    expect_equal(colnames(res), c("name", "date", "total_assets"))
    unlink(tmp)
})
