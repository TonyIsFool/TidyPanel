test_that("validate_panel detects duplicate rows", {
    df <- data.frame(
        id  = c("A", "A", "B"),
        val = c(1.0, 1.0, 2.0)
    )
    report <- validate_panel(df, verbose = FALSE)
    expect_equal(report$n_duplicates, 1)
})

test_that("validate_panel flags high-NA columns", {
    df <- data.frame(
        id    = c("A", "B", "C", "D"),
        messy = c(1, NA, NA, NA)
    )
    report <- validate_panel(df, na_warn_threshold = 0.5, verbose = FALSE)
    expect_true("messy" %in% report$high_na_cols)
})

test_that("validate_panel detects should-be-numeric character columns", {
    df <- data.frame(
        id  = c("A", "B", "C"),
        val = c("1.1", "2.2", "3.3"),
        stringsAsFactors = FALSE
    )
    report <- validate_panel(df, verbose = FALSE)
    expect_true("val" %in% report$mistyped_cols)
})

test_that("validate_panel detects outliers via IQR", {
    df <- data.frame(
        val = c(1, 2, 3, 2, 3, 2, 3, 2, 999)
    )
    report <- validate_panel(df, verbose = FALSE)
    expect_true("val" %in% report$outlier_cols)
})
