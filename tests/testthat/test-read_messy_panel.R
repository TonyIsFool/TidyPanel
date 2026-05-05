test_that("Engine parses a standard clean panel", {
    tmp <- tempfile(fileext = ".xlsx")
    df <- data.frame(
        Category = c("A", "B"),
        Value = c("100", "200"),
        stringsAsFactors = FALSE
    )
    writexl::write_xlsx(df, tmp)

    res <- read_messy_panel(tmp)
    expect_equal(nrow(res), 2)
    expect_equal(as.numeric(res$value), c(100, 200))
    unlink(tmp)
})
