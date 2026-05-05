test_that("normalize_units scales columns declared in millions", {
    df <- data.frame(
        `Revenue ($M)` = c(1.5, 2.0),
        check.names = FALSE
    )
    res <- normalize_units(df)
    expect_equal(res$`Revenue`, c(1500000, 2000000))
})
