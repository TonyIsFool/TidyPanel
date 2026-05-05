test_that("infer_data_types cleans financial NAs and coerces to numeric", {
    df <- data.frame(
        val = c("1.5", "-", "2.0", "N/A"),
        stringsAsFactors = FALSE
    )
    res <- infer_data_types(df)
    expect_true(is.numeric(res$val))
    expect_equal(res$val, c(1.5, NA, 2.0, NA))
})
