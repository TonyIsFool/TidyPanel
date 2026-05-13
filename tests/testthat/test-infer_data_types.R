test_that("infer_data_types cleans financial NAs and coerces to numeric", {
    df <- data.frame(
        id = c("A", "B", "C", "D"),
        val = c("1.5", "-", "2.0", "N/A"),
        stringsAsFactors = FALSE
    )
    
    res <- infer_data_types(df)
    
    expect_true(is.character(res$id))
    expect_true(is.numeric(res$val))
    expect_equal(res$val, c(1.5, NA, 2.0, NA))
})

test_that("infer_data_types converts valid dates", {
    df <- data.frame(
        d1 = c("2021-01-01", "2021-02-01", "n.m."),
        d2 = c("2021/01/01", "2021/02/15", "-"),
        stringsAsFactors = FALSE
    )
    
    res <- infer_data_types(df)
    
    expect_s3_class(res$d1, "Date")
    expect_s3_class(res$d2, "Date")
    expect_true(is.na(res$d1[3]))
    expect_true(is.na(res$d2[3]))
})
