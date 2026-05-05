test_that("clean_variable_names maps synonyms and enforces snake_case", {
    df <- data.frame(
        `GVKEY` = 1,
        `Total Revenue ($)` = 2,
        `My Custom Column` = 3,
        check.names = FALSE
    )
    res <- clean_variable_names(df)
    expect_equal(colnames(res), c("id", "revenue", "my_custom_column"))
})
