test_that("clean_variable_names handles multilingual exact matches", {
    df <- data.frame(
        `身份证号` = 1,
        `datum` = 2,
        `kategorie` = 3,
        `montant` = 4,
        check.names = FALSE
    )
    res <- clean_variable_names(df)
    expect_equal(colnames(res), c("id", "date", "category", "value"))
})

test_that("clean_variable_names handles regex fuzzy matches", {
    df <- data.frame(
        `Q3 Total Revenue` = 1,
        `Net Profit Margin` = 2,
        `Operating Expense` = 3,
        check.names = FALSE
    )
    res <- clean_variable_names(df)
    expect_equal(colnames(res), c("revenue", "profit", "cost"))
})

test_that("clean_variable_names enforces strict snake_case for unmapped variables", {
    df <- data.frame(
        `My Custom Column 1!` = 1,
        `Another   Weird -- Column` = 2,
        check.names = FALSE
    )
    res <- clean_variable_names(df)
    expect_equal(colnames(res), c("my_custom_column_1", "another_weird_column"))
})

test_that("clean_variable_names handles Excel dates properly", {
    df <- data.frame(
        `44197` = 1,
        check.names = FALSE
    )
    res <- clean_variable_names(df)
    expect_equal(colnames(res), "2021-01-01")
})
