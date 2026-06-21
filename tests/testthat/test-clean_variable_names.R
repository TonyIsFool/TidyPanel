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

test_that("clean_variable_names keeps Unicode scientific numerals as ASCII digits", {
    df <- data.frame(a = 1, b = 2, c = 3, check.names = FALSE)
    colnames(df) <- c(
        "Annual CO\u2082 emissions",
        "PM\u2082.\u2085 concentration",
        "CH\u2084 intensity"
    )

    res <- clean_variable_names(df)

    expect_equal(
        colnames(res),
        c("annual_co2_emissions", "pm2_5_concentration", "ch4_intensity")
    )
})

test_that("clean_variable_names splits camelCase variable names", {
    df <- data.frame(a = 1, b = 2, c = 3, check.names = FALSE)
    colnames(df) <- c("customerBalanceUSD", "customerCount", "HTTPStatusCode")

    res <- clean_variable_names(df)

    expect_equal(colnames(res), c("customer_balance_usd", "customer_count", "http_status_code"))
})
