test_that("normalize_units properly scales and renames columns", {
    df <- data.frame(
        `Revenue ($M)` = c(1.5, 2.0),
        `Cost (in thousands)` = c(500, 600),
        `Employees ('000s)` = c(10, 12),
        `Unchanged` = c(1, 2),
        check.names = FALSE
    )
    
    res <- normalize_units(df)
    
    expect_equal(res$`Revenue`, c(1500000, 2000000))
    expect_equal(res$`Cost`, c(500000, 600000))
    expect_equal(res$`Employees`, c(10000, 12000))
    expect_equal(res$`Unchanged`, c(1, 2))
})

test_that("normalize_units supports multilingual units", {
    df <- data.frame(
        `Asset (百万)` = c(5, 6),
        `Debt (in millionen)` = c(1, 2),
        `Val (十亿)` = c(1, 2),
        check.names = FALSE
    )
    
    res <- normalize_units(df)
    expect_equal(res$`Asset`, c(5000000, 6000000))
    expect_equal(res$`Debt`, c(1000000, 2000000))
    expect_equal(res$`Val`, c(1000000000, 2000000000))
})
