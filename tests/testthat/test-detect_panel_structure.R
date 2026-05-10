test_that("detect_panel_structure returns correct dimensions", {
    tmp <- tempfile(fileext = ".xlsx")
    df <- data.frame(
        Category = c("A", "B", "C"),
        Revenue  = c("100", "200", "300"),
        stringsAsFactors = FALSE
    )
    writexl::write_xlsx(df, tmp)
    
    report <- detect_panel_structure(tmp, verbose = FALSE)
    
    expect_true(is.list(report))
    expect_equal(report$n_rows, 4)  # header row + 3 data rows
    expect_equal(report$n_cols, 2)
    expect_equal(report$n_phantom_cols, 0)
    expect_equal(report$n_aggregation_rows, 0)
    expect_false(report$multi_level_header)
    unlink(tmp)
})

test_that("detect_panel_structure detects temporal columns for auto_pivot", {
    tmp <- tempfile(fileext = ".xlsx")
    df <- data.frame(
        Company = c("A", "B"),
        `2019` = c("100", "200"),
        `2020` = c("110", "210"),
        `2021` = c("120", "220"),
        check.names = FALSE,
        stringsAsFactors = FALSE
    )
    writexl::write_xlsx(df, tmp)
    
    report <- detect_panel_structure(tmp, verbose = FALSE)
    
    expect_true(report$has_temporal_cols)
    expect_true(grepl("auto_pivot = TRUE", report$recommended_call))
    unlink(tmp)
})

test_that("detect_panel_structure detects aggregation rows", {
    tmp <- tempfile(fileext = ".xlsx")
    df <- data.frame(
        Category = c("A", "B", "Total"),
        Value    = c("100", "200", "300"),
        stringsAsFactors = FALSE
    )
    writexl::write_xlsx(df, tmp)
    
    report <- detect_panel_structure(tmp, verbose = FALSE)
    
    expect_gte(report$n_aggregation_rows, 1)
    unlink(tmp)
})
