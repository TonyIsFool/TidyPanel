library(testthat)
library(TidyPanel)

test_that("Engine throws informative error for empty sheets", {
    # Create an empty file
    tmp <- tempfile(fileext = ".xlsx")
    df_empty <- data.frame()
    
    # We use suppressWarnings because writexl might warn about empty dataframe
    suppressWarnings(writexl::write_xlsx(df_empty, tmp))
    
    expect_error(read_messy_panel(tmp), "Failed to parse any valid panel")
    unlink(tmp)
})

test_that("Engine throws error for non-numeric pure string sheets", {
    tmp <- tempfile(fileext = ".xlsx")
    df_strings <- data.frame(A = c("Hello", "World"), B = c("Foo", "Bar"))
    writexl::write_xlsx(df_strings, tmp)
    
    expect_error(read_messy_panel(tmp), "Failed to parse any valid panel")
    unlink(tmp)
})

test_that("Engine parses inst/extdata deep junk file correctly", {
    # In CRAN testing, inst/extdata becomes extdata
    file_path <- system.file("extdata", "raw_deep_junk.xlsx", package = "TidyPanel")
    
    # Skip if file doesn't exist (e.g. if inst/extdata isn't populated yet)
    skip_if(file_path == "")
    
    res <- read_messy_panel(file_path)
    
    expect_s3_class(res, "data.frame")
    expect_true(nrow(res) > 0)
})

test_that("Engine amputates embedded subtotals", {
    tmp <- tempfile(fileext = ".xlsx")
    df_subtotal <- data.frame(
        Category = c("A", "Subtotal", "B"),
        Value = c("10", "10", "20"),
        stringsAsFactors = FALSE
    )
    writexl::write_xlsx(df_subtotal, tmp)
    
    res <- read_messy_panel(tmp)
    expect_equal(nrow(res), 2)
    expect_equal(as.numeric(res$value), c(10, 20))
    unlink(tmp)
})

test_that("Engine converts scientific notation", {
    tmp <- tempfile(fileext = ".xlsx")
    df_sci <- data.frame(
        ID = c("1", "2"),
        Val = c("2.5 x 10^4", "3.1 * 10^-2"),
        stringsAsFactors = FALSE
    )
    writexl::write_xlsx(df_sci, tmp)
    
    res <- read_messy_panel(tmp)
    expect_equal(as.numeric(res[[2]]), c(25000, 0.031))
    unlink(tmp)
})

test_that("Engine cleans variable names", {
    df_names <- data.frame(
        `Company Name` = c("A"),
        `Fiscal Year` = c("2020"),
        `Assets Total` = c("100"),
        check.names = FALSE
    )
    
    res <- clean_variable_names(df_names)
    expect_equal(colnames(res), c("name", "date", "total_assets"))
})

test_that("Engine amputates multilingual aggregations (French/German)", {
    tmp <- tempfile(fileext = ".xlsx")
    df_multi <- data.frame(
        Category = c("Product A", "Product B", "Gesamt", "Total Général"),
        Value = c("10", "20", "30", "30"),
        stringsAsFactors = FALSE
    )
    writexl::write_xlsx(df_multi, tmp)
    
    res <- read_messy_panel(tmp)
    expect_equal(nrow(res), 2)
    expect_equal(as.numeric(res$value), c(10, 20))
    unlink(tmp)
})

test_that("Engine auto-pivots monthly temporal structures", {
    tmp <- tempfile(fileext = ".xlsx")
    df_monthly <- data.frame(
        Category = c("A", "B"),
        Jan_2020 = c("10", "20"),
        Feb_2020 = c("15", "25"),
        stringsAsFactors = FALSE
    )
    writexl::write_xlsx(df_monthly, tmp)
    
    res <- read_messy_panel(tmp, auto_pivot = TRUE)
    # 2 rows originally * 2 months = 4 rows
    expect_equal(nrow(res), 4)
    expect_true(all(c("category", "time_period", "value") %in% colnames(res)))
    expect_equal(as.numeric(res$value), c(10, 20, 15, 25))
    unlink(tmp)
})

test_that("Engine handles extreme semantic multipliers and parentheses", {
    tmp <- tempfile(fileext = ".xlsx")
    df_extreme <- data.frame(
        Category = c("A", "B", "C"),
        Value = c("$1.5 T", "€2.5M", "(1,234.56k)"),
        stringsAsFactors = FALSE
    )
    writexl::write_xlsx(df_extreme, tmp)
    
    res <- read_messy_panel(tmp)
    expect_equal(as.numeric(res$value), c(1500000000000, 2500000, -1234560))
    unlink(tmp)
})

test_that("Engine discards random garbage noise headers", {
    tmp <- tempfile(fileext = ".xlsx")
    # Simulate a decoy garbage row above the real header
    df_garbage <- data.frame(
        X1 = c("ASDFGHJKL", "Category", "Apple"),
        X2 = c("QWERTYUIO", "Value", "100"),
        stringsAsFactors = FALSE
    )
    writexl::write_xlsx(df_garbage, tmp, col_names = FALSE)
    
    res <- read_messy_panel(tmp)
    expect_equal(colnames(res), c("category", "value"))
    expect_equal(nrow(res), 1)
    expect_equal(res$category[1], "Apple")
    unlink(tmp)
})

test_that("Engine and detector handle dense noise decoy rows", {
    tmp <- tempfile(fileext = ".xlsx")
    df_noise <- data.frame(
        X1 = c("xdasdad", "CDSFC", "CWAEF", "ID", "1", "2"),
        X2 = c("WEDEWADAW", NA, NA, "Product", "Apple", "Orange"),
        X3 = c("CADFCAWFAW", NA, NA, "Price", "1000", "200"),
        stringsAsFactors = FALSE
    )
    # write without col names so X1, X2, X3 are not written
    writexl::write_xlsx(df_noise, tmp, col_names = FALSE)
    
    # 1. Test the static detector
    report <- detect_panel_structure(tmp, verbose = FALSE)
    expect_equal(report$estimated_decoy_rows, 3)
    
    # 2. Test the actual parser
    res <- read_messy_panel(tmp)
    expect_equal(colnames(res), c("id", "product", "price"))
    expect_equal(nrow(res), 2)
    expect_equal(as.numeric(res$price), c(1000, 200))
    
    unlink(tmp)
})

test_that("Engine supports Chinese financial multipliers", {
    tmp <- tempfile(fileext = ".xlsx")
    df_chinese <- data.frame(
        Category = c("Revenue", "Profit", "Cost", "Debt", "Users", "Employees"),
        Value = c("1.5\u4e07", "2.3\u4ebf", "5w", "10y", "8\u5343", "4k"),
        stringsAsFactors = FALSE
    )
    writexl::write_xlsx(df_chinese, tmp)
    
    res <- read_messy_panel(tmp)
    expect_equal(as.numeric(res$value), c(15000, 230000000, 50000, 1000000000, 8000, 4000))
    unlink(tmp)
})

test_that("extract_all_blocks = TRUE returns multiple disjoint panels", {
    tmp <- tempfile(fileext = ".xlsx")
    df_multi <- data.frame(
        VarA = c("First Panel", "A", "B", rep(NA, 6), "Second Panel", "C", "D"),
        VarB = c("ValueA", "10", "20", rep(NA, 6), "ValueB", "30", "40"),
        stringsAsFactors = FALSE
    )
    writexl::write_xlsx(df_multi, tmp)
    
    res_list <- read_messy_panel(tmp, extract_all_blocks = TRUE)
    expect_type(res_list, "list")
    expect_equal(length(res_list), 2)
    
    expect_equal(nrow(res_list$Block_1), 2)
    expect_equal(nrow(res_list$Block_2), 2)
    
    # Check default behavior returns only one block
    res_single <- read_messy_panel(tmp, extract_all_blocks = FALSE)
    expect_s3_class(res_single, "data.frame")
    expect_equal(nrow(res_single), 2)
    
    unlink(tmp)
})

test_that("reads external New.xlsx successfully", {
    # Test reading an external file specifically placed in testdata
    file_path <- "testdata/New.xlsx"
    if (file.exists(file_path)) {
        res <- read_messy_panel(file_path, extract_all_blocks = TRUE)
        expect_type(res, "list")
        expect_true("Block_1" %in% names(res))
        
        df <- res$Block_1
        expect_equal(colnames(df), c("id", "product", "price"))
        expect_equal(nrow(df), 3)
        expect_equal(df$product[1], "Apple")
        expect_equal(df$price[3], 3000)
    } else {
        skip("External test data New.xlsx not found")
    }
})
