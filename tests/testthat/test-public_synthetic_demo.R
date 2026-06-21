library(testthat)
library(TidyPanel)

test_that("public synthetic retail demo parses without internal auto-scale stage", {
    file_path <- test_path("testdata", "synthetic_retail_demo.csv")

    res <- read_messy_panel(file_path, return_audit = TRUE)

    expect_equal(colnames(res$data), c("store_id", "region", "revenue", "return_rate"))
    expect_equal(as.numeric(res$data$store_id), c(101, 102))
    expect_equal(res$data$revenue, c(1.2, 0.8))
    expect_equal(res$data$return_rate, c(0.035, 0.021))
})

test_that("public parser treats period labels as headers, not numeric rows", {
    raw <- data.frame(
        X1 = c("Metric", "Revenue", "Cost"),
        X2 = c("Week 0", "100", "80"),
        X3 = c("Week 12", "130", "90"),
        stringsAsFactors = FALSE
    )

    res <- read_messy_panel(raw)

    expect_equal(colnames(res), c("metric", "week_0", "week_12"))
    expect_equal(res$metric, c("Revenue", "Cost"))
    expect_equal(res$week_0, c(100, 80))
    expect_equal(res$week_12, c(130, 90))
})

test_that("public parser combines simple multi-sheet Excel workbooks", {
    skip_if_not_installed("writexl")

    tmp <- tempfile(fileext = ".xlsx")
    on.exit(unlink(tmp), add = TRUE)

    north <- data.frame(
        X1 = c("Metric", "Revenue", "Cost"),
        X2 = c("Value", "100", "70"),
        stringsAsFactors = FALSE
    )
    south <- data.frame(
        X1 = c("Metric", "Revenue", "Cost"),
        X2 = c("Value", "120", "80"),
        stringsAsFactors = FALSE
    )

    writexl::write_xlsx(list(North = north, South = south), tmp, col_names = FALSE)

    res <- read_messy_panel(tmp, sheet = "ALL")

    expect_equal(colnames(res), c("source_sheet_name", "metric", "value"))
    expect_equal(res$source_sheet_name, c("North", "North", "South", "South"))
    expect_equal(res$value, c(100, 70, 120, 80))
})

test_that("public parser handles a synthetic wide-year public indicator export", {
    raw <- data.frame(
        X1 = c("Data Source", "", "Country Name", "Northland", "Southport"),
        X2 = c("Demo public indicators", "", "Country Code", "NLD", "SPT"),
        X3 = c("", "", "Indicator Name", "Residents", "Residents"),
        X4 = c("", "", "Indicator Code", "POP.TOTAL", "POP.TOTAL"),
        X5 = c("", "", "2022", "1000", "2500"),
        X6 = c("", "", "2023", "1040", "2600"),
        stringsAsFactors = FALSE
    )

    res <- read_messy_panel(raw, auto_pivot = TRUE)

    expect_equal(colnames(res), c(
        "country_name", "country_code", "indicator_name", "indicator_code",
        "time_period", "value"
    ))
    expect_equal(nrow(res), 4)
    expect_equal(unique(res$country_code), c("NLD", "SPT"))
    expect_equal(unique(res$time_period), c("2022", "2023"))
    expect_equal(
        res$value[res$country_code == "SPT" & res$time_period == "2023"],
        2600
    )
})

test_that("public parser handles a synthetic daily weather export", {
    raw <- data.frame(
        date = c("2024-01-01", "2024-01-02", "2024-01-03"),
        precipitation = c("0.0", "4.2", "0.8"),
        temp_max = c("12.5", "10.1", "8.9"),
        temp_min = c("4.2", "2.1", "-1.3"),
        wind = c("3.1", "4.0", "2.7"),
        weather = c("sun", "rain", "snow"),
        stringsAsFactors = FALSE
    )

    res <- read_messy_panel(raw)

    expect_equal(nrow(res), 3)
    expect_equal(colnames(res), c(
        "date", "precipitation", "temp_max", "temp_min", "wind", "weather"
    ))
    expect_equal(res$date, c("2024-01-01", "2024-01-02", "2024-01-03"))
    expect_equal(res$temp_min, c(4.2, 2.1, -1.3))
    expect_equal(res$weather, c("sun", "rain", "snow"))
})

test_that("public parser preserves synthetic dated rows with missing measures", {
    raw <- data.frame(
        observation_date = c("2024-01-01", "2024-01-02", "2024-01-03"),
        index_value = c("10.1", "10.3", ""),
        stringsAsFactors = FALSE
    )

    res <- read_messy_panel(raw)

    expect_equal(nrow(res), 3)
    expect_equal(res$observation_date, c("2024-01-01", "2024-01-02", "2024-01-03"))
    expect_equal(res$index_value, c(10.1, 10.3, NA))
})

test_that("public parser skips explicit delimited metadata preambles", {
    tmp <- tempfile(fileext = ".csv")
    on.exit(unlink(tmp), add = TRUE)
    writeLines(
        c(
            "-BEGIN HEADER-",
            "Synthetic public metadata",
            "Location: Demo",
            "-END HEADER-",
            "year,mo,dy,value",
            "2024,1,1,10.5",
            "2024,1,2,11.0"
        ),
        tmp
    )

    res <- read_messy_panel(tmp, return_audit = TRUE)

    expect_equal(colnames(res$data), c("year", "mo", "dy", "value"))
    expect_equal(res$data$year, c(2024, 2024))
    expect_equal(res$data$mo, c(1, 1))
    expect_equal(res$data$dy, c(1, 2))
    expect_equal(res$data$value, c(10.5, 11.0))
    expect_equal(
        res$audit$Count[res$audit$Operation == "Delimited Preamble Rows Dropped"],
        "4"
    )
})

test_that("public parser skips blank-delimited metadata tables in synthetic exports", {
    tmp <- tempfile(fileext = ".csv")
    on.exit(unlink(tmp), add = TRUE)
    writeLines(
        c(
            "latitude,longitude,elevation",
            "10.0000,20.0000,100",
            "",
            "time,temperature_max,precipitation_sum,value_total",
            "2024-01-01,12.5,0.2,99",
            "2024-01-02,13.1,3.4,98"
        ),
        tmp
    )

    res <- read_messy_panel(tmp, return_audit = TRUE)

    expect_equal(colnames(res$data), c("time", "temperature_max", "precipitation_sum"))
    expect_equal(res$data$time, c("2024-01-01", "2024-01-02"))
    expect_equal(res$data$temperature_max, c(12.5, 13.1))
    expect_equal(res$data$precipitation_sum, c(0.2, 3.4))
    expect_equal(
        res$audit$Count[res$audit$Operation == "Delimited Preamble Rows Dropped"],
        "3"
    )
    expect_equal(
        res$audit$Count[res$audit$Operation == "Subtotal Columns Amputated"],
        "1"
    )
})

test_that("public parser skips synthetic RDB comments and field type rows", {
    tmp <- tempfile(fileext = ".rdb")
    on.exit(unlink(tmp), add = TRUE)
    writeLines(
        c(
            "# Synthetic comment",
            "# Another comment",
            "agency_cd\tsite_no\tdatetime\tvalue\tvalue_cd",
            "5s\t15s\t20d\t14n\t10s",
            "DEMO\t00001234\t2024-01-01\t12.5\tA",
            "DEMO\t00001234\t2024-01-02\t13.0\tA"
        ),
        tmp
    )

    res <- read_messy_panel(tmp, return_audit = TRUE)

    expect_equal(colnames(res$data), c("agency_cd", "site_no", "datetime", "value", "value_cd"))
    expect_equal(res$data$agency_cd, c("DEMO", "DEMO"))
    expect_equal(res$data$site_no, c("00001234", "00001234"))
    expect_equal(res$data$datetime, c("2024-01-01", "2024-01-02"))
    expect_equal(res$data$value, c(12.5, 13.0))
    expect_equal(
        res$audit$Count[res$audit$Operation == "Delimited Preamble Rows Dropped"],
        "2"
    )
    expect_equal(
        res$audit$Count[res$audit$Operation == "RDB Field Type Rows Dropped"],
        "1"
    )
})

test_that("public parser flattens a synthetic YAML record catalog", {
    file_path <- test_path("testdata", "public_languages_demo.yml")

    res <- read_messy_panel(file_path, return_audit = TRUE)

    expect_equal(nrow(res$data), 2)
    expect_equal(
        colnames(res$data),
        c("record_name", "type", "extensions", "aliases", "language_id", "searchable")
    )
    expect_equal(res$data$record_name, c("DemoScript", "DemoMarkup"))
    expect_equal(res$data$type, c("programming", "markup"))
    expect_equal(res$data$extensions, c(".demo, .dms", ".dmk"))
    expect_equal(res$data$aliases, c("demo-script, dscript", "demo-markup"))
    expect_equal(res$data$language_id, c(101, 102))
    expect_equal(res$data$searchable, c(TRUE, FALSE))
    expect_equal(
        res$audit$Count[res$audit$Operation == "YAML Records Parsed"],
        "2"
    )
    expect_equal(
        res$audit$Count[res$audit$Operation == "YAML Vector Fields Collapsed"],
        "2"
    )
})

test_that("public parser handles a synthetic monthly stock export", {
    raw <- data.frame(
        symbol = c("AAA", "AAA", "BBB", "BBB"),
        date = c("Jan 1 2024", "Feb 1 2024", "Jan 1 2024", "Feb 1 2024"),
        price = c("10.50", "11.25", "22.00", "21.75"),
        stringsAsFactors = FALSE
    )

    res <- read_messy_panel(raw)

    expect_equal(colnames(res), c("symbol", "date", "price"))
    expect_equal(res$symbol, c("AAA", "AAA", "BBB", "BBB"))
    expect_equal(res$date, c("Jan 1 2024", "Feb 1 2024", "Jan 1 2024", "Feb 1 2024"))
    expect_equal(res$price, c(10.50, 11.25, 22.00, 21.75))
})

test_that("public parser preserves camelCase signal in default name cleaning", {
    raw <- data.frame(
        storeName = c("Northland", "Southport"),
        reportYear = c("2022", "2023"),
        serviceScore = c("78.1", "80.4"),
        orderValueUSD = c("35500.25", "42100.50"),
        stringsAsFactors = FALSE
    )

    res <- read_messy_panel(raw)

    expect_equal(
        colnames(res),
        c("store_name", "report_year", "service_score", "order_value_usd")
    )
    expect_equal(res$service_score, c(78.1, 80.4))
    expect_equal(res$order_value_usd, c(35500.25, 42100.50))
})

test_that("public parser pivots synthetic slash-date wide columns", {
    raw <- data.frame(
        `Province/State` = c("North", "South", "South"),
        `Country/Region` = c("Demo A", "Demo A", "Demo B"),
        `1/22/20` = c("1", "2", "3"),
        `1/23/20` = c("4", "5", "6"),
        check.names = FALSE
    )

    res <- read_messy_panel(raw, auto_pivot = TRUE, return_audit = TRUE)

    expect_equal(
        colnames(res$data),
        c("province_state", "country_region", "time_period", "value")
    )
    expect_equal(nrow(res$data), 6)
    expect_equal(res$data$time_period, rep(c("1_22_20", "1_23_20"), each = 3))
    expect_equal(res$data$value, c(1, 2, 3, 4, 5, 6))
    expect_true(is.na(res$data$province_state[res$data$country_region == "Demo B"][1]))
    expect_equal(
        res$audit$Count[res$audit$Operation == "Auto-Pivot Wide to Long"],
        "2"
    )
})

test_that("public parser drops synthetic exported row-index columns", {
    raw <- data.frame(
        row_names = 1:4,
        branch = c("North", "South", "East", "West"),
        service_score = c("91", "", "88", "94"),
        stringsAsFactors = FALSE
    )

    res <- read_messy_panel(raw, return_audit = TRUE)

    expect_equal(colnames(res$data), c("branch", "service_score"))
    expect_equal(res$data$branch, c("North", "South", "East", "West"))
    expect_equal(res$data$service_score, c(91, NA, 88, 94))
    expect_equal(
        res$audit$Count[res$audit$Operation == "Export Index Column Dropped"],
        "row_names"
    )
})

test_that("public cleaner standardizes synthetic event-feed column names", {
    raw <- data.frame(a = 1, b = 2, c = 3, d = 4, e = 5, check.names = FALSE)
    colnames(raw) <- c(
        "eventTime", "updatedAt", "eventPlace", "magType", "horizontalError"
    )

    res <- clean_variable_names(raw)

    expect_equal(
        colnames(res),
        c("event_time", "updated_at", "event_place", "mag_type", "horizontal_error")
    )
})

test_that("public parser keeps compact uppercase measurement code columns", {
    raw <- data.frame(
        STATION = c("S1", "S1"),
        DATE = c("2024-01-01", "2024-01-02"),
        TAVG = c("1.2", "2.3"),
        TMAX = c("4.5", "5.6"),
        stringsAsFactors = FALSE
    )

    res <- read_messy_panel(raw)

    expect_true(all(c("tavg", "tmax") %in% colnames(res)))
    expect_equal(res$tavg, c(1.2, 2.3))
    expect_equal(res$tmax, c(4.5, 5.6))
})

test_that("public parser preserves postal codes and clock-time strings", {
    raw <- data.frame(
        zip_code = c("02139", "", "10002"),
        event_time = c("9:10", "13:08", "0:25"),
        count = c("1", "2", "3"),
        stringsAsFactors = FALSE
    )

    res <- read_messy_panel(raw)

    expect_equal(res$zip_code, c("02139", NA, "10002"))
    expect_equal(res$event_time, c("9:10", "13:08", "0:25"))
    expect_equal(res$count, c(1, 2, 3))
})

test_that("public parser preserves synthetic geographic code columns", {
    raw <- data.frame(
        state = c("North", "South", "West"),
        geo_id = c("01001", "", "06059"),
        count = c("1", "2", "3"),
        stringsAsFactors = FALSE
    )

    res <- read_messy_panel(raw)

    expect_equal(colnames(res), c("state", "geo_id", "count"))
    expect_equal(res$state, c("North", "South", "West"))
    expect_equal(res$geo_id, c("01001", NA, "06059"))
    expect_equal(res$count, c(1, 2, 3))
})

test_that("public parser preserves synthetic compact period codes", {
    raw <- data.frame(
        period_code = c("01", "02", "12"),
        label = c("Opening", "Midpoint", "Closing"),
        value = c("10.5", "11.0", "13.2"),
        stringsAsFactors = FALSE
    )

    res <- read_messy_panel(raw)

    expect_equal(colnames(res), c("period_code", "label", "value"))
    expect_equal(res$period_code, c("01", "02", "12"))
    expect_equal(res$value, c(10.5, 11.0, 13.2))
})

test_that("public parser preserves synthetic time-format duration codes", {
    raw <- data.frame(
        time_format = c("P1D", "P1D", "P1M"),
        value = c("1.2", "1.3", "1.4"),
        stringsAsFactors = FALSE
    )

    res <- read_messy_panel(raw)

    expect_equal(res$time_format, c("P1D", "P1D", "P1M"))
    expect_equal(res$value, c(1.2, 1.3, 1.4))
})

test_that("public parser does not treat spaced statistical flags as multipliers", {
    raw <- data.frame(
        metric = c("Population", "Population"),
        value = c("5598920 b", "5635601 b"),
        stringsAsFactors = FALSE
    )

    res <- read_messy_panel(raw)

    expect_equal(colnames(res), c("metric", "value"))
    expect_equal(res$value, c(5598920, 5635601))
})
