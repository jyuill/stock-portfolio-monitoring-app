# Global Data Loading for Stock Portfolio Monitor
# This file runs once when the app starts and loads all necessary data

# Load required libraries
library(shiny)
library(shinydashboard)
library(DT)
library(plotly)
library(dplyr)
library(ggplot2)
library(googlesheets4)
library(quantmod)
library(lubridate)
library(stringr)
library(tidyr)
library(shinymanager)
library(sodium)

cat("=== Loading Stock Portfolio Data ===\n")

auth_google_sheets <- function() {
  # Preferred for deployment: store full JSON key contents in env var.
  sa_json <- trimws(Sys.getenv("GS4_SERVICE_ACCOUNT_JSON", ""))
  # Local fallback path for development.
  sa_path <- trimws(Sys.getenv(
    "GS4_SERVICE_ACCOUNT_PATH",
    "creds/original-return-107905-3b03bf4c17bf.json"
  ))

  if (nzchar(sa_json)) {
    temp_auth_file <- tempfile(fileext = ".json")
    writeLines(sa_json, temp_auth_file, useBytes = TRUE)
    on.exit(unlink(temp_auth_file), add = TRUE)
    gs4_auth(path = temp_auth_file)
    return(invisible(TRUE))
  }

  if (file.exists(sa_path)) {
    gs4_auth(path = sa_path)
    return(invisible(TRUE))
  }

  stop(
    "Google Sheets credentials not found. Set GS4_SERVICE_ACCOUNT_JSON ",
    "or provide a credentials file at GS4_SERVICE_ACCOUNT_PATH."
  )
}

# Initialize Google Sheets authentication
cat("Authenticating with Google Sheets...\n")
tryCatch({
  auth_google_sheets()
  cat("✓ Google Sheets authentication successful!\n")
}, error = function(e) {
  cat("✗ Authentication failed:", e$message, "\n")
  stop("Cannot proceed without authentication")
})

# Load portfolio data from Google Sheets
cat("Loading portfolio data from Google Sheets...\n")
portfolio_data <- tryCatch({
  sheet_url <- "https://docs.google.com/spreadsheets/d/1oievySvQ3m2ojs1On27EKpZ4rqrbd0Ksi_rnQf8YMyY/edit?usp=sharing"

  # Default holdings sources (hard-coded, non-sensitive config).
  holdings_sources <- list(
    list(name = "TD Holdings", range = "A5:G1000"),
    list(name = "Quest Holdings", range = "A5:J1000")
  )
  clean_env_value <- function(x) {
    value <- trimws(x)
    gsub("^['\"]|['\"]$", "", value)
  }

  extra_sheet_name <- clean_env_value(Sys.getenv("EXTRA_HOLDINGS_SHEET_NAME"))
  extra_sheet_range <- clean_env_value(Sys.getenv("EXTRA_HOLDINGS_SHEET_RANGE"))
  if (nzchar(extra_sheet_name) && nzchar(extra_sheet_range)) {
    # Env vars override the default extra sheet config.
    holdings_sources[[2]] <- list(name = extra_sheet_name, range = extra_sheet_range)
  } else if (xor(nzchar(extra_sheet_name), nzchar(extra_sheet_range))) {
    warning(
      "Only one of EXTRA_HOLDINGS_SHEET_NAME / EXTRA_HOLDINGS_SHEET_RANGE is set. ",
      "Using hard-coded default extra sheet."
    )
  }
  cat("Configured holdings sources:", length(holdings_sources), "\n")
  for (cfg in holdings_sources) {
    cat("  *", cfg$name, "|", cfg$range, "\n")
  }

  find_column_name <- function(data, candidates) {
    normalized_names <- gsub("[^a-z0-9]+", "", tolower(names(data)))
    for (candidate in candidates) {
      idx <- which(normalized_names == candidate)
      if (length(idx) > 0) return(names(data)[idx[1]])
    }
    return(NA_character_)
  }

  load_holdings_meta <- function() {
    meta_cfg <- list(name = "Holdings meta", range = "B8:H100")
    cat("Loading holdings metadata from", meta_cfg$name, "(", meta_cfg$range, ")\n")

    raw_meta <- read_sheet(
      sheet_url,
      sheet = meta_cfg$name,
      range = meta_cfg$range,
      col_types = "c"
    )

    if (nrow(raw_meta) == 0) {
      cat("  - Holdings meta is empty\n")
      return(data.frame(
        Source_Symbol = character(),
        Yahoo_Symbol = character(),
        Sector = character(),
        Cost_Currency = character(),
        Price_Currency = character(),
        Manual_Price = numeric(),
        Geo = character(),
        Objective = character(),
        stringsAsFactors = FALSE
      ))
    }

    source_col <- find_column_name(raw_meta, "sourcesymbol")
    yahoo_col <- find_column_name(raw_meta, "yahoosymbol")
    sector_col <- find_column_name(raw_meta, "sector")
    currency_col <- find_column_name(raw_meta, "currency")
    manual_price_col <- find_column_name(raw_meta, "manualprice")
    geo_col <- find_column_name(raw_meta, "geo")
    objective_col <- find_column_name(raw_meta, "objective")

    if (is.na(source_col)) {
      stop("Holdings meta sheet is missing required column: source_symbol")
    }

    selected_cols <- c(source_col, yahoo_col, sector_col, currency_col, manual_price_col, geo_col, objective_col)
    selected_cols <- selected_cols[!is.na(selected_cols)]
    rename_map <- c(Source_Symbol = source_col)
    if (!is.na(yahoo_col)) rename_map <- c(rename_map, Yahoo_Symbol = yahoo_col)
    if (!is.na(sector_col)) rename_map <- c(rename_map, Sector = sector_col)
    if (!is.na(currency_col)) rename_map <- c(rename_map, Currency = currency_col)
    if (!is.na(manual_price_col)) rename_map <- c(rename_map, Manual_Price = manual_price_col)
    if (!is.na(geo_col)) rename_map <- c(rename_map, Geo = geo_col)
    if (!is.na(objective_col)) rename_map <- c(rename_map, Objective = objective_col)

    meta <- raw_meta |>
      select(any_of(selected_cols)) |>
      rename(!!!rename_map)

    if (!"Yahoo_Symbol" %in% names(meta)) {
      meta$Yahoo_Symbol <- NA_character_
    }
    if (!"Sector" %in% names(meta)) {
      meta$Sector <- NA_character_
    }
    if (!"Currency" %in% names(meta)) {
      meta$Currency <- NA_character_
    }
    if (!"Manual_Price" %in% names(meta)) {
      meta$Manual_Price <- NA_real_
    }
    if (!"Geo" %in% names(meta)) {
      meta$Geo <- NA_character_
    }
    if (!"Objective" %in% names(meta)) {
      meta$Objective <- NA_character_
    }

    normalize_currency <- function(x) {
      x <- str_to_upper(str_trim(as.character(x)))
      if (is.na(x) || x == "") return(NA_character_)
      if (x %in% c("CAD", "CA")) return("CAD")
      if (x %in% c("USD", "US")) return("USD")
      NA_character_
    }

    parse_currency_pair <- function(code) {
      clean_code <- gsub("[^A-Za-z]", "", str_to_upper(str_trim(as.character(code))))
      if (is.na(clean_code) || clean_code == "") {
        return(c("CAD", "CAD"))
      }

      n <- nchar(clean_code)
      cost_curr <- NA_character_
      price_curr <- NA_character_

      if (n >= 6) {
        cost_curr <- normalize_currency(substr(clean_code, 1, 3))
        price_curr <- normalize_currency(substr(clean_code, n - 2, n))
      }
      if (is.na(cost_curr) || is.na(price_curr)) {
        if (n >= 4) {
          cost_curr <- normalize_currency(substr(clean_code, 1, 2))
          price_curr <- normalize_currency(substr(clean_code, n - 1, n))
        }
      }

      c(
        ifelse(is.na(cost_curr), "CAD", cost_curr),
        ifelse(is.na(price_curr), "CAD", price_curr)
      )
    }

    parse_meta_numeric <- function(x) {
      x_chr <- as.character(x)
      is_paren_negative <- grepl("^\\s*\\(.*\\)\\s*$", x_chr)

      cleaned <- x_chr
      cleaned <- gsub(",", "", cleaned)
      cleaned <- gsub("[()]", "", cleaned)
      cleaned <- gsub("[^0-9.\\-]", "", cleaned)
      cleaned[cleaned == ""] <- NA_character_

      out <- suppressWarnings(as.numeric(cleaned))
      out[is_paren_negative & !is.na(out)] <- -out[is_paren_negative & !is.na(out)]
      out
    }

    meta <- meta |>
      mutate(
        Source_Symbol = str_trim(str_to_upper(as.character(Source_Symbol))),
        Yahoo_Symbol = str_trim(str_to_upper(as.character(Yahoo_Symbol))),
        Sector = str_trim(as.character(Sector)),
        Currency = str_trim(str_to_upper(as.character(Currency))),
        Manual_Price = parse_meta_numeric(Manual_Price),
        Geo = str_trim(str_to_upper(as.character(Geo))),
        Objective = str_trim(as.character(Objective))
      ) |>
      filter(!is.na(Source_Symbol), Source_Symbol != "") |>
      mutate(
        Yahoo_Symbol = ifelse(Yahoo_Symbol == "", NA_character_, Yahoo_Symbol),
        Sector = ifelse(Sector == "", NA_character_, Sector),
        Currency = ifelse(Currency == "", NA_character_, Currency),
        Geo = ifelse(Geo == "", NA_character_, Geo),
        Objective = ifelse(Objective == "", NA_character_, Objective)
      ) |>
      rowwise() |>
      mutate(
        Cost_Currency = parse_currency_pair(Currency)[1],
        Price_Currency = parse_currency_pair(Currency)[2]
      ) |>
      ungroup() |>
      group_by(Source_Symbol) |>
      summarise(
        Yahoo_Symbol = {
          vals <- na.omit(Yahoo_Symbol)
          if (length(vals) > 0) vals[1] else NA_character_
        },
        Sector = {
          vals <- na.omit(Sector)
          if (length(vals) > 0) vals[1] else NA_character_
        },
        Cost_Currency = {
          vals <- na.omit(Cost_Currency)
          if (length(vals) > 0) vals[1] else "CAD"
        },
        Price_Currency = {
          vals <- na.omit(Price_Currency)
          if (length(vals) > 0) vals[1] else "CAD"
        },
        Manual_Price = {
          vals <- na.omit(Manual_Price)
          if (length(vals) > 0) vals[1] else NA_real_
        },
        Geo = {
          vals <- na.omit(Geo)
          if (length(vals) > 0) vals[1] else NA_character_
        },
        Objective = {
          vals <- na.omit(Objective)
          if (length(vals) > 0) vals[1] else NA_character_
        },
        .groups = "drop"
      )

    cat(
      "  - Holdings meta rows:", nrow(meta),
      "| Symbol overrides:", sum(!is.na(meta$Yahoo_Symbol) & meta$Yahoo_Symbol != meta$Source_Symbol), "\n"
    )

    meta
  }

  holdings_meta <- load_holdings_meta()

  parse_sheet_holdings <- function(raw_data, source_name) {
    parse_numeric_value <- function(x) {
      x_chr <- as.character(x)
      is_paren_negative <- grepl("^\\s*\\(.*\\)\\s*$", x_chr)

      cleaned <- x_chr
      cleaned <- gsub(",", "", cleaned)
      cleaned <- gsub("[()]", "", cleaned)
      cleaned <- gsub("[^0-9.\\-]", "", cleaned)
      cleaned[cleaned == ""] <- NA_character_

      out <- suppressWarnings(as.numeric(cleaned))
      out[is_paren_negative & !is.na(out)] <- -out[is_paren_negative & !is.na(out)]
      out
    }

    required_map <- c(
      Date = "date",
      Account = "account",
      Symbol = "symbol",
      Quantity = "quantity"
    )
    optional_map <- c(
      Average_Cost = "averagecost"
    )

    selected_names <- c(
      sapply(required_map, function(x) find_column_name(raw_data, x)),
      sapply(optional_map, function(x) find_column_name(raw_data, x))
    )

    missing_required <- names(required_map)[is.na(selected_names[names(required_map)])]
    if (length(missing_required) > 0) {
      stop(
        paste0(
          "Sheet '", source_name, "' is missing required column(s): ",
          paste(missing_required, collapse = ", ")
        )
      )
    }

    selected_existing <- selected_names[!is.na(selected_names)]
    rename_map <- stats::setNames(unname(selected_existing), names(selected_existing))

    cat("    Rows read from", source_name, ":", nrow(raw_data), "\n")

    cleaned <- raw_data |>
      select(any_of(unname(selected_existing))) |>
      rename(!!!rename_map)

    if (!"Average_Cost" %in% names(cleaned)) {
      cleaned$Average_Cost <- NA_real_
    }

    cleaned <- cleaned |>
      filter(if_any(everything(), ~ !is.na(.))) |>
      mutate(
        Date = as.Date(Date),
        Symbol = str_trim(str_to_upper(as.character(Symbol))),
        Account = str_trim(as.character(Account)),
        Quantity = parse_numeric_value(Quantity),
        Average_Cost = parse_numeric_value(Average_Cost)
      ) |>
      filter(!is.na(Date), !is.na(Symbol), Symbol != "", !is.na(Quantity), Quantity > 0)

    if (nrow(cleaned) == 0) {
      cat("    Rows retained after cleaning", source_name, ": 0\n")
      return(cleaned)
    }

    # Keep the most recent snapshot for each source sheet.
    latest_date <- max(cleaned$Date, na.rm = TRUE)
    filtered <- cleaned |>
      filter(Date == latest_date)

    cat(
      "    Latest date in", source_name, ":", format(latest_date, "%Y-%m-%d"),
      "| Rows retained:", nrow(filtered),
      "| Rows with Average_Cost:", sum(!is.na(filtered$Average_Cost)), "\n"
    )

    filtered
  }

  parsed_sources <- list()
  source_summaries <- list()
  for (i in seq_along(holdings_sources)) {
    source_cfg <- holdings_sources[[i]]
    cat("  - Reading", source_cfg$name, "(", source_cfg$range, ")\n")
    raw_data <- read_sheet(
      sheet_url,
      sheet = source_cfg$name,
      range = source_cfg$range,
      col_types = "c"
    )
    parsed_source <- parse_sheet_holdings(raw_data, source_cfg$name)
    parsed_sources[[i]] <- parsed_source
    source_summaries[[i]] <- data.frame(
      Source = source_cfg$name,
      Range = source_cfg$range,
      Rows_Read = nrow(raw_data),
      Rows_Latest = nrow(parsed_source),
      Latest_Date = if (nrow(parsed_source) > 0) max(parsed_source$Date, na.rm = TRUE) else as.Date(NA),
      Missing_Average_Cost = sum(is.na(parsed_source$Average_Cost)),
      stringsAsFactors = FALSE
    )
  }

  holdings_source_summary <- bind_rows(source_summaries)
  cat("Source validation summary:\n")
  print(holdings_source_summary)

  combined_holdings <- bind_rows(parsed_sources)
  if (nrow(combined_holdings) == 0) {
    stop("No valid holdings rows found across configured sheet sources")
  }
  cat("Combined holdings rows across sources:", nrow(combined_holdings), "\n")

  raw_positions <- combined_holdings |>
    group_by(Symbol, Account) |>
    summarise(
      Quantity = sum(Quantity, na.rm = TRUE),
      Date = max(Date, na.rm = TRUE),
      Average_Cost = ifelse(
        sum(ifelse(is.na(Average_Cost), 0, Quantity), na.rm = TRUE) > 0,
        sum(Quantity * Average_Cost, na.rm = TRUE) /
          sum(ifelse(is.na(Average_Cost), 0, Quantity), na.rm = TRUE),
        NA_real_
      ),
      .groups = "drop"
    ) |>
    left_join(holdings_meta, by = c("Symbol" = "Source_Symbol")) |>
    mutate(
      Yahoo_Symbol = coalesce(Yahoo_Symbol, Symbol),
      Sector = ifelse(is.na(Sector) | Sector == "", "Unclassified", Sector),
      Cost_Currency = coalesce(Cost_Currency, "CAD"),
      Price_Currency = coalesce(Price_Currency, "CAD"),
      Manual_Price = as.numeric(Manual_Price),
      Geo = ifelse(is.na(Geo) | Geo == "", "Unspecified", Geo),
      Objective = ifelse(is.na(Objective) | Objective == "", "Unspecified", Objective),
      Average_Cost = round(Average_Cost, 4)
    )

  excluded_symbols <- raw_positions |>
    filter(!is.na(Manual_Price), Manual_Price == 0) |>
    distinct(Symbol) |>
    pull(Symbol)

  if (length(excluded_symbols) > 0) {
    cat(
      "  - Excluding symbols with Manual_Price = 0:",
      paste(excluded_symbols, collapse = ", "),
      "\n"
    )
  }

  portfolio_positions <- raw_positions |>
    filter(is.na(Manual_Price) | Manual_Price != 0) |>
    arrange(Symbol, Account)

  if (nrow(portfolio_positions) == 0) {
    stop("All holdings were excluded (manual price set to 0).")
  }

  portfolio <- portfolio_positions |>
    group_by(Symbol) |>
    summarise(
      Total_Quantity = sum(Quantity, na.rm = TRUE),
      Accounts = paste(sort(unique(Account)), collapse = ", "),
      Date = max(Date, na.rm = TRUE),
      Average_Cost = ifelse(
        sum(ifelse(is.na(Average_Cost), 0, Quantity), na.rm = TRUE) > 0,
        sum(Quantity * Average_Cost, na.rm = TRUE) /
          sum(ifelse(is.na(Average_Cost), 0, Quantity), na.rm = TRUE),
        NA_real_
      ),
      Yahoo_Symbol = first(Yahoo_Symbol),
      Sector = first(Sector),
      Cost_Currency = first(Cost_Currency),
      Price_Currency = first(Price_Currency),
      Manual_Price = first(Manual_Price),
      Geo = first(Geo),
      Objective = first(Objective),
      .groups = "drop"
    ) |>
    mutate(Average_Cost = round(Average_Cost, 4)) |>
    arrange(Symbol)
  
  cat("✓ Portfolio data loaded successfully!\n")
  cat("  - Unique assets:", nrow(portfolio), "\n")
  cat("  - Account positions:", nrow(portfolio_positions), "\n")
  cat("  - Data from:", format(first(portfolio$Date), "%Y-%m-%d"), "\n")
  cat("  - Symbols:", paste(head(portfolio$Symbol, 10), collapse = ", "), 
      if(nrow(portfolio) > 10) "..." else "", "\n")
  
  portfolio
  
}, error = function(e) {
  cat("✗ Error loading portfolio data:", e$message, "\n")
  stop("Cannot proceed without portfolio data")
})

# Fetch price data for all symbols
cat("\nFetching price data from Yahoo Finance...\n")
price_data <- tryCatch({
  symbol_map <- portfolio_data |>
    select(Symbol, Yahoo_Symbol, Average_Cost, Manual_Price)

  parse_quote_numeric <- function(x) {
    x_chr <- as.character(x)
    x_chr <- gsub(",", "", x_chr)
    x_chr <- gsub("[()]", "", x_chr)
    x_chr <- gsub("[^0-9.\\-]", "", x_chr)
    suppressWarnings(as.numeric(x_chr))
  }

  fetch_live_quote_price <- function(ticker) {
    quote_data <- tryCatch(
      getQuote(ticker, src = "yahoo"),
      error = function(e) NULL
    )
    if (is.null(quote_data) || nrow(quote_data) == 0) return(NA_real_)

    # Prefer explicit last/market price fields; avoid taking arbitrary numeric fields.
    quote_row <- quote_data[1, , drop = FALSE]
    quote_cols <- names(quote_row)
    quote_cols_lower <- tolower(quote_cols)

    preferred_patterns <- c(
      "^last trade \\(price only\\)$",
      "^last$",
      "regularmarketprice",
      "^price$",
      "last trade"
    )

    for (pattern in preferred_patterns) {
      matches <- which(grepl(pattern, quote_cols_lower))
      if (length(matches) == 0) next
      for (idx in matches) {
        val <- parse_quote_numeric(quote_row[[idx]][1])
        if (!is.na(val) && val > 0) return(val)
      }
    }

    NA_real_
  }

  mutual_fund_fallback_tickers <- list(
    TDB905 = c("TDB905.CF", "TDB905.TO", "TDB905"),
    TDB3085 = c("TDB3085.CF", "TDB3085.TO", "TDB3085")
  )
  manual_only_symbols <- c("GIC")

  fetch_mutual_fund_fallback <- function(source_symbol, avg_cost, manual_price = NA_real_) {
    candidate_tickers <- mutual_fund_fallback_tickers[[source_symbol]]
    if (!is.null(candidate_tickers)) {
      for (ticker in candidate_tickers) {
        quote_data <- tryCatch(
          getQuote(ticker, src = "yahoo"),
          error = function(e) NULL
        )
        if (is.null(quote_data) || nrow(quote_data) == 0) next

        numeric_candidates <- suppressWarnings(unlist(lapply(quote_data[1, ], parse_quote_numeric)))
        numeric_candidates <- numeric_candidates[!is.na(numeric_candidates) & numeric_candidates > 0]
        if (length(numeric_candidates) == 0) next

        price_val <- numeric_candidates[1]
        cat("  Fallback quote success for", source_symbol, "using", ticker, "at", round(price_val, 4), "\n")
        return(data.frame(
          Date = Sys.Date(),
          Price = as.numeric(price_val),
          Symbol = source_symbol
        ))
      }
    }

    if (!is.na(manual_price) && manual_price > 0) {
      cat("  Fallback to Manual_Price for", source_symbol, "at", round(manual_price, 4), "\n")
      return(data.frame(
        Date = Sys.Date(),
        Price = as.numeric(manual_price),
        Symbol = source_symbol
      ))
    }

    if (!is.na(avg_cost) && avg_cost > 0) {
      cat("  Fallback to Average_Cost for", source_symbol, "at", round(avg_cost, 4), "\n")
      return(data.frame(
        Date = Sys.Date(),
        Price = as.numeric(avg_cost),
        Symbol = source_symbol
      ))
    }

    NULL
  }

  build_symbol_variants <- function(source_symbol, yahoo_symbol) {
    seeds <- unique(c(yahoo_symbol, source_symbol))
    variants <- c()
    for (seed in seeds) {
      seed <- str_trim(str_to_upper(as.character(seed)))
      if (!nzchar(seed)) next
      dash_seed <- gsub("\\.", "-", seed)
      variants <- c(
        variants,
        seed,
        paste0(seed, ".TO"),
        paste0(seed, ".TSE"),
        dash_seed,
        paste0(dash_seed, ".TO"),
        paste0(dash_seed, ".TSE")
      )
    }
    unique(variants)
  }
  
  # Calculate date ranges
  end_date <- Sys.Date()
  start_date <- end_date - years(2) # Get 2 years of data
  
  price_list <- list()
  total_symbols <- nrow(symbol_map)
  successful_symbols <- 0
  
  cat("Fetching data for", total_symbols, "symbols...\n")
  
  for (i in seq_len(total_symbols)) {
    source_symbol <- symbol_map$Symbol[i]
    yahoo_symbol <- symbol_map$Yahoo_Symbol[i]
    average_cost <- symbol_map$Average_Cost[i]
    manual_price <- symbol_map$Manual_Price[i]
    
    if (i %% 5 == 0 || i == total_symbols) {
      cat("  Progress:", i, "/", total_symbols, "symbols processed\n")
    }

    if (source_symbol %in% manual_only_symbols && !is.na(manual_price) && manual_price > 0) {
      cat("  Manual-only price for", source_symbol, "at", round(manual_price, 4), "\n")
      price_list[[source_symbol]] <- data.frame(
        Date = Sys.Date(),
        Price = as.numeric(manual_price),
        Symbol = source_symbol
      )
      successful_symbols <- successful_symbols + 1
      next
    }
    
    # Try different symbol formats if needed
    symbol_variants <- build_symbol_variants(source_symbol, yahoo_symbol)
    
    success <- FALSE
    for (variant in symbol_variants) {
      tryCatch({
        price_data_raw <- getSymbols(variant, 
                                   src = "yahoo",
                                   from = start_date,
                                   to = end_date,
                                   auto.assign = FALSE,
                                   warnings = FALSE)
        
        if (!is.null(price_data_raw) && nrow(price_data_raw) > 0) {
          # Extract adjusted close prices
          adj_close_col <- paste0(variant, ".Adjusted")
          if (!adj_close_col %in% colnames(price_data_raw)) {
            adj_close_col <- paste0(variant, ".Close")
          }
          
          if (adj_close_col %in% colnames(price_data_raw)) {
            prices <- data.frame(
              Date = index(price_data_raw),
              Price = as.numeric(price_data_raw[, adj_close_col]),
              Symbol = source_symbol
            ) |>
              filter(!is.na(Price)) |>
              arrange(Date)
            
            if (nrow(prices) > 0) {
              # Overlay live quote when available:
              # - during market hours: append today's quote row
              # - after close: replace same-day row with latest quote if needed
              live_price <- fetch_live_quote_price(variant)
              if (!is.na(live_price) && live_price > 0) {
                latest_hist_date <- max(prices$Date, na.rm = TRUE)
                if (Sys.Date() > latest_hist_date) {
                  prices <- bind_rows(
                    prices,
                    data.frame(Date = Sys.Date(), Price = live_price, Symbol = source_symbol)
                  ) |>
                    arrange(Date)
                } else if (Sys.Date() == latest_hist_date) {
                  prices$Price[nrow(prices)] <- live_price
                }
              }

              price_list[[source_symbol]] <- prices
              successful_symbols <- successful_symbols + 1
              success <- TRUE
              break
            }
          }
        }
      }, error = function(e) {
        # Silent error, try next variant
      })
    }
    
    if (!success) {
      fallback_prices <- fetch_mutual_fund_fallback(source_symbol, average_cost, manual_price)
      if (!is.null(fallback_prices) && nrow(fallback_prices) > 0) {
        price_list[[source_symbol]] <- fallback_prices
        successful_symbols <- successful_symbols + 1
        success <- TRUE
      }
    }

    if (!success) {
      cat("  Warning: Could not fetch data for", source_symbol, "(tried", paste(symbol_variants, collapse = ", "), ")\n")
    }
  }
  
  if (length(price_list) > 0) {
    combined_prices <- bind_rows(price_list)
    cat("✓ Price data fetched successfully!\n")
    cat("  - Successfully fetched:", successful_symbols, "/", total_symbols, "symbols\n")
    cat("  - Price records:", nrow(combined_prices), "\n")
    combined_prices
  } else {
    cat("✗ No price data could be fetched\n")
    stop("Cannot proceed without price data")
  }
  
}, error = function(e) {
  cat("✗ Error fetching price data:", e$message, "\n")
  stop("Cannot proceed without price data")
})

# Calculate performance metrics
cat("\nCalculating performance metrics...\n")
performance_data <- tryCatch({
  # Define time periods
  periods <- list(
    "1d" = 1,
    "7d" = 7,
    "30d" = 30,
    "90d" = 90,
    "6m" = 180,
    "1y" = 365
  )
  
  performance_list <- list()
  symbols <- unique(price_data$Symbol)
  
  for (i in seq_along(symbols)) {
    symbol <- symbols[i]
    
    symbol_prices <- price_data |>
      filter(Symbol == symbol) |>
      arrange(Date)
    
    if (nrow(symbol_prices) > 0) {
      latest_price_date <- max(symbol_prices$Date, na.rm = TRUE)
      current_price <- tail(symbol_prices$Price, 1)
      
      performance_row <- data.frame(Symbol = symbol, Current_Price = current_price)
      
      for (period_name in names(periods)) {
        historical_price <- NULL

        if (period_name == "1d") {
          # 1-day change should use the previous available trading row.
          if (nrow(symbol_prices) >= 2) {
            historical_price <- symbol_prices |>
              head(-1) |>
              tail(1)
          }
        } else {
          days_back <- periods[[period_name]]
          target_date <- latest_price_date - days(days_back)
          
          # Find the closest available historical price on/before target date.
          historical_price <- symbol_prices |>
            filter(Date <= target_date) |>
            tail(1)
        }
        
        if (!is.null(historical_price) && nrow(historical_price) > 0) {
          pct_change <- ((current_price - historical_price$Price) / historical_price$Price) * 100
          performance_row[[period_name]] <- round(pct_change, 2)
        } else {
          performance_row[[period_name]] <- NA
        }
      }
      
      performance_list[[symbol]] <- performance_row
    }
  }
  
  if (length(performance_list) > 0) {
    performance_df <- bind_rows(performance_list)
    cat("✓ Performance metrics calculated!\n")
    cat("  - Performance data for", nrow(performance_df), "symbols\n")
    performance_df
  } else {
    stop("No performance data could be calculated")
  }
  
}, error = function(e) {
  cat("✗ Error calculating performance:", e$message, "\n")
  stop("Cannot proceed without performance data")
})

# Create a summary for display
data_summary <- list(
  portfolio_count = nrow(portfolio_data),
  portfolio_date = format(first(portfolio_data$Date), "%Y-%m-%d"),
  symbols_with_prices = nrow(performance_data),
  total_symbols = nrow(portfolio_data),
  data_loaded_at = Sys.time()
)

cat("\n=== Data Loading Complete ===\n")
cat("Portfolio assets:", data_summary$portfolio_count, "\n")
cat("Assets with price data:", data_summary$symbols_with_prices, "\n")
cat("Data loaded at:", format(data_summary$data_loaded_at, "%Y-%m-%d %H:%M:%S"), "\n")
cat("Ready to launch Shiny app!\n")
