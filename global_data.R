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

cat("=== Loading Stock Portfolio Data ===\n")

# Initialize Google Sheets authentication
cat("Authenticating with Google Sheets...\n")
tryCatch({
  gs4_auth(path = "creds/original-return-107905-3b03bf4c17bf.json")
  cat("✓ Google Sheets authentication successful!\n")
}, error = function(e) {
  cat("✗ Authentication failed:", e$message, "\n")
  stop("Cannot proceed without authentication")
})

# Load portfolio data from Google Sheets
cat("Loading portfolio data from Google Sheets...\n")
portfolio_data <- tryCatch({
  sheet_url <- "https://docs.google.com/spreadsheets/d/1oievySvQ3m2ojs1On27EKpZ4rqrbd0Ksi_rnQf8YMyY/edit?usp=sharing"

  # Add additional sources in this list using the same file URL.
  # Example: list(name = "My Other Holdings", range = "A5:E1000")
  holdings_sources <- list(
    list(name = "TD Holdings", range = "A5:G1000")
  )
  clean_env_value <- function(x) {
    value <- trimws(x)
    gsub("^['\"]|['\"]$", "", value)
  }

  extra_sheet_name <- clean_env_value(Sys.getenv("EXTRA_HOLDINGS_SHEET_NAME"))
  extra_sheet_range <- clean_env_value(Sys.getenv("EXTRA_HOLDINGS_SHEET_RANGE"))
  if (nzchar(extra_sheet_name) && nzchar(extra_sheet_range)) {
    holdings_sources[[length(holdings_sources) + 1]] <- list(
      name = extra_sheet_name,
      range = extra_sheet_range
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

  parse_sheet_holdings <- function(raw_data, source_name) {
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
        Quantity = as.numeric(Quantity),
        Average_Cost = as.numeric(Average_Cost)
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
      "| Rows retained:", nrow(filtered), "\n"
    )

    filtered
  }

  parsed_sources <- list()
  for (i in seq_along(holdings_sources)) {
    source_cfg <- holdings_sources[[i]]
    cat("  - Reading", source_cfg$name, "(", source_cfg$range, ")\n")
    raw_data <- read_sheet(
      sheet_url,
      sheet = source_cfg$name,
      range = source_cfg$range,
      col_types = "c"
    )
    parsed_sources[[i]] <- parse_sheet_holdings(raw_data, source_cfg$name)
  }

  combined_holdings <- bind_rows(parsed_sources)
  if (nrow(combined_holdings) == 0) {
    stop("No valid holdings rows found across configured sheet sources")
  }
  cat("Combined holdings rows across sources:", nrow(combined_holdings), "\n")

  portfolio <- combined_holdings |>
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
      .groups = "drop"
    ) |>
    mutate(Average_Cost = round(Average_Cost, 4)) |>
    arrange(Symbol)
  
  cat("✓ Portfolio data loaded successfully!\n")
  cat("  - Unique assets:", nrow(portfolio), "\n")
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
  symbols <- portfolio_data$Symbol
  
  # Calculate date ranges
  end_date <- Sys.Date()
  start_date <- end_date - years(2) # Get 2 years of data
  
  price_list <- list()
  total_symbols <- length(symbols)
  successful_symbols <- 0
  
  cat("Fetching data for", total_symbols, "symbols...\n")
  
  for (i in seq_along(symbols)) {
    symbol <- symbols[i]
    
    if (i %% 5 == 0 || i == total_symbols) {
      cat("  Progress:", i, "/", total_symbols, "symbols processed\n")
    }
    
    # Try different symbol formats if needed
    symbol_variants <- c(symbol, paste0(symbol, ".TO"), paste0(symbol, ".TSE"))
    
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
              Symbol = symbol
            ) |>
              filter(!is.na(Price)) |>
              arrange(Date)
            
            if (nrow(prices) > 0) {
              price_list[[symbol]] <- prices
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
      cat("  Warning: Could not fetch data for", symbol, "\n")
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
  
  current_date <- Sys.Date()
  performance_list <- list()
  symbols <- unique(price_data$Symbol)
  
  for (i in seq_along(symbols)) {
    symbol <- symbols[i]
    
    symbol_prices <- price_data |>
      filter(Symbol == symbol) |>
      arrange(Date)
    
    if (nrow(symbol_prices) > 0) {
      current_price <- tail(symbol_prices$Price, 1)
      
      performance_row <- data.frame(Symbol = symbol, Current_Price = current_price)
      
      for (period_name in names(periods)) {
        days_back <- periods[[period_name]]
        target_date <- current_date - days(days_back)
        
        # Find the closest available price to the target date
        historical_price <- symbol_prices |>
          filter(Date <= target_date) |>
          tail(1)
        
        if (nrow(historical_price) > 0) {
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
