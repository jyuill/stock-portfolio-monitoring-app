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
  
  # Read the TD Holdings sheet starting from A3
  raw_data <- read_sheet(sheet_url, sheet = "TD Holdings", range = "A3:G1000")
  
  # Clean and process the data
  portfolio <- raw_data |>
    # Remove completely empty rows
    filter(if_any(everything(), ~ !is.na(.))) |>
    # Convert Date column to proper date format
    mutate(Date = as.Date(Date)) |>
    # Filter for the most recent date
    filter(Date == max(Date, na.rm = TRUE)) |>
    # Clean and standardize symbol names
    mutate(
      Symbol = str_trim(str_to_upper(Symbol)),
      Quantity = as.numeric(Quantity),
      Account = str_trim(Account)
    ) |>
    # Remove rows with missing symbols or zero quantity
    filter(!is.na(Symbol), !is.na(Quantity), Quantity > 0, Symbol != "") |>
    # Group by symbol and sum quantities across accounts
    group_by(Symbol) |>
    summarise(
      Total_Quantity = sum(Quantity, na.rm = TRUE),
      Accounts = paste(unique(Account), collapse = ", "),
      Date = first(Date),
      Market = first(Market),
      Description = first(Description),
      .groups = "drop"
    ) |>
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