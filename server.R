library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(googlesheets4)
library(quantmod)
library(lubridate)
library(stringr)

server <- function(input, output, session) {
  
  # Initialize Google Sheets authentication with JSON credentials
  observe({
    tryCatch({
      # Use service account authentication with the actual JSON file
      gs4_auth(path = "creds/original-return-107905-3b03bf4c17bf.json")
      showNotification("Google Sheets authentication successful!", type = "success")
    }, error = function(e) {
      showNotification(paste("Authentication failed:", e$message), type = "error")
    })
  })
  
  # Load portfolio data from Google Sheets
  portfolio_data <- reactive({
    req(input$refresh_data)
    
    withProgress(message = 'Loading portfolio data...', value = 0, {
      tryCatch({
        # Read the specific Google Sheet
        sheet_url <- "https://docs.google.com/spreadsheets/d/1oievySvQ3m2ojs1On27EKpZ4rqrbd0Ksi_rnQf8YMyY/edit?usp=sharing"
        
        incProgress(0.3, detail = "Connecting to Google Sheets...")
        
        # Read the TD Holdings sheet starting from A3 (skipping header rows)
        raw_data <- read_sheet(sheet_url, sheet = "TD Holdings", range = "A3:G1000", col_names = TRUE)
        
        incProgress(0.5, detail = "Processing data...")
        
        # Clean column names and filter for most recent date
        portfolio <- raw_data |>
          # Remove completely empty rows
          filter(!if_all(everything(), is.na)) |>
          # Convert Date column to proper date format
          mutate(Date = as.Date(Date)) |>
          # Remove rows where Date is NA
          filter(!is.na(Date)) |>
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
            Accounts = paste(unique(Account[!is.na(Account)]), collapse = ", "),
            Date = first(Date),
            .groups = "drop"
          ) |>
          arrange(Symbol)
        
        incProgress(1.0, detail = "Data loaded successfully!")
        
        showNotification(paste("Loaded", nrow(portfolio), "unique assets from", 
                             format(first(portfolio$Date), "%Y-%m-%d")), 
                        type = "success")
        
        return(portfolio)
        
      }, error = function(e) {
        showNotification(paste("Error loading data:", e$message), type = "error")
        return(NULL)
      })
    })
  })
  
  # Fetch price data for all symbols
  price_data <- reactive({
    req(portfolio_data())
    
    symbols <- portfolio_data()$Symbol
    
    withProgress(message = 'Fetching price data...', value = 0, {
      
      # Calculate date ranges
      end_date <- Sys.Date()
      start_date <- end_date - years(2) # Get 2 years of data for safety
      
      price_list <- list()
      total_symbols <- length(symbols)
      
      for (i in seq_along(symbols)) {
        symbol <- symbols[i]
        
        incProgress(1/total_symbols, detail = paste("Fetching", symbol, "..."))
        
        tryCatch({
          # Try different symbol formats if needed
          symbol_variants <- c(symbol, paste0(symbol, ".TO")) # Add .TO for TSX stocks if needed
          
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
            showNotification(paste("Could not fetch data for", symbol), type = "warning")
          }
          
        }, error = function(e) {
          showNotification(paste("Error fetching", symbol, ":", e$message), type = "warning")
        })
      }
      
      if (length(price_list) > 0) {
        combined_prices <- bind_rows(price_list)
        showNotification(paste("Successfully fetched price data for", 
                             length(unique(combined_prices$Symbol)), "symbols"), 
                        type = "success")
        return(combined_prices)
      } else {
        showNotification("No price data could be fetched", type = "error")
        return(NULL)
      }
    })
  })
  
  # Calculate performance metrics
  performance_data <- reactive({
    req(price_data())
    
    withProgress(message = 'Calculating performance...', value = 0, {
      
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
      symbols <- unique(price_data()$Symbol)
      
      for (i in seq_along(symbols)) {
        symbol <- symbols[i]
        incProgress(1/length(symbols), detail = paste("Processing", symbol, "..."))
        
        symbol_prices <- price_data() |>
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
        return(performance_df)
      } else {
        return(NULL)
      }
    })
  })
  
  # Render portfolio summary
  output$portfolio_summary <- renderText({
    if (!is.null(portfolio_data())) {
      paste0(
        "Portfolio loaded: ", nrow(portfolio_data()), " unique assets",
        " | Data from: ", format(first(portfolio_data()$Date), "%B %d, %Y"),
        " | Total positions: ", sum(portfolio_data()$Total_Quantity > 0, na.rm = TRUE)
      )
    } else {
      "No portfolio data loaded"
    }
  })
  
  # Filter performance data based on user inputs
  filtered_performance <- reactive({
    req(performance_data())
    
    perf_data <- performance_data()
    
    # Apply symbol filter
    if (!is.null(input$symbol_filter) && input$symbol_filter != "") {
      symbols_to_show <- str_split(str_to_upper(input$symbol_filter), "[,\\s]+")[[1]]
      symbols_to_show <- str_trim(symbols_to_show)
      symbols_to_show <- symbols_to_show[symbols_to_show != ""]
      
      if (length(symbols_to_show) > 0) {
        perf_data <- perf_data |>
          filter(Symbol %in% symbols_to_show)
      }
    }
    
    # Sort data
    if (input$sort_by != "Symbol") {
      perf_data <- perf_data |>
        arrange(desc(get(input$sort_by)))
    } else {
      perf_data <- perf_data |>
        arrange(Symbol)
    }
    
    return(perf_data)
  })
  
  # Create performance heatmap
  output$performance_heatmap <- renderPlotly({
    req(filtered_performance())
    
    perf_data <- filtered_performance()
    
    # Select only the performance columns
    time_periods <- c("1d", "7d", "30d", "90d", "6m", "1y")
    heatmap_data <- perf_data |>
      select(Symbol, all_of(time_periods)) |>
      pivot_longer(cols = all_of(time_periods), names_to = "Period", values_to = "Performance") |>
      filter(!is.na(Performance))
    
    # Create the heatmap
    p <- ggplot(heatmap_data, aes(x = factor(Period, levels = time_periods), 
                                  y = reorder(Symbol, Performance, FUN = function(x) mean(x, na.rm = TRUE)), 
                                  fill = Performance)) +
      geom_tile(color = "white", size = 0.1) +
      scale_fill_gradient2(low = "red", mid = "white", high = "green", 
                          midpoint = 0, name = "Performance\n(%)") +
      labs(title = "Stock Performance Heatmap",
           subtitle = "Percentage change across different time periods",
           x = "Time Period", y = "Symbol") +
      theme_minimal() +
      theme(axis.text.x = element_text(angle = 45, hjust = 1),
            panel.grid = element_blank()) +
      geom_text(aes(label = paste0(round(Performance, 1), "%")), 
                size = 3, color = "black")
    
    ggplotly(p, tooltip = c("x", "y", "fill")) |>
      layout(title = list(text = paste0("Stock Performance Heatmap<br>",
                                       "<sub>Data as of ", Sys.Date(), "</sub>")))
  })
  
  # Render performance table
  output$performance_table <- renderDT({
    req(filtered_performance(), portfolio_data())
    
    # Join with portfolio data to show quantities
    table_data <- filtered_performance() |>
      left_join(portfolio_data(), by = "Symbol") |>
      select(Symbol, Current_Price, Total_Quantity, Accounts, `1d`, `7d`, `30d`, `90d`, `6m`, `1y`) |>
      mutate(
        Current_Price = round(Current_Price, 2),
        Total_Quantity = round(Total_Quantity, 2)
      )
    
    datatable(table_data,
              options = list(
                pageLength = 25,
                scrollX = TRUE,
                dom = 'Bfrtip'
              ),
              rownames = FALSE) |>
      formatRound(columns = c("1d", "7d", "30d", "90d", "6m", "1y"), digits = 2) |>
      formatStyle(columns = c("1d", "7d", "30d", "90d", "6m", "1y"),
                  backgroundColor = styleInterval(cuts = c(-5, 0, 5),
                                                values = c("#ffcccc", "#ffffcc", "#ccffcc", "#ccffff")))
  })
  
  # Reactive values to store data
  values <- reactiveValues(
    portfolio_data = NULL,
    price_data = NULL,
    performance_data = NULL,
    last_updated = NULL
  )
  
  # Function to load portfolio data from Google Sheets
  load_portfolio_data <- function(sheet_ids) {
    if (is.null(sheet_ids) || sheet_ids == "") {
      return(NULL)
    }
    
    sheet_list <- trimws(strsplit(sheet_ids, ",")[[1]])
    all_data <- list()
    
    tryCatch({
      for (i in seq_along(sheet_list)) {
        sheet_data <- read_sheet(sheet_list[i])
        sheet_data$source_sheet <- i
        all_data[[i]] <- sheet_data
      }
      
      combined_data <- bind_rows(all_data)
      return(combined_data)
    }, error = function(e) {
      showNotification(paste("Error loading sheets:", e$message), type = "error")
      return(NULL)
    })
  }
  
  # Function to get stock price data
  get_price_data <- function(symbols, period = "2y") {
    price_list <- list()
    
    for (symbol in symbols) {
      tryCatch({
        stock_data <- getSymbols(symbol, auto.assign = FALSE, from = Sys.Date() - years(2))
        if (!is.null(stock_data)) {
          prices <- as.data.frame(stock_data) |>
            mutate(date = as.Date(rownames(.))) |>
            select(date, close = contains("Close")) |>
            mutate(symbol = symbol)
          price_list[[symbol]] <- prices
        }
      }, error = function(e) {
        message(paste("Could not fetch data for", symbol, ":", e$message))
      })
    }
    
    if (length(price_list) > 0) {
      return(bind_rows(price_list))
    } else {
      return(NULL)
    }
  }
  
  # Function to calculate performance metrics
  calculate_performance <- function(price_data) {
    if (is.null(price_data)) return(NULL)
    
    performance <- price_data |>
      arrange(symbol, date) |>
      group_by(symbol) |>
      mutate(
        price_1d = lag(close, 1),
        price_7d = lag(close, 7),
        price_30d = lag(close, 30),
        price_90d = lag(close, 90),
        price_6m = lag(close, 126), # ~6 months of trading days
        price_1y = lag(close, 252)  # ~1 year of trading days
      ) |>
      filter(date == max(date)) |>
      mutate(
        perf_1d = ifelse(!is.na(price_1d), (close - price_1d) / price_1d * 100, NA),
        perf_7d = ifelse(!is.na(price_7d), (close - price_7d) / price_7d * 100, NA),
        perf_30d = ifelse(!is.na(price_30d), (close - price_30d) / price_30d * 100, NA),
        perf_90d = ifelse(!is.na(price_90d), (close - price_90d) / price_90d * 100, NA),
        perf_6m = ifelse(!is.na(price_6m), (close - price_6m) / price_6m * 100, NA),
        perf_1y = ifelse(!is.na(price_1y), (close - price_1y) / price_1y * 100, NA)
      ) |>
      select(symbol, close, starts_with("perf_")) |>
      ungroup()
    
    return(performance)
  }
  
  # Observe refresh button
  observeEvent(input$refresh_data, {
    withProgress(message = 'Loading data...', value = 0, {
      
      # Load portfolio data
      incProgress(0.2, detail = "Loading portfolio data...")
      portfolio_data <- load_portfolio_data(input$sheet_ids)
      values$portfolio_data <- portfolio_data
      
      if (!is.null(portfolio_data)) {
        # Extract unique symbols (assuming there's a symbol/ticker column)
        symbol_columns <- c("symbol", "ticker", "Symbol", "Ticker", "SYMBOL", "TICKER")
        symbol_col <- intersect(names(portfolio_data), symbol_columns)[1]
        
        if (!is.null(symbol_col)) {
          symbols <- unique(portfolio_data[[symbol_col]])
          symbols <- symbols[!is.na(symbols) & symbols != ""]
          
          # Get price data
          incProgress(0.5, detail = paste("Fetching price data for", length(symbols), "symbols..."))
          price_data <- get_price_data(symbols)
          values$price_data <- price_data
          
          # Calculate performance
          incProgress(0.8, detail = "Calculating performance metrics...")
          performance_data <- calculate_performance(price_data)
          values$performance_data <- performance_data
          
          values$last_updated <- Sys.time()
          
          # Update asset filter choices
          updateSelectizeInput(session, "asset_filter",
                               choices = symbols,
                               selected = symbols)
        } else {
          showNotification("No symbol/ticker column found in portfolio data", type = "warning")
        }
      }
      
      incProgress(1, detail = "Complete!")
    })
  })
  
  # Performance heatmap
  output$performance_heatmap <- renderPlotly({
    req(values$performance_data, input$time_periods)
    
    perf_data <- values$performance_data
    
    # Filter assets if specified
    if (!is.null(input$asset_filter) && length(input$asset_filter) > 0) {
      perf_data <- perf_data |>
        filter(symbol %in% input$asset_filter)
    }
    
    # Select only requested time periods
    period_cols <- paste0("perf_", input$time_periods)
    available_cols <- intersect(period_cols, names(perf_data))
    
    if (length(available_cols) == 0) {
      return(plotly_empty())
    }
    
    heatmap_data <- perf_data |>
      select(symbol, all_of(available_cols)) |>
      pivot_longer(cols = -symbol, names_to = "period", values_to = "performance") |>
      mutate(
        period = case_when(
          period == "perf_1d" ~ "1 Day",
          period == "perf_7d" ~ "7 Days",
          period == "perf_30d" ~ "30 Days", 
          period == "perf_90d" ~ "90 Days",
          period == "perf_6m" ~ "6 Months",
          period == "perf_1y" ~ "1 Year",
          TRUE ~ period
        ),
        period = factor(period, levels = c("1 Day", "7 Days", "30 Days", "90 Days", "6 Months", "1 Year"))
      )
    
    p <- ggplot(heatmap_data, aes(x = period, y = symbol, fill = performance)) +
      geom_tile(color = "white", linewidth = 0.1) +
      scale_fill_gradient2(low = "red", mid = "white", high = "green", 
                           midpoint = 0, name = "Performance\n(%)") +
      theme_minimal() +
      theme(
        axis.text.x = element_text(angle = 45, hjust = 1),
        axis.title = element_blank(),
        plot.title = element_text(hjust = 0.5)
      ) +
      labs(title = "Portfolio Performance Heatmap")
    
    if (input$show_values) {
      p <- p + geom_text(aes(label = ifelse(!is.na(performance), 
                                           paste0(round(performance, 1), "%"), "")),
                         size = 3, color = "black")
    }
    
    ggplotly(p, height = input$heatmap_height) |>
      layout(title = list(text = "Portfolio Performance Heatmap", x = 0.5))
  })
  
  # Performance summary table
  output$performance_table <- DT::renderDataTable({
    req(values$performance_data)
    
    perf_data <- values$performance_data
    
    # Filter assets if specified
    if (!is.null(input$asset_filter) && length(input$asset_filter) > 0) {
      perf_data <- perf_data |>
        filter(symbol %in% input$asset_filter)
    }
    
    display_data <- perf_data |>
      mutate(
        `Current Price` = round(close, 2),
        `1 Day` = paste0(round(perf_1d, 2), "%"),
        `7 Days` = paste0(round(perf_7d, 2), "%"),
        `30 Days` = paste0(round(perf_30d, 2), "%"),
        `90 Days` = paste0(round(perf_90d, 2), "%"),
        `6 Months` = paste0(round(perf_6m, 2), "%"),
        `1 Year` = paste0(round(perf_1y, 2), "%")
      ) |>
      select(Symbol = symbol, `Current Price`, `1 Day`, `7 Days`, `30 Days`, `90 Days`, `6 Months`, `1 Year`)
    
    datatable(display_data, options = list(pageLength = 15, scrollX = TRUE)) |>
      formatStyle(columns = 3:8, 
                  backgroundColor = styleInterval(c(-5, -1, 0, 1, 5), 
                                                  c('#ffcccc', '#ffe6e6', '#ffffff', '#e6ffe6', '#ccffcc', '#99ff99')))
  })
  
  # Data status output
  output$data_status <- renderText({
    if (is.null(values$last_updated)) {
      "No data loaded"
    } else {
      paste("Last updated:", format(values$last_updated, "%Y-%m-%d %H:%M:%S"))
    }
  })
  
  # Portfolio overview outputs (placeholder)
  output$asset_allocation_pie <- renderPlotly({
    plotly_empty() |> 
      layout(title = "Asset allocation data not yet implemented")
  })
  
  output$top_performers <- DT::renderDataTable({
    req(values$performance_data)
    
    top_perf <- values$performance_data |>
      arrange(desc(perf_30d)) |>
      head(10) |>
      select(Symbol = symbol, `30-Day Performance` = perf_30d) |>
      mutate(`30-Day Performance` = paste0(round(`30-Day Performance`, 2), "%"))
    
    datatable(top_perf, options = list(pageLength = 10, dom = 't'))
  })
  
  output$portfolio_holdings <- DT::renderDataTable({
    req(values$portfolio_data)
    datatable(values$portfolio_data, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Data management outputs
  output$sheets_preview <- renderText({
    if (is.null(values$portfolio_data)) {
      "No portfolio data loaded"
    } else {
      paste("Portfolio data loaded:",
            nrow(values$portfolio_data), "rows,",
            ncol(values$portfolio_data), "columns")
    }
  })
  
  output$price_data_status <- renderText({
    if (is.null(values$price_data)) {
      "No price data loaded"
    } else {
      symbols_count <- length(unique(values$price_data$symbol))
      date_range <- range(values$price_data$date)
      paste("Price data for", symbols_count, "symbols",
            "\nDate range:", format(date_range[1], "%Y-%m-%d"), 
            "to", format(date_range[2], "%Y-%m-%d"))
    }
  })
  
  output$raw_data_table <- DT::renderDataTable({
    req(values$portfolio_data)
    datatable(values$portfolio_data, options = list(pageLength = 10, scrollX = TRUE))
  })
  
  # Render session stats for settings tab
  output$session_stats <- renderText({
    stats <- c(
      paste("Authentication: Service Account"),
      paste("Credentials file: creds/original-return-107905-3b03bf4c17bf.json"),
      paste("Data range: Starting from A3"),
      paste("Last refresh:", Sys.time())
    )
    if (!is.null(portfolio_data())) {
      stats <- c(stats, 
        paste("Symbols loaded:", nrow(portfolio_data())),
        paste("Data date:", format(first(portfolio_data()$Date), "%Y-%m-%d"))
      )
    }
    if (!is.null(price_data())) {
      stats <- c(stats, 
        paste("Price data symbols:", length(unique(price_data()$Symbol)))
      )
    }
    paste(stats, collapse = "\n")
  })
}
