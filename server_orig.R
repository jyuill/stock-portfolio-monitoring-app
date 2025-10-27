library(shiny)
library(dplyr)
library(ggplot2)
library(plotly)
library(DT)
library(quantmod)
library(lubridate)
library(stringr)
library(tidyr)

# Load portfolio data from Google Sheets
source('global_data.R')

server <- function(input, output, session) {
  
  # Use pre-loaded data from global_data.R (note: lowercase variable names)
  portfolio_data_reactive <- reactive({
    return(portfolio_data)  # Using lowercase from global_data.R
  })
  
  # Price data is already loaded in global_data.R, make it reactive for refresh
  price_data_reactive <- reactive({
    req(input$refresh_data)
    return(price_data)  # Using pre-loaded price data
  })
  
  # Performance data is already calculated in global_data.R
  performance_data_reactive <- reactive({
    req(input$refresh_data)
    return(performance_data)  # Using pre-calculated performance data
  })
  
  # Render portfolio summary using pre-loaded data
  output$portfolio_summary <- renderText({
    paste0(
      "Portfolio loaded: ", nrow(portfolio_data), " unique assets",
      " | Data from: ", format(max(portfolio_data$Date), "%B %d, %Y"),
      " | Total positions: ", sum(portfolio_data$Total_Quantity > 0, na.rm = TRUE),
      " | Price data for: ", length(unique(price_data$Symbol)), " symbols"
    )
  })
  
  # Filter performance data based on user inputs
  filtered_performance <- reactive({
    req(performance_data_reactive())
    
    perf_data <- performance_data_reactive()
    
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
    req(filtered_performance())
    
    # Join with portfolio data to show quantities
    table_data <- filtered_performance() |>
      left_join(portfolio_data, by = "Symbol") |>
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
  # Filter performance data based on user inputs
  filtered_performance <- reactive({
    req(performance_data_reactive())
    
    perf_data <- performance_data_reactive()
      return(NULL)
    })
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
                                       "<sub>Portfolio data from ", format(max(portfolio_data$Date), "%Y-%m-%d"), "</sub>")))
          # Calculate performance
          incProgress(0.8, detail = "Calculating performance metrics...")
          performance_data <- calculate_performance(price_data)
          values$performance_data <- performance_data
    req(filtered_performance())
    
    # Join with portfolio data to show quantities
    table_data <- filtered_performance() |>
      left_join(portfolio_data, by = "Symbol") |>er",
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
  # Add session statistics output
  output$session_stats <- renderText({
    paste0(
      "Portfolio symbols: ", nrow(portfolio_data), "\n",
      "Symbols with price data: ", length(unique(price_data$Symbol)), "\n",
      "Total price records: ", nrow(price_data), "\n",
      "Performance calculations: ", nrow(performance_data), "\n",
      "Data loaded: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
  })