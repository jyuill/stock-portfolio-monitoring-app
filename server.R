# Stock Portfolio Performance Dashboard - Server Logic
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
    #req(input$refresh_data) # to show data on load, not require click on refresh btn
    
    perf_data <- performance_data
    
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
    
    # Use the order from filtered_performance (which respects user sorting)
    symbol_order <- unique(perf_data$Symbol)

    # Create the heatmap
    p <- ggplot(heatmap_data, aes(x = factor(Period, levels = time_periods), 
                                  y = factor(Symbol, levels = rev(symbol_order)), # Use user's sort order 
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
                                       "<sub>Portfolio data from ", 
                                       format(max(portfolio_data$Date), "%Y-%m-%d"), 
                                       "</sub>"),
                         pad = list(t = 20, b=20),
                        font = list(size = 20, color = "green", style = "italic"),
                      subtitle = list(text = paste0("Percentage change across different time periods. Data as of ", Sys.Date()))),
            margin = list(t = 40, b = 40, l = 40, r = 20))
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
                pageLength = 30,
                scrollX = TRUE,
                dom = 'Bfrtip'
              ),
              rownames = FALSE) |>
      formatRound(columns = c("1d", "7d", "30d", "90d", "6m", "1y"), digits = 2) |>
      formatStyle(columns = c("1d", "7d", "30d", "90d", "6m", "1y"),
                  backgroundColor = styleInterval(cuts = c(-5, 0, 5),
                                                values = c("#ffcccc", "#ffffcc", "#ccffcc", "#ccffff")))
  })
  
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
}