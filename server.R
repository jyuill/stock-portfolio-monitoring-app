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
  parse_numeric <- function(x) {
    cleaned <- gsub(",", "", as.character(x))
    cleaned <- gsub("[^0-9.\\-]", "", cleaned)
    suppressWarnings(as.numeric(cleaned))
  }

  get_usdcad_rate <- reactive({
    quote_data <- tryCatch(
      getQuote("USDCAD=X", src = "yahoo"),
      error = function(e) NULL
    )

    if (!is.null(quote_data) && nrow(quote_data) > 0) {
      for (col in names(quote_data)) {
        val <- parse_numeric(quote_data[[col]][1])
        if (!is.na(val) && val > 0) return(val)
      }
    }

    fx_hist <- tryCatch(
      getSymbols("USDCAD=X", src = "yahoo", from = Sys.Date() - 14, to = Sys.Date(), auto.assign = FALSE, warnings = FALSE),
      error = function(e) NULL
    )

    if (!is.null(fx_hist) && nrow(fx_hist) > 0) {
      fx_close_col <- "USDCAD=X.Close"
      if (!fx_close_col %in% colnames(fx_hist)) {
        fx_close_col <- "USDCAD=X.Adjusted"
      }
      if (fx_close_col %in% colnames(fx_hist)) {
        fx_val <- tail(as.numeric(fx_hist[, fx_close_col]), 1)
        if (!is.na(fx_val) && fx_val > 0) return(fx_val)
      }
    }

    1
  })

  convert_amount <- function(amount, from_currency, to_currency, fx_usdcad) {
    from <- str_to_upper(coalesce(from_currency, "CAD"))
    to <- str_to_upper(coalesce(to_currency, "CAD"))
    output <- as.numeric(amount)

    idx_usd_to_cad <- from == "USD" & to == "CAD"
    output[idx_usd_to_cad] <- output[idx_usd_to_cad] * fx_usdcad

    idx_cad_to_usd <- from == "CAD" & to == "USD"
    output[idx_cad_to_usd] <- output[idx_cad_to_usd] / fx_usdcad

    output
  }

  target_currency <- reactive({
    if (identical(input$currency_view, "cad")) "CAD" else "NATIVE"
  })

  observe({
    sector_choices <- c("All", sort(unique(portfolio_data$Sector)))
    updateSelectInput(session, "sector_filter", choices = sector_choices, selected = "All")

    account_choices <- sort(unique(portfolio_positions$Account))
    account_choices <- account_choices[!is.na(account_choices) & account_choices != ""]
    updateSelectInput(session, "account_filter", choices = c("All", account_choices), selected = "All")
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

    # Apply sector filter
    if (!is.null(input$sector_filter) && input$sector_filter != "All") {
      symbols_in_sector <- portfolio_data |>
        filter(Sector == input$sector_filter) |>
        pull(Symbol)
      perf_data <- perf_data |>
        filter(Symbol %in% symbols_in_sector)
    }

    # Apply account filter
    if (!is.null(input$account_filter) && input$account_filter != "All") {
      symbols_in_account <- portfolio_data |>
        rowwise() |>
        mutate(
          account_match = ifelse(
            is.na(Accounts),
            FALSE,
            input$account_filter %in% trimws(unlist(strsplit(Accounts, ",\\s*")))
          )
        ) |>
        ungroup() |>
        filter(account_match) |>
        pull(Symbol)
      perf_data <- perf_data |>
        filter(Symbol %in% symbols_in_account)
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

  filtered_positions <- reactive({
    positions <- portfolio_positions

    if (!is.null(input$symbol_filter) && input$symbol_filter != "") {
      symbols_to_show <- str_split(str_to_upper(input$symbol_filter), "[,\\s]+")[[1]]
      symbols_to_show <- str_trim(symbols_to_show)
      symbols_to_show <- symbols_to_show[symbols_to_show != ""]
      if (length(symbols_to_show) > 0) {
        positions <- positions |>
          filter(Symbol %in% symbols_to_show)
      }
    }

    if (!is.null(input$sector_filter) && input$sector_filter != "All") {
      positions <- positions |>
        filter(Sector == input$sector_filter)
    }

    if (!is.null(input$account_filter) && input$account_filter != "All") {
      positions <- positions |>
        filter(Account == input$account_filter)
    }

    positions
  })

  overview_symbol_data <- reactive({
    positions <- filtered_positions()
    if (nrow(positions) == 0) {
      return(data.frame())
    }

    current_prices <- filtered_performance() |>
      select(Symbol, Current_Price)

    positions |>
      group_by(Symbol, Sector) |>
      summarise(
        Quantity = sum(Quantity, na.rm = TRUE),
        Average_Cost = ifelse(
          sum(ifelse(is.na(Average_Cost), 0, Quantity), na.rm = TRUE) > 0,
          sum(Quantity * Average_Cost, na.rm = TRUE) /
            sum(ifelse(is.na(Average_Cost), 0, Quantity), na.rm = TRUE),
          NA_real_
        ),
        Accounts = paste(sort(unique(Account)), collapse = ", "),
        Cost_Currency = first(coalesce(Cost_Currency, "CAD")),
        Price_Currency = first(coalesce(Price_Currency, "CAD")),
        .groups = "drop"
      ) |>
      left_join(current_prices, by = "Symbol") |>
      mutate(
        fx_usdcad = get_usdcad_rate(),
        Average_Cost = if (target_currency() == "CAD") {
          convert_amount(Average_Cost, Cost_Currency, "CAD", fx_usdcad)
        } else {
          Average_Cost
        },
        Current_Price = if (target_currency() == "CAD") {
          convert_amount(Current_Price, Price_Currency, "CAD", fx_usdcad)
        } else {
          Current_Price
        },
        Value = Current_Price * Quantity,
        Investment = ifelse(is.na(Average_Cost), 0, Average_Cost * Quantity),
        Gain_Loss = Value - Investment
      ) |>
      select(-fx_usdcad)
  })

  converted_positions <- reactive({
    positions <- filtered_positions()
    if (nrow(positions) == 0) {
      return(data.frame())
    }

    current_prices <- filtered_performance() |>
      select(Symbol, Current_Price)

    positions |>
      left_join(current_prices, by = "Symbol") |>
      mutate(
        Cost_Currency = coalesce(Cost_Currency, "CAD"),
        Price_Currency = coalesce(Price_Currency, "CAD"),
        fx_usdcad = get_usdcad_rate(),
        Average_Cost_Conv = if (target_currency() == "CAD") {
          convert_amount(Average_Cost, Cost_Currency, "CAD", fx_usdcad)
        } else {
          Average_Cost
        },
        Current_Price_Conv = if (target_currency() == "CAD") {
          convert_amount(Current_Price, Price_Currency, "CAD", fx_usdcad)
        } else {
          Current_Price
        },
        Value = Current_Price_Conv * Quantity,
        Investment = ifelse(is.na(Average_Cost_Conv), 0, Average_Cost_Conv * Quantity),
        Gain_Loss = Value - Investment
      )
  })

  account_breakdown <- reactive({
    positions <- converted_positions()
    if (nrow(positions) == 0) {
      return(data.frame())
    }

    output <- positions |>
      group_by(Account) |>
      summarise(
        Value = sum(Value, na.rm = TRUE),
        Investment = sum(Investment, na.rm = TRUE),
        Gain_Loss = sum(Gain_Loss, na.rm = TRUE),
        .groups = "drop"
      ) |>
      arrange(desc(Value))

    total_value <- sum(output$Value, na.rm = TRUE)
    total_rows <- nrow(output)
    output |>
      mutate(
        PortfolioPct = if (total_value > 0) {
          Value / total_value
        } else {
          rep(NA_real_, total_rows)
        },
        Gain_Loss_Pct = ifelse(Investment != 0, Gain_Loss / Investment, NA_real_)
      )
  })

  sector_breakdown <- reactive({
    symbol_data <- overview_symbol_data()
    if (nrow(symbol_data) == 0) {
      return(data.frame())
    }

    output <- symbol_data |>
      group_by(Sector) |>
      summarise(
        Value = sum(Value, na.rm = TRUE),
        Investment = sum(Investment, na.rm = TRUE),
        Gain_Loss = sum(Gain_Loss, na.rm = TRUE),
        .groups = "drop"
      ) |>
      arrange(desc(Value))

    total_value <- sum(output$Value, na.rm = TRUE)
    total_rows <- nrow(output)
    output |>
      mutate(
        PortfolioPct = if (total_value > 0) {
          Value / total_value
        } else {
          rep(NA_real_, total_rows)
        },
        Gain_Loss_Pct = ifelse(Investment != 0, Gain_Loss / Investment, NA_real_)
      )
  })

  fmt_currency <- function(x, digits = 0) {
    if (is.na(x)) return("N/A")
    paste0("$", format(round(x, digits), big.mark = ",", scientific = FALSE, trim = TRUE, nsmall = digits))
  }

  output$overview_total_investment <- renderValueBox({
    data <- overview_symbol_data()
    total_investment <- sum(data$Investment, na.rm = TRUE)
    valueBox(
      value = fmt_currency(total_investment, 0),
      subtitle = if (target_currency() == "CAD") "Total Investment (CAD)" else "Total Investment",
      icon = icon("wallet"),
      color = "light-blue"
    )
  })

  output$overview_total_value <- renderValueBox({
    data <- overview_symbol_data()
    total_value <- sum(data$Value, na.rm = TRUE)
    valueBox(
      value = fmt_currency(total_value, 0),
      subtitle = if (target_currency() == "CAD") "Current Portfolio Value (CAD)" else "Current Portfolio Value",
      icon = icon("chart-pie"),
      color = "aqua"
    )
  })

  output$overview_total_gain_loss <- renderValueBox({
    data <- overview_symbol_data()
    total_investment <- sum(data$Investment, na.rm = TRUE)
    total_value <- sum(data$Value, na.rm = TRUE)
    total_gain_loss <- total_value - total_investment
    total_gain_loss_pct <- ifelse(total_investment > 0, total_gain_loss / total_investment, NA_real_)
    gain_loss_label <- paste0(
      fmt_currency(total_gain_loss, 0),
      " (",
      ifelse(is.na(total_gain_loss_pct), "N/A", paste0(round(total_gain_loss_pct * 100, 1), "%")),
      ")"
    )

    valueBox(
      value = gain_loss_label,
      subtitle = if (target_currency() == "CAD") "Total Gain / Loss (CAD)" else "Total Gain / Loss",
      icon = icon("balance-scale"),
      color = ifelse(total_gain_loss >= 0, "green", "red")
    )
  })

  output$account_value_donut <- renderPlotly({
    data <- account_breakdown()
    req(nrow(data) > 0)

    plot_ly(
      data,
      labels = ~Account,
      values = ~Value,
      type = "pie",
      hole = 0.55,
      textinfo = "label+percent",
      hovertemplate = "%{label}<br>Value: $%{value:,.0f}<br>Share: %{percent}<extra></extra>"
    ) |>
      layout(showlegend = FALSE)
  })

  output$sector_value_donut <- renderPlotly({
    data <- sector_breakdown()
    req(nrow(data) > 0)

    plot_ly(
      data,
      labels = ~Sector,
      values = ~Value,
      type = "pie",
      hole = 0.55,
      textinfo = "label+percent",
      hovertemplate = "%{label}<br>Value: $%{value:,.0f}<br>Share: %{percent}<extra></extra>"
    ) |>
      layout(showlegend = FALSE)
  })

  output$account_breakdown_table <- renderDT({
    data <- account_breakdown()
    req(nrow(data) > 0)
    data <- data |>
      select(Account, Value, PortfolioPct, Investment, Gain_Loss, Gain_Loss_Pct)

    datatable(
      data,
      options = list(pageLength = 10, searching = FALSE, paging = FALSE, info = FALSE, scrollX = TRUE),
      rownames = FALSE
    ) |>
      formatCurrency(columns = c("Investment", "Value", "Gain_Loss"), currency = "$", digits = 0) |>
      formatPercentage(columns = c("PortfolioPct", "Gain_Loss_Pct"), digits = 1)
  })

  output$sector_breakdown_table <- renderDT({
    data <- sector_breakdown()
    req(nrow(data) > 0)
    data <- data |>
      select(Sector, Value, PortfolioPct, Investment, Gain_Loss, Gain_Loss_Pct)

    datatable(
      data,
      options = list(pageLength = 10, searching = FALSE, paging = FALSE, info = FALSE, scrollX = TRUE),
      rownames = FALSE
    ) |>
      formatCurrency(columns = c("Value", "Investment", "Gain_Loss"), currency = "$", digits = 0) |>
      formatPercentage(columns = c("PortfolioPct", "Gain_Loss_Pct"), digits = 1)
  })

  output$account_value_gain_bar <- renderPlotly({
    data <- account_breakdown() |>
      arrange(desc(Value))
    req(nrow(data) > 0)
    data$Account <- factor(data$Account, levels = data$Account)

    plot_ly(data, x = ~Account, y = ~Value, type = "bar", name = "Value", marker = list(color = "#2ca25f")) |>
      add_trace(y = ~Gain_Loss, type = "bar", name = "Gain/Loss", marker = list(color = "#3182bd")) |>
      layout(
        barmode = "group",
        yaxis = list(title = if (target_currency() == "CAD") "Amount (CAD)" else "Amount"),
        xaxis = list(
          title = "Account",
          categoryorder = "array",
          categoryarray = as.character(data$Account)
        ),
        legend = list(orientation = "h", y = -0.2)
      )
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
                         pad = list(t = 0, b=20), # no effect
                         font = list(size = 18, color = "green", style = "italic"), # only size, color has effect
                      subtitle = list(text = paste0("Percentage change across different time periods. Data as of ", Sys.Date()))),
            margin = list(t = 80, b = 40, l = 40, r = 20)) # controls position of data block
  })
  
  # Render performance table
  performance_table_data <- reactive({
    req(filtered_performance())
    
    # Join with portfolio data to show quantities
    table_data <- filtered_performance() |>
      left_join(portfolio_data, by = "Symbol") |>
      mutate(
        Cost_Currency = coalesce(Cost_Currency, "CAD"),
        Price_Currency = coalesce(Price_Currency, "CAD"),
        fx_usdcad = get_usdcad_rate(),
        Average_Cost = if (target_currency() == "CAD") {
          convert_amount(Average_Cost, Cost_Currency, "CAD", fx_usdcad)
        } else {
          Average_Cost
        },
        Current_Price = if (target_currency() == "CAD") {
          convert_amount(Current_Price, Price_Currency, "CAD", fx_usdcad)
        } else {
          Current_Price
        },
        Value = Current_Price * Total_Quantity,
        Cost_Basis = Average_Cost * Total_Quantity,
        `Gain/Loss` = Value - Cost_Basis,
        `% Gain/Loss` = ifelse(
          Cost_Basis != 0,
          `Gain/Loss` / Cost_Basis,
          NA_real_
        )
      )

    filtered_total_value <- sum(table_data$Value, na.rm = TRUE)

    table_data <- table_data |>
      mutate(
        `Portfolio%` = if (filtered_total_value > 0) {
          (Value / filtered_total_value) * 100
        } else {
          NA_real_
        }
      ) |>
      select(Symbol, Sector, Accounts, Average_Cost, Total_Quantity, Current_Price, Value, `Gain/Loss`, `% Gain/Loss`, `Portfolio%`, `1d`, `7d`, `30d`, `90d`, `6m`, `1y`) |>
      rename(
        Quantity = Total_Quantity
      )

    table_data
  })

  output$performance_table <- renderDT({
    req(performance_table_data())
    table_data <- performance_table_data() |>
      mutate(
        `Portfolio%` = `Portfolio%` / 100,
        `1d` = `1d` / 100,
        `7d` = `7d` / 100,
        `30d` = `30d` / 100,
        `90d` = `90d` / 100,
        `6m` = `6m` / 100,
        `1y` = `1y` / 100
      )
    
    datatable(table_data,
              options = list(
                pageLength = 50,
                scrollX = TRUE,
                dom = 'Bfrtip'
              ),
              rownames = FALSE) |>
      formatCurrency(columns = c("Average_Cost", "Current_Price"), currency = "$", digits = 2) |>
      formatCurrency(columns = c("Value", "Gain/Loss"), currency = "$", digits = 0) |>
      formatRound(columns = c("Quantity"), digits = 0) |>
      formatPercentage(columns = c("Portfolio%"), digits = 1) |>
      formatPercentage(columns = c("% Gain/Loss"), digits = 0) |>
      formatPercentage(columns = c("1d", "7d", "30d", "90d", "6m", "1y"), digits = 1) |>
      formatStyle(columns = c("% Gain/Loss"),
                  backgroundColor = styleInterval(cuts = c(-0.05, 0, 0.05),
                                                values = c("#ffcccc", "#ffffcc", "#ccffcc", "#ccffff"))) |>
      formatStyle(columns = c("1d", "7d", "30d", "90d", "6m", "1y"),
                  backgroundColor = styleInterval(cuts = c(-0.05, 0, 0.05),
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

  output$data_source_validation <- renderDT({
    req(exists("holdings_source_summary"), nrow(holdings_source_summary) > 0)

    validation_data <- holdings_source_summary |>
      mutate(
        Latest_Date = as.character(Latest_Date)
      )

    datatable(
      validation_data,
      options = list(
        pageLength = 10,
        searching = FALSE,
        paging = FALSE,
        info = FALSE,
        scrollX = TRUE
      ),
      rownames = FALSE
    )
  })
}
