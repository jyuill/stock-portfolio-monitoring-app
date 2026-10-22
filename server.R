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
library(shinymanager)
library(sodium)

# Load portfolio data from Google Sheets
source('global_data.R')

load_auth_credentials <- function() {
  parse_csv <- function(x) {
    vals <- trimws(unlist(strsplit(x, ",")))
    vals[vals != ""]
  }

  users <- parse_csv(Sys.getenv("APP_LOGIN_USERS", ""))
  pwd_hashes <- parse_csv(Sys.getenv("APP_LOGIN_PWD_HASHES", ""))
  pwds_plain <- parse_csv(Sys.getenv("APP_LOGIN_PWDS", ""))
  admins <- parse_csv(Sys.getenv("APP_LOGIN_ADMINS", ""))

  if (length(users) == 0 || (length(pwd_hashes) == 0 && length(pwds_plain) == 0)) {
    user_single <- trimws(Sys.getenv("APP_LOGIN_USER", ""))
    hash_single <- trimws(Sys.getenv("APP_LOGIN_PWD_HASH", ""))
    pwd_single <- trimws(Sys.getenv("APP_LOGIN_PWD", ""))
    admin_single <- tolower(trimws(Sys.getenv("APP_LOGIN_ADMIN", "false")))

    if (nzchar(user_single) && (nzchar(hash_single) || nzchar(pwd_single))) {
      users <- user_single
      if (nzchar(hash_single)) {
        pwd_hashes <- hash_single
      } else {
        pwds_plain <- pwd_single
      }
      admins <- admin_single
    }
  }

  if (length(users) == 0 || (length(pwd_hashes) == 0 && length(pwds_plain) == 0)) {
    stop(
      "Missing app login credentials. Set APP_LOGIN_USER + APP_LOGIN_PWD_HASH ",
      "or APP_LOGIN_USER + APP_LOGIN_PWD ",
      "(multi-user: APP_LOGIN_USERS + APP_LOGIN_PWD_HASHES or APP_LOGIN_PWDS)."
    )
  }

  if (length(pwd_hashes) > 0 && length(pwds_plain) > 0) {
    stop("Set either hashed passwords or plain passwords, not both.")
  }

  hash_type <- NULL
  if (length(pwds_plain) > 0) {
    if (length(users) != length(pwds_plain)) {
      stop("APP_LOGIN_USERS and APP_LOGIN_PWDS must contain the same number of entries.")
    }
    passwords <- pwds_plain
    hash_type <- rep("plain", length(users))
  } else if (length(users) != length(pwd_hashes)) {
    stop("APP_LOGIN_USERS and APP_LOGIN_PWD_HASHES must contain the same number of entries.")
  } else {
    passwords <- pwd_hashes
    hash_type <- rep("sodium", length(users))
  }

  if (length(admins) == 0) {
    admins <- rep("false", length(users))
  } else if (length(admins) != length(users)) {
    stop("APP_LOGIN_ADMINS must be empty or match the number of APP_LOGIN_USERS.")
  }

  admins_bool <- tolower(admins) %in% c("true", "1", "yes", "y")

  data.frame(
    user = users,
    password = passwords,
    hash_type = hash_type,
    admin = admins_bool,
    stringsAsFactors = FALSE
  )
}

make_auth_checker <- function(credentials_df) {
  function(user, password) {
    credentials_df <- as.data.frame(credentials_df, stringsAsFactors = FALSE)
    if (!user %in% credentials_df$user) {
      return(list(result = FALSE, expired = FALSE, authorized = FALSE, user_info = NULL))
    }

    row <- credentials_df[credentials_df$user == user, , drop = FALSE][1, , drop = FALSE]
    hash_type <- row$hash_type
    stored_password <- row$password

    good_password <- FALSE
    if (identical(hash_type, "plain")) {
      good_password <- isTRUE(identical(stored_password, password))
    } else if (identical(hash_type, "sodium")) {
      good_password <- isTRUE(sodium::password_verify(stored_password, password))
    }

    user_info <- row[, setdiff(names(row), c("password", "hash_type")), drop = FALSE]

    if (good_password) {
      list(result = TRUE, expired = FALSE, authorized = TRUE, user_info = user_info)
    } else {
      list(result = FALSE, expired = FALSE, authorized = TRUE, user_info = user_info)
    }
  }
}

server <- function(input, output, session) {
  credentials <- load_auth_credentials()
  secure_server(
    check_credentials = make_auth_checker(credentials)
  )

  data_version <- reactiveVal(0)
  last_data_refresh <- reactiveVal({
    if (exists("data_summary", inherits = TRUE) &&
        "data_loaded_at" %in% names(data_summary) &&
        !is.null(data_summary$data_loaded_at)) {
      as.POSIXct(data_summary$data_loaded_at, tz = Sys.timezone())
    } else {
      Sys.time()
    }
  })

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
    # fx_usdcad is either one rate for everything or one rate per amount.
    fx_vec <- if (length(fx_usdcad) == length(output)) as.numeric(fx_usdcad) else rep(as.numeric(fx_usdcad)[1], length(output))

    idx_usd_to_cad <- from == "USD" & to == "CAD"
    output[idx_usd_to_cad] <- output[idx_usd_to_cad] * fx_vec[idx_usd_to_cad]

    idx_cad_to_usd <- from == "CAD" & to == "USD"
    output[idx_cad_to_usd] <- output[idx_cad_to_usd] / fx_vec[idx_cad_to_usd]

    output
  }

  target_currency <- reactive({
    if (identical(input$currency_view, "cad")) "CAD" else "NATIVE"
  })

  return_period_labels <- list(
    "1d" = "1 Day",
    "7d" = "7 Day",
    "30d" = "30 Day",
    "90d" = "90 Day",
    "6m" = "6 Month",
    "1y" = "1 Year",
    "total_gain_loss_pct" = "Total Gain/Loss %"
  )

  heatmap_fill_values <- c("#8b1e1e", "#c73e3e", "#f2b6b6", "#fff6d5", "#c9efc8", "#4db56a", "#0b7d32")

  treemap_colorscale <- list(
    list(0.00, "#8b1e1e"),
    list(0.20, "#c73e3e"),
    list(0.40, "#f2b6b6"),
    list(0.50, "#fff6d5"),
    list(0.60, "#c9efc8"),
    list(0.80, "#4db56a"),
    list(1.00, "#0b7d32")
  )

  portfolio_data_r <- reactive({
    data_version()
    portfolio_data
  })

  portfolio_positions_r <- reactive({
    data_version()
    portfolio_positions
  })

  price_data_r <- reactive({
    data_version()
    price_data
  })

  performance_data_r <- reactive({
    data_version()
    performance_data
  })

  holdings_source_summary_r <- reactive({
    data_version()
    if (exists("holdings_source_summary", inherits = TRUE)) {
      holdings_source_summary
    } else {
      data.frame()
    }
  })

  observeEvent(input$refresh_data, {
    showNotification("Refreshing data from Google Sheets and market sources...", type = "message", duration = 3)
    tryCatch({
      source("global_data.R", local = FALSE)
      data_version(data_version() + 1)
      if (exists("data_summary", inherits = TRUE) &&
          "data_loaded_at" %in% names(data_summary) &&
          !is.null(data_summary$data_loaded_at)) {
        last_data_refresh(as.POSIXct(data_summary$data_loaded_at, tz = Sys.timezone()))
      } else {
        last_data_refresh(Sys.time())
      }
      showNotification("Data refresh complete.", type = "message", duration = 3)
    }, error = function(e) {
      showNotification(
        paste("Data refresh failed:", e$message),
        type = "error",
        duration = 8
      )
    })
  })

  output$last_data_refresh_note <- renderText({
    paste0(
      "Data as of:\u00A0\u00A0\u00A0",
      format(last_data_refresh(), "%Y-%m-%d %H:%M:%S %Z")
    )
  })

  observe({
    sector_choices <- c("All", sort(unique(portfolio_data_r()$Sector)))
    updateSelectInput(session, "sector_filter", choices = sector_choices, selected = "All")

    account_choices <- sort(unique(portfolio_positions_r()$Account))
    account_choices <- account_choices[!is.na(account_choices) & account_choices != ""]
    selected_accounts <- isolate(input$account_filter)
    selected_accounts <- selected_accounts[selected_accounts %in% account_choices]
    if (length(selected_accounts) == 0) {
      selected_accounts <- "All"
    }
    updateSelectInput(
      session,
      "account_filter",
      choices = c("All", account_choices),
      selected = selected_accounts
    )

    geo_choices <- sort(unique(portfolio_data_r()$Geo))
    geo_choices <- geo_choices[!is.na(geo_choices) & geo_choices != ""]
    updateSelectInput(session, "geo_filter", choices = c("All", geo_choices), selected = "All")

    objective_choices <- sort(unique(portfolio_data_r()$Objective))
    objective_choices <- objective_choices[!is.na(objective_choices) & objective_choices != ""]
    updateSelectInput(session, "objective_filter", choices = c("All", objective_choices), selected = "All")
  })
  
  observeEvent(input$account_filter, {
    selected_accounts <- input$account_filter
    if (length(selected_accounts) > 1 && "All" %in% selected_accounts) {
      updateSelectInput(
        session,
        "account_filter",
        selected = setdiff(selected_accounts, "All")
      )
    }
  }, ignoreInit = TRUE)

  # Render portfolio summary using pre-loaded data
  output$portfolio_summary <- renderText({
    paste0(
      "Portfolio loaded: ", nrow(portfolio_data_r()), " unique assets",
      " | Data from: ", format(max(portfolio_data_r()$Date), "%B %d, %Y"),
      " | Total positions: ", sum(portfolio_data_r()$Total_Quantity > 0, na.rm = TRUE),
      " | Price data for: ", length(unique(price_data_r()$Symbol)), " symbols"
    )
  })
  
  # Filter performance data based on user inputs
  filtered_performance <- reactive({
    #req(input$refresh_data) # to show data on load, not require click on refresh btn
    
    perf_data <- performance_data_r()
    
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
      symbols_in_sector <- portfolio_data_r() |>
        filter(Sector == input$sector_filter) |>
        pull(Symbol)
      perf_data <- perf_data |>
        filter(Symbol %in% symbols_in_sector)
    }

    # Apply account filter
    selected_accounts <- input$account_filter
    if (!is.null(selected_accounts) &&
        length(selected_accounts) > 0 &&
        !"All" %in% selected_accounts) {
      symbols_in_account <- portfolio_data_r() |>
        rowwise() |>
        mutate(
          account_match = !is.na(Accounts) &&
            length(intersect(
              selected_accounts,
              trimws(unlist(strsplit(Accounts, ",\\s*")))
            )) > 0
        ) |>
        ungroup() |>
        filter(account_match) |>
        pull(Symbol)
      perf_data <- perf_data |>
        filter(Symbol %in% symbols_in_account)
    }

    if (!is.null(input$geo_filter) && input$geo_filter != "All") {
      symbols_in_geo <- portfolio_data_r() |>
        filter(Geo == input$geo_filter) |>
        pull(Symbol)
      perf_data <- perf_data |>
        filter(Symbol %in% symbols_in_geo)
    }

    if (!is.null(input$objective_filter) && input$objective_filter != "All") {
      symbols_in_objective <- portfolio_data_r() |>
        filter(Objective == input$objective_filter) |>
        pull(Symbol)
      perf_data <- perf_data |>
        filter(Symbol %in% symbols_in_objective)
    }
    
    # Sort data (input$sort_by can be NULL before UI inputs initialize)
    sort_by <- input$sort_by
    if (!is.null(sort_by) && length(sort_by) > 0 && nzchar(sort_by) &&
        sort_by != "Symbol" && sort_by %in% names(perf_data)) {
      perf_data <- perf_data |>
        arrange(desc(.data[[sort_by]]))
    } else {
      perf_data <- perf_data |>
        arrange(Symbol)
    }
    
    return(perf_data)
  })

  filtered_positions <- reactive({
    positions <- portfolio_positions_r()

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

    selected_accounts <- input$account_filter
    if (!is.null(selected_accounts) &&
        length(selected_accounts) > 0 &&
        !"All" %in% selected_accounts) {
      positions <- positions |>
        filter(Account %in% selected_accounts)
    }

    if (!is.null(input$geo_filter) && input$geo_filter != "All") {
      positions <- positions |>
        filter(Geo == input$geo_filter)
    }

    if (!is.null(input$objective_filter) && input$objective_filter != "All") {
      positions <- positions |>
        filter(Objective == input$objective_filter)
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
      group_by(Symbol, Sector, Geo, Objective) |>
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

  portfolio_details_data <- reactive({
    positions <- converted_positions()
    if (nrow(positions) == 0) {
      return(data.frame())
    }

    details <- positions |>
      mutate(
        Average_Cost = Average_Cost_Conv,
        Current_Price = Current_Price_Conv,
        `% Gain/Loss` = ifelse(Investment != 0, Gain_Loss / Investment, NA_real_)
      ) |>
      select(Account, Symbol, Sector, Average_Cost, Quantity, Current_Price, Value, Gain_Loss, `% Gain/Loss`)

    account_totals <- details |>
      group_by(Account) |>
      summarise(Account_Value = sum(Value, na.rm = TRUE), .groups = "drop")

    details |>
      left_join(account_totals, by = "Account") |>
      mutate(
        `% of Account` = ifelse(Account_Value > 0, Value / Account_Value, NA_real_)
      ) |>
      select(Account, Symbol, Sector, Average_Cost, Quantity, Current_Price, Value, Gain_Loss, `% Gain/Loss`, `% of Account`) |>
      rename(`Gain/Loss` = Gain_Loss) |>
      arrange(Account, desc(Value), Symbol)
  })

  account_sector_data <- reactive({
    details <- portfolio_details_data()
    if (nrow(details) == 0) {
      return(data.frame())
    }

    details |>
      group_by(Account, Sector) |>
      summarise(Value = sum(Value, na.rm = TRUE), .groups = "drop") |>
      group_by(Account) |>
      mutate(
        Account_Total = sum(Value, na.rm = TRUE),
        AccountPct = ifelse(Account_Total > 0, Value / Account_Total, 0)
      ) |>
      ungroup() |>
      filter(!is.na(Value), Value > 0)
  })

  account_plot_id <- function(account_name) {
    paste0("account_sector_pie_", gsub("[^A-Za-z0-9]+", "_", account_name))
  }

  output$account_sector_pies_ui <- renderUI({
    pie_data <- account_sector_data()
    req(nrow(pie_data) > 0)

    account_list <- sort(unique(pie_data$Account))
    chart_boxes <- lapply(account_list, function(acct) {
      box(
        title = acct,
        status = "primary",
        solidHeader = TRUE,
        width = 4,
        plotlyOutput(account_plot_id(acct), height = "280px")
      )
    })

    do.call(fluidRow, chart_boxes)
  })

  observe({
    pie_data <- account_sector_data()
    req(nrow(pie_data) > 0)

    for (acct in unique(pie_data$Account)) {
      local({
        account_name <- acct
        plot_id <- account_plot_id(account_name)
        output[[plot_id]] <- renderPlotly({
          acct_data <- pie_data |>
            filter(Account == account_name) |>
            arrange(Sector)

          plot_ly(
            acct_data,
            x = ~Sector,
            y = ~Value,
            type = "bar",
            marker = list(color = "#4c78a8"),
            text = ~paste0(round(AccountPct * 100, 1), "%"),
            textposition = "outside",
            hovertemplate = paste0(
              "%{x}<br>",
              account_name,
              " Value: $%{y:,.0f}<br>",
              "Share: %{text}<extra></extra>"
            ),
            showlegend = FALSE
          ) |>
            layout(
              xaxis = list(title = "Sector", categoryorder = "array", categoryarray = acct_data$Sector),
              yaxis = list(title = "Value"),
              margin = list(t = 10, r = 10, b = 70, l = 55)
            )
        })
      })
    }
  })

  output$portfolio_details_table <- renderDT({
    data <- portfolio_details_data()
    req(nrow(data) > 0)

    datatable(
      data,
      options = list(
        pageLength = 50,
        scrollX = TRUE
      ),
      rownames = FALSE
    ) |>
      formatCurrency(columns = c("Average_Cost", "Current_Price"), currency = "$", digits = 2) |>
      formatCurrency(columns = c("Value", "Gain/Loss"), currency = "$", digits = 0) |>
      formatRound(columns = c("Quantity"), digits = 0) |>
      formatPercentage(columns = c("% Gain/Loss", "% of Account"), digits = 1)
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

  geo_breakdown <- reactive({
    symbol_data <- overview_symbol_data()
    if (nrow(symbol_data) == 0) {
      return(data.frame())
    }

    symbol_data |>
      group_by(Geo) |>
      summarise(
        Value = sum(Value, na.rm = TRUE),
        .groups = "drop"
      ) |>
      filter(!is.na(Geo), Geo != "", !is.na(Value), Value > 0) |>
      arrange(desc(Value))
  })

  objective_breakdown <- reactive({
    symbol_data <- overview_symbol_data()
    if (nrow(symbol_data) == 0) {
      return(data.frame())
    }

    symbol_data |>
      group_by(Objective) |>
      summarise(
        Value = sum(Value, na.rm = TRUE),
        .groups = "drop"
      ) |>
      filter(!is.na(Objective), Objective != "", !is.na(Value), Value > 0) |>
      arrange(desc(Value))
  })

  fmt_currency <- function(x, digits = 0) {
    out <- rep("N/A", length(x))
    valid_idx <- !is.na(x)
    out[valid_idx] <- paste0(
      "$",
      format(
        round(x[valid_idx], digits),
        big.mark = ",",
        scientific = FALSE,
        trim = TRUE,
        nsmall = digits
      )
    )
    out
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

  output$geo_value_donut <- renderPlotly({
    data <- geo_breakdown()
    req(nrow(data) > 0)

    plot_ly(
      data,
      labels = ~Geo,
      values = ~Value,
      type = "pie",
      hole = 0.55,
      textinfo = "label+percent",
      hovertemplate = "%{label}<br>Value: $%{value:,.0f}<br>Share: %{percent}<extra></extra>"
    ) |>
      layout(showlegend = FALSE)
  })

  output$objective_value_donut <- renderPlotly({
    data <- objective_breakdown()
    req(nrow(data) > 0)

    plot_ly(
      data,
      labels = ~Objective,
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

  # ---- Portfolio Overview tab: monthly balance trends -----------------------
  # Driven by portfolio_monthly_data (loaded in global_data.R from the
  # 'portfolio_mthly' Google Sheet tab, which extract_stmt.R + upload_holdings.R
  # populate from bank/brokerage statements; account_label comes from the 'ref'
  # sheet, mapping statement account numbers to the same account names used
  # elsewhere in the app, e.g. "20LWH2-S" -> "TD-RSP"). This is still a
  # different granularity from the holdings-based Account filter in the
  # sidebar (whole-account cash+securities balances vs. per-symbol holdings),
  # so it intentionally doesn't hook into that filter — figures below are
  # always across all statement accounts.
  # Follows the sidebar Currency View: in CAD view, USD accounts (per the
  # sheet's 'currency' column, e.g. TD-USD) are converted at each month's
  # month-end USD/CAD rate (portfolio_monthly_fx, from global_data.R), falling
  # back to the current spot rate where a month's rate is missing. Beginning
  # balances use the prior month's rate (they're the prior month's ending
  # balance) and change_in_balance is recomputed from the converted balances,
  # so months still chain and the CAD change includes FX moves on USD holdings.
  portfolio_monthly_all_r <- reactive({
    data_version()
    data <- portfolio_monthly_data
    if (nrow(data) == 0 || target_currency() != "CAD") {
      return(data)
    }

    fx_by_month <- portfolio_monthly_fx
    spot_rate <- get_usdcad_rate()
    month_rate <- function(m) coalesce(fx_by_month$usdcad[match(m, fx_by_month$Month_Date)], spot_rate)

    data |>
      mutate(
        fx_end = month_rate(Month_Date),
        fx_begin = month_rate(Month_Date %m-% months(1)),
        beginning_balance = convert_amount(beginning_balance, currency, "CAD", fx_begin),
        ending_balance = convert_amount(ending_balance, currency, "CAD", fx_end),
        change_in_balance = ifelse(
          is.na(ending_balance) | is.na(beginning_balance),
          convert_amount(change_in_balance, currency, "CAD", fx_end),
          ending_balance - beginning_balance
        ),
        dividends_this_period = convert_amount(dividends_this_period, currency, "CAD", fx_end),
        dividends_ytd = convert_amount(dividends_ytd, currency, "CAD", fx_end),
        ending_cash_balance = convert_amount(ending_cash_balance, currency, "CAD", fx_end),
        notable_contribution = convert_amount(notable_contribution, currency, "CAD", fx_end),
        currency = ifelse(str_to_upper(coalesce(currency, "CAD")) %in% c("USD", "CAD"), "CAD", currency)
      ) |>
      select(-fx_end, -fx_begin)
  })

  pm_currency_label <- reactive({
    if (target_currency() == "CAD") "CAD" else "native currency"
  })

  pm_month_choices <- reactiveVal(character())

  # Keep the month picker's choices in sync with the loaded months (labelled
  # "Sep 2026", valued by Month_Date). Defaults to all months; on a refresh,
  # if every month was selected, any newly loaded month is selected too,
  # otherwise the user's selection is kept.
  observeEvent(portfolio_monthly_all_r(), {
    data <- portfolio_monthly_all_r()
    months_available <- sort(unique(data$Month_Date))
    choices <- setNames(as.character(months_available), format(months_available, "%b %Y"))

    current <- isolate(input$pm_months)
    previous_choices <- isolate(pm_month_choices())
    selected <- if (length(current) == 0 || setequal(current, previous_choices)) {
      choices
    } else {
      intersect(current, choices)
    }
    pm_month_choices(unname(choices))
    updateSelectizeInput(session, "pm_months", choices = choices, selected = selected)
  })

  # An empty selection is treated as "all months" rather than an empty tab.
  portfolio_monthly_r <- reactive({
    data <- portfolio_monthly_all_r()
    selected <- input$pm_months
    if (nrow(data) == 0 || length(selected) == 0) {
      return(data)
    }
    data |> filter(as.character(Month_Date) %in% selected)
  })

  pm_combined <- reactive({
    data <- portfolio_monthly_r()
    if (nrow(data) == 0) {
      return(data.frame())
    }

    data |>
      group_by(Month_Date) |>
      summarise(
        beginning_balance = sum(beginning_balance, na.rm = TRUE),
        ending_balance = sum(ending_balance, na.rm = TRUE),
        change_in_balance = sum(change_in_balance, na.rm = TRUE),
        dividends_this_period = sum(dividends_this_period, na.rm = TRUE),
        notable_contribution = sum(notable_contribution, na.rm = TRUE),
        # A month is flagged estimated if ANY account's figure that month was
        # gap-filled (see global_data.R), even though the combined total also
        # includes other accounts' actual reported figures.
        Is_Estimated = any(Is_Estimated),
        .groups = "drop"
      ) |>
      arrange(Month_Date) |>
      mutate(
        growth_rate_pct = ifelse(beginning_balance != 0, change_in_balance / beginning_balance * 100, NA_real_)
      )
  })

  pm_cash_by_account <- reactive({
    data <- portfolio_monthly_r()
    if (nrow(data) == 0) {
      return(data.frame())
    }

    data |>
      group_by(Month_Date, account_label) |>
      summarise(
        ending_cash_balance = sum(ending_cash_balance, na.rm = TRUE),
        Is_Estimated = any(Is_Estimated),
        .groups = "drop"
      ) |>
      arrange(account_label, Month_Date)
  })

  pm_dividends_by_account <- reactive({
    data <- portfolio_monthly_r()
    if (nrow(data) == 0) {
      return(data.frame())
    }

    data |>
      group_by(Month_Date, account_label) |>
      summarise(
        dividends_this_period = sum(dividends_this_period, na.rm = TRUE),
        Is_Estimated = any(Is_Estimated),
        .groups = "drop"
      ) |>
      arrange(account_label, Month_Date)
  })

  pm_grouped <- reactive({
    data <- portfolio_monthly_r()
    if (nrow(data) == 0) {
      return(data.frame())
    }

    group_col <- input$pm_group_by
    if (is.null(group_col) || !group_col %in% c("account_label", "institution")) {
      group_col <- "account_label"
    }

    data |>
      group_by(Month_Date, Group = .data[[group_col]]) |>
      summarise(
        ending_balance = sum(ending_balance, na.rm = TRUE),
        Is_Estimated = any(Is_Estimated),
        .groups = "drop"
      ) |>
      arrange(Group, Month_Date)
  })

  # Adds a solid "actual" line/marker trace plus a dotted "estimated" trace to
  # a plotly object for one series, so months with no statement yet (gap-
  # filled in global_data.R) read visually as an estimate rather than as
  # confirmed data. The dotted segment is bridged from the last actual point
  # so it connects to, rather than floats disconnected from, the solid line.
  add_estimate_aware_trace <- function(p, series_data, name, color, show_name_in_legend = TRUE) {
    series_data <- series_data |> arrange(Month_Date)
    actual <- series_data |> filter(!Is_Estimated)
    estimated <- series_data |> filter(Is_Estimated)

    if (nrow(actual) > 0) {
      p <- p |> add_trace(
        data = actual, x = ~Month_Date, y = ~ending_balance, type = "scatter", mode = "lines+markers",
        name = name, legendgroup = name, showlegend = show_name_in_legend,
        line = list(color = color), marker = list(color = color),
        hovertemplate = paste0("%{x|%Y-%m}<br>", name, "<br>Value: $%{y:,.0f}<extra></extra>")
      )
    }

    if (nrow(estimated) > 0) {
      bridge <- if (nrow(actual) > 0) tail(actual, 1) else estimated[0, ]
      estimated_with_bridge <- bind_rows(bridge, estimated) |> arrange(Month_Date)

      p <- p |> add_trace(
        data = estimated_with_bridge, x = ~Month_Date, y = ~ending_balance, type = "scatter", mode = "lines+markers",
        name = paste0(name, " (estimated)"), legendgroup = name, showlegend = FALSE,
        line = list(color = color, dash = "dot"), marker = list(color = color, symbol = "circle-open"),
        hovertemplate = paste0(
          "%{x|%Y-%m}<br>", name, " (estimated — statement not yet available)<br>Value: $%{y:,.0f}<extra></extra>"
        )
      )
    }

    p
  }

  output$pm_total_value <- renderValueBox({
    combined <- pm_combined()
    if (nrow(combined) == 0) {
      return(valueBox(value = "N/A", subtitle = "Combined Portfolio Value", icon = icon("chart-line"), color = "aqua"))
    }
    latest <- combined |> filter(Month_Date == max(Month_Date))
    valueBox(
      value = fmt_currency(latest$ending_balance, 0),
      subtitle = paste0(
        "Combined Portfolio Value (", format(latest$Month_Date, "%Y-%m"), ")",
        if (latest$Is_Estimated) " — includes estimate" else ""
      ),
      icon = icon("chart-line"),
      color = "aqua"
    )
  })

  output$pm_month_change <- renderValueBox({
    combined <- pm_combined()
    if (nrow(combined) == 0) {
      return(valueBox(value = "N/A", subtitle = "Change in Balance", icon = icon("arrow-trend-up"), color = "light-blue"))
    }
    latest <- combined |> filter(Month_Date == max(Month_Date))
    change_pct <- ifelse(latest$beginning_balance != 0, latest$change_in_balance / latest$beginning_balance, NA_real_)
    label <- paste0(
      fmt_currency(latest$change_in_balance, 0),
      " (", ifelse(is.na(change_pct), "N/A", paste0(round(change_pct * 100, 1), "%")), ")"
    )
    valueBox(
      value = label,
      subtitle = paste0(
        "Change in Balance (", format(latest$Month_Date, "%Y-%m"), ")",
        if (latest$Is_Estimated) " — includes estimate" else ""
      ),
      icon = icon(ifelse(latest$change_in_balance >= 0, "arrow-trend-up", "arrow-trend-down")),
      color = ifelse(latest$change_in_balance >= 0, "green", "red")
    )
  })

  output$pm_period_change <- renderValueBox({
    combined <- pm_combined()
    if (nrow(combined) == 0) {
      return(valueBox(value = "N/A", subtitle = "Change in Balance (Selected Period)", icon = icon("arrow-trend-up"), color = "light-blue"))
    }
    # Sum of the selected months' changes (including deposits/withdrawals, same
    # as the monthly figures), so excluded months in between don't count; % is
    # relative to the first selected month's opening balance.
    first <- combined |> filter(Month_Date == min(Month_Date))
    last <- combined |> filter(Month_Date == max(Month_Date))
    period_change <- sum(combined$change_in_balance, na.rm = TRUE)
    change_pct <- ifelse(first$beginning_balance != 0, period_change / first$beginning_balance, NA_real_)
    label <- paste0(
      fmt_currency(period_change, 0),
      " (", ifelse(is.na(change_pct), "N/A", paste0(round(change_pct * 100, 1), "%")), ")"
    )
    valueBox(
      value = label,
      subtitle = paste0(
        "Change in Balance (", nrow(combined), " month", if (nrow(combined) != 1) "s" else "", ": ",
        format(first$Month_Date, "%Y-%m"), " to ", format(last$Month_Date, "%Y-%m"), ")",
        if (any(combined$Is_Estimated)) " — includes estimate" else ""
      ),
      icon = icon(ifelse(period_change >= 0, "arrow-trend-up", "arrow-trend-down")),
      color = ifelse(period_change >= 0, "green", "red")
    )
  })

  output$pm_dividends_ttm <- renderValueBox({
    combined <- pm_combined()
    if (nrow(combined) == 0) {
      return(valueBox(value = "N/A", subtitle = "Dividends (Trailing 12mo)", icon = icon("coins"), color = "yellow"))
    }
    # Trailing 12 months ending at the selected range's last month, taken from
    # the unfiltered data so a short selected range doesn't understate it.
    range_end <- max(combined$Month_Date)
    cutoff <- range_end - months(11)
    ttm_dividends <- portfolio_monthly_all_r() |>
      filter(Month_Date >= cutoff, Month_Date <= range_end) |>
      pull(dividends_this_period) |>
      sum(na.rm = TRUE)
    valueBox(
      value = fmt_currency(ttm_dividends, 0),
      subtitle = paste0("Dividends Received (Trailing 12mo to ", format(range_end, "%Y-%m"), ", Combined)"),
      icon = icon("coins"),
      color = "yellow"
    )
  })

  output$pm_combined_value_line <- renderPlotly({
    data <- pm_combined()
    req(nrow(data) > 0)

    plot_ly() |>
      add_estimate_aware_trace(data, name = "Combined", color = "#2ca25f", show_name_in_legend = FALSE) |>
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = paste0("Ending Balance (", pm_currency_label(), ")"), rangemode = "tozero"),
        showlegend = FALSE
      )
  })

  pm_group_palette <- c("#4c78a8", "#f58518", "#54a24b", "#e45756", "#72b7b2", "#eeca3b", "#b279a2", "#ff9da6")

  output$pm_account_value_line <- renderPlotly({
    data <- pm_grouped()
    req(nrow(data) > 0)

    groups <- sort(unique(data$Group))
    colors <- setNames(rep(pm_group_palette, length.out = length(groups)), groups)

    p <- plot_ly()
    for (g in groups) {
      p <- add_estimate_aware_trace(p, data |> filter(Group == g), name = g, color = colors[[g]])
    }

    p |>
      layout(
        xaxis = list(title = ""),
        yaxis = list(title = paste0("Ending Balance (", pm_currency_label(), ")")),
        legend = list(orientation = "h", y = -0.2)
      )
  })

  output$pm_monthly_change_bar <- renderPlotly({
    data <- pm_combined()
    req(nrow(data) > 0)
    bar_colors <- ifelse(data$change_in_balance >= 0, "#2ca25f", "#de2d26")
    bar_opacity <- ifelse(data$Is_Estimated, 0.45, 1)
    hover_text <- paste0(
      format(data$Month_Date, "%Y-%m"), "<br>Change: $", format(round(data$change_in_balance, 0), big.mark = ",", trim = TRUE),
      ifelse(data$Is_Estimated, " (partly estimated)", "")
    )

    plot_ly(
      data, x = ~Month_Date, y = ~change_in_balance, type = "bar",
      marker = list(color = bar_colors, opacity = bar_opacity),
      hovertext = hover_text, hoverinfo = "text"
    ) |>
      layout(xaxis = list(title = ""), yaxis = list(title = paste0("Change in Balance (", pm_currency_label(), ")")))
  })

  output$pm_growth_rate_bar <- renderPlotly({
    data <- pm_combined()
    req(nrow(data) > 0)
    bar_colors <- ifelse(data$growth_rate_pct >= 0, "#2ca25f", "#de2d26")
    bar_opacity <- ifelse(data$Is_Estimated, 0.45, 1)
    hover_text <- paste0(
      format(data$Month_Date, "%Y-%m"), "<br>Growth: ", round(data$growth_rate_pct, 1), "%",
      ifelse(data$Is_Estimated, " (partly estimated)", "")
    )

    plot_ly(
      data, x = ~Month_Date, y = ~growth_rate_pct, type = "bar",
      marker = list(color = bar_colors, opacity = bar_opacity),
      hovertext = hover_text, hoverinfo = "text"
    ) |>
      layout(xaxis = list(title = ""), yaxis = list(title = "Growth Rate (%)", ticksuffix = "%"))
  })

  output$pm_cash_stacked_bar <- renderPlotly({
    data <- pm_cash_by_account()
    req(nrow(data) > 0)
    hover_text <- paste0(
      format(data$Month_Date, "%Y-%m"), "<br>", data$account_label,
      "<br>Cash: $", format(round(data$ending_cash_balance, 0), big.mark = ",", trim = TRUE),
      ifelse(data$Is_Estimated, " (estimated)", "")
    )

    plot_ly(
      data, x = ~Month_Date, y = ~ending_cash_balance, color = ~account_label, colors = pm_group_palette,
      type = "bar", hovertext = hover_text, hoverinfo = "text"
    ) |>
      layout(
        barmode = "stack",
        xaxis = list(title = ""),
        yaxis = list(title = paste0("Ending Cash Balance (", pm_currency_label(), ")")),
        legend = list(orientation = "h", y = -0.2)
      )
  })

  output$pm_dividends_bar <- renderPlotly({
    data <- pm_dividends_by_account()
    req(nrow(data) > 0)
    totals <- pm_combined() # for the per-month total label and the average line

    hover_text <- paste0(
      format(data$Month_Date, "%Y-%m"), "<br>", data$account_label,
      "<br>Dividends: $", format(round(data$dividends_this_period, 0), big.mark = ",", trim = TRUE),
      ifelse(data$Is_Estimated, " (estimated)", "")
    )
    avg_dividend <- mean(totals$dividends_this_period, na.rm = TRUE)

    # One label per month showing the stack's total, rather than per-segment
    # labels (which would just repeat account_label's own dollar figure and
    # clutter a stacked chart) — positioned just above the top of each bar.
    total_labels <- lapply(seq_len(nrow(totals)), function(i) {
      list(
        x = totals$Month_Date[i], y = totals$dividends_this_period[i],
        text = paste0("$", format(round(totals$dividends_this_period[i], 0), big.mark = ",", trim = TRUE)),
        showarrow = FALSE, yanchor = "bottom", yshift = 4,
        font = list(size = 11, color = "#333333")
      )
    })
    avg_label <- list(
      xref = "paper", x = 1, yref = "y", y = avg_dividend,
      text = paste0("Avg: $", format(round(avg_dividend, 0), big.mark = ",", trim = TRUE)),
      showarrow = FALSE, xanchor = "left", yanchor = "bottom",
      font = list(size = 11, color = "#555555")
    )

    plot_ly(
      data, x = ~Month_Date, y = ~dividends_this_period, color = ~account_label, colors = pm_group_palette,
      type = "bar", hovertext = hover_text, hoverinfo = "text"
    ) |>
      layout(
        barmode = "stack",
        xaxis = list(title = ""),
        yaxis = list(title = paste0("Dividends Received (", pm_currency_label(), ")")),
        legend = list(orientation = "h", y = -0.2),
        shapes = list(list(
          type = "line", xref = "paper", x0 = 0, x1 = 1,
          yref = "y", y0 = avg_dividend, y1 = avg_dividend,
          line = list(color = "#555555", dash = "dash", width = 1.5)
        )),
        annotations = c(total_labels, list(avg_label))
      )
  })

  output$pm_monthly_table <- renderDT({
    data <- portfolio_monthly_r()
    req(nrow(data) > 0)

    table_data <- data |>
      mutate(Estimated = ifelse(Is_Estimated, "Yes", "")) |>
      select(account_label, institution, currency, month, beginning_balance, ending_balance,
             change_in_balance, dividends_this_period, dividends_ytd,
             ending_cash_balance, notable_contribution, Estimated) |>
      arrange(desc(month), account_label) |>
      rename(
        Account = account_label, Institution = institution, Currency = currency, Month = month,
        `Beginning Balance` = beginning_balance, `Ending Balance` = ending_balance,
        Change = change_in_balance, `Dividends (Period)` = dividends_this_period,
        `Dividends (YTD)` = dividends_ytd, `Ending Cash` = ending_cash_balance,
        `Deposits/Withdrawals` = notable_contribution
      )

    datatable(
      table_data,
      options = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE
    ) |>
      formatCurrency(
        columns = c("Beginning Balance", "Ending Balance", "Change", "Dividends (Period)",
                    "Dividends (YTD)", "Ending Cash", "Deposits/Withdrawals"),
        currency = "$", digits = 0
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
                                       format(max(portfolio_data_r()$Date), "%Y-%m-%d"), 
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
      left_join(portfolio_data_r(), by = "Symbol") |>
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

  performance_color_config <- reactive({
    data <- performance_table_data()
    req(nrow(data) > 0)

    color_values <- c(
      as.numeric(data$`% Gain/Loss`) * 100,
      as.numeric(data$`1d`),
      as.numeric(data$`7d`),
      as.numeric(data$`30d`),
      as.numeric(data$`90d`),
      as.numeric(data$`6m`),
      as.numeric(data$`1y`)
    )
    color_values <- color_values[is.finite(color_values)]

    max_abs_value <- suppressWarnings(max(abs(color_values), na.rm = TRUE))
    if (!is.finite(max_abs_value) || is.na(max_abs_value) || max_abs_value <= 0) {
      max_abs_value <- 5
    }

    color_limit <- max(5, ceiling(max_abs_value))
    cuts_pct <- c(
      -color_limit,
      -color_limit * 0.5,
      -color_limit * 0.1,
      0,
      color_limit * 0.1,
      color_limit * 0.5
    )

    list(
      limit = color_limit,
      cuts_decimal = cuts_pct / 100,
      colors = heatmap_fill_values
    )
  })

  output$performance_table <- renderDT({
    req(performance_table_data())
    color_config <- performance_color_config()
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
                  backgroundColor = styleInterval(
                    cuts = color_config$cuts_decimal,
                    values = color_config$colors
                  )) |>
      formatStyle(columns = c("1d", "7d", "30d", "90d", "6m", "1y"),
                  backgroundColor = styleInterval(
                    cuts = color_config$cuts_decimal,
                    values = color_config$colors
                  ))
  })

  treemap_data <- reactive({
    data <- performance_table_data()
    req(nrow(data) > 0)

    metric <- input$treemap_return_period
    if (is.null(metric) || !nzchar(metric)) {
      metric <- "30d"
    }
    metric_label <- unname(return_period_labels[[metric]])
    if (is.null(metric_label) || is.na(metric_label)) {
      metric <- "30d"
      metric_label <- unname(return_period_labels[[metric]])
    }

    return_values <- if (identical(metric, "total_gain_loss_pct")) {
      data$`% Gain/Loss` * 100
    } else if (metric %in% c("1d", "7d", "30d", "90d", "6m", "1y")) {
      as.numeric(data[[metric]])
    } else {
      as.numeric(data$`30d`)
    }

    data |>
      mutate(
        Sector = coalesce(na_if(trimws(Sector), ""), "Unassigned"),
        Return_Value = return_values
      ) |>
      filter(!is.na(Value), Value > 0) |>
      group_by(Sector, Symbol) |>
      summarise(
        Accounts = paste(sort(unique(Accounts[!is.na(Accounts) & Accounts != ""])), collapse = ", "),
        Holding_Value = sum(Value, na.rm = TRUE),
        Return_Value = weighted.mean(Return_Value, w = Value, na.rm = TRUE),
        .groups = "drop"
      ) |>
      group_by(Sector) |>
      mutate(
        Return_Value = ifelse(is.nan(Return_Value), NA_real_, Return_Value),
        Sector_Value = sum(Holding_Value, na.rm = TRUE),
        Sector_Return = weighted.mean(Return_Value, w = Holding_Value, na.rm = TRUE),
        Sector_Return = ifelse(is.nan(Sector_Return), NA_real_, Sector_Return),
        Return_Label = ifelse(is.na(Return_Value), "N/A", paste0(round(Return_Value, 1), "%")),
        Sector_Return_Label = ifelse(is.na(Sector_Return), "N/A", paste0(round(Sector_Return, 1), "%")),
        Metric_Label = metric_label
      ) |>
      ungroup()
  })

  output$sector_treemap <- renderPlotly({
    data <- treemap_data()
    req(nrow(data) > 0)

    format_treemap_currency <- function(x, digits = 0) {
      out <- rep("N/A", length(x))
      valid_idx <- !is.na(x)
      out[valid_idx] <- paste0(
        "$",
        format(
          round(x[valid_idx], digits),
          big.mark = ",",
          scientific = FALSE,
          trim = TRUE,
          nsmall = digits
        )
      )
      out
    }

    metric_label <- unique(data$Metric_Label)
    metric_label <- metric_label[!is.na(metric_label) & metric_label != ""]
    if (length(metric_label) == 0) {
      metric_label <- "Return"
    } else {
      metric_label <- metric_label[1]
    }
    colorbar_title <- if (identical(metric_label, "Total Gain/Loss %")) {
      metric_label
    } else {
      paste0(metric_label, " (%)")
    }
    max_abs_return <- suppressWarnings(max(abs(data$Return_Value), abs(data$Sector_Return), na.rm = TRUE))
    if (!is.finite(max_abs_return) || is.na(max_abs_return) || max_abs_return <= 0) {
      max_abs_return <- 5
    }
    color_limit <- max(5, ceiling(max_abs_return))
    total_portfolio_value <- sum(unique(data[, c("Sector", "Sector_Value")])$Sector_Value, na.rm = TRUE)

    sector_nodes <- data |>
      distinct(Sector, Sector_Value, Sector_Return, Sector_Return_Label, Metric_Label) |>
      mutate(
        id = paste0("sector::", Sector),
        label = Sector,
        parent = "",
        value = Sector_Value,
        color_value = Sector_Return,
        tile_text = Sector_Return_Label,
        size_share_label = ifelse(
          total_portfolio_value > 0,
          paste0(round((Sector_Value / total_portfolio_value) * 100, 1), "% of total value"),
          "N/A"
        ),
        text = paste0(
          "<b>", Sector, "</b>",
          "<br>Total Value: ", format_treemap_currency(Sector_Value, 0),
          "<br>Tile Size: ", size_share_label,
          "<br>", Metric_Label, ": ", Sector_Return_Label
        ),
        hover_group = Metric_Label
      )

    holding_nodes <- data |>
      transmute(
        id = paste0("holding::", Sector, "::", Symbol),
        label = Symbol,
        parent = paste0("sector::", Sector),
        value = Holding_Value,
        color_value = Return_Value,
        tile_text = Return_Label,
        size_share_label = ifelse(
          Sector_Value > 0,
          paste0(round((Holding_Value / Sector_Value) * 100, 1), "% of sector value"),
          "N/A"
        ),
        text = paste0(
          "<b>", Symbol, "</b>",
          "<br>Sector: ", Sector,
          ifelse(Accounts == "", "", paste0("<br>Accounts: ", Accounts)),
          "<br>Value: ", format_treemap_currency(Holding_Value, 0),
          "<br>Tile Size: ", size_share_label,
          "<br>", Metric_Label, ": ", Return_Label
        ),
        hover_group = Metric_Label
      )

    plot_data <- if (isTRUE(input$treemap_sector_only)) {
      sector_nodes
    } else {
      bind_rows(sector_nodes, holding_nodes)
    }
    plot_data$color_value[is.na(plot_data$color_value)] <- 0

    plot_ly(
      data = plot_data,
      type = "treemap",
      ids = ~id,
      labels = ~label,
      parents = ~parent,
      values = ~value,
      branchvalues = "total",
      text = ~text,
      customdata = ~tile_text,
      textinfo = "label+text",
      texttemplate = "%{label}<br>%{customdata}",
      hovertemplate = "%{text}<extra></extra>",
      marker = list(
        colors = ~color_value,
        colorscale = treemap_colorscale,
        cmin = -color_limit,
        cmax = color_limit,
        line = list(width = 1, color = "white"),
        showscale = TRUE,
        colorbar = list(
          title = colorbar_title
        )
      ),
      tiling = list(pad = 2)
    ) |>
      layout(
        uniformtext = list(minsize = 11, mode = "hide"),
        margin = list(t = 20, r = 20, b = 20, l = 20)
      )
  })

  risk_return_data <- reactive({
    positions <- filtered_positions()
    prices <- price_data_r()
    req(nrow(positions) > 0, nrow(prices) > 0)

    risk_window_days <- c("3m" = 90, "6m" = 180, "1y" = 365, "2y" = 730)
    selected_window <- input$risk_window
    window_days <- risk_window_days[[selected_window]]
    if (is.null(window_days) || is.na(window_days)) {
      window_days <- 365
    }

    symbols <- unique(positions$Symbol)
    symbol_meta <- positions |>
      group_by(Symbol, Sector, Geo, Objective) |>
      summarise(
        Quantity = sum(Quantity, na.rm = TRUE),
        Accounts = paste(sort(unique(Account)), collapse = ", "),
        Price_Currency = first(coalesce(Price_Currency, "CAD")),
        .groups = "drop"
      )

    latest_prices <- prices |>
      filter(Symbol %in% symbols) |>
      arrange(Symbol, Date) |>
      group_by(Symbol) |>
      slice_tail(n = 1) |>
      ungroup() |>
      transmute(
        Symbol = Symbol,
        Current_Price = as.numeric(Price)
      )

    symbol_values <- symbol_meta |>
      left_join(latest_prices, by = "Symbol") |>
      mutate(
        fx_usdcad = get_usdcad_rate(),
        Current_Price_Conv = if (target_currency() == "CAD") {
          convert_amount(Current_Price, Price_Currency, "CAD", fx_usdcad)
        } else {
          Current_Price
        },
        Value = Current_Price_Conv * Quantity
      ) |>
      select(-fx_usdcad)

    window_start <- Sys.Date() - window_days
    risk_metrics <- prices |>
      filter(Symbol %in% symbols, Date >= window_start) |>
      arrange(Symbol, Date) |>
      group_by(Symbol) |>
      mutate(Daily_Return = (Price / lag(Price)) - 1) |>
      summarise(
        Trading_Days = sum(!is.na(Daily_Return)),
        Price_Start = first(Price),
        Price_End = last(Price),
        Return_Window = ifelse(
          !is.na(Price_Start[1]) && Price_Start[1] != 0,
          ((Price_End[1] - Price_Start[1]) / Price_Start[1]) * 100,
          NA_real_
        ),
        Volatility_1y = ifelse(
          Trading_Days >= 2,
          stats::sd(Daily_Return, na.rm = TRUE) * sqrt(252) * 100,
          NA_real_
        ),
        .groups = "drop"
      ) |>
      select(Symbol, Trading_Days, Return_Window, Volatility_1y)

    risk_data <- symbol_values |>
      left_join(risk_metrics, by = "Symbol") |>
      mutate(
        Return_1y = as.numeric(Return_Window),
        Value = as.numeric(Value),
        Sharpe_1y = ifelse(
          !is.na(Return_1y) & !is.na(Volatility_1y) & Volatility_1y > 0,
          Return_1y / Volatility_1y,
          NA_real_
        )
      ) |>
      filter(!is.na(Return_1y), !is.na(Volatility_1y), !is.na(Value), Value > 0) |>
      arrange(desc(Value))

    val_scale <- sqrt(risk_data$Value)
    scale_min <- min(val_scale, na.rm = TRUE)
    scale_max <- max(val_scale, na.rm = TRUE)
    if (isTRUE(all.equal(scale_min, scale_max))) {
      risk_data$Marker_Size <- rep(28, nrow(risk_data))
    } else {
      risk_data$Marker_Size <- 12 + ((val_scale - scale_min) / (scale_max - scale_min)) * 34
    }

    risk_data
  })

  output$risk_return_bubble <- renderPlotly({
    data <- risk_return_data()
    req(nrow(data) > 0)

    currency_label <- if (target_currency() == "CAD") "CAD" else "Native"

    plot_ly(
      data = data,
      x = ~Return_1y,
      y = ~Volatility_1y,
      type = "scatter",
      mode = "markers",
      color = ~Sector,
      colors = "Set2",
      text = ~paste0(
        "<b>", Symbol, "</b>",
        "<br>Sector: ", Sector,
        "<br>Accounts: ", Accounts,
        "<br>Window Return: ", round(Return_1y, 1), "%",
        "<br>Window Volatility: ", round(Volatility_1y, 1), "%",
        "<br>Sharpe: ", round(Sharpe_1y, 2),
        "<br>Value (", currency_label, "): $", format(round(Value, 0), big.mark = ",", scientific = FALSE),
        "<br>Trading days: ", Trading_Days
      ),
      hoverinfo = "text",
      marker = list(
        size = ~Marker_Size,
        sizemode = "diameter",
        opacity = 0.65,
        line = list(width = 1, color = "#FFFFFF")
      )
    ) |>
      layout(
        template = "none",
        hovermode = "closest",
        hoverdistance = 8,
        xaxis = list(title = "Window Return (%)", zeroline = TRUE, zerolinecolor = "#999999"),
        yaxis = list(title = "Window Volatility (Annualized %)", zeroline = TRUE, zerolinecolor = "#999999"),
        legend = list(title = list(text = "Sector"), orientation = "h", y = -0.18),
        paper_bgcolor = "#FFFFFF",
        plot_bgcolor = "#FFFFFF",
        margin = list(l = 70, r = 30, t = 20, b = 80)
      )
  })

  risk_correlation_data <- reactive({
    positions <- filtered_positions()
    prices <- price_data_r()
    req(nrow(positions) > 0, nrow(prices) > 0)

    risk_window_days <- c("3m" = 90, "6m" = 180, "1y" = 365, "2y" = 730)
    selected_window <- input$risk_window
    window_days <- risk_window_days[[selected_window]]
    if (is.null(window_days) || is.na(window_days)) {
      window_days <- 365
    }

    symbols <- unique(positions$Symbol)
    window_start <- Sys.Date() - window_days

    returns_long <- prices |>
      filter(Symbol %in% symbols, Date >= window_start) |>
      arrange(Symbol, Date) |>
      group_by(Symbol) |>
      mutate(Daily_Return = (Price / lag(Price)) - 1) |>
      ungroup() |>
      filter(!is.na(Daily_Return)) |>
      select(Date, Symbol, Daily_Return)

    returns_wide <- returns_long |>
      pivot_wider(names_from = Symbol, values_from = Daily_Return)

    if (nrow(returns_wide) == 0) {
      return(NULL)
    }

    returns_mat <- as.matrix(returns_wide[, setdiff(names(returns_wide), "Date"), drop = FALSE])
    if (ncol(returns_mat) < 2) {
      return(NULL)
    }

    corr_mat <- stats::cor(returns_mat, use = "pairwise.complete.obs")
    corr_df <- as.data.frame(as.table(corr_mat), stringsAsFactors = FALSE)
    names(corr_df) <- c("Symbol_X", "Symbol_Y", "Correlation")

    list(
      corr_matrix = corr_mat,
      corr_long = corr_df
    )
  })

  output$risk_correlation_heatmap <- renderPlotly({
    corr_data <- risk_correlation_data()
    validate(need(!is.null(corr_data), "Need at least 2 symbols with sufficient daily price history to compute correlation."))

    corr_df <- corr_data$corr_long
    symbol_order <- colnames(corr_data$corr_matrix)

    plot_ly(
      data = corr_df,
      x = ~factor(Symbol_X, levels = symbol_order),
      y = ~factor(Symbol_Y, levels = rev(symbol_order)),
      z = ~Correlation,
      type = "heatmap",
      zmin = -1,
      zmax = 1,
      colorscale = "RdBu",
      reversescale = TRUE,
      colorbar = list(title = "Corr")
    ) |>
      layout(
        template = "none",
        xaxis = list(title = "", tickangle = -45),
        yaxis = list(title = ""),
        paper_bgcolor = "#FFFFFF",
        plot_bgcolor = "#FFFFFF",
        margin = list(l = 90, r = 30, t = 20, b = 120)
      )
  })

  output$risk_return_table <- renderDT({
    data <- risk_return_data() |>
      select(Symbol, Sector, Accounts, Value, Return_1y, Volatility_1y, Sharpe_1y, Trading_Days) |>
      rename(
        Return_Period = Return_1y,
        Volatility_Period = Volatility_1y,
        Sharpe_Period = Sharpe_1y
      ) |>
      mutate(
        Return_Period = Return_Period / 100,
        Volatility_Period = Volatility_Period / 100
      )
    req(nrow(data) > 0)

    datatable(
      data,
      options = list(pageLength = 25, scrollX = TRUE),
      rownames = FALSE
    ) |>
      formatCurrency(columns = c("Value"), currency = "$", digits = 0) |>
      formatPercentage(columns = c("Return_Period", "Volatility_Period"), digits = 1) |>
      formatRound(columns = c("Sharpe_Period"), digits = 2) |>
      formatStyle(
        columns = c("Sharpe_Period"),
        backgroundColor = styleInterval(
          cuts = c(0, 1, 2),
          values = c("#ffcccc", "#ffffcc", "#ccffff", "#ccffcc")
        )
      )
  })

  action_queue_data <- reactive({
    table_data <- performance_table_data()
    req(nrow(table_data) > 0)

    abs_gain <- abs(table_data$`Gain/Loss`)
    contribution_threshold <- if (all(is.na(abs_gain))) {
      Inf
    } else {
      as.numeric(stats::quantile(abs_gain, probs = 0.75, na.rm = TRUE))
    }

    table_data |>
      mutate(
        PortfolioPctNum = as.numeric(`Portfolio%`),
        GainLossPctNum = as.numeric(`% Gain/Loss`) * 100,
        Return7dNum = as.numeric(`7d`),
        ContributionFlag = abs(`Gain/Loss`) >= contribution_threshold,
        DrawdownFlag = !is.na(GainLossPctNum) & GainLossPctNum <= -10 & PortfolioPctNum >= 2,
        RecentDropFlag = !is.na(Return7dNum) & Return7dNum <= -5 & PortfolioPctNum >= 1,
        OversizedFlag = !is.na(PortfolioPctNum) & PortfolioPctNum >= 12,
        AttentionScore =
          as.integer(DrawdownFlag) * 2L +
          as.integer(OversizedFlag) * 2L +
          as.integer(RecentDropFlag) * 1L +
          as.integer(ContributionFlag) * 1L
      ) |>
      rowwise() |>
      mutate(
        AttentionReason = paste(
          na.omit(c(
            if (DrawdownFlag) "Material drawdown" else NA_character_,
            if (OversizedFlag) "Oversized position" else NA_character_,
            if (RecentDropFlag) "7d decline" else NA_character_,
            if (ContributionFlag) "Large P/L contributor" else NA_character_
          )),
          collapse = "; "
        )
      ) |>
      ungroup() |>
      mutate(
        AttentionReason = ifelse(
          AttentionReason == "" | is.na(AttentionReason),
          "No critical flags",
          AttentionReason
        )
      ) |>
      select(
        Symbol, Sector, Accounts, Value, `Gain/Loss`, `% Gain/Loss`, `Portfolio%`, `7d`, `30d`, AttentionScore, AttentionReason
      ) |>
      arrange(desc(AttentionScore), desc(abs(`Gain/Loss`)))
  })

  output$contribution_bar <- renderPlotly({
    q_data <- action_queue_data() |>
      mutate(
        Contribution = `Gain/Loss`
      ) |>
      arrange(desc(abs(Contribution))) |>
      head(20) |>
      mutate(
        SymbolOrder = reorder(Symbol, Contribution),
        BarColor = ifelse(Contribution >= 0, "#1a9850", "#d73027"),
        HoverText = paste0(
          Symbol,
          "<br>Contribution: $",
          format(round(Contribution, 0), big.mark = ",", scientific = FALSE),
          "<br>Portfolio: ",
          round(`Portfolio%`, 1),
          "%",
          "<br>7d: ",
          round(`7d`, 1),
          "%",
          "<br>30d: ",
          round(`30d`, 1),
          "%"
        )
      )
    req(nrow(q_data) > 0)

    plot_ly(
      q_data,
      x = ~Contribution,
      y = ~SymbolOrder,
      type = "bar",
      orientation = "h",
      marker = list(color = q_data$BarColor),
      text = ~paste0("$", format(round(Contribution, 0), big.mark = ",", scientific = FALSE)),
      hovertext = ~HoverText,
      hoverinfo = "text",
      hovertemplate = paste0(
        "%{hovertext}<extra></extra>"
      ),
      showlegend = FALSE
    ) |>
      layout(
        xaxis = list(title = "Gain/Loss Contribution"),
        yaxis = list(title = ""),
        margin = list(l = 90, r = 20, t = 20, b = 50)
      )
  })

  output$attention_queue_table <- renderDT({
    data <- action_queue_data() |>
      filter(AttentionScore >= 2)

    if (nrow(data) == 0) {
      data <- data.frame(
        Note = "No high-priority flags for current filters.",
        stringsAsFactors = FALSE
      )
      return(datatable(data, options = list(dom = "t"), rownames = FALSE))
    }

    data <- data |>
      mutate(
        `Portfolio%` = `Portfolio%` / 100,
        `7d` = `7d` / 100,
        `30d` = `30d` / 100
      )

    datatable(
      data,
      options = list(pageLength = 15, scrollX = TRUE),
      rownames = FALSE
    ) |>
      formatCurrency(columns = c("Value", "Gain/Loss"), currency = "$", digits = 0) |>
      formatPercentage(columns = c("% Gain/Loss"), digits = 0) |>
      formatPercentage(columns = c("Portfolio%", "7d", "30d"), digits = 1)
  })

  output$monitor_queue_table <- renderDT({
    data <- action_queue_data() |>
      filter(AttentionScore < 2)

    if (nrow(data) == 0) {
      data <- data.frame(
        Note = "All current holdings are flagged as high-priority.",
        stringsAsFactors = FALSE
      )
      return(datatable(data, options = list(dom = "t"), rownames = FALSE))
    }

    data <- data |>
      mutate(
        `Portfolio%` = `Portfolio%` / 100,
        `7d` = `7d` / 100,
        `30d` = `30d` / 100
      )

    datatable(
      data,
      options = list(pageLength = 20, scrollX = TRUE),
      rownames = FALSE
    ) |>
      formatCurrency(columns = c("Value", "Gain/Loss"), currency = "$", digits = 0) |>
      formatPercentage(columns = c("% Gain/Loss"), digits = 0) |>
      formatPercentage(columns = c("Portfolio%", "7d", "30d"), digits = 1)
  })
  
  # Add session statistics output
  output$session_stats <- renderText({
    paste0(
      "Portfolio symbols: ", nrow(portfolio_data_r()), "\n",
      "Symbols with price data: ", length(unique(price_data_r()$Symbol)), "\n",
      "Total price records: ", nrow(price_data_r()), "\n",
      "Performance calculations: ", nrow(performance_data_r()), "\n",
      "Data loaded: ", format(Sys.time(), "%Y-%m-%d %H:%M:%S")
    )
  })

  output$data_source_validation <- renderDT({
    validation_source <- holdings_source_summary_r()
    req(nrow(validation_source) > 0)

    validation_data <- validation_source |>
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
