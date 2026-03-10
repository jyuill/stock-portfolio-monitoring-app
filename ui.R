library(shiny)
library(shinydashboard)
library(DT)
library(plotly)

ui <- dashboardPage(
  dashboardHeader(title = "Stock Portfolio Monitor"),
  
  dashboardSidebar(
    sidebarMenu(
      menuItem("Portfolio Overview", tabName = "overview", icon = icon("chart-line")),
      menuItem("Performance Analysis", tabName = "performance", icon = icon("table")),
      menuItem("Portfolio Details", tabName = "details", icon = icon("list")),
      menuItem("Settings", tabName = "settings", icon = icon("cog"))
    ),
    hr(),
    h4("Filters", style = "margin-left: 6px;"),
    textInput("symbol_filter", 
              "Symbols (comma-separated)",
              placeholder = "e.g., AAPL, GOOGL, TSLA"),
    selectInput("sector_filter", "Sector:",
                choices = c("All"),
                selected = "All"),
    selectInput("account_filter", "Account:",
                choices = c("All"),
                selected = "All"),
    radioButtons("currency_view", "Currency View",
                 choices = c("Native (as sourced)" = "native",
                             "CAD (convert USD)" = "cad"),
                 selected = "native"),
    selectInput("sort_by", "Sort by:",
                choices = list("Symbol" = "Symbol",
                               "1 Day" = "1d",
                               "7 Day" = "7d", 
                               "30 Day" = "30d",
                               "90 Day" = "90d",
                               "6 Month" = "6m",
                               "1 Year" = "1y"),
                selected = "Symbol"),
    actionButton(
      "refresh_data",
      "Refresh Data",
      icon = icon("rotate"),
      width = "auto",
      class = "btn-success",
      style = "display: block; width: calc(100% - 24px); margin: 8px 12px 0 12px; padding: 8px 12px; background-color: #b9e9c9; border-color: #93d7ab; color: #1f5132; font-weight: 600;"
    ),
    tags$p(
      "Note: amounts do not include cash held in accounts",
      style = "font-size: 12px; color: #8f98a3; margin-top: 8px; margin-left: 6px;"
    )
  ),
  
  dashboardBody(
    tags$head(
      tags$style(HTML("
        .content-wrapper, .right-side {
          background-color: #f8f9fa;
        }
        .box {
          box-shadow: 0 1px 3px rgba(0,0,0,0.12), 0 1px 2px rgba(0,0,0,0.24);
        }
        .small-box {
          border-radius: 5px;
        }
      ")),
      # additional custom CSS
      tags$link(rel = "stylesheet", type = "text/css", href = "custom.css")
    ),
    
    tabItems(
      # Portfolio Overview Tab
      tabItem(tabName = "overview",
        fluidRow(
          valueBoxOutput("overview_total_value", width = 4),
          valueBoxOutput("overview_total_investment", width = 4),
          valueBoxOutput("overview_total_gain_loss", width = 4)
        ),
        
        fluidRow(
          box(
            title = "Portfolio Value by Account", status = "primary", solidHeader = TRUE,
            width = 6,
            plotlyOutput("account_value_donut", height = "320px")
          ),
          box(
            title = "Portfolio Value by Sector", status = "success", solidHeader = TRUE,
            width = 6,
            plotlyOutput("sector_value_donut", height = "320px")
          )
        ),

        fluidRow(
          box(
            title = "Account Value vs Gain/Loss", status = "warning", solidHeader = TRUE,
            width = 12,
            plotlyOutput("account_value_gain_bar", height = "320px")
          )
        ),

        fluidRow(
          box(
            title = "Account Breakdown", status = "info", solidHeader = TRUE,
            width = 6,
            DTOutput("account_breakdown_table")
          ),
          box(
            title = "Sector Breakdown", status = "info", solidHeader = TRUE,
            width = 6,
            DTOutput("sector_breakdown_table")
          )
        )
      ),

      # Performance Analysis Tab  
      tabItem(tabName = "performance",
        fluidRow(
          box(
            title = "Detailed Performance Table", status = "primary", solidHeader = TRUE,
            width = 12,
            DTOutput("performance_table")
          )
        ),
        
        fluidRow(
          box(
            title = "Performance Legend", status = "info", solidHeader = TRUE,
            width = 12,
            h5("Color Coding:"),
            tags$ul(
              tags$li(tags$span("Red: ", style = "color: red; font-weight: bold;"), "Negative performance (losses)"),
              tags$li(tags$span("White: ", style = "color: black; font-weight: bold;"), "Neutral performance (around 0%)"),
              tags$li(tags$span("Green: ", style = "color: green; font-weight: bold;"), "Positive performance (gains)")
            ),
            h5("Time Periods:"),
            tags$ul(
              tags$li("1d: 1 day performance"),
              tags$li("7d: 1 week performance"),
              tags$li("30d: 1 month performance"),
              tags$li("90d: 3 month performance"),
              tags$li("6m: 6 month performance"),
              tags$li("1y: 1 year performance")
            )
          )
        )
      ),

      # Portfolio Details Tab
      tabItem(tabName = "details",
        fluidRow(
          box(
            title = "Sector Allocation by Account", status = "primary", solidHeader = TRUE,
            width = 12,
            uiOutput("account_sector_pies_ui")
          )
        ),
        fluidRow(
          box(
            title = "Account-Level Holdings Details", status = "info", solidHeader = TRUE,
            width = 12,
            DTOutput("portfolio_details_table")
          )
        )
      ),
      
      # Settings Tab
      tabItem(tabName = "settings",
        fluidRow(
          box(
            title = "Application Settings", status = "primary", solidHeader = TRUE,
            width = 6,
            h4("Data Configuration"),
            p(strong("Google Sheets URL:"), br(),
              "https://docs.google.com/spreadsheets/d/1oievySvQ3m2ojs1On27EKpZ4rqrbd0Ksi_rnQf8YMyY/edit"),
            p(strong("Sheet Name:"), "TD Holdings"),
            p(strong("Authentication:"), "Service Account (JSON credentials)"),
            p(strong("Credentials File:"), "creds/original-return-107905-3b03bf4c17bf.json"),
            
            hr(),
            h4("Data Processing"),
            tags$ul(
              tags$li("Automatically filters for most recent date"),
              tags$li("Aggregates quantities across accounts"),
              tags$li("Cleans and standardizes symbol names"),
              tags$li("Fetches real-time price data from Yahoo Finance")
            )
          ),
          
          box(
            title = "Portfolio Statistics", status = "info", solidHeader = TRUE,
            width = 6,
            h4("Current Session"),
            verbatimTextOutput("session_stats"),
            
            hr(),
            h4("Source Validation"),
            DTOutput("data_source_validation"),
            
            hr(),
            h4("Data Quality"),
            p("The application will show notifications for:"),
            tags$ul(
              tags$li("Symbols that couldn't be found"),
              tags$li("Missing price data"),
              tags$li("Authentication issues"),
              tags$li("Data loading progress")
            )
          )
        ),
        
        fluidRow(
          box(
            title = "Troubleshooting", status = "warning", solidHeader = TRUE,
            width = 12,
            h4("Common Issues:"),
            tags$ul(
              tags$li(strong("Authentication Error:"), "Ensure creds/original-return-107905-3b03bf4c17bf.json exists and has proper permissions"),
              tags$li(strong("No Data Loaded:"), "Check that the Google Sheet is accessible with the service account email"),
              tags$li(strong("Missing Symbols:"), "Some symbols may not be available on Yahoo Finance - check symbol format"),
              tags$li(strong("Canadian Stocks:"), "TSX symbols may need '.TO' suffix (automatically attempted)")
            ),
            
            h4("Data Requirements:"),
            tags$ul(
              tags$li("Date column: Must be in recognizable date format"),
              tags$li("Symbol column: Stock ticker symbols"),
              tags$li("Quantity column: Number of shares held"),
              tags$li("Account column: Account identifier (for aggregation)")
            )
          )
        )
      )
    )
  )
)
