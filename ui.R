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
      menuItem("Settings", tabName = "settings", icon = icon("cog"))
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
      "))
    ),
    
    tabItems(
      # Portfolio Overview Tab
      tabItem(tabName = "overview",
        fluidRow(
          box(
            title = "Portfolio Status", status = "primary", solidHeader = TRUE,
            width = 12,
            textOutput("portfolio_summary"),
            br(),
            actionButton("refresh_data", "Refresh Data", 
                        class = "btn-primary", icon = icon("sync"))
          )
        ),
        
        fluidRow(
          box(
            title = "Performance Filters", status = "info", solidHeader = TRUE,
            width = 4,
            textInput("symbol_filter", 
                     "Filter Symbols (comma-separated):",
                     placeholder = "e.g., AAPL, GOOGL, TSLA"),
            selectInput("sort_by", "Sort by:",
                       choices = list("Symbol" = "Symbol",
                                    "1 Day" = "1d",
                                    "7 Day" = "7d", 
                                    "30 Day" = "30d",
                                    "90 Day" = "90d",
                                    "6 Month" = "6m",
                                    "1 Year" = "1y"),
                       selected = "Symbol"),
            hr(),
            h5("Data Source:"),
            p("TD Holdings sheet from Google Sheets"),
            p("Filters for most recent date automatically"),
            p("Aggregates holdings across all accounts")
          ),
          
          box(
            title = "Performance Heatmap", status = "success", solidHeader = TRUE,
            width = 8,
            plotlyOutput("performance_heatmap", height = "500px")
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