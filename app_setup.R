# Stock Portfolio Monitoring App Setup
# This script helps set up the environment and launch the Shiny app

# Load required libraries
required_packages <- c(
  "shiny", "shinydashboard", "DT", "plotly", "dplyr", 
  "ggplot2", "googlesheets4", "quantmod", "lubridate", 
  "stringr", "tidyr"
)

# Function to install missing packages
install_missing_packages <- function() {
  missing_packages <- required_packages[!required_packages %in% installed.packages()[,"Package"]]
  
  if(length(missing_packages) > 0) {
    cat("Installing missing packages:", paste(missing_packages, collapse = ", "), "\n")
    install.packages(missing_packages)
  } else {
    cat("All required packages are already installed.\n")
  }
}

# Function to check credentials setup
check_credentials <- function() {
  creds_path <- "creds/original-return-107905-3b03bf4c17bf.json"
  
  if (file.exists(creds_path)) {
    cat("✓ Credentials file found at:", creds_path, "\n")
    
    # Try to read and validate the JSON structure
    tryCatch({
      creds <- jsonlite::fromJSON(creds_path)
      required_fields <- c("type", "project_id", "private_key_id", "private_key", "client_email")
      
      if (all(required_fields %in% names(creds))) {
        cat("✓ Credentials file appears to be valid\n")
        cat("✓ Service account email:", creds$client_email, "\n")
        cat("\nMake sure this email has been granted access to your Google Sheet!\n")
        return(TRUE)
      } else {
        cat("✗ Credentials file is missing required fields\n")
        cat("Required fields:", paste(required_fields, collapse = ", "), "\n")
        return(FALSE)
      }
    }, error = function(e) {
      cat("✗ Error reading credentials file:", e$message, "\n")
      return(FALSE)
    })
  } else {
    cat("✗ Credentials file not found at:", creds_path, "\n")
    cat("Please ensure you have placed your Google service account JSON file there.\n")
    return(FALSE)
  }
}

# Function to test Google Sheets connection
test_sheets_connection <- function() {
  cat("\n=== Testing Google Sheets Connection ===\n")
  
  if (!check_credentials()) {
    return(FALSE)
  }
  
  tryCatch({
    # Load required library
    library(googlesheets4)
    
    # Authenticate with service account - use correct path
    gs4_auth(path = "creds/original-return-107905-3b03bf4c17bf.json")
    cat("✓ Authentication successful\n")
    
    # Test reading the sheet starting from A3
    sheet_url <- "https://docs.google.com/spreadsheets/d/1oievySvQ3m2ojs1On27EKpZ4rqrbd0Ksi_rnQf8YMyY/edit?usp=sharing"
    
    cat("Testing connection to TD Holdings sheet (starting from A3)...\n")
    test_data <- read_sheet(sheet_url, sheet = "TD Holdings", range = "A3:G1000", col_names = TRUE)
    
    cat("✓ Successfully connected to Google Sheet\n")
    cat("✓ Sample data preview (first few rows from A3):\n")
    print(head(test_data, 3))
    
    # Check for required columns after cleaning
    available_cols <- names(test_data)
    cat("✓ Available columns:", paste(available_cols, collapse = ", "), "\n")
    
    # Look for expected column patterns
    required_patterns <- c("Date", "Symbol", "Quantity", "Account")
    found_cols <- sapply(required_patterns, function(pattern) {
      any(grepl(pattern, available_cols, ignore.case = TRUE))
    })
    
    if (all(found_cols)) {
      cat("✓ All required column patterns found\n")
      
      # Test filtering for actual data
      clean_data <- test_data |>
        filter(!if_all(everything(), is.na)) |>
        filter(!is.na(Date))
      
      cat("✓ Found", nrow(clean_data), "rows of actual data\n")
      
      if (nrow(clean_data) > 0) {
        cat("✓ Most recent date:", format(max(as.Date(clean_data$Date), na.rm = TRUE), "%Y-%m-%d"), "\n")
      }
      
      return(TRUE)
    } else {
      missing_patterns <- names(found_cols)[!found_cols]
      cat("✗ Missing column patterns:", paste(missing_patterns, collapse = ", "), "\n")
      return(FALSE)
    }
    
  }, error = function(e) {
    cat("✗ Connection test failed:", e$message, "\n")
    cat("\nPossible issues:\n")
    cat("1. Service account email not granted access to the sheet\n")
    cat("2. Sheet URL or sheet name is incorrect\n") 
    cat("3. Network connectivity issues\n")
    cat("4. Data structure changed in the sheet\n")
    return(FALSE)
  })
}

# Function to launch the app
launch_app <- function() {
  cat("\n=== Launching Stock Portfolio Monitor ===\n")
  
  # Check if all required files exist
  required_files <- c("ui.R", "server.R", "global_data.R")
  missing_files <- required_files[!file.exists(required_files)]
  
  if (length(missing_files) > 0) {
    cat("✗ Missing required files:", paste(missing_files, collapse = ", "), "\n")
    return(FALSE)
  }
  
  cat("✓ All app files found\n")
  
  # Install missing packages
  install_missing_packages()
  
  # Test connection (optional - app will handle this)
  cat("\nTesting Google Sheets connection (optional)...\n")
  test_sheets_connection()
  
  # Load global data before starting the app
  cat("\n📊 Loading portfolio data...\n")
  tryCatch({
    source("global_data.R")
    cat("✓ Portfolio data loaded successfully\n")
    cat("✓ Found", nrow(PORTFOLIO_DATA), "unique symbols\n")
    cat("✓ Data from:", format(PORTFOLIO_DATE, "%Y-%m-%d"), "\n")
  }, error = function(e) {
    cat("✗ Error loading portfolio data:", e$message, "\n")
    cat("Please check your credentials and Google Sheets access.\n")
    return(FALSE)
  })
  
  # Launch the app
  cat("\n🚀 Starting Shiny app...\n")
  cat("The app will open in your default browser.\n")
  cat("Use the 'Refresh Data' button to load price data.\n\n")
  
  shiny::runApp(launch.browser = TRUE)
}

# Setup function for first-time users
setup_app <- function() {
  cat("=== Stock Portfolio Monitor Setup ===\n\n")
  
  cat("1. Checking required packages...\n")
  install_missing_packages()
  
  cat("\n2. Checking credentials setup...\n")
  check_credentials()
  
  cat("\n3. Testing Google Sheets connection...\n")
  test_sheets_connection()
  
  cat("\n=== Setup Complete ===\n")
  cat("Run launch_app() to start the application.\n")
}

# Helper function to create directory structure
create_directories <- function() {
  if (!dir.exists("creds")) {
    dir.create("creds")
    cat("Created 'creds' directory\n")
    cat("Please place your Google service account JSON file in: creds/credentials.json\n")
  }
}
# Print welcome message when script is sourced
cat("Stock Portfolio Monitoring App - Setup Script\n")
cat("=============================================\n\n")
cat("Available functions:\n")
cat("• setup_app()           - Run complete setup check\n")
cat("• launch_app()          - Launch the Shiny application\n")
cat("• test_sheets_connection() - Test Google Sheets access\n")
cat("• create_directories()  - Create required directories\n\n")
cat("Quick start: run setup_app() then launch_app()\n")

# Uncomment the line below to launch the app immediately:
# launch_app()