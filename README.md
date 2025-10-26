# Stock Portfolio Monitoring App

A Shiny application for monitoring stock portfolio performance with data sourced from Google Sheets and real-time price feeds.

## Features

- **Google Sheets Integration**: Automatically loads portfolio data from your TD Holdings sheet
- **Real-time Price Data**: Fetches current and historical prices from Yahoo Finance
- **Performance Heatmap**: Visual representation of gains/losses across multiple time periods
- **Multi-Account Support**: Aggregates holdings across different accounts
- **Interactive Dashboard**: Filter, sort, and analyze your portfolio performance

## Prerequisites

### Required R Packages
- shiny
- shinydashboard  
- DT
- plotly
- dplyr
- ggplot2
- googlesheets4
- quantmod
- lubridate
- stringr
- tidyr

### Google Sheets Setup

1. **Service Account Credentials**:
   - You need a Google service account JSON credentials file
   - Place this file at: `creds/original-return-107905-3b03bf4c17bf.json`
   - The service account email must have access to your Google Sheet

2. **Sheet Structure**:
   - **Sheet URL**: `https://docs.google.com/spreadsheets/d/1oievySvQ3m2ojs1On27EKpZ4rqrbd0Ksi_rnQf8YMyY/edit?usp=sharing`
   - **Sheet Name**: `TD Holdings`
   - **Required Columns**:
     - `Date`: Date of the data (app automatically uses most recent)
     - `Symbol`: Stock ticker symbols
     - `Quantity`: Number of shares held
     - `Account`: Account identifier (for aggregation across accounts)

## Installation & Setup

1. **Clone or download** the app files to your working directory

2. **Create credentials directory**:
   ```r
   source("app_setup.R")
   create_directories()
   ```

3. **Place your credentials file** at `creds/original-return-107905-3b03bf4c17bf.json`

4. **Run setup check**:
   ```r
   setup_app()
   ```
   This will:
   - Install missing R packages
   - Verify credentials file
   - Test Google Sheets connection

5. **Launch the application**:
   ```r
   launch_app()
   ```

## Usage

### Loading Data
## File Structure

```
├── ui.R                 # Shiny UI definition
├── server.R             # Shiny server logic  
├── app_setup.R          # Setup and utility functions
├── README.md            # This documentation
└── creds/
    └── credentials.json # Google service account credentials (you provide)
```

## Data Processing Details

### Portfolio Data Processing
1. Reads TD Holdings sheet from Google Sheets
2. Converts Date column to proper date format
3. Filters for most recent date only
4. Cleans and standardizes symbol names (uppercase, trimmed)
5. Aggregates quantities across accounts for each symbol
6. Removes invalid entries (missing symbols, zero quantities)

### Price Data Processing
1. Fetches historical price data from Yahoo Finance
2. Attempts multiple symbol formats (e.g., adds .TO for TSX stocks)
3. Uses adjusted close prices when available
4. Calculates percentage changes for each time period
5. Handles missing data gracefully

## Troubleshooting

### Authentication Issues
- Ensure `creds/<credentials-file>.json` exists and is valid
- Verify the service account email has been granted access to your Google Sheet
- Check that the sheet URL and sheet name are correct

### Missing Price Data
- Some symbols may not be available on Yahoo Finance
- Canadian stocks may need `.TO` suffix (automatically attempted)
- Check symbol spelling and format

### Data Loading Issues
- Verify column names match exactly: Date, Symbol, Quantity, Account
- Ensure Date column contains valid dates
- Check that Quantity column contains numeric values

### Performance Issues
- Large portfolios (100+ symbols) may take longer to load
- Consider filtering to focus on specific symbols of interest
- Price data is cached during the session to improve performance

## Customization

### Adding New Time Periods
Edit the `periods` list in `server.R`:
```r
periods <- list(
  "1d" = 1,
  "7d" = 7,
  "30d" = 30,
  "90d" = 90,
  "6m" = 180,
  "1y" = 365,
  "2y" = 730  # Add new period
)
```

### Changing Data Source
To use a different Google Sheet:
1. Update the `sheet_url` in `server.R`
## Support

For issues related to:
- **Google Sheets API**: Check Google Cloud Console and service account setup
- **Yahoo Finance Data**: Verify symbol formats and availability
- **R Package Issues**: Ensure all dependencies are installed and up to date

The app provides notification messages for most common issues and their likely causes.
- Check the R console for detailed error messages
- Use the "Data Management" tab to verify data loading status

## Customization

### Adding More Time Periods
To add additional time periods, modify both `ui.R` and `server.R`:

1. In `ui.R`, add new choices to the `time_periods` checkboxGroupInput
2. In `server.R`, update the `calculate_performance()` function with new lag periods

### Styling
- Modify CSS styling in `ui.R` 
- Customize ggplot2 themes in the heatmap rendering code
- Adjust color schemes in the heatmap scale_fill_gradient2

### Additional Metrics
Extend the `calculate_performance()` function to include:
- Volatility calculations
- Risk-adjusted returns
- Correlation analysis

## Support

For questions about:
- **Shiny functionality**: Use `@shiny` command in Positron chat
- **Quarto documentation**: Visit https://quarto.org/docs/guide/
- **Positron features**: Visit https://positron.posit.co/