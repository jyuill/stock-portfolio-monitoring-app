# Extract portfolio info from bank statements
# by running extract_stmt.R function on every statement in stmts/
# that hasn't already been processed, storing results in csv,
# then upload the updated csv to the 'portfolio_mthly' gsheet tab.

library(tidyverse)

source('extract_stmt.R')
source('upload_holdings.R')

process_stmts()
upload_portfolio_monthly()