# Extract portfolio info from bank statements
# download stmt to /downloads folder
# extract_stmt.R functions will:
# - automatically move file to /stmts from /downloads
# - process every statement in stmts/
# that hasn't already been processed
# - store results in csv.
# upload_holdings.R function will:
# - upload the updated csv to the 'portfolio_mthly' gsheet tab.

library(tidyverse)
# mthly portfolio balance stmts
source('extract_stmt.R') 
# portfolio holdings snapshots + fn for uploading mthly stmts
source('upload_holdings.R') 

# process mthly stmts into consistent format
process_stmts()
# in upload_holdings.R: run to upload mthly stmts to gsheet
upload_portfolio_monthly()