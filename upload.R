# Running a batch of uploads to gsheets
# For Portfolio holdings snapshots
# - separate from mthly Portfolio Balance stmts
# 1. export files from brokers to downloads folder
#   - see instructions in Invest Money Mgmt googlesheet 
# 2. run this script to leverage upload_holdings.R functions and upload to gsheets

library(tidyverse)

# MAIN SECTION
# - run this section AFTER exporting files from broker accts

# upload_holdings.R: source for functions
# includes the HOLDINGS_SOURCES table to match with funcion call
source('upload_holdings.R')

# AFTER EXPORTING FILES TO DOWNLOADS FOLDER, RUN THIS TO UPLOAD TO GSHEETS
# each call is wrapped in tryCatch so a problem with one file (malformed export,
# Sheets error, etc.) is reported but doesn't stop the remaining uploads.
# missing files are already handled inside upload_holdings() (message + skip).
failed <- character(0)
for (i in seq_len(nrow(HOLDINGS_SOURCES))) {
  fid <- HOLDINGS_SOURCES$file_id[i]
  tryCatch(
    upload_holdings(file_id = fid),
    error = function(e) {
      cat("  \U2717 ERROR for ", fid, ": ", conditionMessage(e), "\n\n", sep = "")
      failed <<- c(failed, fid)
    }
  )
}
if (length(failed) > 0) {
  cat("Uploads with errors (fix and re-run via upload_individual.R):",
      paste(failed, collapse = ", "), "\n")
} else {
  cat("All available files processed without errors.\n")
}
# DONE: unless errors, everything should be uploaded to gsheets
# - ready to be viewed in portfolio tracker app
# - if errors or testing needed: upload_individual.R