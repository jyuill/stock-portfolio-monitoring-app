# Running a batch of uploads to gsheets
# 1. export files from brokers to downloads folder
# 2. run this script to leverage upload_holdings.R functions and upload to gsheets

# upload_holdings.R: source for functions
# includes the HOLDINGS_SOURCES table to match with funcion call
source('upload_holdings.R')

# AFTER EXPORTING FILES TO DOWNLOADS FOLDER, RUN THIS TO UPLOAD TO GSHEETS
for (i in 1:nrow(HOLDINGS_SOURCES)) {
  upload_holdings(
    file_id = HOLDINGS_SOURCES$file_id[i]
  )
}
# DONE: unless errors, everything should be uploaded to gsheets

#============
# individual run for testing
hs <- 5 # identify file name by row in HOLDINGS_SOURCES
upload_holdings(
   file_id = HOLDINGS_SOURCES$file_id[hs]
)

# test individual functions, if bugs

file_id <- HOLDINGS_SOURCES$file_id[1]
downloads_dir <- "~/Downloads"
sheet_url <- SHEET_URL
tab_name <- HOLDINGS_SOURCES$sheet_tab[1]

existing <- find_existing_snapshot_rows(
  
)
empty <- next_empty_row(
  sheet_url = sheet_url,
  tab_name = HOLDINGS_SOURCES$sheet_tab[1]
)