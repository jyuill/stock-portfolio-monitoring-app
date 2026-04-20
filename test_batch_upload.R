# quick test for running a batch of uploads to gsheets
# from the downloads folder, using the file_id to identify the source file
# code and functions are in upload_holdings.R
# includes the HOLDINGS_SOURCES table to match with funcion call
source('upload_holdings.R')

# issue with row start not updating
# looks like the range is not updating with the new data, so it is always starting at row 5
for (i in 1:nrow(HOLDINGS_SOURCES)) {
  upload_holdings(
    file_id = HOLDINGS_SOURCES$file_id[i]
  )
}

# individual run for testing
upload_holdings(
   file_id = HOLDINGS_SOURCES$file_id[5]
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