# Upload broker-export holdings CSVs into the portfolio Google Sheet.
#
# Usage (interactive):
#   source("upload_holdings.R")
#   upload_holdings("20LWH2S")                   # picks latest matching CSV in ~/Downloads
#   upload_holdings("20LWH2S", dry_run = TRUE)   # parse + preview only, no write
#
# Usage (command line):
#   Rscript upload_holdings.R 20LWH2S
#   Rscript upload_holdings.R 20LWH2S ~/some/other/downloads
#
# Auth: reuses the same service-account pattern as global_data.R
#   (GS4_SERVICE_ACCOUNT_JSON env var, then GS4_SERVICE_ACCOUNT_PATH file).
#
# Adding a new broker CSV source: add a row to HOLDINGS_SOURCES below.
# The file_id must be a unique substring of the CSV filename
# (e.g. "20LWH2S" matches "20LWH2S-holdings-17-Apr-2026.csv").
#
# Duplicate-run behaviour: if the target tab already contains rows with the
# same Date + Account as the CSV, those rows are deleted (shifted up) before
# the new rows are appended. Matching is by (Date, Account) so uploading one
# account never touches other accounts sharing the same tab.

suppressPackageStartupMessages({
  library(dplyr)
  library(tidyr)
  library(readr)
  library(stringr)
  library(lubridate)
  library(tibble)
  library(googlesheets4)
})

# ---- Config -----------------------------------------------------------------

SHEET_URL <- "https://docs.google.com/spreadsheets/d/1oievySvQ3m2ojs1On27EKpZ4rqrbd0Ksi_rnQf8YMyY/edit"

# Lookup: CSV filename identifier -> account label (col B) and target sheet tab.
# Account label should match the value already used in column B of the target tab
# so existing filters / joins continue to work.
# Add a row here as each new broker CSV format is handled.
HOLDINGS_SOURCES <- tribble(
  ~file_id,   ~account,                         ~sheet_tab, ~header_row,
  "20LWH2S",  "TD-RSP",  "TD Holdings",         5
   , "603978A",  "TD-CDN",          "TD Holdings", 5
   , "603978B",  "TD-USD",          "TD Holdings", 5
   , "603978J",  "TD-TFSA",          "TD Holdings", 5
  # add sources as needed:
  # , "<file identifier in downloads folder>",  "<account label>",  "<target gsheet tab>"
)

# ---- Auth (mirrors global_data.R::auth_google_sheets) -----------------------

auth_google_sheets <- function() {
  sa_json <- trimws(Sys.getenv("GS4_SERVICE_ACCOUNT_JSON", ""))
  sa_path <- trimws(Sys.getenv(
    "GS4_SERVICE_ACCOUNT_PATH",
    "creds/original-return-107905-3b03bf4c17bf.json"
  ))

  if (nzchar(sa_json)) {
    temp_auth_file <- tempfile(fileext = ".json")
    writeLines(sa_json, temp_auth_file, useBytes = TRUE)
    on.exit(unlink(temp_auth_file), add = TRUE)
    gs4_auth(path = temp_auth_file)
    return(invisible(TRUE))
  }

  if (file.exists(sa_path)) {
    gs4_auth(path = sa_path)
    return(invisible(TRUE))
  }

  stop(
    "Google Sheets credentials not found. Set GS4_SERVICE_ACCOUNT_JSON ",
    "or provide a credentials file at GS4_SERVICE_ACCOUNT_PATH."
  )
}

# ---- CSV discovery ----------------------------------------------------------

find_latest_csv <- function(file_id, downloads_dir = "~/Downloads") {
  dir_norm <- path.expand(downloads_dir)
  if (!dir.exists(dir_norm)) {
    stop("Downloads directory not found: ", dir_norm)
  }
  pattern <- sprintf("^%s-holdings-.*\\.csv$", file_id)
  files <- list.files(dir_norm, pattern = pattern, full.names = TRUE)
  if (length(files) == 0) {
    stop("No CSVs matching '", pattern, "' in ", dir_norm)
  }
  info <- file.info(files)
  files[order(info$mtime, decreasing = TRUE)][1]
}

# ---- CSV parsing ------------------------------------------------------------

# The broker CSV has a preamble (As-of date, Account, Cash, Investments,
# Total Value, Margin, ...) then a header row starting with "Symbol", then
# data rows. This parser finds the "Symbol" header row and returns:
#   - as_of_date: Date (from the "As of Date" preamble row, B1)
#   - account_raw: character (from the "Account" preamble row, B2)
#   - data: tibble of data rows with the original CSV column names
parse_holdings_csv <- function(csv_path) {
  lines <- read_lines(csv_path)

  as_of_line <- lines[str_detect(lines, "^As of Date,")]
  if (length(as_of_line) == 0) {
    stop("Could not find 'As of Date' row in ", csv_path)
  }
  as_of_raw <- str_trim(str_remove(as_of_line[1], "^As of Date,"))
  # Typical format: 2026-04-17 13:10:38 — take just the date part.
  as_of_date <- suppressWarnings(as.Date(str_sub(as_of_raw, 1, 10)))
  if (is.na(as_of_date)) {
    stop("Could not parse As-of date from '", as_of_raw, "'")
  }

  account_line <- lines[str_detect(lines, "^Account,")]
  account_raw <- if (length(account_line) > 0) {
    str_trim(str_remove(account_line[1], "^Account,"))
  } else {
    NA_character_
  }

  header_idx <- which(str_detect(lines, "^Symbol,"))
  if (length(header_idx) == 0) {
    stop("Could not find 'Symbol,' header row in ", csv_path)
  }
  header_idx <- header_idx[1]

  data <- suppressMessages(read_csv(
    csv_path,
    skip = header_idx - 1,
    show_col_types = FALSE,
    progress = FALSE
  ))

  # Drop any fully-empty trailing rows.
  data <- data |> filter(if_any(everything(), ~ !is.na(.)))

  list(
    as_of_date = as_of_date,
    account_raw = account_raw,
    data = data
  )
}

# ---- Target-tab introspection ----------------------------------------------

# Coerce a character vector of sheet-read values into Date. Handles both ISO
# strings ("2026-04-17") and numeric serial dates stored as strings ("46129",
# Google Sheets epoch = 1899-12-30). Anything else returns NA.
to_date_safe <- function(x) {
  # Empty strings -> NA so they don't reach as.Date() / as.numeric()
  x[!is.na(x) & x == ""] <- NA_character_
  # Explicit format avoids charToDate() hard-erroring on non-date strings
  # (e.g. the "Date" header in row 5); unparseable values return NA silently.
  iso <- suppressWarnings(as.Date(x, format = "%Y-%m-%d"))
  as_num <- suppressWarnings(as.numeric(x))
  # Sanity range: 20000 = 1954-10-05, 80000 = 2119-01-02
  serial_ok <- !is.na(as_num) & as_num >= 20000 & as_num <= 80000
  from_serial <- rep(as.Date(NA), length(x))
  from_serial[serial_ok] <- as.Date(as_num[serial_ok], origin = "1899-12-30")
  coalesce(iso, from_serial)
}

# Return 1-based row numbers in target tab where Date (col A) matches the given
# date AND Account (col B) matches the given account. Reads columns A:B only.
find_existing_snapshot_rows <- function(sheet_url, tab_name, match_date, match_account) {
  existing <- read_sheet(
    sheet_url,
    sheet = tab_name,
    range = "A:B",
    col_names = FALSE,
    col_types = "cc"
  )
  if (nrow(existing) == 0) return(integer(0))

  dates <- to_date_safe(existing[[1]])
  accts <- if (ncol(existing) >= 2) existing[[2]] else rep(NA_character_, nrow(existing))

  which(
    !is.na(dates) & dates == match_date &
      !is.na(accts) & accts == match_account
  )
}

# Return the 1-based row number where new data should be written. This is one
# past the last non-empty row in column C (Symbol) on the target tab.
# THIS DOES NOT WORK WITH C - can be blank symbols
# try with E - has data in all rows, so next empty row is always last row + 1
next_empty_row <- function(sheet_url, tab_name, header_row) {
  col_new_row <- read_sheet(
    sheet_url,
    sheet = tab_name,
    range = "E:E",
    col_names = TRUE,
    col_types = "c"
  )
  if (nrow(col_new_row) == 0) {
    # Tab is empty — start at row determined by header_row.
    return(header_row + 1L)
  }
  # careful to use col that has no empty values
  non_empty <- which(!is.na(col_new_row[[1]]) & col_new_row[[1]] != "")
  if (length(non_empty) == 0) header_row + 1L else max(non_empty) + header_row + 1L
}

# ---- Main -------------------------------------------------------------------

upload_holdings <- function(
  file_id,
  downloads_dir = "~/Downloads",
  sheet_url = SHEET_URL,
  dry_run = FALSE
) {
  cat("=== Upload holdings: ", file_id, " ===\n", sep = "")
  # get holding source info that matches file_id; error if not exactly one match
  cfg <- HOLDINGS_SOURCES |> filter(.data$file_id == !!file_id)
  if (nrow(cfg) == 0) {
    stop(
      "No HOLDINGS_SOURCES entry for file_id '", file_id,
      "'. Add a row to HOLDINGS_SOURCES in upload_holdings.R."
    )
  }
  if (nrow(cfg) > 1) {
    stop("Multiple HOLDINGS_SOURCES entries match file_id '", file_id, "'")
  }
  cat("  Account   :", cfg$account, "\n")
  cat("  Sheet tab :", cfg$sheet_tab, "\n")

  csv_path <- find_latest_csv(file_id, downloads_dir)
  cat("  CSV       :", csv_path, "\n")

  parsed <- parse_holdings_csv(csv_path)
  cat("  As of     :", format(parsed$as_of_date, "%Y-%m-%d"), "\n")
  cat("  Data rows :", nrow(parsed$data), "\n")

  if (!is.na(parsed$account_raw) && parsed$account_raw != cfg$account) {
    cat(
      "  ! CSV Account ('", parsed$account_raw,
      "') differs from configured account ('", cfg$account,
      "'). Using configured value.\n",
      sep = ""
    )
  }

  # Build upload tibble: Date, Account, then all CSV columns as-is.
  # Date is written as an ISO character string so Sheets displays it as text
  # (rather than a bare serial number like 46129) while remaining parseable via
  # as.Date() on the read side in global_data.R.
  upload_df <- parsed$data |>
    mutate(
      Date = format(parsed$as_of_date, "%Y-%m-%d"),
      Account = cfg$account,
      .before = 1
    )
  cat("  Upload    :", nrow(upload_df), "rows x ", ncol(upload_df), "cols\n")

  if (dry_run) {
    cat("\n-- Dry run: preview of first 3 rows --\n")
    print(head(upload_df, 3))
    cat("\n(no write to Google Sheets)\n")
    return(invisible(upload_df))
  }

  cat("Authenticating to Google Sheets...\n")
  auth_google_sheets()

  # Duplicate-run guard: if rows with the same (Date, Account) already exist
  # in the target tab, delete them (shifting subsequent rows up) before the
  # new rows are appended. Matching rows must be contiguous.
  matches <- find_existing_snapshot_rows(
    sheet_url, cfg$sheet_tab, parsed$as_of_date, cfg$account
  )
  if (length(matches) > 0) {
    if (any(diff(matches) != 1L)) {
      stop(
        "Existing rows matching Date=", format(parsed$as_of_date, "%Y-%m-%d"),
        " + Account='", cfg$account, "' in tab '", cfg$sheet_tab,
        "' are not contiguous (rows: ", paste(matches, collapse = ", "),
        "). Please clean up the sheet manually before re-running."
      )
    }
    first_row <- min(matches)
    last_row  <- max(matches)
    cat(
      "  Replacing ", length(matches), " existing row(s) at ",
      cfg$sheet_tab, "!A", first_row, ":", last_row, "\n",
      sep = ""
    )
    range_delete(
      ss = sheet_url,
      sheet = cfg$sheet_tab,
      range = cell_rows(c(first_row, last_row))
    )
  }

  next_row <- next_empty_row(sheet_url, cfg$sheet_tab, cfg$header_row)
  write_range <- sprintf("A%d", next_row)
  cat("  Writing to ", cfg$sheet_tab, "!", write_range, "\n", sep = "")

  range_write(
    ss = sheet_url,
    data = upload_df,
    sheet = cfg$sheet_tab,
    range = write_range,
    col_names = FALSE,
    reformat = FALSE
  )

  cat("\U2713 Uploaded ", nrow(upload_df), " rows to ", cfg$sheet_tab,
      " starting at ", write_range, "\n", sep = "")
  invisible(upload_df)
}

# ---- CLI --------------------------------------------------------------------

if (!interactive() && sys.nframe() == 0L) {
  args <- commandArgs(trailingOnly = TRUE)
  if (length(args) < 1L) {
    stop(
      "Usage: Rscript upload_holdings.R <file_id> [downloads_dir] [--dry-run]"
    )
  }
  file_id <- args[1]
  remaining <- args[-1]
  dry_run <- "--dry-run" %in% remaining
  remaining <- remaining[remaining != "--dry-run"]
  downloads_dir <- if (length(remaining) >= 1L) remaining[1] else "~/Downloads"
  upload_holdings(file_id, downloads_dir = downloads_dir, dry_run = dry_run)
}
