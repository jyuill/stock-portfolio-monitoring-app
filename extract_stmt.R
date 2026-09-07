## extract_td_statement.R
## Extracts key portfolio-level metrics from a TD Direct Investing statement
## PDF and appends a row per month to portfolio_monthly.csv. A single PDF may
## bundle multiple monthly statements back-to-back (each starting on its own
## page with a "<Month> D, YYYY to <Month> D, YYYY" period header) — this is
## detected automatically and one row is written per month found.
##
## Usage:
##   source("extract_stmt.R")
##   extract_statement("path/to/Statement_20LWH2.pdf", csv_path = "data/portfolio_monthly.csv")

library(pdftools)
library(stringr)
library(dplyr)
library(readr)
library(lubridate)
library(purrr)

# Marks the first page of each monthly statement within the PDF.
period_pattern <- "([A-Z][a-z]+) \\d{1,2}, (\\d{4}) to [A-Z][a-z]+ \\d{1,2}, \\d{4}"

# --- Extract one month's metrics from the pages belonging to that statement -
extract_one_period <- function(txt, pdf_path) {

  # --- Account number ------------------------------------------------------
  # Pulled from the statement text itself (e.g. "Account number: 20LWH2-S"),
  # rather than the filename, since filenames aren't guaranteed to stay
  # consistent.
  account_id <- str_match(txt, "Account number:\\s*(\\S+)")[, 2]
  if (is.na(account_id)) {
    warning("Could not find 'Account number:' in ", basename(pdf_path), ". Check manually.")
  }

  # Some accounts (e.g. "Direct Trading") hold a CDN and a US dollar leg
  # under the *same* account number, distinguished only by "Account
  # currency:". Without disambiguation those two legs would collide under
  # one account_id/month key. The CDN leg keeps the bare account number
  # (matching existing behavior); the USD leg gets a "-USD" suffix.
  account_currency <- str_match(txt, "Account currency:\\s*(.+)")[, 2] |> str_trim()
  currency <- if (!is.na(account_currency) && str_detect(account_currency, "US")) "USD" else "CAD"
  if (currency == "USD") {
    account_id <- paste0(account_id, "-USD")
  }

  # --- Statement period (for the month label) ---------------------------
  period_match <- str_match(txt, period_pattern)
  month_name <- period_match[, 2]
  year       <- period_match[, 3]
  month_label <- format(ymd(paste(year, match(month_name, month.name), "01")), "%Y-%m")

  # --- Account at a glance -----------------------------------------------
  # These figures appear once near the top, under "Your account at a glance"
  beginning_balance <- str_match(txt, "Beginning balance\\s+\\$?([\\d,]+\\.\\d{2})")[, 2] |>
    str_remove_all(",") |> as.numeric()

  # Note: the "This period" figure appears directly after "Change in your
  # account", not after the word "balance" — a sidebar box on the statement
  # ("Please see page X ... about your account.") gets interleaved into the
  # extracted text and pushes "balance" onto a later line.
  change_match <- str_match(txt, "Change in your account\\s+(-?)\\$?([\\d,]+\\.\\d{2})")
  change_sign <- change_match[, 2]
  change_in_balance <- change_match[, 3] |> str_remove_all(",") |> as.numeric()
  change_in_balance <- ifelse(change_sign == "-", -change_in_balance, change_in_balance)

  ending_balance <- str_match(txt, "Ending balance\\s+\\$?([\\d,]+\\.\\d{2})")[, 2] |>
    str_remove_all(",") |> as.numeric()

  # --- Ending cash balance (from "Activity in your account this period") -
  # When nothing happened in the account that month, TD omits the "Activity
  # in your account this period" section (and its "Ending cash balance"
  # line) entirely. Fall back to the "Cash" row of the Holdings table, which
  # reports the same period-end cash figure and is always present.
  ending_cash_balance <- str_match(txt, "Ending cash balance\\s+\\$?([\\d,]+\\.\\d{2})")[, 2] |>
    str_remove_all(",") |> as.numeric()
  # The section is genuinely absent (as opposed to a parsing miss) only when
  # no dollar figure followed the label — the phrase "Activity in your
  # account this period" itself also turns up in unrelated boilerplate text
  # on the definitions page, so it can't be used as the signal.
  no_activity_section <- is.na(ending_cash_balance)
  if (no_activity_section) {
    ending_cash_balance <- str_match(
      txt, "\\bCash\\s+([\\d,]+\\.\\d{2})\\s+([\\d,]+\\.\\d{2})\\s+-?[\\d,]+\\.\\d{2}\\s+[\\d.]+%"
    )[, 3] |> str_remove_all(",") |> as.numeric()
  }

  # --- Dividends: this period + YTD (from "Details of investment income") -
  div_block <- str_match(
    txt,
    "Dividends\\s+([\\d,]+\\.\\d{2})\\s+([\\d,]+\\.\\d{2})"
  )
  dividends_this_period <- div_block[, 2] |> str_remove_all(",") |> as.numeric()
  dividends_ytd         <- div_block[, 3] |> str_remove_all(",") |> as.numeric()

  # A missing activity section (same cause as the cash fallback above) means
  # literally nothing happened that month, including no dividends.
  if (is.na(dividends_this_period) && no_activity_section) {
    dividends_this_period <- 0
    if (month_name == "January") {
      # January is the first month of the year, so year-to-date equals
      # this period's (zero) dividends.
      dividends_ytd <- 0
    } else {
      warning("No activity/income section found for ", account_id, " / ", month_label,
               ", and it isn't January, so dividends_ytd could not be inferred and is left NA. ",
               "Check the statement manually.")
    }
  }

  # --- Flag possible in-kind / non-cash contributions ---------------------
  # TD shows these as a "Contribution" line + a "Value X CONTRIBUTION" line
  # in the activity section, distinct from a cash deposit.
  contribution_match <- str_match(txt, "Value\\s+([\\d,]+\\.\\d{2}) CONTRIBUTION")[, 2]
  notable_contribution <- if (!is.na(contribution_match)) {
    as.numeric(str_remove_all(contribution_match, ","))
  } else {
    0
  }

  row <- tibble(
    account_id = account_id,
    currency = currency,
    month = month_label,
    beginning_balance = beginning_balance,
    ending_balance = ending_balance,
    change_in_balance = change_in_balance,
    dividends_this_period = dividends_this_period,
    dividends_ytd = dividends_ytd,
    ending_cash_balance = ending_cash_balance,
    notable_contribution = notable_contribution
  )

  message("Extracted ", account_id, " / ", month_label,
          ": ending balance $", format(ending_balance, big.mark = ","),
          " | change $", format(change_in_balance, big.mark = ","),
          " | dividends this period $", dividends_this_period)

  if (notable_contribution > 0) {
    message("NOTE: an in-kind/non-cash contribution of $", format(notable_contribution, big.mark = ","),
            " was detected in ", month_label,
            ". This inflates 'change_in_balance' relative to true market growth — ",
            "consider a 'true_growth' column of change_in_balance - notable_contribution when charting.")
  }

  row
}

# --- Extract every month found in a (possibly multi-month) statement PDF ----
extract_statement <- function(pdf_path, csv_path = "data/portfolio_monthly.csv") {

  pages <- pdf_text(pdf_path)

  # Each monthly statement bundled in the PDF starts on a page carrying its
  # own period header, so split the pages into one group per statement.
  section_starts <- which(str_detect(pages, period_pattern))

  if (length(section_starts) == 0) {
    stop("Could not find any statement period headers in ", basename(pdf_path))
  }

  section_ends <- c(section_starts[-1] - 1, length(pages))

  new_rows <- map2(section_starts, section_ends, function(start, end) {
    txt <- paste(pages[start:end], collapse = "\n")
    extract_one_period(txt, pdf_path)
  }) |> list_rbind() |>
    mutate(date_processed = format(Sys.time(), "%Y-%m-%d %H:%M:%S"))

  if (file.exists(csv_path)) {
    # date_processed is read as character so it binds cleanly with new_rows'
    # character timestamp above, regardless of how readr would otherwise
    # guess the column's type from existing values.
    existing <- read_csv(csv_path, show_col_types = FALSE,
                          col_types = cols(date_processed = col_character(), .default = col_guess()))
    key_existing <- paste(existing$account_id, existing$month)
    key_new <- paste(new_rows$account_id, new_rows$month)
    dup <- key_existing %in% key_new
    if (any(dup)) {
      message(sum(dup), " existing row(s) in ", csv_path,
              " match account/month(s) being (re)extracted — overwriting.")
      existing <- existing |> filter(!dup)
    }
    combined <- bind_rows(existing, new_rows) |> arrange(account_id, month)
  } else {
    combined <- new_rows |> arrange(account_id, month)
  }

  write_csv(combined, csv_path)

  message("Wrote ", nrow(new_rows), " month(s) from ", basename(pdf_path), " to ", csv_path)

  invisible(combined)
}

## Example (uncomment to run):
# extract_statement("Statement_Sept2026.pdf", csv_path = "data/portfolio_monthly.csv")

# --- Process every statement in a folder, skipping ones already up to date -
# A statement PDF is (re)processed only if it was downloaded (by file mtime)
# more recently than the last date_processed recorded for any account found
# inside it. This lets statements for any number of accounts, with any
# filenames, be dropped into `stmts_dir` and picked up automatically.
process_stmts <- function(stmts_dir = "stmts", csv_path = "data/portfolio_monthly.csv") {

  pdf_files <- list.files(stmts_dir, pattern = "\\.pdf$", full.names = TRUE, ignore.case = TRUE)

  if (length(pdf_files) == 0) {
    message("No PDF statements found in ", stmts_dir)
    return(invisible(NULL))
  }

  # date_processed is read as character (rather than left to readr's type
  # guessing) so a mix of old date-only values and new date+time values in
  # the same column both parse cleanly below.
  read_existing <- function() {
    if (!file.exists(csv_path)) return(NULL)
    read_csv(csv_path, show_col_types = FALSE,
             col_types = cols(date_processed = col_character(), .default = col_guess()))
  }

  existing <- read_existing()

  for (pdf_path in pdf_files) {

    raw_accounts <- str_match_all(pdf_text(pdf_path), "Account number:\\s*(\\S+)") |>
      map(~ .x[, 2]) |>
      unlist() |>
      unique()

    file_time <- file.info(pdf_path)$mtime

    last_processed <- if (!is.null(existing) && nrow(existing) > 0) {
      is_match <- map_lgl(existing$account_id, \(id) any(str_starts(id, fixed(raw_accounts))))
      if (any(is_match)) {
        # older rows may only have a date (no time); treated as midnight,
        # which still compares correctly against a same-day file's mtime.
        max(parse_date_time(existing$date_processed[is_match],
                             orders = c("Ymd HMS", "Ymd"), tz = Sys.timezone()),
            na.rm = TRUE)
      } else {
        NA
      }
    } else {
      NA
    }

    if (is.na(last_processed) || file_time > last_processed) {
      message("--- Processing ", basename(pdf_path),
              " (downloaded ", format(file_time, "%Y-%m-%d %H:%M:%S"), ") ---")
      extract_statement(pdf_path, csv_path = csv_path)
      existing <- read_existing()
    } else {
      message(basename(pdf_path), " already processed as of ", format(last_processed, "%Y-%m-%d %H:%M:%S"),
              " (file downloaded ", format(file_time, "%Y-%m-%d %H:%M:%S"), ") — skipping.")
    }
  }

  invisible(NULL)
}
