# Project instructions

## Overview

This is an R Shiny portfolio-monitoring application.

- `ui.R` defines the dashboard UI, inputs, outputs, and authentication wrapper.
- `server.R` handles authentication, session reactives, filtering, calculations, and rendered outputs.
- `global_data.R` owns Google Sheets loading, holdings normalization, Yahoo Finance data, and base performance datasets.
- `upload_holdings.R` parses broker exports and writes holdings snapshots to Google Sheets.
- `upload.R` runs the upload workflow.
- `www/custom.css` contains custom styling.

## Development rules

- Use the project `renv` environment; update `renv.lock` only when dependencies intentionally change.
- Never commit `.Renviron`, `creds/`, service-account JSON files, passwords, password hashes, or API secrets.
- Keep external data loading and normalization in `global_data.R`; keep Shiny presentation and session-specific reactives in `server.R`.
- When adding or renaming a Shiny input or output, update both `ui.R` and `server.R`.
- Do not treat `app_setup.R` as the authoritative startup path; it is legacy code.
- Run the application from the repository root.

## Reliability

- Handle missing prices, invalid symbols, missing metadata, and empty filtered data without crashing.
- Preserve the standard performance periods: `1d`, `7d`, `30d`, `90d`, `6m`, and `1y`.
- Keep currency conversion consistent between native and CAD views.
- Be careful modifying refresh behavior: `global_data.R` reloads shared global data.

## Validation

Before completing a change:

1. Check syntax for every modified R file.
2. Start the app from the repository root.
3. Verify initial data loading and authentication.
4. Test **Refresh Data**, dashboard filters, and both currency views.
5. Test empty and missing-price cases.
6. For upload changes, use `dry_run = TRUE` before writing to Google Sheets.