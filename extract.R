# Extract portfolio info from bank statements
# by running extract_stmt.R function on every statement in stmts/
# that hasn't already been processed, storing results in csv

library(tidyverse)

source('extract_stmt.R')

process_stmts()