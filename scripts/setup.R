# ── Create working and data directories ───────────────────────────────────────

wdir <- "~/Downloads/capstone/capstone_code"
ddir <- "~/Downloads/capstone/capstone_code/data_raw"

# ── Install and call packages ─────────────────────────────────────────────────

# install.packages(c("httr2", "jsonlite", "tidyverse", "keyring", "readr", "usethis", "here", "ggrepel", "gtrendsR", "Rcpp"))
library("httr")
library("jsonlite")
library("tidyverse")
library("keyring")
library("readr")
library("usethis")
library("here")
library("ggrepel")

# ── Set up keychain access for API ────────────────────────────────────────────

key_name <- "nyt-api-key"
# key_set(key_name) # only need to do this if you haven't done so yet

# Failsafe: REnviron File
usethis::edit_r_environ()