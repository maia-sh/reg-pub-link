library(dplyr)
library(purrr)

source(here::here("R", "functions", "drks-functions.R"))

dir <- fs::dir_create(here::here("data", "raw", "drks"))

# Get drks trns
drks_trns <-
  readr::read_rds(here::here("data", "processed", "trn-all.rds")) %>%
  filter(registry == "DRKS") %>%
  pull(trn)

# Download drks records
drks_trns <-
  readr::read_rds(here::here("data", "processed", "trn-all.rds")) %>%
  filter(registry == "DRKS") %>%
  pull(trn)

purrr::walk(drks_trns, download_drks, dir = dir)
