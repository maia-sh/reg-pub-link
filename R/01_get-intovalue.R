library(dplyr)

dir <- fs::dir_create(here::here("data", "raw"))

# IntoValue dataset
# Versions 1 and 2 combined and cleaned, enhanced with PMIDs/DOIs
# NOTE: access not working so manually downloaded on 2021-07-06
# "https://github.com/quest-bih/IntoValue2/blob/msh-dataset-checks/data/iv_main_dataset.csv"
# "https://github.com/quest-bih/IntoValue2/blob/master/data/iv_main_dataset.csv"
# "https://raw.githubusercontent.com/quest-bih/IntoValue2/master/data/iv_main_dataset.csv?token=AJJJ2UEAXMXNWNBD7NUBHMDA4SG2A"

intovalue <-

  readr::read_csv(fs::path(dir, "iv_main_dataset.csv")) %>%

  # Create shorter names for publication ids
  rename(
    doi = publication_doi,
    pmid = publication_pmid,
    url = publication_url
  )

readr::write_csv(intovalue, fs::path(dir, "intovalue.csv"))

# intovalue <- readr::read_csv(
#   here::here("data", "raw", "intovalue.csv"),
#   col_types = cols(
#     publication_pmid = col_number(),
#     facility_cities = col_character()
#   )
# )


