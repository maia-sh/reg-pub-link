library(dplyr)

dir <- fs::dir_create(here::here("data", "raw"))

# IntoValue dataset
# Versions 1 and 2 combined and cleaned, enhanced with PMIDs/DOIs
# NOTE: access not working so manually downloaded on 2021-05-14
# "https://raw.githubusercontent.com/quest-bih/IntoValue2/master/data/iv_enhanced_pmids_dois_dataset.csv?token=AJJJ2UH4RVIY43GA2VQNDVLATZFBC"
# "https://AJJJ2UH4RVIY43GA2VQNDVLATZFBC@raw.githubusercontent.com/quest-bih/IntoValue2/master/data/iv_enhanced_pmids_dois_dataset.csv"
# "https://github.com/quest-bih/IntoValue2/blob/msh-dataset-checks/data/iv_main_dataset.csv"
# "https://github.com/quest-bih/IntoValue2/blob/master/data/iv_main_dataset.csv"

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


