library(dplyr)
library(fs)
library(readr)
library(here)

# Full-text acquired via a combination of automated and manual techniques
# Full-text searched for via DOI, if available, else PMID
# Converted to XML via GROBID
# Converted XML manually added to `dir_doi` and `dir_pmid`

# PDFs
dir_doi_raw <- here("data", "raw", "ft-pdf-doi")
dir_pmid_raw <- here("data", "raw", "ft-pdf-pmid")

# PDFs converted to XML
dir_doi <- here("data", "raw", "ft-pdf-doi-grobid-xml")
dir_pmid <- here("data", "raw", "ft-pdf-pmid-grobid-xml")

intovalue <- readr::read_csv(here("data", "raw", "intovalue.csv"))

source(here("R", "functions", "retrieve_pdf.R"))
source(here("R", "functions", "resolve_doi.R"))
source(here("R", "functions", "correct_pdf_url.R"))

# Get pdfs from dois ------------------------------------------------------

# Limit to dois for pubmed records
dois <-
  intovalue %>%
  filter(!is.na(pmid) & !is.na(doi)) %>%
  distinct(doi) %>%
  pull()

# If dois already downloaded/converted, remove those from list to download
# Note: Currently only already converted to xml, but could be as pdf
if (fs::dir_exists(dir_doi)){

  ft_doi_retrieved <-
    fs::dir_ls(dir_doi) %>%
    fs::path_file() %>%
    stringr::str_remove(".tei.xml$") %>%
    stringr::str_replace_all("\\+", "/")


  # Check whether unneeded dois retrieved, and manually review and remove
  dois_downloaded_unused <- setdiff(ft_doi_retrieved, dois)
  if (length(dois_downloaded_unused) > 0){
    rlang::warn(glue::glue("Unused dois downloaded:{dois_downloaded_unused}"))
  }

  # Limit to missing dois
  dois <- setdiff(dois, ft_doi_retrieved) %>% sort()
}

# Download remaining dois, if any
if (length(dois) > 0) {

  # Use email locally stored as "rm-email", if available
  # Else ask user and store
  email <-
    ifelse(
      nrow(keyring::key_list("rm-email")) == 1,
      keyring::key_get("rm-email"),
      keyring::key_set("rm-email")
    )

  dois %>%
    purrr::walk(retrieve_pdf,
                dir  = dir_doi_raw,
                source = "unpaywall", # Or "publisher" with VPN turned on
                email = email
    )
}

# Get pdfs from pmids -----------------------------------------------------

# Limit to pmids without dois for pubmed records
pmids_no_dois <-
  intovalue %>%
  filter(!is.na(pmid) & is.na(doi)) %>%
  distinct(pmid) %>%
  pull()

# Since so few publications, manually search and add to `dir_pmid_raw`, then convert to XML and add to `dir_pmid`

# If pmids already downloaded/converted, remove those from list to download
# Note: Currently only already converted to xml, but could be as pdf
if (fs::dir_exists(dir_pmid)){

  ft_pmid_retrieved <-
    fs::dir_ls(dir_pmid) %>%
    fs::path_file() %>%
    stringr::str_remove(".tei.xml$")


  # Check whether unneeded pmids retrieved, and manually review and remove
  pmids_downloaded_unused <- setdiff(ft_pmid_retrieved, pmids_no_dois)
  if (length(pmids_downloaded_unused) > 0){
    rlang::warn(glue::glue("Unused pmids downloaded:{pmids_downloaded_unused}"))
  }

  # Limit to missing pmids
  pmids_no_dois <- setdiff(pmids_no_dois, ft_pmid_retrieved) %>% sort()
}
