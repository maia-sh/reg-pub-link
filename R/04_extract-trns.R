library(dplyr)
library(readr)
library(fs)
library(ctregistries)
library(tidypubmed)

source(here::here("R", "functions", "extract_pubmed.R"))
source(here::here("R", "functions", "get_grobid_ft_trn.R"))

intovalue <- readr::read_csv(here::here("data", "raw", "intovalue.csv"))

# Get all unique pmid/doi combinations (for consistent pub ids as pmids across trns)
pmids_dois <-
  intovalue %>%
  distinct(pmid, doi) %>%
  tidyr::drop_na(pmid, doi)

# Extract TRNs from: PubMed secondary identifier, PubMed abstract, and PDF full-text

pm_dir <- here::here("data", "raw", "pubmed-xml")

pm_xmls <-
  pm_dir %>%
  dir_ls()


# Secondary identifier ----------------------------------------------------

si <-
  pm_xmls %>%
  purrr::map_dfr(extract_pubmed, datatype = "databanks", quiet = FALSE) %>%
  ctregistries::mutate_trn_registry(accession_number)

write_rds(si, path_wd("data", "processed", "pubmed-si", ext = "rds"))
# si <- read_rds(path_wd("data", "processed", "pubmed-si", ext = "rds"))

# Visually inspect mismatching trns and registries
# 1 trn is eudract with preceeding letters (detected, preceeding letter removed)
# 1 trn is eudract with country code (detected, country code removed)
# 5 trns are incorrectly formatted drks with no preceding "drks" (n = 4) or only "s" (n = 1) (not detected)
# 3 accession numbers are figshare
# 1 registry is attributed to genbank but drks
# 1 registry is attributed to genbank but ct.gov
# For now leave and later could discuss/analyze,
# Or, combine databank/accession_number and check for trn match
si_trn_mismatches <-
  si %>%
  filter(!accession_number %in% trn |
           !databank %in% registry)
# write_rds(si_trn_mismatches, path_wd("data", "processed", "si-trn-mismatches", ext = "rds"))

si <-
  si %>%
  tidyr::drop_na(trn) %>%
  select(pmid, registry, trn_detected = trn) %>%
  distinct() %>%
  group_by(pmid) %>%
  mutate(n_detected = row_number()) %>%
  ungroup() %>%
  mutate(
    source = "secondary_id",
    trn_cleaned = purrr::map_chr(trn_detected, ctregistries::clean_trn)
  ) %>%
  left_join(pmids_dois, by  = "pmid")

write_rds(si, path_wd("data", "processed", "trn-si", ext = "rds"))

# Abstract ----------------------------------------------------------------

abs <-
  pm_xmls %>%
  purrr::map_dfr(extract_pubmed, datatype = "abstract", quiet = FALSE) %>%
  ctregistries::mutate_trn_registry(abstract)

write_rds(abs, path_wd("data", "processed", "pubmed-abstract", ext = "rds"))
# abs <- read_rds(path_wd("data", "processed", "pubmed-abstract", ext = "rds"))

abs <-
  abs %>%
  tidyr::drop_na(trn) %>%
  distinct(pmid, registry, trn_detected = trn) %>%
  group_by(pmid) %>%
  mutate(n_detected = row_number()) %>%
  ungroup() %>%
  mutate(
    source = "abstract",
    trn_cleaned = purrr::map_chr(trn_detected, ctregistries::clean_trn)
  ) %>%
  left_join(pmids_dois, by  = "pmid")

write_rds(abs, path_wd("data", "processed", "trn-abstract", ext = "rds"))
# abs <- read_rds(path_wd("data", "processed", "trn-abstract", ext = "rds"))

# Full-text PDF DOI -------------------------------------------------------

ft_pdf_doi_dir <-
  here::here("data", "raw", "ft-pdf-doi-grobid-xml")

ft_pdf_doi_xmls <-
  ft_pdf_doi_dir %>%
  dir_ls()

ft_pdf_doi <-
  ft_pdf_doi_xmls %>%
  purrr::map_dfr(get_grobid_ft_trn) %>%
  select(-pmid) %>%
  rename(trn_detected = trn, n_detected = n) %>%
  mutate(
    source = "ft_pdf",
    trn_cleaned = purrr::map_chr(trn_detected, ctregistries::clean_trn)
  ) %>%
  left_join(pmids_dois, by  = "doi")

write_rds(ft_pdf_doi, path_wd("data", "processed", "trn-ft-pdf-doi", ext = "rds"))

# Full-text PDF PMID ------------------------------------------------------

ft_pdf_pmid_dir <-
  here::here("data", "raw", "ft-pdf-pmid-grobid-xml")

ft_pdf_pmid_xmls <-
  ft_pdf_pmid_dir %>%
  dir_ls()

ft_pdf_pmid <-
  ft_pdf_pmid_xmls %>%
  purrr::map_dfr(get_grobid_ft_trn) %>%
  select(-doi) %>%
  rename(trn_detected = trn, n_detected = n) %>%
  mutate(
    source = "ft_pdf",
    trn_cleaned = purrr::map_chr(trn_detected, ctregistries::clean_trn)
  ) %>%
  left_join(pmids_dois, by  = "pmid")

write_rds(ft_pdf_pmid, path_wd("data", "processed", "trn-ft-pdf-pmid", ext = "rds"))


# Note: Could add whether typo and dupe after cleaning
# mutate(has_trn_typo = if_else(trn_detected != trn_cleaned, TRUE, FALSE)) %>%
# add_count(pmid, trn_cleaned, source)


# Combine reported TRNs (secondary id, abstract, full-text) ---------------

trn_combined <-
  bind_rows(si, abs, ft_pdf_doi, ft_pdf_pmid) %>%

  distinct(pmid, doi, trn = trn_cleaned, registry, source) %>%

  # All records should have a pmid and a trn
  assertr::assert(assertr::not_na, pmid, trn)

write_rds(trn_combined, path_wd("data", "processed", "trn-reported-long", ext = "rds"))


# Prepare retrieved pubmed and pdfs (doi and pmid) ------------------------

pubmed_dir <-
  here::here("data", "raw", "pubmed-xml")

pubmed_retrieved <-
  pubmed_dir %>%
  fs::dir_ls() %>%
  fs::path_file() %>%
  fs::path_ext_remove()

ft_pdf_doi_retrieved <-
  ft_pdf_doi_xmls %>%
  fs::path_file() %>%
  stringr::str_remove(".tei.xml$") %>%
  stringr::str_replace_all("\\+", "/")

ft_pdf_pmid_retrieved <-
  ft_pdf_pmid_xmls %>%
  fs::path_file() %>%
  stringr::str_remove(".tei.xml$")

# Create df of retrieved pubmed and pdfs (doi and pmid), with NA if no pmid
# Also add whether source of pdf if doi or pmid (TRUE/FALSE only)
pubmed_ft_pdf_retrieved <-
  intovalue %>%
  select(id, doi, pmid) %>%
  mutate(
    has_pubmed = case_when(
      is.na(pmid) ~ NA,
      pmid %in% pubmed_retrieved ~ TRUE,
      TRUE ~ FALSE
    ),

    has_ft_pdf = case_when(
      is.na(pmid) ~ NA,
      (doi %in% ft_pdf_doi_retrieved) | (pmid %in% ft_pdf_pmid_retrieved) ~ TRUE,
      TRUE ~ FALSE
    ),

    ft_pdf_doi = if_else(doi %in% ft_pdf_doi_retrieved, TRUE, FALSE),
    ft_pdf_pmid = if_else(pmid %in% ft_pdf_pmid_retrieved, TRUE, FALSE),
  ) %>%

  # Remove duplicates due to intovalue versions
  distinct()

write_rds(pubmed_ft_pdf_retrieved, path_wd("data", "processed", "pubmed-ft-pdf-retrieved", ext = "rds"))


# Pivot wider to for one row per TRN with sources as columns --------------

trn_combined <-
  trn_combined %>%

  # Note: `value_fill` is FALSE for all but some will be replaced with NA, e.g., because no full-text
  mutate(value = TRUE) %>%
  tidyr::pivot_wider(
    names_from = source, names_prefix = "has_trn_",
    values_from = value, values_fill = FALSE
  ) %>%

  # Change "has_trn_SOURCE" from FALSE to NA if source *not* searched
  mutate(

    # TRN in secondary id and abstract are NA if no pubmed record
    # has_trn_secondary_id = if_else(
    #   !pmid %in% pubmed_retrieved,
    #   NA, has_trn_secondary_id
    # ),
    # has_trn_abstract = if_else(
    #   !pmid %in% pubmed_retrieved,
    #   NA, has_trn_abstract
    # ),

    # TRN in full-text is NA no full-text
    has_trn_ft_pdf = if_else(
      (!doi %in% ft_pdf_doi_retrieved) & (!pmid %in% ft_pdf_pmid_retrieved),
      NA, has_trn_ft_pdf
  ))

write_rds(trn_combined, path_wd("data", "processed", "trn-reported-wide", ext = "rds"))


# Combine all trns (intovalue and reported) -------------------------------

trn_all <-
  bind_rows(
    distinct(intovalue, trn = id, registry),
    distinct(trn_combined, trn, registry)
  ) %>%
  distinct()

write_rds(trn_all, path_wd("data", "processed", "trn-all", ext = "rds"))
