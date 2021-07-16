library(dplyr)
library(readr)
library(here)
library(fs)
library(lubridate)

source(here("R", "functions", "duration_days.R"))

# Get data ----------------------------------------------------------------

intovalue <- read_csv(here("data", "raw", "intovalue.csv"))
registry_studies <- read_rds(here("data", "processed", "registry-studies.rds"))
registry_references <- read_rds(here("data", "processed", "registry-references.rds"))
n_cross_registrations <- read_rds(here("data", "processed", "n-cross-registrations.rds"))
trn_reported_wide <- read_rds(here("data", "processed", "trn-reported-wide.rds"))
pubmed_ft_pdf_retrieved <- read_rds(here("data", "processed", "pubmed-ft-pdf-retrieved.rds"))


# Prepare trials ----------------------------------------------------------

# Prepare columns to keep from intovalue
# Exclude "days_DATE_to_publication" columns since will recalculate from registries
iv_cols_to_keep <-
  setdiff(colnames(intovalue), colnames(registry_studies)) %>%
  setdiff(stringr::str_subset(., "^days_")) %>%
  c("id", .)

trials <-

  # Select intovalue columns to keep
  intovalue %>%

  select(all_of(iv_cols_to_keep)) %>%

  # Add updated registry data
  left_join(registry_studies, by = "id") %>%

  mutate(

    days_cd_to_publication = duration_days(completion_date, publication_date),
    days_pcd_to_publication = duration_days(primary_completion_date, publication_date),
    days_reg_to_publication = duration_days(registration_date, publication_date),

    # Note: some (n = 3) trials !has_publication do have pmids (abstract only)
    has_pmid = if_else(!is.na(pmid), TRUE, FALSE)
  ) %>%

  # Add info about pubmed and ft (pdf) retrieval
  left_join(pubmed_ft_pdf_retrieved, by = c("id", "doi", "pmid")) %>%

  # Add info about whether intovalue trn in secondary id, abstract, ft pdf
  left_join(trn_reported_wide, by = c("id" = "trn", "registry", "doi", "pmid")) %>%

  # Add number of cross-registrations in registry, secondary id, abstract, ft pdf
  left_join(distinct(n_cross_registrations), by = c("id", "doi", "pmid")) %>%

  # Check that same number of rows as intovalue
  assertr::verify(nrow(.) == nrow(intovalue)) %>%

  # Trials without trn reported anywhere are not in `trn_reported` and have NA for all `has_trn`
  # However, `has_trn` should be FALSE if source retrieved
  # Also, rename to clarify that iv_trn (not any trn)
  mutate(
    has_iv_trn_abstract =
      if_else(has_pubmed & is.na(has_trn_abstract), FALSE, has_trn_abstract),
    has_iv_trn_secondary_id =
      if_else(has_pubmed & is.na(has_trn_secondary_id), FALSE, has_trn_secondary_id),
    has_iv_trn_ft_pdf =
      if_else(has_ft_pdf & is.na(has_trn_ft_pdf), FALSE, has_trn_ft_pdf)
  ) %>%
  select(-starts_with("has_trn_")) %>%

  # If trial has pubmed, trn in abstract/si must not be NA
  pointblank::col_vals_not_null(
    vars(has_iv_trn_secondary_id, has_iv_trn_secondary_id),
    preconditions = ~ . %>% filter(has_pubmed)
  ) %>%

  # If trial has ft pdf, trn in ft pds must not be NA
  pointblank::col_vals_not_null(
    vars(has_iv_trn_ft_pdf),
    preconditions = ~ . %>% filter(has_pubmed & has_ft_pdf)
  )

# Add info about registry reference links
pmid_references <-
  registry_references %>%
  select(-doi, -reference_type) %>%
  tidyr::drop_na(pmid) %>%
  mutate(pmid_link = TRUE) %>%
  distinct()

doi_references <-
  registry_references %>%
  select(-pmid, -reference_type) %>%
  tidyr::drop_na(doi) %>%
  mutate(doi_link = TRUE) %>%
  distinct()

trials <-
  trials %>%

  # Join in registry references by pmid
  left_join(pmid_references, by = c("id", "pmid")) %>%
  left_join(doi_references, by = c("id", "doi")) %>%

  # Check that same number of rows as trials
  # Rows would be added if kept `reference_type`, since same pub may have multiple `reference_type`
  assertr::verify(nrow(.) == nrow(trials)) %>%

  mutate(
    reference_derived = coalesce(reference_derived.x, reference_derived.y),
    .keep = "unused"
  ) %>%

  mutate(

    # Add FALSE if trials has doi/pmid and no link found
    doi_link = case_when(
      !is.na(doi_link) ~ doi_link,
      !is.na(doi) ~ FALSE,
      TRUE ~ NA
    ),

    pmid_link = case_when(
      !is.na(pmid_link) ~ pmid_link,
      !is.na(pmid) ~ FALSE,
      TRUE ~ NA
    ),

    # Trial has a registry-publication link if either doi/pmid link
    has_reg_pub_link = case_when(
      doi_link | pmid_link ~ TRUE,
      !doi_link | !pmid_link ~ FALSE,
      is.na(doi_link) & is.na(pmid_link) ~ NA
    )
  )

# Reapply IntoValue inclusion criteria
trials <-
  trials %>%
  mutate(

    # IntoValue includes trials completed 2009-2017
    iv_completion = if_else(
      completion_year > 2008 & completion_year < 2018,
      TRUE, FALSE, missing = FALSE
    ),

    # IntoValue includes all drks and some ctgov recruitment statuses
    iv_status = if_else(
      registry == "DRKS" |
        (registry == "ClinicalTrials.gov" & recruitment_status %in% c("Completed" , "Terminated" , "Suspended", "Unknown status")),
      TRUE, FALSE
    ),

    # IntoValue includes only interventional studies
    iv_interventional = if_else(study_type == "Interventional", TRUE, FALSE)
  ) %>%

  # Unify date variable type
  mutate(across(ends_with("_date"), as.Date))

# Reorganize columns
trials <-
  trials %>%
  relocate(
    "id",
    "registry",
    "is_resolved",

    # IntoValue info
    "iv_version",
    "identification_step",
    "is_dupe",
    "iv_completion",
    "iv_status",
    "iv_interventional",

    # Registry info (including derived)
    "has_german_umc_lead",
    "lead_cities",
    "facility_cities",
    "center_size",
    "main_sponsor",
    "study_type",
    "intervention_type",
    "phase",
    "enrollment",
    "recruitment_status",
    "masking",
    "allocation",
    "is_randomized",
    "is_multicentric",
    "is_prospective",
    "has_summary_results",

    # Dates
    "registration_date",
    "start_date",
    "completion_date",
    "completion_year",
    "primary_completion_date",
    "primary_completion_year",
    "summary_results_date",

    "days_cd_to_summary",
    "days_pcd_to_summary",
    "days_reg_to_start",
    "days_reg_to_cd",
    "days_reg_to_pcd",
    "days_cd_to_publication",
    "days_pcd_to_publication",
    "days_reg_to_publication",

    # Publication info
    "doi",
    "pmid",
    "url",
    "publication_date",
    "has_publication",
    "publication_type",
    "has_pmid",
    "has_pubmed",
    "has_ft_pdf",
    "ft_pdf_doi",
    "ft_pdf_pmid",
    "has_iv_trn_abstract",
    "has_iv_trn_secondary_id",
    "has_iv_trn_ft_pdf",
    "has_reg_pub_link",
    "pmid_link",
    "doi_link",
    "reference_derived",
    "n_crossreg_secondary_id",
    "n_crossreg_abstract",
    "n_crossreg_ft_pdf",
    "n_crossreg_reg"
  )

# Check that all intovalue columns in trials
if (!rlang::is_empty(setdiff(colnames(intovalue), colnames(trials)))) {
  rlang::warn("There are intovalue columns missing from trials!")
}

write_rds(trials, here("data", "processed", "trials.rds"))
