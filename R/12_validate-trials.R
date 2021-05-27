library(dplyr)
library(assertr)
library(assertive)
library(pointblank)

trials <- readr::read_rds(here::here("data", "processed", "trials.rds"))
intovalue <- readr::read_csv(here::here("data", "raw", "intovalue.csv"))

# Check that all intovalue columns in trials
if (!rlang::is_empty(setdiff(colnames(intovalue), colnames(trials)))) {
  rlang::warn("There are intovalue columns missing from trials!")
}

trials %>%

  # Validate publication dois
  pointblank::col_vals_regex(
    columns = vars(doi),
    regex = "^10\\.\\d{4,9}/[-.;()/:_[:alnum:]]+$",
    na_pass = TRUE
  ) %>%

  # Validate publication pmids
  pointblank::col_vals_regex(
    columns = vars(pmid),
    regex = "^[0-9]{8}$",
    na_pass = TRUE
  ) %>%

  # Validate publication urls
  pointblank::col_vals_regex(
    columns = vars(url),
    regex = "^http",
    na_pass = TRUE
  ) %>%

  # Publication urls should not go to registries
  assertr::assert(
    function(url) !stringr::str_detect(url, "clinicaltrials.gov|drks.de")|is.na(url),
    url
  ) %>%

  # Check that all trials with publication (including abstract) has pmid, doi, OR url
  assertr::verify(nrow(filter(., identification_step != "No publ" & is.na(url) & is.na(doi) & is.na(pmid))) == 0) %>%

  # Check that if identification_step is no publication or abstract only, not has_publication
  pointblank::col_vals_equal(
    vars(has_publication),
    value = FALSE,
    preconditions = ~ . %>% filter(identification_step %in% c("No publ", "Abstract only"))
  ) %>%

  # Check that if not has_publication, no publication date and identification_step is no publication or abstract only
  pointblank::conjointly(
    ~ col_vals_in_set(.,
                      columns = vars(identification_step),
                      set = c("No publ", "Abstract only")
    ),
    ~ col_vals_null(., vars(publication_date)),
    preconditions = ~ . %>% filter(!has_publication)
  ) %>%

  # Check that if no publication, no publication ids
  pointblank::col_vals_null(
    vars(url, doi, pmid),
    preconditions = ~ . %>% filter(identification_step == "No publ")
  ) %>%

  # Check that if has_publication, has publication_date
  pointblank::col_vals_not_null(
    vars(publication_date),
    preconditions = ~ . %>% filter(has_publication)
  ) %>%

# Check that identification steps match iv version
col_vals_in_set(
  vars(identification_step),
  c("No publ", "Registry linked",
    "Publ found in Google ID search", "Publ found in Google search (no ID)"),
  preconditions = ~ . %>% filter(iv_version == 2)
) %>%

  col_vals_in_set(
    vars(identification_step),
    c("No publ", "Registry linked",
      "Abstract only", "Dissertation", "Hand search", "Pubmed"),
    preconditions = ~ . %>% filter(iv_version == 1)
  )

