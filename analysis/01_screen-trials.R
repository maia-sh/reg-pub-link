# Prepare trials ----------------------------------------------------------

trials_raw <-

  readr::read_csv(here::here("data", "trials-raw.csv")) %>%

  mutate(

    # Create flag for dupes in IV1 for exclusion
    is_not_iv1_dupe = if_else(!(is_dupe & iv_version == 1), TRUE, FALSE),

    # Trials with a journal article have a publication (disregard dissertations and abstracts)
    has_publication = if_else(publication_type == "journal publication", TRUE, FALSE, missing = FALSE),

    has_pmid = if_else(!is.na(pmid), TRUE, FALSE)
  )


# Prepare screening functions ---------------------------------------------

# Apply screening criteria
# Returns list of inclusion counts and filtered dataframe
count_filter <- function(data, vars) {

  counts <-
    tibble(name = as.character(),
           value = as.logical(),
           n = as.integer()
    )

  for (var in vars) {
    counts <-
      data %>%
      count(.data[[var]]) %>%
      pivot_longer(-n) %>%
      add_row(counts, .)

    data <- filter(data, .data[[var]])
  }

  list(data = data, counts = counts)

}

# Report screening summary counts
report_n <- function(counts, var, condition) {
  n <-
    counts %>%
    filter(name == var & value == condition) %>%
    pull(n)

  # If empty, count is 0
  if (rlang::is_empty(n)){n <- 0}

  n
}


# Screen trials -----------------------------------------------------------

screening_criteria <- c(
  "iv_completion",
  "iv_status",
  "iv_interventional",
  "has_german_umc_lead",
  "is_not_iv1_dupe",
  "has_publication",
  "has_pmid",
  "has_pubmed",
  "has_ft"
)

trials_screened <-  count_filter(trials_raw, screening_criteria)

trials <- trials_screened$data

write_csv(trials, here("data", "trials.csv"))

# Tabularize trial screening counts ---------------------------------------

trial_screening <-

  # IntoValue 1 and 2
  trials_raw %>%
  count(iv_version) %>%
  mutate(iv_version = str_c("iv", iv_version)) %>%
  rename(name = iv_version) %>%
  mutate(value =  TRUE, .after = "name") %>%

  bind_rows(trials_screened$counts) %>%

  add_row(
    name = "is_unique_pmid",
    value = c(TRUE, FALSE),
    n = c(n_distinct(trials$pmid),
          nrow(trials) - n_distinct(trials$pmid))
  )


# Report trial screening counts -------------------------------------------

n_trials_iv1 <- report_n(trial_screening, "iv1", TRUE) #n_iv1
n_trials_iv2 <- report_n(trial_screening, "iv2", TRUE) #n_iv2
n_trials_iv <- n_trials_iv1 + n_trials_iv2
n_trials_iv_completion_date <- report_n(trial_screening, "iv_completion", TRUE)
n_trials_iv_completion_date_ex <- report_n(trial_screening, "iv_completion", FALSE)
n_trials_iv_status <- report_n(trial_screening, "iv_status", TRUE)
n_trials_iv_status_ex <- report_n(trial_screening, "iv_status", FALSE)
n_trials_iv_interventional <- report_n(trial_screening, "iv_interventional", TRUE)
n_trials_iv_interventional_ex <- report_n(trial_screening, "iv_interventional", FALSE)
n_trials_lead <- report_n(trial_screening, "has_german_umc_lead", TRUE) #n_trials
n_trials_lead_ex <- report_n(trial_screening, "has_german_umc_lead", FALSE)
n_trials_deduped <- report_n(trial_screening, "is_not_iv1_dupe", TRUE)
n_trials_dupes_ex <- report_n(trial_screening, "is_not_iv1_dupe", FALSE)
n_trials_pub <- report_n(trial_screening, "has_publication", TRUE) #n_pub
n_trials_pub_ex <- report_n(trial_screening, "has_publication", FALSE) #n_pub_ex
n_trials_pmid <- report_n(trial_screening, "has_pmid", TRUE) #n_pmid
n_trials_pmid_ex <- report_n(trial_screening, "has_pmid", FALSE) #n_pmid_ex
n_trials_pubmed <- report_n(trial_screening, "has_pubmed", TRUE)
n_trials_pubmed_ex <- report_n(trial_screening, "has_pubmed", FALSE)
n_trials_ft <- report_n(trial_screening, "has_ft", TRUE)
n_trials_ft_ex <- report_n(trial_screening, "has_ft", FALSE)
n_pubs_unique <- report_n(trial_screening, "is_unique_pmid", TRUE) #n_pmid_deduped
n_pubs_dupes_ex <- report_n(trial_screening, "is_unique_pmid", FALSE)


# Remove unnecessary variables --------------------------------------------

rm(screening_criteria, trials_screened, report_n)

