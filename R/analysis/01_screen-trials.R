trials_all <-
  trials_all %>%

  # Create flag for dupes in IV1 for exclusion
  mutate(is_not_iv1_dupe = if_else(!(is_dupe & iv_version == 1), TRUE, FALSE))

# Screen trials -----------------------------------------------------------

# IntoValue 1 and 2
iv12_counts <-
  trials_all %>%
  count(iv_version) %>%
  mutate(iv_version = str_c("iv", iv_version)) %>%
  rename(name = iv_version) %>%
  mutate(value =  TRUE, .after = "name")

# IntoValue 1 and 2 German UMC leads
# iv12_lead_counts <-
#   trials_all %>%
#   count(iv_version, has_german_umc_lead) %>%
#   mutate(iv_version = str_c("has_german_umc_lead_iv", iv_version)) %>%
#   rename(name = iv_version, value = has_german_umc_lead)

screening_criteria <- c(
  "iv_completion",
  "iv_status",
  "iv_interventional",
  "has_german_umc_lead",
  "is_not_iv1_dupe",
  "has_publication",
  "has_pmid",
  "has_pubmed",
  "has_ft_pdf"
)

trials_screened <-
  trials_all %>%

  # Remove doi, to be replace with more complete dois from publications
  select(-doi) %>%

  # is_dupe needs to be is_not_dupe, since screening keeps TRUE
  mutate(is_not_dupe_trial = if_else(is_dupe, FALSE, TRUE, missing = TRUE)) %>%

  count_filter(screening_criteria)

trials <- trials_screened$data

# Tabularize trial screening counts

trial_screening <-
  bind_rows(
    iv12_counts,
    # iv12_lead_counts,
    trials_screened$counts
  ) %>%
  add_row(
    name = "is_unique_pmid",
    value = c(TRUE, FALSE),
    n = c(n_distinct(trials$pmid),
          nrow(trials) - n_distinct(trials$pmid))
  )

# Report trial screening counts

n_trials_iv1 <- report_n(trial_screening, "iv1", TRUE) #n_iv1
n_trials_iv2 <- report_n(trial_screening, "iv2", TRUE) #n_iv2
n_trials_iv <- n_trials_iv1 + n_trials_iv2
n_trials_iv_completion_date <- report_n(trial_screening, "iv_completion", TRUE)
n_trials_iv_completion_date_ex <- report_n(trial_screening, "iv_completion", FALSE)
n_trials_iv_status <- report_n(trial_screening, "iv_status", TRUE)
n_trials_iv_status_ex <- report_n(trial_screening, "iv_status", FALSE)
n_trials_iv_interventional <- report_n(trial_screening, "iv_interventional", TRUE)
n_trials_iv_interventional_ex <- report_n(trial_screening, "iv_interventional", FALSE)
# n_trials_iv1_lead <- report_n(trial_screening, "has_german_umc_lead_iv1", TRUE) #n_iv1_lead
# n_trials_iv1_lead_ex <- report_n(trial_screening, "has_german_umc_lead_iv1", FALSE) #n_iv1_lead_ex
# n_trials_iv2_lead <- report_n(trial_screening, "has_german_umc_lead_iv2", TRUE) #n_iv2_lead
# n_trials_iv2_lead_ex <- report_n(trial_screening, "has_german_umc_lead_iv2", FALSE) #n_iv2_lead_ex
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
n_trials_ft_pdf <- report_n(trial_screening, "has_ft_pdf", TRUE)
n_trials_ft_pdf_ex <- report_n(trial_screening, "has_ft_pdf", FALSE)
n_pubs_unique <- report_n(trial_screening, "is_unique_pmid", TRUE) #n_pmid_deduped
n_pubs_dupes_ex <- report_n(trial_screening, "is_unique_pmid", FALSE)

# Remove unnecessary variables
rm(iv12_counts, #iv12_lead_counts,
   screening_criteria, trials_screened)
