# How many (additional, non-iv) publications are listed in registrations?
# How many (additional, non-iv) trns are in the registration, full-text, abstract, metadata?

trials %>%
  # filter(n_reg_pub_any>0) %>%
  select(registry,
         starts_with("n_reg_pub"),
         starts_with("n_crossreg"
         )) %>%
  mutate(across(as.integer())) %>%
  tbl_summary(
    by = registry,
    type = list(everything() ~ "continuous"),
    statistic = list(everything() ~ "{median} ({min}, {max})")
  ) %>%
  add_overall()

trials %>%
  # count(n_crossreg_secondary_id)
  count(n_reg_pub_any)
  count(n_reg_pub_doi_or_pmid)

trials %>%
  filter(n_reg_pub_any==13)

n_reg_pub_max_drks <-
  trials %>%
  filter(registry == "DRKS") %>%
  summarise(max(n_reg_pub_any)) %>%
  pull()

n_reg_pub_max_ctgov <-
  trials %>%
  filter(registry == "ClinicalTrials.gov") %>%
  summarise(max(n_reg_pub_any)) %>%
  pull()
