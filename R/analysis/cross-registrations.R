n_trials_crossreg_reg <-
  trials %>%
  filter(n_crossreg_reg > 0) %>%
  nrow()

n_crossreg_reg <- sum(trials$n_crossreg_reg)

# # by registry
# trials %>% count(registry, n_crossreg_reg > 0)
# trials %>%
#   mutate(has_crossreg_reg = n_crossreg_reg > 0) %>%
#   select(registry, has_crossreg_reg) %>%
#   group_by(registry, has_crossreg_reg) %>%
#   summarise(n = n()) %>%
#   mutate(prop = n / sum(n))

tbl_crossreg <-
  trials %>%
  select(
    registry,
    n_crossreg_reg,
    n_crossreg_abstract, n_crossreg_secondary_id, n_crossreg_ft_pdf
  ) %>%
  gtsummary::tbl_summary(
    by = registry,
    label = list(
      n_crossreg_reg ~ "Cross-registrations in registry",
      n_crossreg_abstract ~ "Additional TRNs in abstract",
      n_crossreg_secondary_id ~ "Additional TRNs in PubMed metadata",
      n_crossreg_ft_pdf ~ "Additional TRNs in full-text"
    )
  ) %>%
  add_stat_label(location = NULL) %>%

  modify_header(label = "**Cross-registrations and additional TRNs**") %>%
  # modify_caption("**German UMC-led trials with published result** (N = {N})") %>%
  bold_labels()
