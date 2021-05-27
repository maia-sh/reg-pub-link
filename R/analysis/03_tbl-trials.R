# Summarize characteristics of trials and publications included in sample

# Summarize all trial info ------------------------------------------------

# tbl_trials_full <-
#   trials %>%
#
#   # Add indicators for none and all
#   mutate(
#     has_trn_all = case_when(
#       has_trn_secondary_id & has_trn_abstract & (has_trn_ft_pmc | has_trn_ft_pdf) ~ TRUE,
#       is.na(has_trn_secondary_id) | is.na(has_trn_abstract) | (is.na(has_trn_ft_pmc) & is.na(has_trn_ft_pdf)) ~ NA,
#       TRUE ~ FALSE
#     ),
#     has_trn_none = if_else(
#       !has_trn_secondary_id & !has_trn_abstract & (!has_trn_ft_pmc | !has_trn_ft_pdf),
#       TRUE, FALSE, missing = FALSE
#     )
#   ) %>%
#
#   select(
#     has_summary_results,
#     is_multicentric,
#     enrollment,
#     main_sponsor,
#     # recruitment_status, # could have changed
#     phase,
#     masking,
#     allocation,
#     intervention_type,
#     # lead_cities, # separate table/analysis?
#     starts_with("days_"),
#     starts_with("has_trn") & !ends_with("intovalue"),
#   ) %>%
#   gtsummary::tbl_summary(
#     label = list(
#       has_trn_any ~ "Anywhere",
#       has_trn_all ~ "Metadata + Abstract + Full-text (PMC or PDF)",
#       has_trn_none ~ "Nowhere",
#       has_trn_secondary_id ~ "PubMed Metadata" ,
#       has_trn_abstract ~ "Abstract",
#       has_trn_ft_pmc ~ "PMC Full-Text",
#       has_trn_ft_pdf ~ "PDF Full-Text"
#     ),
#     statistic =
#       # all_logical() ~
#       c("has_trn_any",
#         "has_trn_all",
#         "has_trn_none",
#         "has_trn_secondary_id",
#         "has_trn_abstract",
#         "has_trn_ft_pmc",
#         "has_trn_ft_pdf") ~
#       "{n}/{N_nonmiss} ({p}%)"#,
#     # missing = "no"
#   ) %>%
#     # modify_caption("**Trial registration number (TRN) reporting across published PubMed-indexed results from German UMC-led registered trials**")
#   modify_header(label = "**Trial registration number (TRN) reporting across published PubMed-indexed results from German UMC-led registered trials**")



# Summarize selected trial info -------------------------------------------

tbl_trials <-
  trials %>%

  # Add pubmed journal info and munge so display top 5
  left_join(select(pubmed_all, journal, pmid), by = "pmid") %>%
  # group_by(registry) %>%
  mutate(
    journal = if_else(journal == "Lancet (London, England)", "Lancet", journal),
    journal = forcats::fct_lump_n(journal, n = 5, other_level = NA),
    journal = forcats::fct_infreq(journal)
  ) %>%

  select(
    registry,
    days_cd_to_publication,
    # days_cd_to_summary,
    # has_summary_results,
    # is_multicentric,
    # enrollment,
    is_randomized,
    is_prospective,
    # n_crossreg_reg,
    # n_crossreg_abstract, n_crossreg_secondary_id, n_crossreg_ft_pdf,
    journal,
    completion_year
  ) %>%
  gtsummary::tbl_summary(
    by = registry,
    # # type = all_continuous() ~ "continuous2",
    # type = c(days_comp_to_pub, days_comp_to_summary) ~ "continuous2",
    # statistic =
    #   c(days_comp_to_pub, days_comp_to_summary) ~
    #   c("{N_nonmiss} ({p_nonmiss}%)",
    #     "{median} ({p25}, {p75})"#,
    #     # "{min}, {max}"
    #   ),
    # missing = "no",
    # missing_text = "NA",
    label = list(
      is_randomized ~ "Randomized",
      is_prospective ~ "Prospective registration",
      # n_crossreg_reg ~ "Cross-registrations in registry",
      # n_crossreg_abstract ~ "Additional TRNs in abstract",
      # n_crossreg_secondary_id ~ "Additional TRNs in PubMed metadata",
      # n_crossreg_ft_pdf ~ "Additional TRNs in full-text",
      days_cd_to_publication ~ "Time from trial completion to publication (days)",
      # has_summary_results ~ "Registry summary results available",
      # days_cd_to_summary ~ "Time from trial completion to summary results (days)",

      # is_multicentric ~ "Multicentric trial",
      # enrollment ~ "Trial enrollment",
      journal ~ "Top 5 journals",
      completion_year ~ "Trial completion date"
    )
    # sort = list(
    #   cross_registry ~ "frequency",
    #   countries ~ "frequency"
    # )
  ) %>%

  # Move stats legend to each line
  add_stat_label() %>%

  modify_header(label = "**German UMC-led trials with published results**") %>%
  # modify_caption("**German UMC-led trials with published result** (N = {N})") %>%
  bold_labels()
