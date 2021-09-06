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

# ClinicalTrials.gov and DRKS use different phase names
# We use IntoValue's lookup table to coalesce names

phase_lookup <-
  read_csv("https://zenodo.org/record/5141343/files/iv_data_lookup_registries.csv?download=1") %>%
  filter(name == "phase") %>%
  select(phase = level_registry, phase_unified = level_unified)

N_JOURNALS <- 6

tbl_trials <-
  trials %>%

  # Add pubmed journal info and munge so display top N_JOURNALS
  # left_join(select(pubmed_all, journal, pmid), by = "pmid") %>%
  rename(journal = journal_pubmed) %>%
  mutate(
    journal = if_else(journal == "Lancet (London, England)", "Lancet", journal),


    # journal = stringi::stri_trans_totitle(journal),
    journal = gsub("\\b([a-z])", "\\U\\1", journal, perl=TRUE),
    journal = str_replace(journal, "Of", "of"),

    journal = forcats::fct_lump_n(journal, n = N_JOURNALS, other_level = NA),
    journal = forcats::fct_infreq(journal)
  ) %>%


  # Prepare industry sponsor
  mutate(industry_sponsor = ifelse(main_sponsor == "Industry", TRUE, FALSE)) %>%

  # Prepare phase
  left_join(phase_lookup, by = "phase") %>%

  select(
    registry,
    days_cd_to_publication,
    days_cd_to_summary,
    has_summary_results,
    is_prospective,
    is_randomized,
    is_multicentric,
    industry_sponsor,
    enrollment,
    phase_unified,
    # center_size
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
      has_summary_results ~ "Registry summary results available",
      days_cd_to_summary ~ "Time from trial completion to summary results (days)",

      is_multicentric ~ "Multicentric trial",
      enrollment ~ "Trial enrollment",
      industry_sponsor ~ "Industry sponsor",
      phase_unified ~ "Phase",
      journal ~ glue::glue("Top {N_JOURNALS} journals"),
      completion_year ~ "Trial completion date"
    ),
    # sort = list(
    #   cross_registry ~ "frequency",
    #   countries ~ "frequency"
    # )
    digits = everything() ~ 0
  ) %>%

  add_overall() %>%

  # Move stats legend to each line
  add_stat_label() %>%

  # modify_header(label = "**German UMC-led trials with published results**") %>%
  modify_caption("**Characteristics of German UMC-conducted trials with published results** A trial was considered randomized if allocation included randomization. A trial was considered prospectively registered if registered in the same or previous months to start date. Summary results were taken from a structured data field in ClinicalTrials.gov, and determined based on manual inspection for terms such as *Ergebnisbericht* or *Abschlussbericht* in DRKS. *Top* journals refer to the journals with the greatest number of trial publications in our sample.") %>%
  bold_labels() %>%

  # Remove rowname label
  modify_header(label = "")

# x<-
# tbl_trials %>%
#   as_gt() %>%
#   tab_footnote(
#     footnote = "Color indicates height of sun.",
#     locations = cells_stub(
#     rows = 2
#   )
# )
