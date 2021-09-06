# Individual results ------------------------------------------------------

# Trials with all links
n_trials_link_full <-
  trials %>%
  filter(
    has_reg_pub_link &
      has_iv_trn_secondary_id &
      has_iv_trn_abstract &
      has_iv_trn_ft
  ) %>%
  nrow()

# Trials with no links
n_trials_link_none <-
  trials %>%
  filter(
    !has_reg_pub_link &
      !has_iv_trn_secondary_id &
      !has_iv_trn_abstract &
      !has_iv_trn_ft
  ) %>%
  nrow()

# Trials with full-text and abstract links (CONSORT/ICMJE compliant)
n_trials_link_ft_abs <-
  trials %>%
  filter(has_iv_trn_ft & has_iv_trn_abstract) %>%
  nrow()

# Trials with full-text, abstract and registry links (PubMed metadata not in researchers control)
n_trials_link_ft_abs_reg <-
  trials %>%
  filter(
    has_reg_pub_link &
      has_iv_trn_abstract &
      has_iv_trn_ft
  ) %>%
  nrow()

# Trials with only full-text link
n_trials_link_ft_only <-
  trials %>%
  filter(
    has_iv_trn_ft &
      !has_iv_trn_abstract & !has_iv_trn_secondary_id & !has_reg_pub_link
  ) %>%
  nrow()

# Trials with trn in ft or abs
n_trials_link_ft_or_abs <-
  trials %>%
  filter(has_iv_trn_ft | has_iv_trn_abstract) %>%
  nrow()

# Trials with trn in ft or abs and NOT in si
n_trials_link_ft_or_abs_no_si <-
  trials %>%
  filter(
    has_iv_trn_ft | has_iv_trn_abstract,
    !has_iv_trn_secondary_id
  ) %>%
  nrow()

# DRKS trials with trn in si
n_trials_drks_link_si <-
  trials %>%
  filter(
    registry == "DRKS",
    has_iv_trn_secondary_id
  ) %>%
  nrow()

# DRKS trials with trn in si and NOT linked pub in registry
n_trials_drks_link_si_not_reg <-
  trials %>%
  filter(
    registry == "DRKS",
    has_iv_trn_secondary_id,
    !has_reg_pub_link
  ) %>%
  nrow()



# ClinicalTrials.gov trials with publication in registration: auto vs. manual
# inline_text(tbl_reg_pub_link_ctgov_auto_v_manual, reference_derived, column = "stat_1", pattern = "{p}%, {n}/{N}")
# report_trials(n_trials_link_reg_ctgov_auto, n_trials_link_reg_ctgov, pattern = "{p}, {n}/{N}")

# ClinicalTrials.gov trials with publication in registration
# inline_text(tbl_link_by_reg, has_reg_pub_link, "ClinicalTrials.gov")
n_trials_link_reg_ctgov <-
  trials %>%
  filter(
    registry == "ClinicalTrials.gov",
    has_reg_pub_link
  ) %>%
  nrow()

# ClinicalTrials.gov trials with publication in registration, derived from PubMed si
n_trials_link_reg_ctgov_auto <-
  trials %>%
  filter(
    registry == "ClinicalTrials.gov",
    has_reg_pub_link,
    reference_derived
  ) %>%
  nrow()

# How many publications (including iv) are listed in registrations?

reg_pub_max_drks <-
  trials %>%
  filter(registry == "DRKS") %>%
  slice_max(n_reg_pub_any)

reg_pub_max_ctgov <-
  trials %>%
  filter(registry == "ClinicalTrials.gov") %>%
  slice_max(n_reg_pub_any)

# Tabular results ---------------------------------------------------------

# Prevalence of link types, overall and by registry
tbl_link_by_reg <-
  trials %>%
  select(
    registry,
    has_iv_trn_ft,
    has_iv_trn_abstract,
    has_iv_trn_secondary_id,
    has_reg_pub_link
  ) %>%
  gtsummary::tbl_summary(
    by = registry,
    label = list(
      has_iv_trn_ft ~ "TRN in full-text",
      has_iv_trn_abstract ~ "TRN in abstract",
      has_iv_trn_secondary_id ~ "TRN in PubMed metadata",
      has_reg_pub_link ~ "Publication in registration"
    )
  ) %>%

  add_overall() %>%
   #show_header_names()
  # Move stats legend to each line
  # add_stat_label(location = NULL) %>%
  # modify_header(label = NULL) #%>%
  modify_header(label = "") %>%
  modify_caption("Registration-publication links overall and by registry") %>%
  # modify_header(label = "**Registration-Publication Linkage**") %>%
  # modify_caption("**German UMC-led trials with published result** (N = {N})") %>%
  bold_labels() %>%
  # show_header_names()
  # modify_header(update = all_stat_cols() ~ "**{level}**<br>N = {N}") %>%
  # add_overall(col_label = "**Overall**<br>N = {N}") %>%
  # modify_header(update = all_stat_cols() ~ "**{level}**\nN = {N}") %>%
  # add_overall(col_label = "**Overall**\nN = {N}") %>%
  modify_footnote(everything() ~ NA)

# Prevalence of link types, overall and by registry
# Trials published as of 2014 to account for MEDLINE incorporation of DRKS
# https://www.nlm.nih.gov/bsd/medline_databank_source.html
tbl_link_by_reg_2014 <-
  trials %>%
  filter(publication_date > as.Date("2013-12-31")) %>%
  select(
    registry,
    has_iv_trn_ft,
    has_iv_trn_abstract,
    has_iv_trn_secondary_id,
    has_reg_pub_link
  ) %>%
  gtsummary::tbl_summary(
    by = registry,
    label = list(
      has_iv_trn_ft ~ "TRN in full-text",
      has_iv_trn_abstract ~ "TRN in abstract",
      has_iv_trn_secondary_id ~ "TRN in PubMed metadata",
      has_reg_pub_link ~ "Publication in registration"
    )
  ) %>%
  add_overall() %>%
  modify_header(label = "") %>%
  modify_caption("Registration-publication links overall and by registry for trials published as of 2014") %>%
  bold_labels() %>%
  modify_footnote(everything() ~ NA)

# Manually-linked publication in registration -----------------------------

tbl_reg_pub_link_manual <-
  trials %>%
  filter(!reference_derived | is.na(reference_derived)) %>%
  select(registry, has_reg_pub_link) %>%
  gtsummary::tbl_summary(
    by = registry,
    statistic = everything() ~ "{p}% ({n} / {N})",
    label = everything() ~ "Publication in registration"
  ) %>%

  # Remove rowname label
  modify_header(label = "") %>%

  # Remove N from column names
  modify_header(all_stat_cols() ~ "**{level}**") %>%

  # Add overall column and remove N
  add_overall() %>%
  modify_header(stat_0 ~ "**Overall**") %>%

  # Remove footnote
  modify_footnote(everything() ~ NA) %>%
  bold_labels()
