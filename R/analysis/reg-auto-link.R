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


# ClinicalTrials.gov publication in registration: auto vs. manual --------

tbl_reg_pub_link_ctgov_auto_v_manual <-

  trials %>%
  filter(
    registry == "ClinicalTrials.gov",
    has_reg_pub_link
  ) %>%
  # tbl_cross(
  #   has_reg_pub_link, reference_derived,
  #   percent = "row",
  #   margin = "row"
  # )
  select(registry, reference_derived) %>%
  gtsummary::tbl_summary(
    by = registry,
    statistic = everything() ~ "{p}% ({n} / {N})",
    label = everything() ~ "Automated link"
  ) %>%

  # Remove rowname label
  modify_header(label = "") %>%

  # Remove N from column names
  modify_header(all_stat_cols() ~ "**{level}**") %>%

  # Remove footnote
  modify_footnote(everything() ~ NA) %>%
  bold_labels()
