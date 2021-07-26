# Function to create tables for si by registry ----------------------------

tbl_si_registry <- function(var, var_name) {
  trials %>%
    mutate(has_iv_trn_abs_or_ft = has_iv_trn_abstract | has_iv_trn_ft_pdf) %>%
    filter({{var}}) %>%
    select(registry, has_iv_trn_secondary_id) %>%
    gtsummary::tbl_summary(
      by = registry,
      statistic = has_iv_trn_secondary_id ~ "{p}% ({n} / {N})",
      label = has_iv_trn_secondary_id ~ var_name
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
}


# Prepare abstract and full-text tables -----------------------------------
tbl_si_abs_or_ft <- tbl_si_registry(has_iv_trn_abs_or_ft, "TRN in abstract or full-text")
tbl_si_abs <- tbl_si_registry(has_iv_trn_abstract, "TRN in abstract")
tbl_si_ft <- tbl_si_registry(has_iv_trn_ft_pdf, "TRN in full-text")


# Display si table --------------------------------------------------------

tbl_si_if_abs_ft <-
  tbl_stack(list(tbl_si_abs_or_ft, tbl_si_abs, tbl_si_ft)) %>%
  modify_caption("Number and proportion of trials with TRN in metadata given TRN in abstract, full-text, or either. Denominator indicates number of trials with TRN in abstract and/or full-text, from the respective registry.")
