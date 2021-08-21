# http://www.danieldsjoberg.com/gt-and-gtsummary-presentation/#34

# Prepare trials and regression functions ---------------------------------

# Convert variable to labelled factors for regressions
trials_model <-
  trials %>%
  mutate(
    across(c(registry, has_reg_pub_link, starts_with("has_iv_trn_")),
           as.factor)
  ) %>%
  labelled::set_variable_labels(
    has_iv_trn_ft = "TRN in full-text",
    has_iv_trn_abstract = "TRN in abstract",
    has_iv_trn_secondary_id = "TRN in PubMed metadata",
    has_reg_pub_link = "Publication in registration",
    completion_year = "Completion year",
    registry = "Registry"
  )

# Multivariate regressions (adjusted odds ratio)
tbl_glm_mv_trials <- function(trials_formula) {

  glm(
    formula = trials_formula,
    data = trials_model,
    family = "binomial"
  ) %>%

    gtsummary::tbl_regression(
      exponentiate = TRUE,
      show_single_row = c(-completion_year, -registry),
      # add_estimate_to_reference_rows = TRUE # Change reference row
    ) %>%

    modify_table_styling(
      column = estimate,
      rows = !is.na(estimate),
      cols_merge_pattern = "{estimate} ({ci}; {p.value})",
      label = "**aOR (95% CI; p-value)**"
    ) %>%

    # Change reference label
    # Thanks to gtsummary dev: https://github.com/ddsjoberg/gtsummary/issues/942
    modify_table_styling(
      column = estimate,
      rows = reference_row %in% TRUE,
      missing_symbol = "1 (Ref)"
    ) %>%

    modify_header(update = list(label ~ ""))
}

# Univariate regressions (crude odds ratio)

tbl_glm_uv_trials <- function(outcome) {

  trials_model %>%

    select(completion_year, registry, has_iv_trn_ft, has_iv_trn_abstract, has_iv_trn_secondary_id,  has_reg_pub_link) %>%

    tbl_uvregression(
      method = glm,
      y = {{outcome}},
      method.args = list(family = binomial),
      exponentiate = TRUE,
      hide_n = TRUE,
      show_single_row = c(-completion_year, -registry)
    ) %>%

    # Thanks to gtsummary dev: https://stackoverflow.com/questions/68359367/change-gtsummarytbl-regression-columns
    modify_table_styling(
      column = estimate,
      rows = !is.na(estimate),
      cols_merge_pattern = "{estimate} ({ci}; {p.value})",
      label = "**cOR (95% CI; p-value)**",
      # footnote_abbrev = "cOR = Crude Odds Ratio,  CI = Confidence Interval "
    ) %>%

    # Change reference label
    modify_table_styling(
      column = estimate,
      rows = reference_row %in% TRUE,
      missing_symbol = "1 (Ref)"
    ) %>%

    modify_header(update = list(label ~ ""))
  # modify_footnote(or ~ "cOR = Crude Odds Ratio")
}


# Prepare multivariate regressions (adjusted odds ratio) ------------------

aor_reg <-
  tbl_glm_mv_trials(
    "has_reg_pub_link ~ completion_year + registry +
      has_iv_trn_ft + has_iv_trn_abstract + has_iv_trn_secondary_id"
  )


aor_si <-
  tbl_glm_mv_trials(
    "has_iv_trn_secondary_id ~ completion_year + registry +
      has_iv_trn_ft + has_iv_trn_abstract + has_reg_pub_link"
  )

aor_abs <-
  tbl_glm_mv_trials(
    "has_iv_trn_abstract ~ completion_year + registry +
      has_iv_trn_ft + has_iv_trn_secondary_id + has_reg_pub_link"
  )

aor_ft <-
  tbl_glm_mv_trials(
    "has_iv_trn_ft ~ completion_year + registry +
      has_iv_trn_abstract + has_iv_trn_secondary_id + has_reg_pub_link"
  )


# Prepare univariate regressions (crude odds ratio) -----------------------

cor_ft <- tbl_glm_uv_trials(has_iv_trn_ft)
cor_abs <- tbl_glm_uv_trials(has_iv_trn_abstract)
cor_si <- tbl_glm_uv_trials(has_iv_trn_secondary_id)
cor_reg <- tbl_glm_uv_trials(has_reg_pub_link)

# Display regressions (grouped by outcome) --------------------------------
# i.e., both cor and aor for publication in registry

or_ft <- tbl_merge(list(cor_ft, aor_ft))
or_abs <- tbl_merge(list(cor_abs, aor_abs))
or_si <- tbl_merge(list(cor_si, aor_si))
or_reg <- tbl_merge(list(cor_reg, aor_reg))

tbl_trials_regression <-
  tbl_merge(
    list(or_ft, or_abs, or_si, or_reg),
    tab_spanner = c("TRN in Full-text", "TRN in abstract", "TRN in PubMed metadata", "Publication in registration")
  ) %>%
  modify_footnote(update = everything() ~ NA, abbreviation = TRUE) %>%
  modify_footnote(update = starts_with("estimate") ~ "cOR = Crude Odds Ratio, aOR = Adjusted Odds Ratio, CI = Confidence Interval") %>%
  modify_caption("Crude and adjusted odds ratios for factors associated with publication-registration links")

#TODO: replace empty cells with explicit NA or dash
#https://stackoverflow.com/questions/68593672/change-empty-cells-in-gtsummarytbl-merge
# object.size(tbl_trials_regression_old) %>% format(units = "Mb")
# object.size(tbl_trials_regression_butchered) %>% format(units = "Mb")
# tbl_trials_regression_butchered <-
# tbl_trials_regression_butchered %>%  #bstfun::gtsummary_butcher() %>%
#   modify_table_styling(
#     column = everything(),
#     rows = !is.na(variable),
#     missing_symbol = "Hi"
# )
