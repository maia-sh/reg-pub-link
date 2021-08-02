# http://www.danieldsjoberg.com/gt-and-gtsummary-presentation/#34
library(dplyr)
library(labelled)
library(gtsummary)


# Prepare trials and regression functions ---------------------------------

# Convert variable to labelled factors for regressions
trials_model <-
  trials %>%
  mutate(
    across(c(registry, has_reg_pub_link, starts_with("has_iv_trn_")),
           as.factor)
  ) %>%
  labelled::set_variable_labels(
    has_reg_pub_link = "Publication in Registry",
    has_iv_trn_secondary_id = "TRN in PubMed Metadata",
    has_iv_trn_abstract = "TRN in Abstract",
    has_iv_trn_ft_pdf = "TRN in Full-text",
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

    select(completion_year, registry, has_iv_trn_secondary_id, has_iv_trn_ft_pdf, has_reg_pub_link, has_iv_trn_abstract) %>%

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
      label = "**cOR (95% CI; p value)**",
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
      has_iv_trn_secondary_id + has_iv_trn_abstract + has_iv_trn_ft_pdf"
  )


aor_si <-
  tbl_glm_mv_trials(
    "has_iv_trn_secondary_id ~ completion_year + registry +
      has_iv_trn_abstract + has_iv_trn_ft_pdf + has_reg_pub_link"
  )

aor_abs <-
  tbl_glm_mv_trials(
    "has_iv_trn_abstract ~ completion_year + registry +
      has_iv_trn_secondary_id + has_iv_trn_ft_pdf + has_reg_pub_link"
  )

aor_ft <-
  tbl_glm_mv_trials(
    "has_iv_trn_ft_pdf ~ completion_year + registry +
      has_iv_trn_secondary_id + has_iv_trn_abstract + has_reg_pub_link"
  )


# Prepare univariate regressions (crude odds ratio) -----------------------

cor_reg <- tbl_glm_uv_trials(has_reg_pub_link)
cor_si <- tbl_glm_uv_trials(has_iv_trn_secondary_id)
cor_abs <- tbl_glm_uv_trials(has_iv_trn_abstract)
cor_ft <- tbl_glm_uv_trials(has_iv_trn_ft_pdf)


# Display regressions (grouped by outcome) --------------------------------
# i.e., both cor and aor for publication in registry

or_reg <- tbl_merge(list(cor_reg, aor_reg))
or_si <- tbl_merge(list(cor_si, aor_si))
or_abs <- tbl_merge(list(cor_abs, aor_abs))
or_ft <- tbl_merge(list(cor_ft, aor_ft))

tbl_trials_regression <-
  tbl_merge(
    list(or_reg, or_si, or_abs, or_ft),
    tab_spanner = c("Publication in Registry", "TRN in PubMed Metadata", "TRN in Abstract", "TRN in Full-text")
  ) %>%
  modify_footnote(update = everything() ~ NA, abbreviation = TRUE) %>%
  modify_footnote(update = starts_with("estimate") ~ "cOR = Crude Odds Ratio, aOR = Adjusted Odds Ratio, CI = Confidence Interval") #%>%

#TODO: replace empty cells with explicit NA or dash
#https://stackoverflow.com/questions/68593672/change-empty-cells-in-gtsummarytbl-merge
tbl_trials_regression %>%
# modify_table_styling(
#   missing_symbol = "HELLO"
# )
modify_table_styling(
  column = everything(),
  rows = !is.na(variable),
  missing_symbol = "Hi"
)


# Display regressions (grouped by regression type) ------------------------
# i.e., all cor for all outcomes, before any aor

# tbl_merge(
#   tbls = list(aor_reg, aor_si, aor_abs, aor_ft),
#   tab_spanner = c("AOR Publication in Registry", "AOR TRN in PubMed Metadata", "AOR TRN in Abstract", "AOR TRN in Full-text")
# ) %>%
#   modify_spanning_header(starts_with("estimate") ~ "**Adjusted Odds Ratio**")

#modify_header(update = all_stat_cols() ~ "**{level}**, N = {n} ({style_percent(p)}%)")


# DEPRECATED --------------------------------------------------------------

# or_abs <-
#   tbl_merge(
#     tbls = list(cor_abs, aor_abs),
#     # tab_spanner = "TRN in Abstract"
#   ) %>% #show_header_names()
#   modify_spanning_header(c(estimate_1, estimate_2) ~  "TRN in Abstract")
#
# or_reg <-
#   tbl_merge(list(cor_reg, aor_reg)) %>%
#   modify_spanning_header(starts_with("estimate") ~  "Publication in Registry")
#
# or_si <-
#   tbl_merge(list(cor_si, aor_si)) %>%
#   modify_spanning_header(starts_with("estimate") ~  "TRN in PubMed Metadata")
#
# or_abs <-
#   tbl_merge(list(cor_abs, aor_abs)) %>%
#   modify_spanning_header(starts_with("estimate") ~  "TRN in Abstract")
#
# or_ft <-
#   tbl_merge(list(cor_ft, aor_ft)) %>%
#   modify_spanning_header(starts_with("estimate") ~  "TRN in Full-text")
