# Report results as: N (%)
# Most results are relative to `n_trials_ft_pdf`
# However, some have different denominator, so optional parameter
report_n_pct_trials <- function(numerator, denominator = n_trials_ft_pdf){
  pct <- gtsummary::style_percent(numerator/denominator, digits = 0, symbol =  TRUE)
  glue::glue("{numerator} ({pct})")
}

# Report results as % (n = NUM/DENOM)
report_pct_num_denom_trials <- function(numerator, denominator = n_trials_ft_pdf){
  pct <- gtsummary::style_percent(numerator/denominator, digits = 0, symbol =  TRUE)
  glue::glue("{pct} ({numerator}/{denominator})")
}

# report_n_pct_trials(n_trials_link_reg_drks, n_trials_drks)
# report_pct_num_denom_trials(n_trials_link_reg_drks, n_trials_drks)

# Full linkage ------------------------------------------------------------

n_trials_link_full <-
  trials %>%
  filter(has_reg_pub_link &
           has_iv_trn_secondary_id &
           has_iv_trn_abstract &
           has_iv_trn_ft_pdf
  ) %>%
  nrow()

n_trials_link_none <-
  trials %>%
  filter(
    !has_reg_pub_link &
      !has_iv_trn_secondary_id &
      !has_iv_trn_abstract &
      !has_iv_trn_ft_pdf
  ) %>%
  nrow()

n_trials_link_ft_abs_reg <-
  trials %>%
  filter(has_reg_pub_link &
           # has_iv_trn_secondary_id &
           has_iv_trn_abstract &
           has_iv_trn_ft_pdf
  ) %>%
  nrow()


# Publication reference in registration -----------------------------------

n_trials_link_reg <-
  trials %>%
  filter(has_reg_pub_link) %>%
  nrow()

n_trials_link_reg_ctgov <-
  trials %>%
  filter(has_reg_pub_link & registry == "ClinicalTrials.gov") %>%
  nrow()

n_trials_link_reg_drks <-
  trials %>%
  filter(has_reg_pub_link & registry == "DRKS") %>%
  nrow()

n_trials_ctgov <-
  trials %>%
  filter(registry == "ClinicalTrials.gov") %>%
  nrow()

n_trials_drks <-
  trials %>%
  filter(registry == "DRKS") %>%
  nrow()


# Trial registration number reporting -------------------------------------

n_trials_link_trn_none <-
  trials %>%
  filter(!has_iv_trn_secondary_id &
           !has_iv_trn_abstract &
           !has_iv_trn_ft_pdf
  ) %>%
  nrow()

n_trials_link_trn_all <-
  trials %>%
  filter(has_iv_trn_secondary_id &
           has_iv_trn_abstract &
           has_iv_trn_ft_pdf
  ) %>%
  nrow()

n_trials_link_trn_si <-
  trials %>%
  filter(has_iv_trn_secondary_id) %>%
  nrow()

n_trials_link_trn_abs <-
  trials %>%
  filter(has_iv_trn_abstract) %>%
  nrow()

n_trials_link_trn_ft <-
  trials %>%
  filter(has_iv_trn_ft_pdf) %>%
  nrow()

n_trials_link_trn_ft_abs <-
  trials %>%
  filter(has_iv_trn_ft_pdf & has_iv_trn_abstract) %>%
  nrow()

n_trials_link_trn_ft_only <-
  trials %>%
  filter(has_iv_trn_ft_pdf & !has_iv_trn_abstract & !has_iv_trn_secondary_id & !has_reg_pub_link) %>%
  nrow()

# How many trials with trn in ft or abs do not have trn in si?
n_trials_link_trn_ft_or_abs <-
  trials %>%
  filter(has_iv_trn_ft_pdf | has_iv_trn_abstract) %>%
  nrow()

n_trials_link_trn_ft_or_abs_no_si <-
  trials %>%
  filter(has_iv_trn_ft_pdf | has_iv_trn_abstract) %>%
  filter(!has_iv_trn_secondary_id) %>%
  nrow()


n_trials_link_trn_si_drks <-
  trials %>%
  filter(has_iv_trn_secondary_id & registry == "DRKS") %>%
  nrow()

# How many drks trials with trn in si have reg link
trials %>%
  filter(has_iv_trn_secondary_id & registry == "DRKS") %>%
  count(has_reg_pub_link)

trials %>%
  filter(has_iv_trn_secondary_id & registry == "ClinicalTrials.gov") %>%
  count(has_reg_pub_link)

# For all the xxx publications that provided a TRN either in the abstract or in full text, a TRN was available in the metadata in x% (n=xxx).

trials %>%
  filter(has_iv_trn_ft_pdf | has_iv_trn_abstract) %>%
  count(registry, has_reg_pub_link)
705/(705+372)
62/(62+233)

trials %>%
  filter(has_iv_trn_ft_pdf & has_iv_trn_abstract) %>%
  count(registry, has_reg_pub_link)
365/(365+16)
17/(17+75)

trials %>%
  filter(has_iv_trn_ft_pdf | has_iv_trn_abstract) %>%
  count(registry, has_reg_pub_link, has_iv_trn_ft_pdf, has_iv_trn_abstract)

trials %>%
  filter(has_iv_trn_abstract) %>%
  count(registry, has_reg_pub_link)
563/(563+17)
31/(31+103)

trials %>%
  filter(has_iv_trn_ft_pdf) %>%
  count(registry, has_reg_pub_link)
507/(507+371)
48/(48+205)


# Compare derived links and trn in metadata -------------------------------

# Check that all trials with derived links:
# are ctgov, have trn in si, and have pub link in registry
# And if so save number of trials
n_trials_ctgov_link_reg_trn_si_derived <-
  trials %>%
  filter(reference_derived) %>%
  pointblank::col_vals_equal(
    vars(has_iv_trn_secondary_id, has_reg_pub_link),
    value = TRUE
  ) %>%
  pointblank::col_vals_equal(
    vars(has_iv_trn_secondary_id, has_reg_pub_link),
    value = TRUE
  ) %>%
  nrow()

# Ctgov trials with trn in si
trials_ctgov_link_trn_si <-
  trials %>%
  filter(registry == "ClinicalTrials.gov", has_iv_trn_secondary_id)

n_trials_ctgov_link_trn_si <- nrow(trials_ctgov_link_trn_si)

# Ctgov trials with trn in si with linked pub (derived and manual)
trials_ctgov_link_reg_trn_si <-
  trials_ctgov_link_trn_si %>%
  filter(has_reg_pub_link)

n_trials_ctgov_link_reg_trn_si <- nrow(trials_ctgov_link_reg_trn_si)

# Ctgov trials with trn in si with linked pub (DERIVED)
# NOTE: calculated above
# n_trials_ctgov_link_reg_trn_si_derived <-
#   trials_ctgov_link_reg_trn_si %>%
#   filter(reference_derived) %>%
#   nrow()

# Ctgov trials with trn in si with linked pub (MANUAL, i.e., not derived)
n_trials_ctgov_link_reg_trn_si_manual <-
  trials_ctgov_link_reg_trn_si %>%
  filter(!reference_derived) %>%
  nrow()

# Ctgov trials with trn in si with NO linked pub
# This should be 0 because auto-link.
# One edge case: NCT01614301 26417987 where trn in pubmed si but under "genbank" so ctgov doesn't index it.
n_trials_ctgov_no_link_reg_trn_si <-
  trials_ctgov_link_trn_si %>%
  filter(!has_reg_pub_link) %>%
  nrow()

if (n_trials_ctgov_no_link_reg_trn_si != 1){
  rlang::abort("There are more/fewer trials with trn in secondary id and no linked pub in ctgov!")
}

# How many drks trials have trn in si AND do NOT have linked pub? DRKS could index these.
n_trials_drks_trn_si_not_linked_pub <-
  trials %>%
  filter(
    registry == "DRKS",
    has_iv_trn_secondary_id,
    !has_reg_pub_link
  ) %>%
  nrow()


# TODO
# How many trials have linked pub, excluding derived?
trials %>%

  # Remove automatic links
  filter(!reference_derived | is.na(reference_derived)) %>%
  # count(registry, has_reg_pub_link) %>%
  group_by(registry, has_reg_pub_link) %>%
  summarise(n = n()) %>%
  mutate(prop = n / sum(n))

# 2021-05-20 daniel comment -----------------------------------------------
# Daniel: From the 92 publications that reported a TRN in the metadata but neither in abstract nor full text almost all registrations (91) linked this publication manually

# trials %>%
#   filter(has_iv_trn_secondary_id, !has_iv_trn_abstract, !has_iv_trn_ft_pdf) %>%
#   count(registry, has_reg_pub_link)

# Would be: From the 101 trials (88 ctgov, 13 drks) that reported a TRN in the metadata but neither in abstract nor full text, almost all ctgov registrations (87/88) linked this publication manually whereas few drks registrations (1/13) did so.


# Compare linkage by registry ---------------------------------------------
tbl_link_by_reg <-
  trials %>%
  select(
    registry,
    has_reg_pub_link,
    has_iv_trn_secondary_id,
    has_iv_trn_abstract,
    has_iv_trn_ft_pdf
  ) %>%
  gtsummary::tbl_summary(
    by = registry,
    label = list(
      has_reg_pub_link ~ "Reference in registration",
      has_iv_trn_secondary_id ~ "TRN (PubMed metadata)",
      has_iv_trn_abstract ~ "TRN (Abstract)",
      has_iv_trn_ft_pdf ~ "TRN (Full-text)"
    )
  ) %>%

  #TODO: figure out what test/stats to include
  add_difference() %>%
  # add_p() %>%
  add_overall() %>% #show_header_names()
  # Move stats legend to each line
  # add_stat_label(location = NULL) %>%
  modify_header(label = "**Registration-Publication Linkage**") %>%
  # modify_caption("**German UMC-led trials with published result** (N = {N})") %>%
  bold_labels()

# TODO: Continuity correct set to FALSE. is this ok?
# chisq_link_by_reg_pdf_tidy <-
#   infer::chisq_test(trials, registry ~ has_iv_trn_ft_pdf, correct = FALSE)

chisq_link_by_reg_reg <-
  chisq.test(trials$registry, trials$has_reg_pub_link, correct = FALSE)

chisq_link_by_reg_si <-
  chisq.test(trials$registry, trials$has_iv_trn_secondary_id, correct = FALSE)

chisq_link_by_reg_abs <-
  chisq.test(trials$registry, trials$has_iv_trn_abstract, correct = FALSE)

chisq_link_by_reg_pdf <-
  chisq.test(trials$registry, trials$has_iv_trn_ft_pdf, correct = FALSE)

# apa_print(chisq.test(trials$registry, trials$has_iv_trn_ft_pdf, correct = FALSE))
# apa_print(chisq_link_by_reg_pdf)

# apa::apa(chisq_link_by_reg_pdf, print_n = TRUE)
# apastats::describe.chi(chisq_link_by_reg_pdf, addN=T)
# *X2* (`r chisq_link_by_reg_pdf$parameter`, N = `r sum(chisq_link_by_reg_pdf$observed)`) = `r round(chisq_link_by_reg_pdf$statistic, digits = 2)`, p = `r round(chisq_link_by_reg_pdf$p.value, digits = 2)`
# (*X2* (`r chisq_link_by_reg_pdf$parameter`, N = `r sum(chisq_link_by_reg_pdf$observed)`) = `r printnum(chisq_link_by_reg_pdf$statistic)`, p = `r printp(chisq_link_by_reg_pdf$p.value)`)

# https://calerovaldez.com/post/using-the-apastats-package-to-write-reproducible-reports/
# http://frederikaust.com/papaja_man/reporting.html
# https://benwhalley.github.io/just-enough-r/apa-output.html
