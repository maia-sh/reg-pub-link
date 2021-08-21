# Generic reporting function
# Based on gtsummary::inline_text
# Default denominator (N) is analysis population (`n_trials_ft`)
# report_trials(1137")
# report_trials(1137, 5000")
# report_trials(1137, pattern = "{p}% ({n}/{N})")
report_trials <- function(n,
                          N = n_trials_ft,
                          pattern = "{n} ({p}%)",
                          digits = 0){
  # Format numbers
  p <- gtsummary::style_percent(n/N, digits = digits, symbol =  FALSE)
  n <- gtsummary::style_number(n)
  N <- gtsummary::style_number(N)

  glue::glue(pattern)
}


