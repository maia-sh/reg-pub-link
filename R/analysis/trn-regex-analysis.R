si <- read_rds(here("data", "processed", "pubmed-si.rds"))

# Visually inspected all detected TRNs and are all TP (no FP)
si_tp <- nrow(filter(si, !is.na(trn)))
si_fp <- 0

# Visually inspect mismatching trns and registries

# NOT CATEGORIZED AS TRNs:
# 5 trns are severely misformatted drks with no preceding "drks" (n = 4) or only "s" (n = 1) (not detected)
# 3 accession numbers are figshare

# CATEGORIZED AS TRNs:
# MINOR MISFORMATTING CLEANED:
# 1 trn is eudract with preceeding letters (detected, preceeding letter removed)
# 1 trn is eudract with country code (detected, country code removed)
# INCORRECT REGISTRY:
# 1 registry is attributed to genbank but drks
# 1 registry is attributed to genbank but ct.gov
si_trn_mismatches <-
  si %>%
  filter(!accession_number %in% trn |
           !databank %in% registry)

# Encode si not categorized as trns
si_figshare <- 3
si_drks_misformatted <- 5

# Consider DRKS misformatting TN since severe and visually not classifiable
si_tn <- si_figshare + si_drks_misformatted
si_fn <- 0

# Calculate sens/spec
si_sens <- si_tp/(si_tp + si_fn)
si_spec <- si_tn/(si_tn + si_fp)

# Recalculate with alternate assumption that DRKS are TP
si_tp_alt <- si_tp + si_drks_misformatted
si_tn_alt <- si_figshare
si_fn_alt <- si_drks_misformatted

si_sens_alt <- si_tp_alt/(si_tp_alt + si_fn_alt)
si_spec_alt <- si_tn_alt/(si_tn_alt + si_fp)
