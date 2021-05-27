# Prepare labels
label_iv_all <- glue('IntoValue trials\n(n = {n_trials_iv})\nIV1 = {n_trials_iv1} & IV2 = {n_trials_iv2}')
label_iv_screened <- glue('Trials meeting IntoValue criteria\nwith new registry data\n(n = {n_trials_iv_interventional})')
label_trials_lead <- glue('IntoValue German UMC Lead Trials\n(n = {n_trials_lead})')
label_trials_deduped <- glue('Trials with duplicates removed\n(n = {n_trials_deduped})')
label_pub <- glue('Trials with published results\n(n = {n_trials_pub})')
label_pmid <- glue('Trials with published results\nwith PMID\n(n = {n_trials_pmid})')
label_pubmed <- glue('Trials with published results\nwith resolved PubMed record\n(n = {n_trials_pubmed})')
label_fulltext <- glue('Trials with published results\nas full-text (pdf)\n(n = {n_trials_ft_pdf})')

label_iv_screened_ex <- glue('Trials not meeting IntoValue criteria\nwith new registry data\n(n = {n_trials_iv_completion_date_ex + n_trials_iv_status_ex + n_trials_iv_interventional_ex})\nNot completed 2009-2017 (n = {n_trials_iv_completion_date_ex})\nNot included status (n = {n_trials_iv_status_ex})\nNot interventional (n = {n_trials_iv_interventional_ex})')
label_iv_lead_ex <- glue('No German UMC Lead\n(n = {n_trials_lead_ex})')
label_trials_dupes_ex <- glue('Duplicate trials\n(n = {n_trials_dupes_ex})')
label_pub_ex <- glue('No published results\n(n = {n_trials_pub_ex})')
label_pmid_ex <- glue('No PMID\n(n = {n_trials_pmid_ex})')
label_pubmed_ex <- glue('PMID unresolved in PubMed\n(n = {n_trials_pubmed_ex})')
label_fulltext_ex <- glue('No fulltext (pdf) found\n(n = {n_trials_ft_pdf_ex})')

# Prepare flowchart
flow_trials <- DiagrammeR::grViz("digraph trials {

# GRAPH
graph [layout = dot, rankdir = LR, splines = false]
node [shape = rectangle, width = 3, height = 1, fixedsize = true]
edge [penwidth = 1]

# INCLUSION SUBGRAPH
subgraph included {

# NODES INCLUSION
iv_all [label = '@@1']
iv_screened [label = '@@2']
trials_lead [label = '@@3']
trials_deduped [label = '@@4']
pub [label = '@@5']
pmid [label = '@@6']
pubmed [label = '@@7']
fulltext [label = '@@8']

# NODES BLANK
node [label = '', width = 0.01, height = 0.01, style = invis]

rank = same

# EDGES INCLUSION
edge [minlen = 1]
iv_all -> iv_screened -> trials_lead -> trials_deduped -> pub -> pmid -> pubmed -> fulltext

# EDGES BLANK
edge [dir = none, style = invis]
iv_all -> blank_1
iv_screened -> blank_2
trials_lead -> blank_3
trials_deduped -> blank_4
pub -> blank_5
pmid -> blank_6
pubmed -> blank_7
}

# EXCLUSION SUBGRAPH
subgraph excluded {

node [width = 2.4]

# NODES EXCLUSION
iv_screened_ex [label = '@@9'] [width = 3, height = 1.6]
iv_lead_ex [label = '@@10']
trials_dupes_ex [label = '@@11']
pub_ex [label = '@@12']
pmid_ex [label = '@@13']
pubmed_ex [label = '@@14']
fulltext_ex [label = '@@15']
}

# EDGES EXCLUSION
blank_1 -> iv_screened_ex
blank_2 -> iv_lead_ex
blank_3 -> trials_dupes_ex
blank_4 -> pub_ex
blank_5 -> pmid_ex
blank_6 -> pubmed_ex
blank_7 -> fulltext_ex
}

# LABELS
[1]: label_iv_all
[2]: label_iv_screened
[3]: label_trials_lead
[4]: label_trials_deduped
[5]: label_pub
[6]: label_pmid
[7]: label_pubmed
[8]: label_fulltext
[9]: label_iv_screened_ex
[10]: label_iv_lead_ex
[11]: label_trials_dupes_ex
[12]: label_pub_ex
[13]: label_pmid_ex
[14]: label_pubmed_ex
[15]: label_fulltext_ex
")

# Remove labels
rm(list = ls(pattern = "^label_"))

# export_graph(flow_trials,
#              file_name = here::here("docs", "figures", "flow-trials.png"),
#              file_type = "png"
# )
#
# DiagrammeRsvg::export_svg(flow_trials)
#
# flow_trials %>%
#   DiagrammeRsvg::export_svg() %>%
#   charToRaw() %>%
#   rsvg::rsvg_png(here::here("docs", "figures", "flow-trials.png"))
