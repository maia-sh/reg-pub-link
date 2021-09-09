# https://www.anthonyschmidt.co/post/2020-06-03-making-apa-tables-with-gt/

# Prepare actions and examples --------------------------------------------

action_researchers <- "
- Include TRN in abstract and full-text per CONSORT and ICMJE guidelines
- Link publication in registration
"

example_researchers <- "ClinicalTrials.gov provides a step-by-step tutorial for linking publications in the registration (U.S. National Library of Medicine, 2020)."

action_registries <- "
- Provide guidance on TRN formatting and reminder to include TRN in publications, and publications in registration
- Link publications automatically using TRNs in bibliographic database metadata
- Provide metadata on linked publication type (i.e., result, background, protocol)
"

example_registries <- "ClinicalTrials.gov uses PubMed TRN metadata to automatically link publications in the registration (National Library of Medicine, 2017). Our study shows this automated publication linking is responsible for the majority of linked publications in ClinicalTrials.gov and implies similar potential for DRKS."
# [@nationallibraryofmedicine2017c]

action_institutions <- "
- Educate, support, and incentivize researchers on linking
"

example_institutions <- "
Ethics review offices or UMC-core facilities for clinical research could give researchers an info sheet with trial registration and reporting guidelines when reviewing trial. Bonuses, i.e., *Leistungsorientierte Mittel (LOM)*, could be disseminated for trial registration, reporting, and linking.
"

action_journals <- "
- Request TRN in specialized metadata field
- Review abstract and full-text for TRN inclusion, using automated regexes and/or manual strategies
- Provide TRN as metadata to bibliographic databases
"

example_journals <- "
Taylor & Francis extracts TRNs from the abstract and full-text, submits this metadata to CrossRef and Pubmed, and displays the linked trial via Crossmark on the article page (CrossRef, 2020; Taylor & Francis Group, 2020)
"
# [@taylorfrancisgroup2020; @crossref2020c]
#
action_pubmed <- "
- Integrate publisher-provided TRNs into metadata
- Extract TRN as metadata from abstract and full-text, using automated regexes and/or manual strategies
"

example_pubmed <- "
The National Library of Medicine relies on publisher-provided data and manual indexers to create the TRN PubMed metadata if the TRN appears in the abstract or full-text. Our study shows this manual-only strategy misses TRNs and could be semi-automated to detect more TRNs for manual verification.
"


# Prepare stakeholder table
tbl_stakeholders <-
  dplyr::tribble(
    ~Stakeholder, ~Recommendation, ~Example,
    "Researchers", action_researchers, example_researchers,
    "Registries", action_registries, example_registries,
    "Research Institutions", action_institutions, example_institutions,
    "Publishers/Journals", action_journals, example_journals,
    "Bibliographic Databases", action_pubmed, example_pubmed,
  ) %>%
  gt(
    caption = "Recommended stakeholder actions to improve links between trial registrations and publications"
  ) %>%
  # tab_footnote(
  #   footnote = "[@nationallibraryofmedicine2017c]",
  #   locations = cells_body(
  #     columns = Example,
  #     rows = Stakeholder == "Registries"
  #   )
  # ) %>%
  tab_style(
    style = cell_text(weight = "bold"),
    location = list(
      cells_column_labels(),
      cells_body(columns = Stakeholder)
    )
  ) %>%
  fmt_markdown(columns = everything()) %>%
  cols_width(
    Stakeholder ~ px(200),
    Recommendation ~ px(500),
    Example ~ px(500)
  )
  # tab_options(table.width = px(400))

gtsave(
  tbl_stakeholders,
  path(dir_figures, "tbl-stakeholders.pdf")
  # expand = 10,
  # zoom = 2
)

# tbl_stakeholders <-
#   dplyr::tribble(
#     ~Stakeholder, ~Recommendation, ~Example,
#     "Researchers", action_researchers, example_researchers,
#     "Registries", action_registries, example_registries,
#     "Research Institutions", action_institutions, example_institutions,
#     "Publishers/Journals", action_journals, example_journals,
#     "Bibliographic Databases (e.g., PubMed)", action_pubmed, example_pubmed,
#   ) %>%
#   knitr::kable(
#     format = "markdown",
#     caption = "Recommended stakeholder actions to improve links between trial registrations and publications"
#   ) %>%
#   kable_styling(latex_options = "scale_down")
# https://community.rstudio.com/t/references-in-tables-kable-vs-gt/66780
# https://haozhu233.github.io/kableExtra/
