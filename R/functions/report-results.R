# # Report the % (num/denom) of links in a given year by type ---------------
#
# report_link_year <- function(link_type_in, year) {
#   links_by_year %>%
#     filter(link_type == link_type_in & completion_year == year) %>%
#     mutate(report = glue("{style_percent(prop_link, symbol =  TRUE)} ({n_link}/{n_link_type})")) %>%
#     pull(report)
# }
#
# report_link_year_reg <- function(link_type_in, year, registry_in) {
#   links_by_year_reg %>%
#     filter(
#       link_type == link_type_in & completion_year == year & registry == registry_in
#     ) %>%
#     mutate(report = glue("{style_percent(prop_link, symbol =  TRUE)} ({n_link}/{n_link_type})")) %>%
#     pull(report)
# }
