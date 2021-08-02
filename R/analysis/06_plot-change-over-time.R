# Percentage of trials reg-pub linkage, by year
# Denominator n = 1902 (n trials)

# By registry -------------------------------------------------------------

links_by_year_reg <-
  trials %>%
  select(
    id,
    registry, # could also group by registry
    completion_year,
    has_reg_pub_link,
    has_iv_trn_secondary_id,
    has_iv_trn_abstract,
    has_iv_trn_ft_pdf
  ) %>%
  rename(
    "Reference in registration" = has_reg_pub_link,
    "TRN (PubMed metadata)" = has_iv_trn_secondary_id,
    "TRN (Abstract)" = has_iv_trn_abstract,
    "TRN (Full-text)" = has_iv_trn_ft_pdf
  ) %>%
  pivot_longer(c(-id, -registry, -completion_year),
               names_to = "link_type",
               values_to = "link",
               values_drop_na = TRUE
  ) %>%
  group_by(link_type, registry, completion_year) %>%
  summarise(
    n_link_type= n(),
    n_link = sum(link),
    prop_link = n_link/n_link_type,
    .groups = "drop"
  )

# Prepare plot ------------------------------------------------------------
library(showtext)
font_add_google("Poppins", "Poppins")
font_add_google("Roboto Mono", "Roboto Mono")
showtext_auto()

theme_set(theme_light(base_size = 18, base_family = "Poppins"))
theme_set(ggthemes::theme_fivethirtyeight(base_size = 18, base_family = "Poppins"))

# links_by_year_reg_separated <-
links_by_year_reg %>%
  ggplot(aes(x = completion_year, y = prop_link, color = registry, linetype = registry)) +
  facet_wrap(~link_type, ncol = 1) +
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1), expand = c(0.005, 0.005)) +
  scale_x_continuous(breaks = unique(links_by_year_reg$completion_year)) +
  scale_color_grey() +
  # scale_colour_viridis_d() +
  #TODO: show ticks for all years
  labs(
    # y = "Registration-Publication Linkage (%)",
    y = NULL,
    x = "Trial Completion Year"
  ) +
  guides(
    # colour = guide_legend(title = "Registration-Publication Link Type"),
    # linetype = guide_legend(title = "Registry")
    fill = guide_colorbar()
    # fill = guide_legend(title = "Registry")
  ) +
  theme(
    legend.position = "top",
    legend.box = "vertical",
    legend.margin = margin(),
    axis.title = element_text(size = 16),
    axis.text = element_text(family = "Roboto Mono", size = 12),
    strip.text = element_text(face = "bold", color = "black",
                              hjust = 0, size = 14,
                              margin = margin(0.1,0.5,0.1,0.5, "cm")),
    strip.background = element_rect(fill = "gray80", linetype = "dotted")
  )

# DEPRECATED --------------------------------------------------------------

# Prepare data: reg-pub linkage per completion year -----------------------

# links_by_year <-
#   trials %>%
#   select(
#     id,
#     # registry, # could also group by registry
#     completion_year,
#     has_reg_pub_link,
#     has_iv_trn_secondary_id,
#     has_iv_trn_abstract,
#     has_iv_trn_ft_pdf
#   ) %>%
#   rename(
#     "Reference in registration" = has_reg_pub_link,
#     "TRN (PubMed metadata)" = has_iv_trn_secondary_id,
#     "TRN (Abstract)" = has_iv_trn_abstract,
#     "TRN (Full-text)" = has_iv_trn_ft_pdf
#   ) %>%
#   pivot_longer(c(-id, -completion_year),
#                names_to = "link_type",
#                values_to = "link",
#                values_drop_na = TRUE
#   ) %>%
#   group_by(link_type, completion_year) %>%
#   summarise(
#     n_link_type= n(),
#     n_link = sum(link),
#     prop_link = n_link/n_link_type,
#     .groups = "drop"
#   )


# Plot reg-pub linkage per completion year --------------------------------

# plot_links_year <-
#   links_by_year %>%
#   ggplot(aes(x = completion_year, y = prop_link, color = link_type)) +
#   geom_point() +
#   geom_line() +
#   scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
#   scale_x_continuous(breaks = unique(links_by_year$completion_year)) +
#   scale_colour_viridis_d() +
#   #TODO: show ticks for all years
#   labs(
#     y = "Registration-Publication Linkage (%)",
#     x = "Completion Year"
#   ) +
#   guides(colour = guide_legend(title = "Registration-Publication Link Type")) #+
#   # theme(legend.position= "top"
#         # legend.justification="top"
#         # )

# ggsave(here::here("docs", "figures", "plot-links-year.png"), plot_links_year)


# Plot reg-pub linkage per completion year,  with glm ---------------------


# TODO: Decide whether to add trend line
# plot_links_year_glm <-
#   plot_links_year +
#   geom_smooth(
#     method = glm, #lm probably not appropriate with binary output
#     se = TRUE, #not readable with SE but need to show I think
#     alpha = 0.2
#   )

# ggsave(here::here("docs", "figures", "plot-links-year-glm.png"), plot_links_year_glm)

# plot_links_year_reg <-
#   links_by_year_reg %>%
#   ggplot(aes(x = completion_year, y = prop_link, color = link_type)) +
#   facet_wrap(~registry) +
#   geom_point() +
#   geom_line() +
#   scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
#   scale_x_continuous(breaks = unique(links_by_year$completion_year)) +
#   scale_colour_viridis_d() +
#   #TODO: show ticks for all years
#   labs(
#     y = "Registration-Publication Linkage (%)",
#     x = "Completion Year"
#   ) +
#   guides(colour = guide_legend(title = "Registration-Publication Link Type")) +
#   theme(legend.position= "top")
#
# # ggsave(here::here("docs", "figures", "plot-links-year-reg.png"), plot_links_year_reg)
#
# plot_links_year_reg_glm <-
#   plot_links_year_reg +
#   geom_smooth(
#     method = glm, #lm probably not appropriate with binary output
#     se = TRUE, #not readable with SE but need to show I think
#     alpha = 0.2
#   )

# ggsave(here::here("docs", "figures", "plot-links-year-reg-glm.png"), plot_links_year_reg_glm)

# Report the % (num/denom) of links in a given year by type ---------------

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
