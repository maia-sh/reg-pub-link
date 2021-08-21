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
    has_iv_trn_ft
  ) %>%
  rename(
    "TRN in full-text" = has_iv_trn_ft,
    "TRN in abstract" = has_iv_trn_abstract,
    "TRN in PubMed metadata" = has_iv_trn_secondary_id,
    "Publication in registration" = has_reg_pub_link
  ) %>%
  pivot_longer(c(-id, -registry, -completion_year),
               names_to = "link_type",
               values_to = "link",
               values_drop_na = TRUE
  ) %>%
  mutate(
    link_type = factor(
      link_type,
      # levels = c(
      #   has_iv_trn_ft,
      #   has_iv_trn_abstract,
      #   has_iv_trn_secondary_id,
      #   has_reg_pub_link
      # ),
      levels = c(
        "TRN in full-text",
        "TRN in abstract",
        "TRN in PubMed metadata",
        "Publication in registration"
      )
    )
  ) %>%
  group_by(link_type, registry, completion_year) %>%
  summarise(
    n_link_type= n(),
    n_link = sum(link),
    prop_link = n_link/n_link_type,
    .groups = "drop"
  )

# Prepare plot ------------------------------------------------------------
# library(showtext)
# # font_add_google("Poppins", "Poppins")
# font_add_google("Roboto", "Roboto")
# font_add_google("Roboto Mono", "Roboto Mono")
# showtext_auto()
#
# theme_set(theme_light(base_size = 18, base_family = "Roboto"))
# theme_set(theme_light(base_size = 18, base_family = "Poppins"))
# theme_set(ggthemes::theme_fivethirtyeight(base_size = 18, base_family = "Poppins"))

plot_links_year_reg <-
  links_by_year_reg %>%
  rename(Registry = registry) %>%
  ggplot(aes(x = completion_year, y = prop_link, color = Registry, linetype = Registry)) +
  facet_wrap(~link_type, ncol = 1) +
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1), expand = c(0.005, 0.005)) +
  scale_x_continuous(breaks = unique(links_by_year_reg$completion_year)) +
    scale_color_manual(values = c(lightgray, darkgray)) +
  # scale_color_manual(values = registry_colors) +
    # scale_linetype(palette(registry_linetype))
  # scale_color_grey(start = 0.5, end = 0.8) +
  # scale_colour_viridis_d() +
  #TODO: show ticks for all years
  labs(
    # y = "Registration-Publication Linkage (%)",
    y = NULL,
    x = "Trial Completion Year",
    # title = "Percentage of linked registrations and publication by trial completion year"
  ) +
  guides(
    # colour = guide_legend(title = "Registration-Publication Link Type"),
    # linetype = guide_legend(title = "Registry")
    fill = guide_colorbar()
    # fill = guide_legend(title = "Registry")
  ) +
  theme(
    # legend.box.margin = margin(c(0,0,0,1)),
    legend.position = c(0.99, 1.02),
    legend.direction = "horizontal",
    # legend.position = "bottom",
    legend.justification = "right",
    # legend.box.just = "left",
    # legend.box = "vertical",
    legend.margin = margin(),
    # axis.title = element_text(size = 16),
    # axis.text = element_text(family = "Roboto Mono",
    #                          size = 8),
    strip.text = element_text(face = "bold", color = "black",
                              size = 12,
                              # hjust = 0,
                              # margin = margin(0.1,0.5,0.1,0.5, "cm")
                              ),
    strip.background = element_rect(fill = "gray85", linetype = "dotted"),
    axis.title.x = element_text(size = 12)
    # plot.title = element_text(size = 20)
  )

ggsave(
  path(dir_figures, "plot-link-year.pdf"),
  plot_links_year_reg,
  width = 25, height = 20, unit = "cm",
  bg = "white"
)

ggsave(
  path(dir_figures, "plot-link-year.svg"),
  plot_links_year_reg,
  width = 25, height = 20, unit = "cm",
  bg = "white",
  dpi = 600
)
