# STACKED -----------------------------------------------------------------

plot_links_year_reg_stacked <-
links_by_year_reg %>%
  ggplot(aes(x = completion_year, y = prop_link, color = link_type)) +
  facet_wrap(~registry, ncol = 1) +
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_x_continuous(breaks = unique(links_by_year$completion_year)) +
  scale_colour_viridis_d() +
  #TODO: show ticks for all years
  labs(
    y = "Registration-Publication Linkage (%)",
    x = "Completion Year"
  ) +
  guides(colour = guide_legend(title = "Registration-Publication Link Type")) +
  theme(legend.position= "top")


# LINETYPE ----------------------------------------------------------------

plot_links_year_reg_linetype <-
  links_by_year_reg %>%
  ggplot(aes(x = completion_year, y = prop_link, color = link_type, linetype = registry)) +
  # facet_wrap(~registry, ncol = 1) +
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_x_continuous(breaks = unique(links_by_year$completion_year)) +
  scale_colour_viridis_d() +
  #TODO: show ticks for all years
  labs(
    y = "Registration-Publication Linkage (%)",
    x = "Completion Year"
  ) +
  guides(
    colour = guide_legend(title = "Registration-Publication Link Type"),
    linetype = guide_legend(title = "Registry")
    ) +
  theme(legend.position= "bottom", legend.box="vertical", legend.margin=margin())


# SEPARATED
links_by_year_reg_separated <-
  links_by_year_reg %>%
  ggplot(aes(x = completion_year, y = prop_link, color = registry)) +
  facet_wrap(~link_type, ncol = 1) +
  geom_point() +
  geom_line() +
  scale_y_continuous(labels = scales::percent, limits = c(0, 1)) +
  scale_x_continuous(breaks = unique(links_by_year$completion_year)) +
  scale_colour_viridis_d() +
  #TODO: show ticks for all years
  labs(
    y = "Registration-Publication Linkage (%)",
    x = "Completion Year"
  ) +
  guides(
    colour = guide_legend(title = "Registration-Publication Link Type"),
    linetype = guide_legend(title = "Registry")
  ) +
  theme(legend.position= "bottom", legend.box="vertical", legend.margin=margin())
