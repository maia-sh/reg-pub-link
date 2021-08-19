# Prepare trials with links
# Transform links into list column of intersection sets
trials_links <-
  trials %>%
  select(id,
         registry,
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
  pivot_longer(
    cols = c(-id, -registry),
    names_to = "link"
  ) %>%
  filter(value == TRUE) %>%
  group_by(id) %>%
  mutate(links = list(link)) %>%
  ungroup() %>%
  select(-value, -link) %>%
  distinct()

# Prepare trials without links
# Create dummy links list column
trials_no_links <-
  trials %>%
  filter(
    !has_reg_pub_link &
      !has_iv_trn_secondary_id &
      !has_iv_trn_abstract &
      !has_iv_trn_ft_pdf
  ) %>%
  select(id,
         registry
  ) %>%
  mutate(links = list(NULL))

# plot_upset_links_reg_pub <-
#   bind_rows(trials_links, trials_no_links) %>%
#   ggplot(aes(x = links)) +
#   geom_bar() +
#   geom_text(stat='count', aes(label = after_stat(count)), vjust = -.5) +
#   ggupset::scale_x_upset() +
#   ylab("Number of trials") + #not number of TRNs since 1 reg could have multi-trn
#   xlab(NULL) +
#   ggupset::theme_combmatrix(
#     # combmatrix.label.make_space = TRUE,
#     combmatrix.panel.line.size = 0,
#     combmatrix.label.text = element_text(size=11)
#   )

# ggsave(
#   here("docs", "figures", "plot-upset-links-reg-pub.png"),
#   plot_upset_links_reg_pub,
#   scale = 1.25
# )

plot_upset_links_reg_pub_registry <-
  bind_rows(trials_links, trials_no_links) %>%
  rename(Registry = registry) %>%
  ggplot(aes(x = links)) +

  # TODO: switch to "dodge" and fix numbers
  geom_bar(aes(fill = Registry), position = "stack") +
  geom_text(stat='count', aes(label = after_stat(count)), vjust = -.5) +
  scale_fill_grey() +
  ggupset::scale_x_upset() +
  ylab("Number of trials") + #not number of TRNs since 1 reg could have multi-trn
  xlab(NULL) +
  ggupset::theme_combmatrix(
    # combmatrix.label.make_space = FALSE,
    combmatrix.panel.line.size = 0,
    combmatrix.label.text = element_text(size=11)
  ) +

  theme(legend.position = c(.85, .9))

# plot_upset_links_reg_pub_registry +
#
#   # Move y axis label closer to plot
#   # Thanks to https://stackoverflow.com/questions/68593982
#   theme(axis.title.y=element_blank()) +
#   annotate(geom = "text", x = -0.2, y = 6500, label = "count", angle = 90, size=4) +
#   coord_cartesian(xlim = c(1, 8), clip = "off")


# `ggupset` doesn't currently allow to change label order
# https://github.com/const-ae/ggupset/issues/20

ggsave(
  here("docs", "figures", "plot-upset-links-reg-pub-registry.png"),
  plot_upset_links_reg_pub_registry,
  scale = 1.25
)
