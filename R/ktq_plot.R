ktq <- prot %>%
  mutate(ktq = paste0(country, year, variable_cp)) %>%
  group_by(variable_cp) %>%
  summarize(kt = n_distinct(ktq)) %>%
  arrange(kt) %>%
  mutate(var = factor(variable_cp, levels = variable_cp[order(kt, decreasing = TRUE)])) %>%
  ungroup()

ggplot(ktq, aes(x = var, y = kt)) +
    geom_bar(fill = "#011993", stat = "identity") +
    labs(x = NULL, y = NULL) +
    theme_bw() +
    ggtitle("Count of Country-Years by Item") +
    theme(axis.text.x  = element_text(size=8))
ggsave("paper/figures/ktq.pdf", width = 9, height = 3)
