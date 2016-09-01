g_res1 <- g_res %>% 
    mutate(lab = str_replace(variable_cp, "[23]_gt[12]", ""),
           n2 = r_n < 100) %>% 
    arrange(desc(mean))
g_res1$lab[g_res1$variable_cp=="demo_year3_gt2"] <- "demo_year3"

ggplot(g_res1, aes(x = mean,
                   y = row_number(desc(mean)),
                   colour = n2)) +
    geom_point(na.rm = TRUE) +
    theme_bw() + 
    theme(legend.position="none",
          legend.justification = c(1, 0),
          axis.text.x  = element_text(size=7),
          axis.text.y  = element_text(size=7),
          axis.title.x = element_text(face="bold", size=7)) +
    geom_segment(aes(x = `2.5%`,
                     xend = `97.5%`,
                     y = row_number(desc(mean)), yend = row_number(desc(mean)))) +
    scale_y_continuous(breaks = row_number(desc(g_res1$mean)), labels=g_res1$lab) +
    coord_cartesian(xlim=c(0, 1)) +
    ylab(NULL) + xlab("Discrimination") +
    scale_x_reverse() +
    scale_color_grey(end = .6)
ggsave("paper/figures/gamma.pdf", width = 6, height = 4)


b_res1 <- b_res %>% 
    mutate(lab = str_replace(variable_cp, "[23]_gt[12]", ""),
           n2 = r_n < 100) %>% 
    arrange(desc(mean))
b_res1$lab[b_res1$variable_cp=="demo_year3_gt2"] <- "demo_year3"


ggplot(b_res1, aes(x = mean,
                   y = row_number(desc(mean)),
                   colour = n2)) +
    geom_point(na.rm = TRUE) +
    theme_bw() + 
    theme(legend.position="none",
          legend.justification = c(1, 0),
          axis.text.x  = element_text(size=7),
          axis.text.y  = element_text(size=7),
          axis.title.x = element_text(face="bold", size=7)) +
    geom_segment(aes(x = `2.5%`,
                     xend = `97.5%`,
                     y = row_number(desc(mean)), 
                     yend = row_number(desc(mean)),
                     colour = n2)) +
    scale_y_continuous(breaks = row_number(desc(b_res1$mean)), labels=b_res1$lab) +
    coord_cartesian(xlim=c(-1, 1)) +
    ylab(NULL) + xlab("Difficulty") +
    scale_color_grey(end = .6)
ggsave("paper/figures/beta.pdf", width = 6, height = 4)
