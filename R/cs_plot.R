# make this a function!

p1_data <- a_res %>% group_by(country) %>% top_n(1, year) %>% ungroup() %>%
  arrange(-estimate) %>% mutate(half = ntile(estimate, 2))

p1_data_a <- p1_data %>% filter(half==2) %>% mutate(ranked = row_number(estimate))
p1_data_b <- p1_data %>% filter(half==1) %>% mutate(ranked = row_number(estimate))

p1a <- ggplot(p1_data_a, aes(x = estimate,
                             y = ranked)) +
    geom_segment(aes(x = lb, xend = ub,
                     y = ranked, yend = ranked),
                 na.rm = TRUE) +
    geom_point(aes(), na.rm = TRUE) +
    theme_bw() + 
    theme(legend.position="none") +
    scale_y_continuous(breaks = p1_data_a$ranked, labels=p1_data_a$country) +
    coord_cartesian(xlim=c(0, .6)) +
    labs(x = "Protest", y = NULL)

p1b <- ggplot(p1_data_b, aes(x = estimate,
                             y = ranked)) +
    geom_segment(aes(x = lb, xend = ub,
                     y = ranked, yend = ranked),
                 na.rm = TRUE) +
    geom_point(aes(), na.rm = TRUE) +
    theme_bw() + 
    theme(legend.position="none") +
    scale_y_continuous(breaks = p1_data_b$ranked, labels=p1_data_b$country) +
    coord_cartesian(xlim=c(0, .6)) +
    labs(x = "Protest", y = NULL)

require(grid)

set_panel_size <- function(p=NULL, g=ggplotGrob(p), width=unit(3, "cm"), height=unit(3, "cm")){
  panel_index_w<- g$layout$l[g$layout$name=="panel"]
  panel_index_h<- g$layout$t[g$layout$name=="panel"]
  g$widths[[panel_index_w]] <- width
  g$heights[[panel_index_h]] <- height
  class(g) <- c("fixed", class(g), "ggplot")
  g
}

print.fixed <- function(x) grid.draw(x)

left_width <- function(g){
  axis_l_index <- g$layout$r[g$layout$name=="axis-l"]
  ylab_index <- g$layout$r[g$layout$name=="ylab"]
  g$widths[[axis_l_index]] + g$widths[[ylab_index]]
}

full_width <- function(g){
  sum(g$widths)
}


align_plots <- function(..., width=unit(4, "cm"), height=unit(1, "null")){
  pl <- list(...)
  gl <- lapply(pl, set_panel_size, width=width, height=height)

  left <- lapply(gl, left_width)
  max_left <- max(do.call(unit.c, left))

  widths <- lapply(gl, full_width)
  max_width <- max(do.call(unit.c, widths))

  lay <- grid.layout(nrow=1, ncol=2)
  vp <- viewport(layout=lay)
  pushViewport(vp)

  for(ii in seq_along(gl)){
    pushViewport(viewport(layout.pos.col = ii))
    pushViewport(viewport(x=unit(0.5, "npc") - 0.5*max_width + max_left - left[[ii]],
                          just="left", width=widths[[ii]]))
    grid.draw(gl[[ii]])
    upViewport(2)
  }
  upViewport()
}

align_plots(p1a, p1b)

pdf("paper/figures/cs.pdf")
align_plots(p1a, p1b)
dev.off()

# With year colors
#
# p1_data <- a_res %>% group_by(country) %>% top_n(1, year) %>% ungroup() %>%
#   arrange(-estimate) %>% mutate(half = ntile(estimate, 2),
#                                 ranked = row_number(estimate),
#                                 yr = year)
# p1_data$yr <- car::recode(p1_data$yr, "2001:2010 = 'Before 2011'")
# p1_data$yr <- factor(p1_data$yr, levels = c("Before 2011", "2011", "2012", "2013", "2014", "2015"))
#
# p1_data_a <- p1_data %>% filter(half==2)
#
# p1a <- ggplot(p1_data_a, aes(x = estimate,
#                              y = row_number(estimate), colour=yr)) +
#   scale_fill_manual(values = c("black", "white")) +
#   geom_point(aes(fill = as.factor(civ)), shape = 21, na.rm = TRUE) +
#   theme_bw() + theme(legend.position="none") +
#   geom_segment(aes(x = lb,
#                    xend = ub,
#                    y = row_number(estimate), yend = row_number(estimate),
#                    colour=yr), na.rm = TRUE) +
#   scale_y_discrete(breaks = row_number(p1_data_a$estimate), labels=p1_data_a$country) +
#   coord_cartesian(xlim=c(0, 1)) +
#   ylab("") + xlab("")

p1_data <- a_res %>% group_by(country) %>% top_n(1, year) %>% ungroup() %>%
  arrange(-estimate) %>% mutate(quarter = ntile(estimate, 4))

p1_data_a <- p1_data %>% filter(quarter==4) %>% mutate(ranked = row_number(estimate))
p1_data_b <- p1_data %>% filter(quarter==3) %>% mutate(ranked = row_number(estimate))
p1_data_c <- p1_data %>% filter(quarter==2) %>% mutate(ranked = row_number(estimate))
p1_data_d <- p1_data %>% filter(quarter==1) %>% mutate(ranked = row_number(estimate))

p1_data_c$country[p1_data_c$country=="Trinidad and Tobago"] <- "Trinidad"
p1_data_c$country[p1_data_c$country=="Bosnia and Herzegovina"] <- "Bosnia"


p1a <- ggplot(p1_data_a, aes(x = estimate,
                             y = ranked)) +
  geom_segment(aes(x = lb, xend = ub,
                   y = ranked, yend = ranked),
               na.rm = TRUE) +
  scale_fill_manual(values = c("Marriage"="white",
                               "Civil Union"="grey50",
                               "None"="black"),
                    breaks=c("Marriage", "Civil Union", "None"),
                    name = "Legal Recognition") +
  geom_point(aes(fill = as.factor(law)), shape = 21, size = 1.1, na.rm = TRUE) +
  theme_bw() + theme(legend.position=c(.36, .9),
                     axis.text.x  = element_text(size=7),
                     axis.text.y  = element_text(size=7),
                     axis.title.x = element_text(face="bold", size=7),
                     legend.text = element_text(size = 7),
                     legend.title = element_text(size=7, face = "bold"),
                     legend.key.size = unit(.5, "line"),
                     legend.background = element_rect(linetype = "solid",
                                                      color = "grey80",
                                                      size = .25),
                     legend.key = element_rect(colour = "white")) +
  scale_y_discrete(breaks = p1_data_a$ranked, labels=p1_data_a$country) +
  coord_cartesian(xlim=c(0, 1)) +
  labs(x = "Tolerance", y = NULL)

p1b <- ggplot(p1_data_b, aes(x = estimate,
                             y = ranked)) +
  geom_segment(aes(x = lb, xend = ub,
                   y = ranked, yend = ranked),
               na.rm = TRUE) +
  scale_fill_manual(values = c("Marriage"="white",
                               "Civil Union"="grey50",
                               "None"="black"),
                    breaks=c("Marriage", "Civil Union", "None")) +
  geom_point(aes(fill = as.factor(law)), shape = 21, size = 1.1, na.rm = TRUE) +
  theme_bw() + theme(legend.position="none",
                     axis.text.x  = element_text(size=7),
                     axis.text.y  = element_text(size=7),
                     axis.title.x = element_text(face="bold", size=7)) +
  scale_y_discrete(breaks = p1_data_b$ranked, labels=p1_data_b$country) +
  coord_cartesian(xlim=c(0, 1)) +
  labs(x = "Tolerance", y = NULL)

p1c <- ggplot(p1_data_c, aes(x = estimate,
                             y = ranked)) +
  geom_segment(aes(x = lb, xend = ub,
                   y = ranked, yend = ranked),
               na.rm = TRUE) +
  scale_fill_manual(values = c("Marriage"="white",
                               "Civil Union"="grey50",
                               "None"="black"),
                    breaks=c("Marriage", "Civil Union", "None")) +
  geom_point(aes(fill = as.factor(law)), shape = 21, size = 1.1, na.rm = TRUE) +
  theme_bw() + theme(legend.position="none",
                     axis.text.x  = element_text(size=7),
                     axis.text.y  = element_text(size=7),
                     axis.title.x = element_text(face="bold", size=7)) +
  scale_y_discrete(breaks = p1_data_c$ranked, labels=p1_data_c$country) +
  coord_cartesian(xlim=c(0, 1)) +
  labs(x = "Tolerance", y = NULL)

p1d <- ggplot(p1_data_d, aes(x = estimate,
                             y = ranked)) +
  geom_segment(aes(x = lb, xend = ub,
                   y = ranked, yend = ranked),
               na.rm = TRUE) +
  scale_fill_manual(values = c("Marriage"="white",
                               "Civil Union"="grey50",
                               "None"="black"),
                    breaks=c("Marriage", "Civil Union", "None")) +
  geom_point(aes(fill = as.factor(law)), shape = 21, size = 1.1, na.rm = TRUE) +
  theme_bw() + theme(legend.position="none",
                     axis.text.x  = element_text(size=7),
                     axis.text.y  = element_text(size=7),
                     axis.title.x = element_text(face="bold", size=7)) +
  scale_y_discrete(breaks = p1_data_d$ranked, labels=p1_data_d$country) +
  coord_cartesian(xlim=c(0, 1)) +
  labs(x = "Tolerance", y = NULL)

pdf("paper/figures/cs_top.pdf", height = 3.5, width = 7)
align_plots(p1a, p1b)
dev.off()


pdf("paper/figures/cs_bottom.pdf", height = 3.5, width = 7)
align_plots(p1c, p1d)
dev.off()