# make this a function!

# p1_data <- a_res %>% 
#     group_by(country) %>%
#     top_n(1, year) %>%
#     ungroup() %>%
#     arrange(-estimate) %>%
#     filter(ub - lb < .3) %>%
#     mutate(half = ntile(estimate, 2))

# p1_data_a <- p1_data %>% filter(half==2) %>% mutate(ranked = row_number(estimate))
# p1_data_b <- p1_data %>% filter(half==1) %>% mutate(ranked = row_number(estimate))
# 
# p1a <- ggplot(p1_data_a, aes(x = estimate,
#                              y = ranked)) +
#     geom_segment(aes(x = lb, xend = ub,
#                      y = ranked, yend = ranked),
#                  na.rm = TRUE) +
#     geom_point(aes(), na.rm = TRUE) +
#     theme_bw() + 
#     theme(legend.position="none") +
#     scale_y_continuous(breaks = p1_data_a$ranked, labels=p1_data_a$country) +
#     labs(x = "Protest", y = NULL)
# 
# p1b <- ggplot(p1_data_b, aes(x = estimate,
#                              y = ranked)) +
#     geom_segment(aes(x = lb, xend = ub,
#                      y = ranked, yend = ranked),
#                  na.rm = TRUE) +
#     geom_point(aes(), na.rm = TRUE) +
#     theme_bw() + 
#     theme(legend.position="none") +
#     scale_y_continuous(breaks = p1_data_b$ranked, labels=p1_data_b$country) +
#     labs(x = "Protest", y = NULL)
# 
# require(grid)
# 
# set_panel_size <- function(p=NULL, g=ggplotGrob(p), width=unit(3, "cm"), height=unit(3, "cm")){
#   panel_index_w<- g$layout$l[g$layout$name=="panel"]
#   panel_index_h<- g$layout$t[g$layout$name=="panel"]
#   g$widths[[panel_index_w]] <- width
#   g$heights[[panel_index_h]] <- height
#   class(g) <- c("fixed", class(g), "ggplot")
#   g
# }
# 
# print.fixed <- function(x) grid.draw(x)
# 
# left_width <- function(g){
#   axis_l_index <- g$layout$r[g$layout$name=="axis-l"]
#   ylab_index <- g$layout$r[g$layout$name=="ylab"]
#   g$widths[[axis_l_index]] + g$widths[[ylab_index]]
# }
# 
# full_width <- function(g){
#   sum(g$widths)
# }
# 
# 
# align_plots <- function(..., width=unit(4, "cm"), height=unit(1, "null")){
#   pl <- list(...)
#   gl <- lapply(pl, set_panel_size, width=width, height=height)
# 
#   left <- lapply(gl, left_width)
#   max_left <- max(do.call(unit.c, left))
# 
#   widths <- lapply(gl, full_width)
#   max_width <- max(do.call(unit.c, widths))
# 
#   lay <- grid.layout(nrow=1, ncol=2)
#   vp <- viewport(layout=lay)
#   pushViewport(vp)
# 
#   for(ii in seq_along(gl)){
#     pushViewport(viewport(layout.pos.col = ii))
#     pushViewport(viewport(x=unit(0.5, "npc") - 0.5*max_width + max_left - left[[ii]],
#                           just="left", width=widths[[ii]]))
#     grid.draw(gl[[ii]])
#     upViewport(2)
#   }
#   upViewport()
# }
# 
# align_plots(p1a, p1b)
# 
# pdf("paper/figures/cs.pdf")
# align_plots(p1a, p1b)
# dev.off()

# With year colors, by quartile

p1_data <- a_res %>%    
    group_by(country) %>% 
    top_n(1, year) %>% 
    ungroup() %>%
    arrange(-estimate) %>%
    filter(ub - lb < .3) %>% 
    mutate(quarter = ntile(estimate, 4),
           ranked = row_number(estimate),
           Year = year)
p1_data$Year <- car::recode(p1_data$Year, "2001:2010 = 'Before 2011'")
p1_data$Year <- factor(p1_data$Year, levels = c("Before 2011", "2011", "2012", "2013", "2014", "2015"))

p1_data_a <- p1_data %>% filter(quarter==4)
p1_data_b <- p1_data %>% filter(quarter==3)

p1a <- ggplot(p1_data_a, aes(x = estimate,
                             y = row_number(estimate), colour=Year)) +
    geom_point(na.rm = TRUE) +
    theme_bw() + 
    theme(legend.position=c(1, 0),
          legend.justification = c(1, 0),
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
    geom_segment(aes(x = lb,
                     xend = ub,
                     y = row_number(estimate), yend = row_number(estimate),
                     colour=Year), na.rm = TRUE) +
    scale_y_continuous(breaks = row_number(p1_data_a$estimate), labels=p1_data_a$country) +
    coord_cartesian(xlim=c(0, 1)) +
    ylab(NULL) + xlab("Protest") +
    scale_color_grey(start = .8, end = 0)
ggsave("paper/figures/cs_1q.pdf", width = 6, height = 4)

p1b <- ggplot(p1_data_b, aes(x = estimate,
                             y = row_number(estimate), colour=Year)) +
    geom_point(na.rm = TRUE) +
    theme_bw() + 
    theme(legend.position="none",
          legend.justification = c(1, 0),
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
    geom_segment(aes(x = lb,
                     xend = ub,
                     y = row_number(estimate), yend = row_number(estimate),
                     colour=Year), na.rm = TRUE) +
    scale_y_continuous(breaks = row_number(p1_data_b$estimate), labels=p1_data_b$country) +
    coord_cartesian(xlim=c(0, 1)) +
    ylab(NULL) + xlab("Protest") +
    scale_color_grey(start = .8, end = 0)
ggsave("paper/figures/cs_2q.pdf", width = 6, height = 4)



