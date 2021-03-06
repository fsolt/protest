#Postprocessing
save(out1, file = "data/output.rda")
out <- out1

x1 <- summary(out)
write.table(as.data.frame(x1$summary), file="x1.csv", sep = ",")
x1_sum <- as.data.frame(x1$summary)
x1_sum$parameter <- rownames(x1_sum)
# x1_sum <- read_csv("x1.csv")
# names(x1_sum)[2] <- "mean"
x1_sum$parameter_type <- gsub("([^[]*).*", "\\1", x1_sum$parameter)
View(x1_sum)
View(x1_sum[x1_sum$Rhat>1.1,])
View(x1_sum[x1_sum$Rhat>1.2,])

qcodes <- prot %>% 
    group_by(variable_cp) %>%
    summarize(qcode = first(qcode),
              r_n = n()) %>%
    arrange(qcode)

b_res <- x1_sum %>% 
    filter(parameter_type=="beta") %>% 
    select(parameter, mean, `2.5%`, `97.5%`) %>%
    mutate(qcode = as.numeric(str_extract(parameter, "\\d+"))) %>%
    left_join(qcodes, by="qcode")
g_res <- x1_sum %>%
    filter(parameter_type=="gamma") %>%
    select(parameter, mean, `2.5%`, `97.5%`) %>%
    mutate(qcode = as.numeric(str_extract(parameter, "\\d+"))) %>%
    left_join(qcodes, by="qcode")
# s_dk_res <- x1_sum %>% filter(parameter_type=="sd_k") %>% select(parameter, mean, `2.5%`, `97.5%`)  %>% mutate(rcode = as.numeric(str_extract(parameter, "\\d+"))) %>% left_join(rcodes)

beep()

a_res <- summary(out, pars="alpha", probs=c(.1, .9))
a_res <- as.data.frame(a_res$summary)

a_res$ccode <- as.numeric(gsub("alpha\\[([0-9]*),[0-9]*\\]", "\\1", row.names(a_res)))
a_res$tcode <- as.numeric(gsub("alpha\\[[0-9]*,([0-9]*)\\]", "\\1", row.names(a_res)))

k <- prot %>% group_by(country) %>% summarize(
    ccode = first(ccode),
    firstyr = first(firstyr),
    lastyr = first(lastyr)) %>%
    ungroup()

a_res <- merge(a_res, k, all=T)
a_res$year <- min(a_res$firstyr) + a_res$tcode - 1
a_res <- a_res %>%
  filter(year <= lastyr & year >= firstyr) %>%
  transmute(country = country,
            term = country,
            kk = ccode,
            year = year,
            estimate = mean,
            lb = `10%`,
            ub = `90%`) %>%
  arrange(kk, year)

# count_divergences <- function(fit) {
#   sampler_params <- get_sampler_params(fit, inc_warmup=FALSE)
#   sum(sapply(sampler_params, function(x) c(x[,'n_divergent__']))[,1])
# }

# Plots:
#   1. tolerance by country, most recent available year: cs_plot
#   2. tolerance trends, estimate plus raw data, eight countries
#   3. trends in all countries: ts_plot
#   4. probability of tolerant answer by tolerance (beta and gamma), selected items (modelled on McGann2014, fig 1)
#   5. bar chart of beta and gamma for all items?
