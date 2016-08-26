library(readr)
library(dplyr)

library(foreign)
library(haven)
library(reshape2)
library(beepr)
library(rstan)
library(stringr)

library(ggplot2)

datasets_table <- read_csv("data-raw/datasets_table.csv")
prot0 <- read_csv("data-raw/surveys_prot.csv")
prot <- dcpo_setup(prot0)
write_csv(prot, "data/all_data_prot.csv")

prot <- read_csv("data/all_data_prot.csv")
# prot_a <- prot %>% filter(cc_rank>=10 & firstyr!=lastyr) %>%
#   mutate(ccode = as.numeric(factor(ccode)))

