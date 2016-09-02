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
prot_a <- prot %>%
    mutate(vals = str_extract(variable, "[23]") %>% as.numeric(),
           p = y_r/n) %>% 
    filter(!(vals==3 & cutpoint==1 & variable!="demo_year3")) %>% 
    filter(!str_detect(variable, "petit")) %>% 
    filter(!str_detect(variable, "boycott"))
prot0 <- prot
prot <- prot_a %>% mutate(rcode = as.numeric(factor(rcode, levels = unique(rcode))))
