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
prot <- prot %>%
    filter(!(str_detect(variable, "_ever3") & cutpoint==1)) %>% 
    filter(!(str_detect(variable, "_year3") & cutpoint==2)) %>% 
    mutate(variable_cp = str_replace(variable_cp, "_year4_gt3", "_year")) %>% 
    mutate(variable_cp = str_replace(variable_cp, "_year4_gt2", "_ever")) %>% 
    filter(!str_detect(variable_cp, "_year4_gt1")) %>% 
    mutate(variable_cp = str_replace(variable_cp, "[23]_gt[12]", ""))
