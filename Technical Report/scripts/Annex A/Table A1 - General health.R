# clear global environment
rm(list = ls())

# Load packages -----------------------------------------------------------

library(tidyverse)
library(survey)
library(readxl)
library(haven)
library(sgplot)
library(ggbreak)
library(svglite)
library(cowplot)
library(knitr)
library(dplyr)

sgplot::use_sgplot()

# Setup -------------------------------------------------------------------

# Path to SAS data
sasdata.path <- "//s0177a/sasdata1/ocs_pool/"

# Path to SSCQ 2023 data
sscq.path23 <- paste0(sasdata.path, "sscq2023SBW.sas7bdat")

# Path to file with cluster info 2023
xref.path23 <- paste0(sasdata.path, "xref2023sbw.sas7bdat")

# Data import -------------------------------------------------------------

# import sscq data
sscq23 <- haven::read_sas(sscq.path23,
                          col_select = c(SSCQid, 
                                         LA,
                                         pooled_ind_wt, 
                                         pooled_ind_wt_sc,
                                         genhealth))
nrow(sscq23)

# import cluster data
xref23 <- haven::read_sas(xref.path23,
                          col_select = c(SSCQid, cluster, survey))


# Build SSCQ data --------------------------------------------------------------

analyse23_hh <- sscq23 %>% 
  
  # merge SSCQ and cluster data
  left_join(xref23, by = "SSCQid") %>%
  
  # remove observations whose household weight was 0
  filter(pooled_ind_wt > 0) %>%
  
  # recode gen health variable
  mutate(genhealth = ifelse(genhealth == 99, 0, genhealth),
         
         genhealth = factor(genhealth, levels = c(1, 2, 3, 4, 5),
                            labels = c("Very Good",
                                       "Good",
                                       "Fair",
                                       "Bad",
                                       "Very Bad")))
  

# Analyse data ------------------------------------------------------------

# specify survey design
survey_ind <- svydesign(id = ~cluster, 
                        strata = ~LA,
                        weights = ~pooled_ind_wt,
                        data = analyse23_hh)

# SSCQ by gen health
sscq_estimates <- svymean(~genhealth, survey_ind, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(genhealth = levels(analyse23_hh$genhealth), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(genhealth) != TRUE)
sscq_estimates


# Constituent surveys ------------------------------------------------------------

survey_estimates <- analyse23_hh[c('genhealth', 'survey', 'cluster', 'pooled_ind_wt', 'LA')] 

# SCJS by gen health

scjs_estimates <- filter(survey_estimates,survey == "SCJS")

scjs_ind <- svydesign(id = ~cluster, 
                      strata = ~LA,
                      weights = ~pooled_ind_wt,
                      data = scjs_estimates)

scjs_estimates <- svymean(~genhealth, scjs_ind, na.rm = TRUE) %>%
  as_tibble() %>%
  mutate(genhealth = levels(scjs_estimates$genhealth), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) 
scjs_estimates  

# SHeS by gen health
shes_estimates <- filter(survey_estimates,survey == "SHeS")

shes_ind <- svydesign(id = ~cluster, 
                      strata = ~LA,
                      weights = ~pooled_ind_wt,
                      data = shes_estimates)

shes_estimates <- svymean(~genhealth, shes_ind, na.rm = TRUE) %>%
  as_tibble() %>%
  mutate(genhealth = levels(shes_estimates$genhealth), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) 
shes_estimates

# SHS by gen health
shs_estimates <- filter(survey_estimates,survey == "SHS")

shs_ind <- svydesign(id = ~cluster, 
                      strata = ~LA,
                      weights = ~pooled_ind_wt,
                      data = shs_estimates)

shs_estimates <- svymean(~genhealth, shs_ind, na.rm = TRUE) %>%
  as_tibble() %>%
  mutate(genhealth = levels(shs_estimates$genhealth), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) 
shs_estimates
  

# Combining the datasets ------------------------------------------------------------

table_a1 <- bind_rows(sscq_estimates, scjs_estimates, shes_estimates, shs_estimates)

Survey <- c("SSCQ", "SSCQ", "SSCQ", "SSCQ", "SSCQ",
                     "SCJS", "SCJS", "SCJS", "SCJS", "SCJS",
                     "SHeS", "SHeS", "SHeS", "SHeS", "SHeS",
                     "SHS", "SHS", "SHS", "SHS", "SHS")

table_a1$Survey <- Survey

table_a1 <- table_a1 %>% select(Survey, genhealth, perc_with_ci)
table_a1

# Visualise estimates ------------------------------------------------------------

table_a1 <- table_a1 %>%
  select(Survey, genhealth, perc_with_ci) %>%
  group_by(Survey) %>% 
  pivot_wider(
    names_from = genhealth,
    values_from = perc_with_ci)


print(table_a1)


kable(table_a1,
  caption = "Table A.1: Self-assessed General Health 
  by Source Survey (Row % and Margin of Error)")


saveRDS(table_a1, 
        "Technical Report/output/table_a1_estimates.rds")
