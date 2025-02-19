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
library(broom)

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
                                         LTCondition))
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
  mutate(LTCondition = ifelse(LTCondition == 99, 0, LTCondition),
         
         LTCondition = factor(LTCondition, levels = c(1, 2),
                          labels = c("Condition and limits a little or lots",
                                     "No condition/condition that doesn't limit")))

# Analyse data ------------------------------------------------------------

# specify survey design
survey_ind <- svydesign(id = ~cluster, 
                        strata = ~LA,
                        weights = ~pooled_ind_wt,
                        data = analyse23_hh)

# SSCQ by LTCondition
sscq_estimates <- svymean(~LTCondition, survey_ind, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(LTCondition = levels(analyse23_hh$LTCondition), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(LTCondition) != TRUE)
sscq_estimates


# Constituent surveys ------------------------------------------------------------
survey_estimates <- analyse23_hh[c('LTCondition', 'survey', 'cluster', 'pooled_ind_wt', 'LA')] 

# SCJS by gen health
scjs_estimates <- filter(survey_estimates,survey == "SCJS")

scjs_ind <- svydesign(id = ~cluster, 
                      strata = ~LA,
                      weights = ~pooled_ind_wt,
                      data = scjs_estimates)

scjs_estimates <- svymean(~LTCondition, scjs_ind, na.rm = TRUE) %>%
  as_tibble() %>%
  mutate(LTCondition = levels(scjs_estimates$LTCondition), .before = mean) %>%
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

shes_estimates <- svymean(~LTCondition, shes_ind, na.rm = TRUE) %>%
  as_tibble() %>%
  mutate(LTCondition = levels(shes_estimates$LTCondition), .before = mean) %>%
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

shs_estimates <- svymean(~LTCondition, shs_ind, na.rm = TRUE) %>%
  as_tibble() %>%
  mutate(LTCondition = levels(shs_estimates$LTCondition), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) 
shs_estimates


# Combining the datasets ------------------------------------------------------------
table_a3 <- bind_rows(sscq_estimates, scjs_estimates, shes_estimates, shs_estimates)

Survey <- c("SSCQ", "SSCQ",
            "SCJS", "SCJS",
            "SHeS", "SHeS",
            "SHS", "SHS")

table_a3$Survey <- Survey

table_a3 <- table_a3 %>% select(Survey, LTCondition, perc_with_ci)
table_a3


# Visualise estimates ------------------------------------------------------------

table_a3 <- table_a3 %>%
  select(Survey, LTCondition, perc_with_ci) %>%
  group_by(Survey) %>% 
  pivot_wider(
    names_from = LTCondition,
    values_from = perc_with_ci)

print(table_a3)


kable(table_a3,
      caption = "Table A.3: Long-term limiting health 
      condition by source survey 
      (row % and margin of error)")



saveRDS(table_a3, 
        "Technical Report/output/table_a3_estimates.rds")
