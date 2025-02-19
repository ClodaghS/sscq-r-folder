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
                                         pooled_crim_wt, 
                                         pooled_crim_wt_sc,
                                         CrimeArea))
nrow(sscq23)

# import cluster data
xref23 <- haven::read_sas(xref.path23,
                          col_select = c(SSCQid, cluster, survey))


# Build SSCQ data --------------------------------------------------------------

analyse23_hh <- sscq23 %>% 
  
  # merge SSCQ and cluster data
  left_join(xref23, by = "SSCQid") %>%
  
  # remove observations whose household weight was 0
  filter(pooled_crim_wt > 0) %>%
  
  # recode gen health variable
  mutate(CrimeArea = ifelse(CrimeArea == 99, 0, CrimeArea),
         
         CrimeArea = factor(CrimeArea, levels = c(-4, -1, 1, 2, 3, 4, 5),
                            labels = c(NA,
                                       NA,
                                       "A lot more",
                                       "A little more",
                                       "About the same",
                                       "A little less",
                                       "A lot less")))


# Analyse data ------------------------------------------------------------

# specify survey design
survey_ind <- svydesign(id = ~cluster, 
                        strata = ~LA,
                        weights = ~pooled_crim_wt,
                        data = analyse23_hh)

# SSCQ by gen health
sscq_estimates <- svymean(~CrimeArea, survey_ind, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(CrimeArea = levels(analyse23_hh$CrimeArea), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(CrimeArea) != TRUE)
sscq_estimates


# Constituent surveys ------------------------------------------------------------

survey_estimates <- analyse23_hh[c('CrimeArea', 'survey', 'cluster', 'pooled_crim_wt', 'LA')] 

# SCJS by gen health

scjs_estimates <- filter(survey_estimates,survey == "SCJS")

scjs_ind <- svydesign(id = ~cluster, 
                      strata = ~LA,
                      weights = ~pooled_crim_wt,
                      data = scjs_estimates)

scjs_estimates <- svymean(~CrimeArea, scjs_ind, na.rm = TRUE) %>%
  as_tibble() %>%
  mutate(CrimeArea = levels(scjs_estimates$CrimeArea), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>% 
  filter(is.na(CrimeArea) != TRUE)
scjs_estimates  

# SHeS by gen health
shes_estimates <- filter(survey_estimates,survey == "SHeS")

shes_ind <- svydesign(id = ~cluster, 
                      strata = ~LA,
                      weights = ~pooled_crim_wt,
                      data = shes_estimates)

shes_estimates <- svymean(~CrimeArea, shes_ind, na.rm = TRUE) %>%
  as_tibble() %>%
  mutate(CrimeArea = levels(shes_estimates$CrimeArea), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>% 
  filter(is.na(CrimeArea) != TRUE)
shes_estimates

# SHS by gen health
shs_estimates <- filter(survey_estimates,survey == "SHS")

shs_ind <- svydesign(id = ~cluster, 
                     strata = ~LA,
                     weights = ~pooled_crim_wt,
                     data = shs_estimates)

shs_estimates <- svymean(~CrimeArea, shs_ind, na.rm = TRUE) %>%
  as_tibble() %>%
  mutate(CrimeArea = levels(shs_estimates$CrimeArea), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>% 
  filter(is.na(CrimeArea) != TRUE)
shs_estimates


# Combining the datasets ------------------------------------------------------------

table_a8 <- bind_rows(sscq_estimates, scjs_estimates, shes_estimates, shs_estimates)

Survey <- c("SSCQ", "SSCQ", "SSCQ", "SSCQ", "SSCQ",
            "SCJS", "SCJS", "SCJS", "SCJS", "SCJS",
            "SHeS", "SHeS", "SHeS", "SHeS", "SHeS",
            "SHS", "SHS", "SHS", "SHS", "SHS")

table_a8$Survey <- Survey

table_a8 <- table_a8 %>% select(Survey, CrimeArea, perc_with_ci)
table_a8

# Visualise estimates ------------------------------------------------------------

table_a8 <- table_a8 %>%
  select(Survey, CrimeArea, perc_with_ci) %>%
  group_by(Survey) %>% 
  pivot_wider(
    names_from = CrimeArea,
    values_from = perc_with_ci)


print(table_a8)


kable(table_a8,
      caption = "Table A.8: Perception of local crime rate by source survey (row % and margin of error)")


saveRDS(table_a8, 
        "Technical Report/output/table_a8_estimates.rds")
