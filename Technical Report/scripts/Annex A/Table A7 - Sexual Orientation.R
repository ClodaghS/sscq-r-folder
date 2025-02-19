# clear global environment
rm(list = ls())

# Load packages -----------------------------------------------------------

library(tidyverse)
library(survey)
library(readxl)
library(haven)
library(sgplot)
library(ggbreak)

sgplot::use_sgplot()

# Setup -------------------------------------------------------------------

# Path to SAS data
sasdata.path <- "//s0177a/sasdata1/ocs_pool/"

# Path to SSCQ 2022 data
sscq.path23 <- paste0(sasdata.path, "sscq2023SBW.sas7bdat")

# Path to file with cluster info 2022
xref.path23 <- paste0(sasdata.path, "xref2023sbw.sas7bdat")

# Data import -------------------------------------------------------------

# import sscq data
sscq23 <- haven::read_sas(sscq.path23,
                          col_select = c(SSCQid, 
                                         sexID, 
                                         LA,
                                         pooled_ind_wt, 
                                         pooled_ind_wt_sc))
nrow(sscq23)

# import cluster data
xref23 <- haven::read_sas(xref.path23,
                          col_select = c(SSCQid, cluster, survey))


# Build SSCQ data --------------------------------------------------------------

analyse23_hh <- sscq23 %>% 
  
  # merge SSCQ and cluster data
  left_join(xref23, by = "SSCQid") %>%
  
  # remove observations whose individual weight was 0
  filter(pooled_ind_wt > 0) %>%
  
  mutate(sexID = ifelse(sexID %in% c(2:4), 2, 
                        ifelse(sexID %in% c(-1, 5, 9), NA, sexID)),
         sexID = factor(sexID, levels = c(-1, 1, 2),
                        labels = c(NA,
                                   "Heterosexual",
                                   "LGB+")))

# Analyse data ------------------------------------------------------------

# specify survey design
survey_ind <- svydesign(id = ~cluster, 
                        strata = ~LA,
                        weights = ~pooled_ind_wt,
                        data = analyse23_hh)

# SSCQ by sexID
sscq_estimates <- svymean(~sexID, survey_ind, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(sexID = levels(analyse23_hh$sexID), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(sexID) != TRUE)
sscq_estimates


# Constituent surveys ------------------------------------------------------------
survey_estimates <- analyse23_hh[c('sexID', 'survey', 'cluster', 'pooled_ind_wt', 'LA')] 

# SCJS by gen health
scjs_estimates <- filter(survey_estimates,survey == "SCJS")

scjs_ind <- svydesign(id = ~cluster, 
                      strata = ~LA,
                      weights = ~pooled_ind_wt,
                      data = scjs_estimates)

scjs_estimates <- svymean(~sexID, scjs_ind, na.rm = TRUE) %>%
  as_tibble() %>%
  mutate(sexID = levels(scjs_estimates$sexID), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(sexID) != TRUE)
scjs_estimates  


# SHeS by gen health
shes_estimates <- filter(survey_estimates,survey == "SHeS")

shes_ind <- svydesign(id = ~cluster, 
                      strata = ~LA,
                      weights = ~pooled_ind_wt,
                      data = shes_estimates)

shes_estimates <- svymean(~sexID, shes_ind, na.rm = TRUE) %>%
  as_tibble() %>%
  mutate(sexID = levels(shes_estimates$sexID), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(sexID) != TRUE)
shes_estimates


# SHS by gen health
shs_estimates <- filter(survey_estimates,survey == "SHS")

shs_ind <- svydesign(id = ~cluster, 
                     strata = ~LA,
                     weights = ~pooled_ind_wt,
                     data = shs_estimates)

shs_estimates <- svymean(~sexID, shs_ind, na.rm = TRUE) %>%
  as_tibble() %>%
  mutate(sexID = levels(shs_estimates$sexID), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(sexID) != TRUE)
shs_estimates


# Combining the datasets ------------------------------------------------------------
table_a7 <- bind_rows(sscq_estimates, scjs_estimates, shes_estimates, shs_estimates)

Survey <- c("SSCQ", "SSCQ",
            "SCJS", "SCJS",
            "SHeS", "SHeS",
            "SHS", "SHS")

table_a7$Survey <- Survey

table_a7 <- table_a7 %>% select(Survey, sexID, perc_with_ci)
table_a7


# Visualise estimates ------------------------------------------------------------

(table_a7 <- table_a7 %>%
   select(Survey, sexID, perc_with_ci) %>%
   group_by(Survey) %>% 
   pivot_wider(
     names_from = sexID,
     values_from = perc_with_ci))

print(table_a7)

kable(table_a7,
      caption = "Table A.7: Sexual orientation by source survey (row % and margin of error)")



saveRDS(table_a7, 
        "Technical Report/output/table_a7_estimates.rds")
