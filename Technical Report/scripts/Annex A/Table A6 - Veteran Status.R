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
                                         Vets, 
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
  
  # recode Vets
  mutate(Vets = ifelse(Vets == -1, NA, Vets),
         Vets = factor(Vets, levels = c(-1, 0, 1),
                       labels = c(NA,
                                  "Not a UK Armed Forces veteran",
                                  "UK Armed Forces veteran")))

# Analyse data ------------------------------------------------------------

# specify survey design
survey_ind <- svydesign(id = ~cluster, 
                        strata = ~LA,
                        weights = ~pooled_ind_wt,
                        data = analyse23_hh)

# SSCQ by Vets
sscq_estimates <- svymean(~Vets, survey_ind, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(Vets = levels(analyse23_hh$Vets), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(Vets) != TRUE)
sscq_estimates

# Constituent surveys ------------------------------------------------------------
survey_estimates <- analyse23_hh[c('Vets', 'survey', 'cluster', 'pooled_ind_wt', 'LA')] 

# SCJS by gen health
scjs_estimates <- filter(survey_estimates,survey == "SCJS")

scjs_ind <- svydesign(id = ~cluster, 
                      strata = ~LA,
                      weights = ~pooled_ind_wt,
                      data = scjs_estimates)

scjs_estimates <- svymean(~Vets, scjs_ind, na.rm = TRUE) %>%
  as_tibble() %>%
  mutate(Vets = levels(scjs_estimates$Vets), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(Vets) != TRUE)
scjs_estimates  


# SHeS by gen health
shes_estimates <- filter(survey_estimates,survey == "SHeS")

shes_ind <- svydesign(id = ~cluster, 
                      strata = ~LA,
                      weights = ~pooled_ind_wt,
                      data = shes_estimates)

shes_estimates <- svymean(~Vets, shes_ind, na.rm = TRUE) %>%
  as_tibble() %>%
  mutate(Vets = levels(shes_estimates$Vets), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(Vets) != TRUE)
shes_estimates


# SHS by gen health
shs_estimates <- filter(survey_estimates,survey == "SHS")

shs_ind <- svydesign(id = ~cluster, 
                     strata = ~LA,
                     weights = ~pooled_ind_wt,
                     data = shs_estimates)

shs_estimates <- svymean(~Vets, shs_ind, na.rm = TRUE) %>%
  as_tibble() %>%
  mutate(Vets = levels(shs_estimates$Vets), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(Vets) != TRUE)
shs_estimates


# Combining the datasets ------------------------------------------------------------
table_a6 <- bind_rows(sscq_estimates, scjs_estimates, shes_estimates, shs_estimates)

Survey <- c("SSCQ", "SSCQ",
            "SCJS", "SCJS",
            "SHeS", "SHeS",
            "SHS", "SHS")

table_a6$Survey <- Survey

table_a6 <- table_a6 %>% select(Survey, Vets, perc_with_ci)
table_a6


# Visualise estimates ------------------------------------------------------------

(table_a6 <- table_a6 %>%
    select(Survey, Vets, perc_with_ci) %>%
   group_by(Vets) %>% 
    pivot_wider(
      names_from = Vets,
      values_from = perc_with_ci))

print(table_a6)

kable(table_a6,
      caption = "Table A.6: Veteran status by source survey (row % and margin of error)")



saveRDS(table_a6, 
        "Technical Report/output/table_a6_estimates.rds")

