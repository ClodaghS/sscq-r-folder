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
# Need to put correct path in once it becomes available

# Path to SAS data
sasdata.path <- "//s0177a/sasdata1/ocs_pool/"

# Path to SSCQ 2023 data
sscq.path23 <- paste0(sasdata.path, "sscq2023SBW.sas7bdat")

# Path to file with cluster info 2023
xref.path23 <- paste0(sasdata.path, "xref2023sbw.sas7bdat")


# Data HH import -------------------------------------------------------------

# import sscq data
sscq23 <- haven::read_sas(sscq.path23,
                          col_select = c(SSCQid, 
                                         LA,
                                         pooled_hh_wt, 
                                         pooled_hh_wt_sc,
                                         IndCare))
nrow(sscq23)

# import cluster data
xref23 <- haven::read_sas(xref.path23,
                          col_select = c(SSCQid, cluster, survey))


# Build HH SSCQ data --------------------------------------------------------------

analyse23_hh <- sscq23 %>% 
  
  # merge SSCQ and cluster data
  left_join(xref23, by = "SSCQid") %>%
  
  # remove observations whose household weight was 0
  filter(pooled_hh_wt > 0) %>%
  
  # recode variable
  mutate(IndCare = ifelse(IndCare == 99, 0, IndCare),
         
         IndCare = factor(IndCare, levels = c(1, 2),
                          labels = c("Yes, provides unpaid care",
                                     "No, doesn't provide unpaid care")))

# Analyse HH data ------------------------------------------------------------

# specify survey design
survey_hh <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_hh_wt,
                       data = analyse23_hh)

# SSCQ by IndCare variable
sscq_estimates <- svymean(~IndCare, survey_hh, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(IndCare = levels(analyse23_hh$IndCare), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(IndCare) != TRUE)
sscq_estimates


# Constituent surveys ------------------------------------------------------------
survey_estimates <- analyse23_hh[c('IndCare', 'survey', 'cluster', 'pooled_hh_wt', 'LA')] 

# SCJS by gen health
scjs_estimates <- filter(survey_estimates,survey == "SCJS")

scjs_hh <- svydesign(id = ~cluster, 
                      strata = ~LA,
                      weights = ~pooled_hh_wt,
                      data = scjs_estimates)

scjs_estimates <- svymean(~IndCare, scjs_hh, na.rm = TRUE) %>%
  as_tibble() %>%
  mutate(IndCare = levels(scjs_estimates$IndCare), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) 
scjs_estimates  


# SHeS by gen health
shes_estimates <- filter(survey_estimates,survey == "SHeS")

shes_hh <- svydesign(id = ~cluster, 
                      strata = ~LA,
                      weights = ~pooled_hh_wt,
                      data = shes_estimates)

shes_estimates <- svymean(~IndCare, shes_hh, na.rm = TRUE) %>%
  as_tibble() %>%
  mutate(IndCare = levels(shes_estimates$IndCare), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) 
shes_estimates


# SHS by gen health
shs_estimates <- filter(survey_estimates,survey == "SHS")

shs_hh <- svydesign(id = ~cluster, 
                     strata = ~LA,
                     weights = ~pooled_hh_wt,
                     data = shs_estimates)

shs_estimates <- svymean(~IndCare, shs_hh, na.rm = TRUE) %>%
  as_tibble() %>%
  mutate(IndCare = levels(shs_estimates$IndCare), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) 
shs_estimates


# Combining the datasets ------------------------------------------------------------
table_a4 <- bind_rows(sscq_estimates, scjs_estimates, shes_estimates, shs_estimates)

Survey <- c("SSCQ", "SSCQ",
            "SCJS", "SCJS",
            "SHeS", "SHeS",
            "SHS", "SHS")

table_a4$Survey <- Survey

table_a4 <- table_a4 %>% select(Survey, IndCare, perc_with_ci)
table_a4


# Visualise estimates ------------------------------------------------------------

(table_a4 <- table_a4 %>%
   select(Survey, IndCare, perc_with_ci) %>%
   group_by(Survey) %>% 
   pivot_wider(
     names_from = IndCare,
     values_from = perc_with_ci))

print(table_a4)

kable(table_a4,
      caption = "Table A.4: Provides unpaid care by source survey (row % and margin of error)")



saveRDS(table_a4, 
        "Technical Report/output/table_a4_estimates.rds")
