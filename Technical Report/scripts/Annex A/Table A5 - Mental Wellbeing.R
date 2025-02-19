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
                                         swemwbs))
nrow(sscq23)

# import cluster data
xref23 <- haven::read_sas(xref.path23,
                          col_select = c(SSCQid, cluster, survey))


# Build SSCQ data --------------------------------------------------------------

analyse23_hh <- sscq23 %>% 
  
  # merge SSCQ and cluster data
  left_join(xref23, by = "SSCQid") %>%
  
  # replace -1 with NA
  mutate(across(where(is.numeric), ~ ifelse(. == -1, NA, .))) %>%
  
  # remove observations whose household weight was 0
  filter(pooled_ind_wt > 0)


# Analyse data ------------------------------------------------------------

# specify survey design
survey_ind <- svydesign(id = ~cluster, 
                        strata = ~LA,
                        weights = ~pooled_ind_wt,
                        data = analyse23_hh)

# SSCQ by swemwbs
sscq_estimates <- svymean(~swemwbs, survey_ind, na.rm = TRUE)
sscq_estimates
#manually add the SE output from svymean where SE is 
#highlighted in the ci calculation
sscq_estimates <- as.numeric(sscq_estimates)  

sscq_estimates <- as_tibble(sscq_estimates) %>% 
  mutate(lower_ci = (value - 1.96 * 0.043),#SE
         upper_ci = (value + 1.96 * 0.043),#SE
         margin_of_error = upper_ci - value,
         mean_with_ci = paste0(round(value, 1), " ± ", round(margin_of_error, 1)))
sscq_estimates

  
# Constituent surveys ------------------------------------------------------------

survey_estimates <- analyse23_hh[c('swemwbs', 'survey', 'cluster', 'pooled_ind_wt', 'LA')] 

# SCJS by gen health

scjs_estimates <- filter(survey_estimates,survey == "SCJS")

scjs_ind <- svydesign(id = ~cluster, 
                      strata = ~LA,
                      weights = ~pooled_ind_wt,
                      data = scjs_estimates)

scjs_estimates <- svymean(~swemwbs, scjs_ind, na.rm = TRUE)
scjs_estimates
#manually add the SE output from svymean where SE is 
#highlighted in the ci calculation
scjs_estimates <- as.numeric(scjs_estimates)  

scjs_estimates <- as_tibble(scjs_estimates) %>% 
  mutate(lower_ci = (value - 1.96 * 0.0637),#SE
         upper_ci = (value + 1.96 * 0.0637),#SE
         margin_of_error = upper_ci - value,
         mean_with_ci = paste0(round(value, 1), " ± ", round(margin_of_error, 1)))
scjs_estimates

# SHeS by gen health
shes_estimates <- filter(survey_estimates,survey == "SHeS")

shes_ind <- svydesign(id = ~cluster, 
                      strata = ~LA,
                      weights = ~pooled_ind_wt,
                      data = shes_estimates)

shes_estimates <- svymean(~swemwbs, shes_ind, na.rm = TRUE)
shes_estimates
#manually add the SE output from svymean where SE is 
#highlighted in the ci calculation
shes_estimates <- as.numeric(shes_estimates)  

shes_estimates <- as_tibble(shes_estimates) %>% 
  mutate(lower_ci = (value - 1.96 * 0.0873),#SE
         upper_ci = (value + 1.96 * 0.0873),#SE
         margin_of_error = upper_ci - value,
         mean_with_ci = paste0(round(value, 1), " ± ", round(margin_of_error, 1)))
shes_estimates

# SHS by gen health
shs_estimates <- filter(survey_estimates,survey == "SHS")

shs_ind <- svydesign(id = ~cluster, 
                     strata = ~LA,
                     weights = ~pooled_ind_wt,
                     data = shs_estimates)

shs_estimates <- svymean(~swemwbs, shs_ind, na.rm = TRUE)
shs_estimates
#manually add the SE output from svymean where SE is 
#highlighted in the ci calculation
shs_estimates <- as.numeric(shs_estimates)  

shs_estimates <- as_tibble(shs_estimates) %>% 
  mutate(lower_ci = (value - 1.96 * 0.0547),#SE
         upper_ci = (value + 1.96 * 0.0547),#SE
         margin_of_error = upper_ci - value,
         mean_with_ci = paste0(round(value, 1), " ± ", round(margin_of_error, 1)))
shs_estimates


# Combining the datasets ------------------------------------------------------------

table_a5 <- bind_rows(sscq_estimates, scjs_estimates, shes_estimates, shs_estimates)

Survey <- c("SSCQ",
            "SCJS",
            "SHeS",
            "SHS")

table_a5$Survey <- Survey

table_a5 <- table_a5 %>% select(Survey, mean_with_ci)
table_a5

# Visualise estimates ------------------------------------------------------------

table_a5 <- table_a5 %>%
  select(Survey, mean_with_ci) %>%
  group_by(Survey) %>% 
  rename("Average Mental Wellbeing Score" = mean_with_ci)


print(table_a5)


kable(table_a5,
      caption = "Table A.5: Average mental wellbeing score by source survey (row average and margin of error)")


saveRDS(table_a5, 
        "Technical Report/output/table_a5_estimates.rds")
