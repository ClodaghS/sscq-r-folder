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
                                         PolConF))
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
  mutate(PolConF = ifelse(PolConF == 99, 0, PolConF),
         
         PolConF = factor(PolConF, levels = c(-1, 1, 2, 3, 4),
                          labels = c(NA,
                                     "Very confident",
                                     "Fairly confident",
                                     "Not very confident",
                                     "Not at all confident")))


# Analyse data ------------------------------------------------------------

# specify survey design
survey_ind <- svydesign(id = ~cluster, 
                        strata = ~LA,
                        weights = ~pooled_crim_wt,
                        data = analyse23_hh)

# SSCQ by gen health
sscq_estimates <- svymean(~PolConF, survey_ind, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(PolConF = levels(analyse23_hh$PolConF), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(PolConF) != TRUE)
sscq_estimates


# Constituent surveys ------------------------------------------------------------

survey_estimates <- analyse23_hh[c('PolConF', 'survey', 'cluster', 'pooled_crim_wt', 'LA')] 

# SCJS by gen health

scjs_estimates <- filter(survey_estimates,survey == "SCJS")

scjs_ind <- svydesign(id = ~cluster, 
                      strata = ~LA,
                      weights = ~pooled_crim_wt,
                      data = scjs_estimates)

scjs_estimates <- svymean(~PolConF, scjs_ind, na.rm = TRUE) %>%
  as_tibble() %>%
  mutate(PolConF = levels(scjs_estimates$PolConF), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>% 
  filter(is.na(PolConF) != TRUE)
scjs_estimates  

# SHeS by gen health
#shes_estimates <- filter(survey_estimates,survey == "SHeS")

#shes_ind <- svydesign(id = ~cluster, 
 #                     strata = ~LA,
  #                    weights = ~pooled_crim_wt,
   #                   data = shes_estimates)

#shes_estimates <- svymean(~PolConF, shes_ind, na.rm = TRUE) %>%
 # as_tibble() %>%
  #mutate(PolConF = levels(shes_estimates$PolConF), .before = mean) %>%
  #mutate(lower_ci = (mean - 1.96 * SE) *100,
   #      upper_ci = (mean + 1.96 * SE) *100,
    #     perc = mean *100,
     #    margin_of_error = upper_ci - perc,
      #   perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  #select(-SE) %>% 
  #filter(is.na(PolConF) != TRUE)
#shes_estimates

# SHS by gen health
shs_estimates <- filter(survey_estimates,survey == "SHS")

shs_ind <- svydesign(id = ~cluster, 
                     strata = ~LA,
                     weights = ~pooled_crim_wt,
                     data = shs_estimates)

shs_estimates <- svymean(~PolConF, shs_ind, na.rm = TRUE) %>%
  as_tibble() %>%
  mutate(PolConF = levels(shs_estimates$PolConF), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>% 
  filter(is.na(PolConF) != TRUE)
shs_estimates


# Combining the datasets ------------------------------------------------------------

table_a9f <- bind_rows(sscq_estimates, scjs_estimates, #shes_estimates,
                       shs_estimates)

Survey <- c("SSCQ", "SSCQ", "SSCQ", "SSCQ",
            "SCJS", "SCJS", "SCJS", "SCJS",
           # "SHeS", "SHeS", "SHeS", "SHeS",
            "SHS", "SHS", "SHS", "SHS")

table_a9f$Survey <- Survey

table_a9f <- table_a9f %>% select(Survey, PolConF, perc_with_ci)
table_a9f

# Visualise estimates ------------------------------------------------------------

table_a9f <- table_a9f %>%
  select(Survey, PolConF, perc_with_ci) %>%
  group_by(Survey) %>% 
  pivot_wider(
    names_from = PolConF,
    values_from = perc_with_ci)


print(table_a9f)


kable(table_a9f,
      caption = "Table A.9.F: Confidence in the police to catch criminals by source survey (row % and margin of error)")


saveRDS(table_a9f, 
        "Technical Report/output/table_a9f_estimates.rds")
