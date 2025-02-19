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
sscq.path23 <- paste0(sasdata.path, "sscq2022SBW.sas7bdat")

# Path to file with cluster info 2023
xref.path23 <- paste0(sasdata.path, "xref2022sbw.sas7bdat")

# Data Ind import -------------------------------------------------------------

# import sscq data
sscq23 <- haven::read_sas(sscq.path23,
                          col_select = c(SSCQid, 
                                         variable, 
                                         LA,
                                         pooled_ind_wt, 
                                         pooled_ind_wt_sc))
nrow(sscq23)

# import cluster data
xref23 <- haven::read_sas(xref.path23,
                          col_select = c(SSCQid, cluster))

# Data HH import -------------------------------------------------------------

# import sscq data
sscq23 <- haven::read_sas(sscq.path23,
                          col_select = c(SSCQid, 
                                         LA,
                                         pooled_hh_wt, 
                                         pooled_hh_wt_sc,
                                         variable))
nrow(sscq23)

# import cluster data
xref23 <- haven::read_sas(xref.path23,
                          col_select = c(SSCQid, cluster))


# Build Ind SSCQ data --------------------------------------------------------------

analyse23_ind <- sscq23 %>% 
  
  # merge SSCQ and cluster data
  left_join(xref23, by = "SSCQid") %>%
  
  # remove observations whose individual weight was 0
  filter(pooled_ind_wt > 0,
         age < 100)

analyse23_ind <- analyse23_ind %>%
  
  mutate(variable = factor(variable, levels = sort(unique(analyse23_ind$variable))))

# Build HH SSCQ data --------------------------------------------------------------

analyse23_hh <- sscq23 %>% 
  
  # merge SSCQ and cluster data
  left_join(xref23, by = "SSCQid") %>%
  
  # remove observations whose household weight was 0
  filter(pooled_hh_wt > 0) %>%
  
  # recode variable
  mutate(variable = factor(varaible, levels = c(-1, 1, 2, 3, 4, 5),
                                  labels = c(NA,
                                             "Owned outright",
                                             "Mortgaged",
                                             "Social rented",
                                             "Private rented",
                                             "Lives rent free")),
         
         varaible = ifelse(variable==5, 4, variable),
         varaible = factor(variable, levels = c(-1, 1, 2, 3, 4),
                         labels = c(NA,
                                    "Owned outright",
                                    "Mortgaged",
                                    "Social rented",
                                    "Private rented")))

# Analyse Ind data ------------------------------------------------------------

# specify survey design
survey_ind <- svydesign(id = ~cluster, 
                        strata = ~LA,
                        weights = ~pooled_ind_wt,
                        data = analyse23_ind)

# SSCQ by Ind variable
sscq_estimates <- svymean(~variable, survey_ind, na.rm = TRUE) %>%
  as_tibble() %>% 
  mutate(variable = levels(analyse23_ind$variable), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         mean = mean *100) %>%
  `names<-`(replace(names(.), 2, c('perc'))) %>%
  select(-SE) %>% 
  arrange(variable)
sscq_estimates

sscq_estimates <- sscq_estimates %>%
  mutate(margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1)))

sscq_estimates


# Analyse HH data ------------------------------------------------------------

# specify survey design
survey_hh <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_hh_wt,
                       data = analyse23_hh)

# SSCQ by HH variable
sscq_estimates <- svymean(~variable, survey_hh, na.rm = FALSE)%>%
  as_tibble() %>% 
  mutate(variable = levels(analyse23_hh$variable), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         mean = mean *100) %>%
  `names<-`(replace(names(.), 2, c('perc'))) %>%
  select(-SE) %>%
  filter(is.na(variable) != TRUE)
sscq_estimates

sscq_estimates <- sscq_estimates %>%
  mutate(margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1)))

sscq_estimates

# Visualise estimates ------------------------------------------------------------

(table <- sscq_estimates %>%
  select(variable, perc_with_ci) %>%
  pivot_wider(
    names_from = variable,
    values_from = perc_with_ci))

print(table)

kable(table,
      caption = "")
#caption from the tech report r markdown file


saveRDS(sscq_estimates, 
        "output/table_estimates.rds")
