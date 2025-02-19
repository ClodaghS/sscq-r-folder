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
library(kableExtra)
library(flextable)

sgplot::use_sgplot()

# Setup -------------------------------------------------------------------
# Need to put correct path in once it becomes available

# Path to SAS data
sasdata.path <- "//s0177a/sasdata1/ocs_pool/"

# Path to SSCQ 2023 data
sscq.path23 <- paste0(sasdata.path, "sscq2023SBW.sas7bdat")

# Path to file with cluster info 2023
xref.path23 <- paste0(sasdata.path, "xref2023sbw.sas7bdat")

# Data Ind import -------------------------------------------------------------

# import sscq data
sscq23 <- haven::read_sas(sscq.path23,
                          col_select = c(SSCQid, 
                                         Religion, 
                                         LA,
                                         pooled_ind_wt, 
                                         pooled_ind_wt_sc))
nrow(sscq23)

# import cluster data
xref23 <- haven::read_sas(xref.path23,
                          col_select = c(SSCQid, cluster))


# Build HH SSCQ data --------------------------------------------------------------

analyse23_ind <- sscq23 %>% 
  
  # merge SSCQ and cluster data
  left_join(xref23, by = "SSCQid") %>%
  
  # remove observations whose household weight was 0
  filter(pooled_ind_wt > 0) %>%
  
  # recode variable
  mutate(Religion = factor(Religion, levels = c(-1, 1, 2, 3, 4, 5, 6, 7, 8, 9, 10, 11),
                           labels = c(NA,
                                      "None",
                                      "Church of Scotland",
                                      "Roman Catholic",
                                      "Other Christian",
                                      "Muslim",
                                      "Buddhist",
                                      "Sikh",
                                      "Jewish",
                                      "Hindu",
                                      "Pagan",
                                      "Another religion")))

# Visualise ungrouped data ------------------------------------------------------------

table_2.1 <- analyse23_ind %>% 
  count(Religion) %>% 
  slice(-1) #removes NA row

table_2.1 <- table_2.1 %>% 
  rename("Base Collection Categories" = Religion,
         "Sample" = n)

kable(table_2.1)
flextable(table_2.1)


saveRDS(table_2.1, 
        "Technical Report/output/table_2.1.rds")


# Visualise grouped data ------------------------------------------------------------
#group the data for SSCQ

table_2.2 <- analyse23_ind 

levels(table_2.2$Religion)[levels(table_2.2$Religion)=="Buddhist"] <-"Other"
levels(table_2.2$Religion)[levels(table_2.2$Religion)=="Sikh"] <-"Other"
levels(table_2.2$Religion)[levels(table_2.2$Religion)=="Jewish"] <-"Other"
levels(table_2.2$Religion)[levels(table_2.2$Religion)=="Hindu"] <-"Other"
levels(table_2.2$Religion)[levels(table_2.2$Religion)=="Pagan"] <-"Other"
levels(table_2.2$Religion)[levels(table_2.2$Religion)=="Another religion"] <-"Other"


table_2.2 <- table_2.2 %>% 
  count(Religion) %>%
  slice(-7) %>% 
  rename("SSCQ Groups" = Religion,
         "Sample" = n)
  
kable(table_2.2) 
flextable(table_2.2)


saveRDS(table_2.2, 
        "Technical Report/output/table_2.2.rds")

