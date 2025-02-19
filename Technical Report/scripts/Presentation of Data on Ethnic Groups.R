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
                                         ethGroup, 
                                         LA,
                                         pooled_ind_wt, 
                                         pooled_ind_wt_sc))
nrow(sscq23)

# import cluster data
xref23 <- haven::read_sas(xref.path23,
                          col_select = c(SSCQid, cluster))


# Build Ind SSCQ data --------------------------------------------------------------
analyse23_ind <- sscq23 %>% 
  
  # merge SSCQ and cluster data
  left_join(xref23, by = "SSCQid") %>%
  
  # remove observations whose individual weight was 0
  filter(pooled_ind_wt > 0) %>%
  
  # recode ethCensus
  mutate(ethSuperGroup = ifelse(ethGroup == 5, 3,
                                ifelse(ethGroup %in% c(3, 4, 6), 4, 
                                       ifelse(ethGroup %in% c(8:12), 5,
                                              ifelse(ethGroup == -1, 6,
                                                     ifelse(ethGroup == 1, 1,
                                                            ifelse(ethGroup == 2, 2, 6)))))),
         ethSuperGroup = factor(ethSuperGroup, levels = c(1:6),
                                labels = c("White: Scottish",
                                           "White: Other British",
                                           "White: Polish",
                                           "White: Other",
                                           "Asian",
                                           "All other ethnic groups")),
         
         ethGroup = ifelse(ethGroup == -1, 19, ethGroup),
         ethGroup = factor(ethGroup, levels = c(1:22),
                           labels = c("A - WHITE - White Scottish",
                                      "A - WHITE - Other British",
                                      "A - WHITE - Irish",
                                      "A - WHITE - Gypsy/Traveller",
                                      "A - WHITE - Polish",
                                      "A - WHITE - Roma",
                                      "A - WHITE - Any other white ethnic group",
                                      "B - MIXED OR MULTIPLE ETHNIC GROUP - Any mixed or multiple ethnic groups",
                                      "C - ASIAN, ASIAN SCOTTISH OR ASIAN BRITISH - Pakistani, Pakistani Scottish or Pakistani British",
                                      "C - ASIAN, ASIAN SCOTTISH OR ASIAN BRITISH - Indian, Indian Scottish or Indian British",
                                      "C - ASIAN, ASIAN SCOTTISH OR ASIAN BRITISH - Bangladeshi, Bangladeshi Scottish or Bangladeshi British",
                                      "C - ASIAN, ASIAN SCOTTISH OR ASIAN BRITISH - Chinese, Chinese Scottish or Chinese British",
                                      "C - ASIAN, ASIAN SCOTTISH OR ASIAN BRITISH - Other Asian, Asian Scottish or Asian British",
                                      "D - AFRICAN - African, African Scottish or African British",
                                      "D - AFRICAN - Other African background",
                                      "E - CARIBBEAN OR BLACK - Caribbean, Caribbean Scottish or Caribbean British",
                                      "E - CARIBBEAN OR BLACK - Black, Black Scottish or Black British",
                                      "E - CARIBBEAN OR BLACK",
                                      "E - CARIBBEAN OR BLACK - Other Caribbean or Black background",
                                      "F - OTHER ETHNIC GROUP - Arab, Arab Scottish or Arab British",
                                      "F - OTHER ETHNIC GROUP - Other",
                                      "A - WHITE - Showman / Showwoman")))

# Visualise ungrouped data ------------------------------------------------------------
table_3.1 <- analyse23_ind %>% 
  count(ethGroup) %>%
  rename("Base Collection Categories" = ethGroup,
         "Sample" = n)

#Any observations less than 10 need to be changed to <10
table_3.1$Sample <- ifelse(table_3.1$Sample <10, "<10", table_3.1$Sample)
 

kable(table_3.1) %>% 
  kable_styling(full_width = F, position = "float_left")


saveRDS(table_3.1, 
        "Technical Report/output/table_3.1.rds")


# Visualise grouped data ------------------------------------------------------------
table_3.2 <- analyse23_ind %>% 
  count(ethSuperGroup) %>%
  rename("SSCQ Groups" = ethSuperGroup,
         "Sample" = n)

kable(table_3.2) %>% 
  kable_styling(full_width = F, position = "right")


saveRDS(table_3.2, 
        "Technical Report/output/table_3.2.rds")
