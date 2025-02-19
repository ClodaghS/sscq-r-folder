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
# Need to put correct path in once it becomes available

# Path to SAS data
sasdata.path <- "//s0177a/sasdata1/ocs_pool/"

# Path to SCJS 2022 data
scjs22.path <- "//s0177a/sasdata1/scjs/main2_2021_22.sas7bdat"

# Path to SHeS 2022 data
shes22.path <- "//s0177a/sasdata1/scottish_health_survey/public/shes22i.sas7bdat"

# path to shes trans data
shestrans22.path <- "//s0177a/datashare/OCS/OCS 2/Centralised Weighting Project/2022 Weighting/SHeS 2022/Data from ScotCen/shes22details_2.csv"

# Path to SHS 2022 data
shs22.path <- "//s0177a/sasdata1/shs/shs/randad22.sas7bdat"

# Path to SSCQ 2022 data
sscq.path22 <- paste0(sasdata.path, "sscq2022SBW.sas7bdat")

# Path to file with cluster info 2022
xref.path22 <- paste0(sasdata.path, "xref2022sbw.sas7bdat")

# Path to SCJS 2023 data
scjs23.path <- "//s0177a/sasdata1/scjs/main3_2023_24.sas7bdat"

# Path to SHeS 2023 data
shes23.path <- "//s0177a/sasdata1/scottish_health_survey/public/shes23i.sas7bdat"

# path to shes trans data
shestrans23.path <- "//s0177a/datashare/OCS/OCS 2/Centralised Weighting Project/2023 Weighting/SHeS 2023/Data from ScotCen/shes23details.csv"

# Path to SHS 2023 data
shs23.path <- "//s0177a/sasdata1/shs/shs/randad23.sas7bdat"

# Path to SSCQ 2023 data
sscq.path23 <- paste0(sasdata.path, "sscq2023SBW.sas7bdat")

# Path to file with cluster info 2023
xref.path23 <- paste0(sasdata.path, "xref2023sbw.sas7bdat")


# Data import -------------------------------------------------------------
# import SCJS22 data
scjs22 <- haven::read_sas(scjs22.path,
                        col_select = c(SERIAL,
                                       QDTRANS,
                                       QDTRANSTAT)) %>%
  rename(oldref_scjs =  SERIAL,
         trans_status_scjs = QDTRANS)

# import SHeS22 data
#shes <- haven::read_sas(shesdata.path,
#                        col_select = c(CPSerialA))
shes_trans22 <- read_csv(shestrans22.path) %>%
  select(CPSerialA, TRANS, TRANS2, TransY) %>%
  rename(oldref_shes =  CPSerialA,
         trans_status_shes = TRANS) 

# import SHS22 data
shs22 <- haven::read_sas(shs22.path,
                       col_select = c(UNIQID,
                                      RANDTRANS))%>%
  rename(oldref_shs =  UNIQID,
         trans_status_shs = RANDTRANS)

# import sscq22 data
sscq22 <- haven::read_sas(sscq.path22,
                          col_select = c(SSCQid, 
                                         LA,
                                         pooled_ind_wt, 
                                         pooled_ind_wt_sc))
nrow(sscq22)

# import cluster data
xref22 <- haven::read_sas(xref.path22,
                          col_select = c(SSCQid, cluster, oldref))



# import SCJS23 data
scjs23 <- haven::read_sas(scjs23.path,
                        col_select = c(Serial,
                                       QDTRANS)) %>%
  rename(oldref_scjs =  Serial,
         trans_status_scjs = QDTRANS)

# import SHeS data
#shes <- haven::read_sas(shes23.path,
#                        col_select = c(CPSerialA))
shes_trans23 <- read_csv(shestrans23.path) %>%
  select(CPSerialA, TRANS, TransY) %>%
  rename(oldref_shes =  CPSerialA,
         trans_status_shes = TRANS) 

# import SHS data
shs23 <- haven::read_sas(shs23.path,
                       col_select = c(UNIQID,
                                      RANDTRANS))%>%
  rename(oldref_shs =  UNIQID,
         trans_status_shs = RANDTRANS)

# import sscq data
sscq23 <- haven::read_sas(sscq.path23,
                          col_select = c(SSCQid, 
                                         LA,
                                         pooled_ind_wt, 
                                         pooled_ind_wt_sc))
nrow(sscq23)

# import cluster data
xref23 <- haven::read_sas(xref.path23,
                          col_select = c(SSCQid, cluster, oldref))

# Data wrangling -------------------------------------------------------------------
allsurvey22 <- bind_rows(scjs22, shes_trans22, shs22) %>%
  mutate(oldref = ifelse(is.na(oldref_scjs) == FALSE, oldref_scjs, 
                         ifelse(is.na(oldref_shes) == FALSE, oldref_shes, oldref_shs)),
         trans_status = ifelse(is.na(trans_status_scjs) == FALSE, trans_status_scjs, 
                               ifelse(is.na(trans_status_shes) == FALSE, trans_status_shes, trans_status_shs))) %>%
  select(-c(starts_with("oldref_"), starts_with("trans_status_"))) %>%
  mutate(trans_status = ifelse(trans_status < 0, -1, trans_status))



allsurvey23 <- bind_rows(scjs23, shes_trans23, shs23) %>%
  mutate(oldref = ifelse(is.na(oldref_scjs) == FALSE, oldref_scjs, 
                         ifelse(is.na(oldref_shes) == FALSE, oldref_shes, oldref_shs)),
         trans_status = ifelse(is.na(trans_status_scjs) == FALSE, trans_status_scjs, 
                               ifelse(is.na(trans_status_shes) == FALSE, trans_status_shes, trans_status_shs))) %>%
  select(-c(starts_with("oldref_"), starts_with("trans_status_"))) %>%
  mutate(trans_status = ifelse(trans_status < 0, -1, trans_status))


# Build SSCQ data --------------------------------------------------------------
sscq23 <- sscq23 %>% 
  left_join(xref23, by = "SSCQid") %>%
  left_join(allsurvey23,
            by = c("oldref")) %>%
  # remove observations whose individual weight was 0
  filter(pooled_ind_wt > 0) %>%
  mutate(trans_status = ifelse(trans_status %in% c(-1, 3), 3, trans_status),
         trans_status = factor(trans_status, levels = c(1:3),
                               labels = c("No: Not trans and does not have a trans history",
                                          "Yes: Trans or has a trans history",
                                          NA)))


sscq22 <- sscq22 %>% 
  # merge SSCQ and cluster data
  left_join(xref22, by = "SSCQid") %>%
  left_join(allsurvey22,
            by = c("oldref")) %>%
  # remove observations whose individual weight was 0
  filter(pooled_ind_wt > 0) %>%
  mutate(trans_status = ifelse(trans_status %in% c(-1, 3), 3, trans_status),
         trans_status = factor(trans_status, levels = c(1:3),
                               labels = c("No: Not trans and does not have a trans history",
                                          "Yes: Trans or has a trans history",
                                          NA)))


# Analyse data ------------------------------------------------------------
# specify survey design
survey_23 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_ind_wt,
                       data = sscq23)

sscq_23 <- svymean(~trans_status, survey_23, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(trans_status = levels(sscq23$trans_status), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(trans_status) != TRUE)
sscq_23


survey_22 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_ind_wt,
                       data = sscq22)

sscq_22 <- svymean(~trans_status, survey_22, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(trans_status = levels(sscq22$trans_status), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(trans_status) != TRUE)
sscq_22


# Combining the datasets ------------------------------------------------------------
Trans <- bind_rows(sscq_23, sscq_22)

Year <- c("2023", "2023",
          "2022", "2022")

Trans$Year <- Year

Trans_table <- Trans %>% select(Year, trans_status, perc_with_ci)
Trans_table

# Visualise estimates ------------------------------------------------------------
Trans_table <- Trans_table %>% 
  group_by(Year) %>% 
  pivot_wider(
    names_from = trans_status,
    values_from = perc_with_ci)

print(Trans_table)

saveRDS(Trans_table, 
        "QA/Outputs/Trans_table.rds")

# by Trans Status
# Yes
Trans1 <- Trans %>% filter(trans_status == 'Yes: Trans or has a trans history')
(Trans_1 <- ggplot(Trans1, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent Trans 
    Status by Year (Yes: Trans or 
    has a trans history)"))

#ggsave(Trans_1, filename = "QA/Outputs/Trans1.pdf",
 #      width = 200, height = 120, units = "mm")

# No
Trans2 <- Trans %>% filter(trans_status == 'No: Not trans and does not have a trans history')
(Trans_2 <- ggplot(Trans2, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent Trans Status 
    by Year (No: Not trans and does not 
    have a trans history)"))

#ggsave(Trans_2, filename = "QA/Outputs/Trans2.pdf",
 #      width = 200, height = 120, units = "mm")
