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

# Path to SSCQ 2023 data
sscq.path23 <- paste0(sasdata.path, "sscq2023SBW.sas7bdat")
# Path to file with cluster info 2023
xref.path23 <- paste0(sasdata.path, "xref2023sbw.sas7bdat")

# Path to SSCQ 2022 data
sscq.path22 <- paste0(sasdata.path, "sscq2022SBW.sas7bdat")
# Path to file with cluster info 2022
xref.path22 <- paste0(sasdata.path, "xref2022sbw.sas7bdat")

# Path to SSCQ 2019 data
sscq.path19 <- paste0(sasdata.path, "sscq2019BC_UR.sas7bdat")
# Path to file with cluster info 2019
xref.path19 <- paste0(sasdata.path, "xref2019BC_UR.sas7bdat")

# Path to SSCQ 2018 data
sscq.path18 <- paste0(sasdata.path, "sscq2018.sas7bdat")
# Path to file with cluster info 2018
xref.path18 <- paste0(sasdata.path, "xref2018.sas7bdat")


# Data import -------------------------------------------------------------
# import sscq23 data
sscq23 <- haven::read_sas(sscq.path23,
                          col_select = c(SSCQid, 
                                         htype2a, 
                                         LA,
                                         pooled_hh_wt))
nrow(sscq23)
# import cluster data
xref23 <- haven::read_sas(xref.path23,
                          col_select = c(SSCQid, cluster))

# import sscq22 data
sscq22 <- haven::read_sas(sscq.path22,
                          col_select = c(SSCQid, 
                                         htype2a, 
                                         LA,
                                         pooled_hh_wt))
nrow(sscq22)
# import cluster data
xref22 <- haven::read_sas(xref.path22,
                          col_select = c(SSCQid, cluster))

# import sscq19 data
sscq19 <- haven::read_sas(sscq.path19,
                          col_select = c(SSCQid, 
                                         htype2a, 
                                         LA,
                                         pooled_hh_wt))
nrow(sscq19)
# import cluster data
xref19 <- haven::read_sas(xref.path19,
                          col_select = c(SSCQid, cluster))

# import sscq18 data
sscq18 <- haven::read_sas(sscq.path18,
                          col_select = c(SSCQid, 
                                         htype2a, 
                                         LA,
                                         pooled_hh_wt))
nrow(sscq18)
# import cluster data
xref18 <- haven::read_sas(xref.path18,
                          col_select = c(SSCQid, cluster))

# Build SSCQ data --------------------------------------------------------------
sscq23 <- sscq23 %>% 
  left_join(xref23, by = "SSCQid") %>%
  filter(pooled_hh_wt > 0) %>% 
  mutate(htype2a = factor(htype2a, levels = c(-1, 1, 2, 3, 4, 5, 6, 7, 8),
                      labels = c(NA,
                                 "Single Adult",
                                 "Small Adult",
                                 "Large Adult",
                                 "Single Parent",
                                 "Small Family",
                                 "Large Family",
                                 "Single Pensioner",
                                 "Older Couple")))

sscq22 <- sscq22 %>% 
  left_join(xref22, by = "SSCQid") %>%
  filter(pooled_hh_wt > 0) %>% 
  mutate(htype2a = factor(htype2a, levels = c(-1, 1, 2, 3, 4, 5, 6, 7, 8),
                          labels = c(NA,
                                     "Single Adult",
                                     "Small Adult",
                                     "Large Adult",
                                     "Single Parent",
                                     "Small Family",
                                     "Large Family",
                                     "Single Pensioner",
                                     "Older Couple")))

sscq19 <- sscq19 %>% 
  left_join(xref19, by = "SSCQid") %>%
  filter(pooled_hh_wt > 0) %>% 
  mutate(htype2a = factor(htype2a, levels = c(-1, 1, 2, 3, 4, 5, 6, 7, 8),
                          labels = c(NA,
                                     "Single Adult",
                                     "Small Adult",
                                     "Large Adult",
                                     "Single Parent",
                                     "Small Family",
                                     "Large Family",
                                     "Single Pensioner",
                                     "Older Couple")))

sscq18 <- sscq18 %>% 
  left_join(xref18, by = "SSCQid") %>%
  filter(pooled_hh_wt > 0) %>% 
  mutate(htype2a = factor(htype2a, levels = c(-1, 1, 2, 3, 4, 5, 6, 7, 8),
                          labels = c(NA,
                                     "Single Adult",
                                     "Small Adult",
                                     "Large Adult",
                                     "Single Parent",
                                     "Small Family",
                                     "Large Family",
                                     "Single Pensioner",
                                     "Older Couple")))

# Analyse data ------------------------------------------------------------
# specify survey design
survey_23 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_hh_wt,
                       data = sscq23)

sscq_23 <- svymean(~htype2a, survey_23, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(htype2a = levels(sscq23$htype2a), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(htype2a) != TRUE)
sscq_23


survey_22 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_hh_wt,
                       data = sscq22)

sscq_22 <- svymean(~htype2a, survey_22, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(htype2a = levels(sscq22$htype2a), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(htype2a) != TRUE)
sscq_22


survey_19 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_hh_wt,
                       data = sscq19)

sscq_19 <- svymean(~htype2a, survey_19, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(htype2a = levels(sscq19$htype2a), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(htype2a) != TRUE)
sscq_19


survey_18 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_hh_wt,
                       data = sscq18)

sscq_18 <- svymean(~htype2a, survey_18, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(htype2a = levels(sscq18$htype2a), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(htype2a) != TRUE)
sscq_18

# Combining the datasets ------------------------------------------------------------
HType <- bind_rows(sscq_23, sscq_22, sscq_19, sscq_18)

Year <- c("2023", "2023", "2023", "2023", "2023", "2023", "2023", "2023",
          "2022", "2022", "2022", "2022", "2022", "2022", "2022", "2022",
          "2019", "2019", "2019", "2019", "2019", "2019", "2019", "2019",
          "2018", "2018", "2018", "2018", "2018", "2018", "2018", "2018")

HType$Year <- Year

HType_table <- HType %>% select(Year, htype2a, perc_with_ci)
HType_table

# Visualise estimates ------------------------------------------------------------
HType_table <- HType_table %>% 
  group_by(Year) %>% 
  pivot_wider(
    names_from = htype2a,
    values_from = perc_with_ci)

print(HType_table)

saveRDS(HType_table, 
        "QA/Outputs/HType_table.rds")

# by HH Type
# Single Adult
HType1 <- HType %>% filter(htype2a == 'Single Adult')
(HType_1 <- ggplot(HType1, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent Household Type
    by Year (Single Adult)"))

#ggsave(HType_1, filename = "QA/Outputs/HType1.pdf",
 #      width = 200, height = 120, units = "mm")

# Small Adult
HType2 <- HType %>% filter(htype2a == 'Small Adult')
(HType_2 <- ggplot(HType2, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent Household Type 
    by Year (Small Adult)"))

#ggsave(HType_2, filename = "QA/Outputs/HType2.pdf",
 #      width = 200, height = 120, units = "mm")

# Large Adult
HType3 <- HType %>% filter(htype2a == 'Large Adult')
(HType_3 <- ggplot(HType3, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent Household Type 
    by Year (Large Adult)"))

#ggsave(HType_3, filename = "QA/Outputs/HType3.pdf",
 #      width = 200, height = 120, units = "mm")

# Single Parent
HType4 <- HType %>% filter(htype2a == 'Single Parent')
(HType_4 <- ggplot(HType4, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent Household Type
    by Year (Single Parent)"))

#ggsave(HType_4, filename = "QA/Outputs/HType4.pdf",
 #      width = 200, height = 120, units = "mm")

# Small Family
HType5 <- HType %>% filter(htype2a == 'Small Family')
(HType_5 <- ggplot(HType5, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent Household Type 
    by Year (Small Family)"))

#ggsave(HType_5, filename = "QA/Outputs/HType5.pdf",
 #      width = 200, height = 120, units = "mm")

# Large Family
HType6 <- HType %>% filter(htype2a == 'Large Family')
(HType_6 <- ggplot(HType6, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent Household Type 
    by Year (Large Family)"))

#ggsave(HType_6, filename = "QA/Outputs/HType6.pdf",
 #      width = 200, height = 120, units = "mm")

# Single Pensioner
HType7 <- HType %>% filter(htype2a == 'Single Pensioner')
(HType_7 <- ggplot(HType7, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent Household Type
    by Year (Single Pensioner)"))

#ggsave(HType_7, filename = "QA/Outputs/HType7.pdf",
 #      width = 200, height = 120, units = "mm")

# Older Couple
HType8 <- HType %>% filter(htype2a == 'Older Couple')
(HType_8 <- ggplot(HType8, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent Household Type 
    by Year (Older Couple)"))

#ggsave(HType_8, filename = "QA/Outputs/HType8.pdf",
 #      width = 200, height = 120, units = "mm")
