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
                                         MaritalStatus, 
                                         LA,
                                         pooled_ind_wt))
nrow(sscq23)
# import cluster data
xref23 <- haven::read_sas(xref.path23,
                          col_select = c(SSCQid, cluster))

# import sscq22 data
sscq22 <- haven::read_sas(sscq.path22,
                          col_select = c(SSCQid, 
                                         MaritalStatus, 
                                         LA,
                                         pooled_ind_wt))
nrow(sscq22)
# import cluster data
xref22 <- haven::read_sas(xref.path22,
                          col_select = c(SSCQid, cluster))

# import sscq19 data
sscq19 <- haven::read_sas(sscq.path19,
                          col_select = c(SSCQid, 
                                         MaritalStatus, 
                                         LA,
                                         pooled_ind_wt))
nrow(sscq19)
# import cluster data
xref19 <- haven::read_sas(xref.path19,
                          col_select = c(SSCQid, cluster))

# import sscq18 data
sscq18 <- haven::read_sas(sscq.path18,
                          col_select = c(SSCQid, 
                                         MaritalStatus, 
                                         LA,
                                         pooled_ind_wt))
nrow(sscq18)
# import cluster data
xref18 <- haven::read_sas(xref.path18,
                          col_select = c(SSCQid, cluster))

# Build SSCQ data --------------------------------------------------------------
sscq23 <- sscq23 %>% 
  left_join(xref23, by = "SSCQid") %>%
  filter(pooled_ind_wt > 0) %>% 
  mutate(MaritalStatus = factor(MaritalStatus, levels = c(-1, 1, 2, 3, 4, 5, 6, 7, 8, 9),
                         labels = c(NA,
                                    "Never married and never registered a same-sex civil partnership",
                                    "Married",
                                    "In a registered same-sex civil partnership",
                                    "Separated, but still legally married",
                                    "Separated, but still legally in a same-sex civil partnership",
                                    "Divorced",
                                    "Formerly in a same-sex civil partnership",
                                    "Widowed",
                                    "Surviving partner from a same-sex civil partnership")))

sscq22 <- sscq22 %>% 
  left_join(xref22, by = "SSCQid") %>%
  filter(pooled_ind_wt > 0) %>% 
  mutate(MaritalStatus = factor(MaritalStatus, levels = c(-1, 1, 2, 3, 4, 5, 6, 7, 8, 9),
                                labels = c(NA,
                                           "Never married and never registered a same-sex civil partnership",
                                           "Married",
                                           "In a registered same-sex civil partnership",
                                           "Separated, but still legally married",
                                           "Separated, but still legally in a same-sex civil partnership",
                                           "Divorced",
                                           "Formerly in a same-sex civil partnership",
                                           "Widowed",
                                           "Surviving partner from a same-sex civil partnership")))

sscq19 <- sscq19 %>% 
  left_join(xref19, by = "SSCQid") %>%
  filter(pooled_ind_wt > 0) %>% 
  mutate(MaritalStatus = factor(MaritalStatus, levels = c(-1, 1, 2, 3, 4, 5, 6, 7, 8, 9),
                                labels = c(NA,
                                           "Never married and never registered a same-sex civil partnership",
                                           "Married",
                                           "In a registered same-sex civil partnership",
                                           "Separated, but still legally married",
                                           "Separated, but still legally in a same-sex civil partnership",
                                           "Divorced",
                                           "Formerly in a same-sex civil partnership",
                                           "Widowed",
                                           "Surviving partner from a same-sex civil partnership")))

sscq18 <- sscq18 %>% 
  left_join(xref18, by = "SSCQid") %>%
  filter(pooled_ind_wt > 0) %>% 
  mutate(MaritalStatus = factor(MaritalStatus, levels = c(-1, 1, 2, 3, 4, 5, 6, 7, 8, 9),
                                labels = c(NA,
                                           "Never married and never registered a same-sex civil partnership",
                                           "Married",
                                           "In a registered same-sex civil partnership",
                                           "Separated, but still legally married",
                                           "Separated, but still legally in a same-sex civil partnership",
                                           "Divorced",
                                           "Formerly in a same-sex civil partnership",
                                           "Widowed",
                                           "Surviving partner from a same-sex civil partnership")))

# Analyse data ------------------------------------------------------------
# specify survey design
survey_23 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_ind_wt,
                       data = sscq23)

sscq_23 <- svymean(~MaritalStatus, survey_23, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(MaritalStatus = levels(sscq23$MaritalStatus), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(MaritalStatus) != TRUE)
sscq_23


survey_22 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_ind_wt,
                       data = sscq22)

sscq_22 <- svymean(~MaritalStatus, survey_22, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(MaritalStatus = levels(sscq22$MaritalStatus), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(MaritalStatus) != TRUE)
sscq_22


survey_19 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_ind_wt,
                       data = sscq19)

sscq_19 <- svymean(~MaritalStatus, survey_19, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(MaritalStatus = levels(sscq19$MaritalStatus), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(MaritalStatus) != TRUE)
sscq_19


survey_18 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_ind_wt,
                       data = sscq18)

sscq_18 <- svymean(~MaritalStatus, survey_18, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(MaritalStatus = levels(sscq18$MaritalStatus), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(MaritalStatus) != TRUE)
sscq_18

# Combining the datasets ------------------------------------------------------------
MaritalStatus <- bind_rows(sscq_23, sscq_22, sscq_19, sscq_18)

Year <- c("2023", "2023", "2023", "2023", "2023", "2023", "2023", "2023", "2023",
          "2022", "2022", "2022", "2022", "2022", "2022", "2022", "2022", "2022", 
          "2019", "2019", "2019", "2019", "2019", "2019", "2019", "2019", "2019", 
          "2018", "2018", "2018", "2018", "2018", "2018", "2018", "2018", "2018")

MaritalStatus$Year <- Year

MaritalStatus_table <- MaritalStatus %>% select(Year, MaritalStatus, perc_with_ci)
MaritalStatus_table

# Visualise estimates ------------------------------------------------------------
MaritalStatus_table <- MaritalStatus_table %>% 
  group_by(Year) %>% 
  pivot_wider(
    names_from = MaritalStatus,
    values_from = perc_with_ci)

print(MaritalStatus_table)

saveRDS(MaritalStatus_table, 
        "QA/Outputs/MaritalStatus_table.rds")

# by Marital Status
# Never married and never registered a same-sex civil partnership
MaritalStatus1 <- MaritalStatus %>% filter(MaritalStatus == 'Never married and never registered a same-sex civil partnership')
(MaritalStatus_1 <- ggplot(MaritalStatus1, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent Marital Status 
    by Year (Never Married)"))

#ggsave(MaritalStatus_1, filename = "QA/Outputs/MaritalStatus1.pdf",
 #      width = 200, height = 120, units = "mm")

# Married
MaritalStatus2 <- MaritalStatus %>% filter(MaritalStatus == 'Married')
(MaritalStatus_2 <- ggplot(MaritalStatus2, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent Marital Status
    by Year (Married)"))

#ggsave(MaritalStatus_2, filename = "QA/Outputs/MaritalStatus2.pdf",
 #      width = 200, height = 120, units = "mm")

# In a registered same-sex civil partnership
MaritalStatus3 <- MaritalStatus %>% filter(MaritalStatus == 'In a registered same-sex civil partnership')
(MaritalStatus_3 <- ggplot(MaritalStatus3, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent Marital Status
    by Year (Same-sex civil partnership)"))

#ggsave(MaritalStatus_3, filename = "QA/Outputs/MaritalStatus3.pdf",
 #      width = 200, height = 120, units = "mm")

# Separated, but still legally married
MaritalStatus4 <- MaritalStatus %>% filter(MaritalStatus == 'Separated, but still legally married')
(MaritalStatus_4 <- ggplot(MaritalStatus4, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent Marital Status by 
    Year (Separated, but still 
    legally married)"))

#ggsave(MaritalStatus_4, filename = "QA/Outputs/MaritalStatus4.pdf",
 #      width = 200, height = 120, units = "mm")

# Separated, but still legally in a same-sex civil partnership
MaritalStatus5 <- MaritalStatus %>% filter(MaritalStatus == 'Separated, but still legally in a same-sex civil partnership')
(MaritalStatus_5 <- ggplot(MaritalStatus5, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent Marital Status by 
    Year (Separated, but still legally in 
    a same-sex civil partnership)"))

#ggsave(MaritalStatus_5, filename = "QA/Outputs/MaritalStatus5.pdf",
 #      width = 200, height = 120, units = "mm")

# Divorced
MaritalStatus6 <- MaritalStatus %>% filter(MaritalStatus == 'Divorced')
(MaritalStatus_6 <- ggplot(MaritalStatus6, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent Marital Status 
    by Year (Divorced)"))

#ggsave(MaritalStatus_6, filename = "QA/Outputs/MaritalStatus6.pdf",
 #      width = 200, height = 120, units = "mm")

# Formerly in a same-sex civil partnership
MaritalStatus7 <- MaritalStatus %>% filter(MaritalStatus == 'Formerly in a same-sex civil partnership')
(MaritalStatus_7 <- ggplot(MaritalStatus7, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent Marital Status 
    by Year (Formerly in a same-sex 
    civil partnership)"))

#ggsave(MaritalStatus_7, filename = "QA/Outputs/MaritalStatus7.pdf",
 #      width = 200, height = 120, units = "mm")

# Widowed
MaritalStatus8 <- MaritalStatus %>% filter(MaritalStatus == 'Widowed')
(MaritalStatus_8 <- ggplot(MaritalStatus8, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent Marital Status 
    by Year (Widowed)"))

#ggsave(MaritalStatus_8, filename = "QA/Outputs/MaritalStatus8.pdf",
 #      width = 200, height = 120, units = "mm")

# Surviving partner from a same-sex civil partnership
MaritalStatus9 <- MaritalStatus %>% filter(MaritalStatus == 'Surviving partner from a same-sex civil partnership')
(MaritalStatus_9 <- ggplot(MaritalStatus9, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent Marital Status 
    by Year (Surviving partner from a 
    same-sex civil partnership)"))

#ggsave(MaritalStatus_9, filename = "QA/Outputs/MaritalStatus9.pdf",
 #      width = 200, height = 120, units = "mm")
