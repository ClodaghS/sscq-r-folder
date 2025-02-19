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
                                         BirthCountry, 
                                         LA,
                                         pooled_ind_wt))
nrow(sscq23)
# import cluster data
xref23 <- haven::read_sas(xref.path23,
                          col_select = c(SSCQid, cluster))

# import sscq22 data
sscq22 <- haven::read_sas(sscq.path22,
                          col_select = c(SSCQid, 
                                         BirthCountry, 
                                         LA,
                                         pooled_ind_wt))
nrow(sscq22)
# import cluster data
xref22 <- haven::read_sas(xref.path22,
                          col_select = c(SSCQid, cluster))

# import sscq19 data
sscq19 <- haven::read_sas(sscq.path19,
                          col_select = c(SSCQid, 
                                         BirthCountry, 
                                         LA,
                                         pooled_ind_wt))
nrow(sscq19)
# import cluster data
xref19 <- haven::read_sas(xref.path19,
                          col_select = c(SSCQid, cluster))

# import sscq18 data
sscq18 <- haven::read_sas(sscq.path18,
                          col_select = c(SSCQid, 
                                         BirthCountry, 
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
  mutate(BirthCountry = ifelse(BirthCountry %in% c(-2, -1, 0, 1001), -1,
                               ifelse(BirthCountry %in% c(40, 56, 100, 191, 203, 208, 233, 246, 276, 300, 348, 372, 380, 428, 440, 442, 470, 528, 616, 620, 642, 703, 705, 752, 901, 911, 913, 981), 3,
                                      ifelse(BirthCountry %in% c(921, 922, 924, 925, 926), 2,
                                             ifelse(BirthCountry %in% c(923), 1,
                                                    ifelse(BirthCountry %in% c(4:9999), 4, NA))))),
         BirthCountry = factor(BirthCountry, levels = c(-1, 1, 2, 3, 4),
                               labels = c(NA,
                                          "Scotland",
                                          "Rest of UK",
                                          "EU27",
                                          "Rest of World")))

sscq22 <- sscq22 %>% 
  left_join(xref22, by = "SSCQid") %>%
  filter(pooled_ind_wt > 0) %>% 
  mutate(BirthCountry = ifelse(BirthCountry %in% c(-2, -1, 0, 1001), -1,
                               ifelse(BirthCountry %in% c(40, 56, 100, 191, 203, 208, 233, 246, 276, 300, 348, 372, 380, 428, 440, 442, 470, 528, 616, 620, 642, 703, 705, 752, 901, 911, 913, 981), 3,
                                      ifelse(BirthCountry %in% c(921, 922, 924, 925, 926), 2,
                                             ifelse(BirthCountry %in% c(923), 1,
                                                    ifelse(BirthCountry %in% c(4:9999), 4, NA))))),
         BirthCountry = factor(BirthCountry, levels = c(-1, 1, 2, 3, 4),
                               labels = c(NA,
                                          "Scotland",
                                          "Rest of UK",
                                          "EU27",
                                          "Rest of World")))

sscq19 <- sscq19 %>% 
  left_join(xref19, by = "SSCQid") %>%
  filter(pooled_ind_wt > 0) %>% 
  mutate(BirthCountry = ifelse(BirthCountry %in% c(-2, -1, 0, 1001), -1,
                               ifelse(BirthCountry %in% c(40, 56, 100, 191, 203, 208, 233, 246, 276, 300, 348, 372, 380, 428, 440, 442, 470, 528, 616, 620, 642, 703, 705, 752, 901, 911, 913, 981), 3,
                                      ifelse(BirthCountry %in% c(921, 922, 924, 925, 926), 2,
                                             ifelse(BirthCountry %in% c(923), 1,
                                                    ifelse(BirthCountry %in% c(4:9999), 4, NA))))),
         BirthCountry = factor(BirthCountry, levels = c(-1, 1, 2, 3, 4),
                               labels = c(NA,
                                          "Scotland",
                                          "Rest of UK",
                                          "EU27",
                                          "Rest of World")))

sscq18 <- sscq18 %>% 
  left_join(xref18, by = "SSCQid") %>%
  filter(pooled_ind_wt > 0) %>% 
  mutate(BirthCountry = ifelse(BirthCountry %in% c(-2, -1, 0, 1001), -1,
                               ifelse(BirthCountry %in% c(40, 56, 100, 191, 203, 208, 233, 246, 276, 300, 348, 372, 380, 428, 440, 442, 470, 528, 616, 620, 642, 703, 705, 752, 901, 911, 913, 981), 3,
                                      ifelse(BirthCountry %in% c(921, 922, 924, 925, 926), 2,
                                             ifelse(BirthCountry %in% c(923), 1,
                                                    ifelse(BirthCountry %in% c(4:9999), 4, NA))))),
         BirthCountry = factor(BirthCountry, levels = c(-1, 1, 2, 3, 4),
                               labels = c(NA,
                                          "Scotland",
                                          "Rest of UK",
                                          "EU27",
                                          "Rest of World")))

# Analyse data ------------------------------------------------------------
# specify survey design
survey_23 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_ind_wt,
                       data = sscq23)

sscq_23 <- svymean(~BirthCountry, survey_23, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(BirthCountry = levels(sscq23$BirthCountry), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(BirthCountry) != TRUE)
sscq_23


survey_22 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_ind_wt,
                       data = sscq22)

sscq_22 <- svymean(~BirthCountry, survey_22, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(BirthCountry = levels(sscq22$BirthCountry), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(BirthCountry) != TRUE)
sscq_22


survey_19 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_ind_wt,
                       data = sscq19)

sscq_19 <- svymean(~BirthCountry, survey_19, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(BirthCountry = levels(sscq19$BirthCountry), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(BirthCountry) != TRUE)
sscq_19


survey_18 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_ind_wt,
                       data = sscq18)

sscq_18 <- svymean(~BirthCountry, survey_18, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(BirthCountry = levels(sscq18$BirthCountry), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(BirthCountry) != TRUE)
sscq_18

# Combining the datasets ------------------------------------------------------------
CoB <- bind_rows(sscq_23, sscq_22, sscq_19, sscq_18)

Year <- c("2023", "2023", "2023", "2023", 
          "2022", "2022", "2022", "2022", 
          "2019", "2019", "2019", "2019",  
          "2018", "2018", "2018", "2018")

CoB$Year <- Year

CoB_table <- CoB %>% select(Year, BirthCountry, perc_with_ci)
CoB_table

# Visualise estimates ------------------------------------------------------------
CoB_table <- CoB_table %>% 
  group_by(Year) %>% 
  pivot_wider(
    names_from = BirthCountry,
    values_from = perc_with_ci)

print(CoB_table)

saveRDS(CoB_table, 
        "QA/Outputs/CoB_table.rds")

# by CoB
# Scotland
CoB1 <- CoB %>% filter(BirthCountry == 'Scotland')
(CoB_1 <- ggplot(CoB1, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent Birth Country 
    by Year (Scotland)"))

#ggsave(CoB_1, filename = "QA/Outputs/CoB1.pdf",
 #      width = 200, height = 120, units = "mm")

# Rest of the UK
CoB2 <- CoB %>% filter(BirthCountry == 'Rest of UK')
(CoB_2 <- ggplot(CoB2, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent Birth Country 
    by Year (Rest of the UK)"))

#ggsave(CoB_2, filename = "QA/Outputs/CoB2.pdf",
 #      width = 200, height = 120, units = "mm")

# EU27
CoB3 <- CoB %>% filter(BirthCountry == 'EU27')
(CoB_3 <- ggplot(CoB3, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent Birth Country 
    by Year (EU27)"))

#ggsave(CoB_3, filename = "QA/Outputs/CoB3.pdf",
 #      width = 200, height = 120, units = "mm")

# Rest of the World
CoB4 <- CoB %>% filter(BirthCountry == 'Rest of World')
(CoB_4 <- ggplot(CoB4, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent Birth Country 
    by Year (Rest of the World)"))

#ggsave(CoB_4, filename = "QA/Outputs/CoB4.pdf",
 #      width = 200, height = 120, units = "mm")
