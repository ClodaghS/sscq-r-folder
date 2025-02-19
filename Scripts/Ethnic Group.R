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
                                         ethGroup, 
                                         LA,
                                         pooled_ind_wt))
nrow(sscq23)
# import cluster data
xref23 <- haven::read_sas(xref.path23,
                          col_select = c(SSCQid, cluster))

# import sscq22 data
sscq22 <- haven::read_sas(sscq.path22,
                          col_select = c(SSCQid, 
                                         ethGroup, 
                                         LA,
                                         pooled_ind_wt))
nrow(sscq22)
# import cluster data
xref22 <- haven::read_sas(xref.path22,
                          col_select = c(SSCQid, cluster))

# import sscq19 data
sscq19 <- haven::read_sas(sscq.path19,
                          col_select = c(SSCQid, 
                                         ethGroup, 
                                         LA,
                                         pooled_ind_wt))
nrow(sscq19)
# import cluster data
xref19 <- haven::read_sas(xref.path19,
                          col_select = c(SSCQid, cluster))

# import sscq18 data
sscq18 <- haven::read_sas(sscq.path18,
                          col_select = c(SSCQid, 
                                         ethGroup, 
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
                                           "All other ethnic groups")))

sscq22 <- sscq22 %>% 
  left_join(xref22, by = "SSCQid") %>%
  filter(pooled_ind_wt > 0) %>% 
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
                                           "All other ethnic groups")))

sscq19 <- sscq19 %>% 
  left_join(xref19, by = "SSCQid") %>%
  filter(pooled_ind_wt > 0) %>% 
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
                                           "All other ethnic groups")))

sscq18 <- sscq18 %>% 
  left_join(xref18, by = "SSCQid") %>%
  filter(pooled_ind_wt > 0) %>% 
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
                                           "All other ethnic groups")))

# Analyse data ------------------------------------------------------------
# specify survey design
survey_23 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_ind_wt,
                       data = sscq23)

sscq_23 <- svymean(~ethSuperGroup, survey_23, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(ethSuperGroup = levels(sscq23$ethSuperGroup), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(ethSuperGroup) != TRUE)
sscq_23


survey_22 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_ind_wt,
                       data = sscq22)

sscq_22 <- svymean(~ethSuperGroup, survey_22, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(ethSuperGroup = levels(sscq22$ethSuperGroup), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(ethSuperGroup) != TRUE)
sscq_22


survey_19 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_ind_wt,
                       data = sscq19)

sscq_19 <- svymean(~ethSuperGroup, survey_19, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(ethSuperGroup = levels(sscq19$ethSuperGroup), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(ethSuperGroup) != TRUE)
sscq_19


survey_18 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_ind_wt,
                       data = sscq18)

sscq_18 <- svymean(~ethSuperGroup, survey_18, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(ethSuperGroup = levels(sscq18$ethSuperGroup), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(ethSuperGroup) != TRUE)
sscq_18

# Combining the datasets ------------------------------------------------------------
Ethnicity <- bind_rows(sscq_23, sscq_22, sscq_19, sscq_18)

Year <- c("2023", "2023", "2023", "2023", "2023", "2023", 
          "2022", "2022", "2022", "2022", "2022", "2022",
          "2019", "2019", "2019", "2019", "2019", "2019", 
          "2018", "2018", "2018", "2018", "2018", "2018")

Ethnicity$Year <- Year

Ethnicity_table <- Ethnicity %>% select(Year, ethSuperGroup, perc_with_ci)
Ethnicity_table

# Visualise estimates ------------------------------------------------------------
Ethnicity_table <- Ethnicity_table %>% 
  group_by(Year) %>% 
  pivot_wider(
    names_from = ethSuperGroup,
    values_from = perc_with_ci)

print(Ethnicity_table)

saveRDS(Ethnicity_table, 
        "QA/Outputs/Ethnicity_table.rds")

# by Ethnicity
# White Scottish
Ethnicity1 <- Ethnicity %>% filter(ethSuperGroup == 'White: Scottish')
(Ethnicity_1 <- ggplot(Ethnicity1, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent Ethnicity 
    by Year (White: Scottish)"))

#ggsave(Ethnicity_1, filename = "QA/Outputs/Ethnicity1.pdf",
 #      width = 200, height = 120, units = "mm")

# White: Other British
Ethnicity2 <- Ethnicity %>% filter(ethSuperGroup == 'White: Other British')
(Ethnicity_2 <- ggplot(Ethnicity2, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent Ethnicity by 
    Year (White: Other British)"))

#ggsave(Ethnicity_2, filename = "QA/Outputs/Ethnicity2.pdf",
 #      width = 200, height = 120, units = "mm")

# White: Polish
Ethnicity3 <- Ethnicity %>% filter(ethSuperGroup == 'White: Polish')
(Ethnicity_3 <- ggplot(Ethnicity3, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent Ethnicity 
    by Year (White: Polish)"))

#ggsave(Ethnicity_3, filename = "QA/Outputs/Ethnicity3.pdf",
 #      width = 200, height = 120, units = "mm")

# White: Other
Ethnicity4 <- Ethnicity %>% filter(ethSuperGroup == 'White: Other')
(Ethnicity_4 <- ggplot(Ethnicity4, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent Ethnicity 
    by Year (White: Other)"))

#ggsave(Ethnicity_4, filename = "QA/Outputs/Ethnicity4.pdf",
 #      width = 200, height = 120, units = "mm")

# Asian
Ethnicity5 <- Ethnicity %>% filter(ethSuperGroup == 'Asian')
(Ethnicity_5 <- ggplot(Ethnicity5, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent Ethnicity 
    by Year (Asian)"))

#ggsave(Ethnicity_5, filename = "QA/Outputs/Ethnicity5.pdf",
 #      width = 200, height = 120, units = "mm")

# All other ethnic groups
Ethnicity6 <- Ethnicity %>% filter(ethSuperGroup == 'All other ethnic groups')
(Ethnicity_6 <- ggplot(Ethnicity6, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent Ethnicity by 
    Year (All other ethnic groups)"))

#ggsave(Ethnicity_6, filename = "QA/Outputs/Ethnicity6.pdf",
 #      width = 200, height = 120, units = "mm")
