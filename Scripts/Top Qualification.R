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
                                         topQual, 
                                         LA,
                                         pooled_ind_wt))
nrow(sscq23)
# import cluster data
xref23 <- haven::read_sas(xref.path23,
                          col_select = c(SSCQid, cluster))

# import sscq22 data
sscq22 <- haven::read_sas(sscq.path22,
                          col_select = c(SSCQid, 
                                         topQual, 
                                         LA,
                                         pooled_ind_wt))
nrow(sscq22)
# import cluster data
xref22 <- haven::read_sas(xref.path22,
                          col_select = c(SSCQid, cluster))

# import sscq19 data
sscq19 <- haven::read_sas(sscq.path19,
                          col_select = c(SSCQid, 
                                         topQual, 
                                         LA,
                                         pooled_ind_wt))
nrow(sscq19)
# import cluster data
xref19 <- haven::read_sas(xref.path19,
                          col_select = c(SSCQid, cluster))

# import sscq18 data
sscq18 <- haven::read_sas(sscq.path18,
                          col_select = c(SSCQid, 
                                         topQual, 
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
  mutate(topQual = factor(topQual, levels = c(-1, 1, 2, 3, 4, 5, 6, 7, 999),
                          labels = c(NA, 
                                     "Level 1 - O Grade, Standard grade or equiv (SVQ level 1 or 2)",
                                     "Level 2 - Higher, A level or equivalent (SVQ Level 3)",
                                     "Level 3 - HNC/HND or equivalent (SVQ Level 4)",
                                     "Level 4 - Degree, Professional qualification (Above SVQ Level 4)",
                                     "Other qualification",
                                     "No qualifications",
                                     "Qualifications not known",
                                     NA)))

sscq22 <- sscq22 %>% 
  left_join(xref22, by = "SSCQid") %>%
  filter(pooled_ind_wt > 0) %>% 
  mutate(topQual = factor(topQual, levels = c(-1, 1, 2, 3, 4, 5, 6, 7, 999),
                          labels = c(NA, 
                                     "Level 1 - O Grade, Standard grade or equiv (SVQ level 1 or 2)",
                                     "Level 2 - Higher, A level or equivalent (SVQ Level 3)",
                                     "Level 3 - HNC/HND or equivalent (SVQ Level 4)",
                                     "Level 4 - Degree, Professional qualification (Above SVQ Level 4)",
                                     "Other qualification",
                                     "No qualifications",
                                     "Qualifications not known",
                                     NA)))

sscq19 <- sscq19 %>% 
  left_join(xref19, by = "SSCQid") %>%
  filter(pooled_ind_wt > 0) %>% 
  mutate(topQual = factor(topQual, levels = c(-1, 1, 2, 3, 4, 5, 6, 7, 999),
                          labels = c(NA, 
                                     "Level 1 - O Grade, Standard grade or equiv (SVQ level 1 or 2)",
                                     "Level 2 - Higher, A level or equivalent (SVQ Level 3)",
                                     "Level 3 - HNC/HND or equivalent (SVQ Level 4)",
                                     "Level 4 - Degree, Professional qualification (Above SVQ Level 4)",
                                     "Other qualification",
                                     "No qualifications",
                                     "Qualifications not known",
                                     NA)))

sscq18 <- sscq18 %>% 
  left_join(xref18, by = "SSCQid") %>%
  filter(pooled_ind_wt > 0) %>% 
  mutate(topQual = factor(topQual, levels = c(-1, 1, 2, 3, 4, 5, 6, 7, 999),
                          labels = c(NA, 
                                     "Level 1 - O Grade, Standard grade or equiv (SVQ level 1 or 2)",
                                     "Level 2 - Higher, A level or equivalent (SVQ Level 3)",
                                     "Level 3 - HNC/HND or equivalent (SVQ Level 4)",
                                     "Level 4 - Degree, Professional qualification (Above SVQ Level 4)",
                                     "Other qualification",
                                     "No qualifications",
                                     "Qualifications not known",
                                     NA)))

# Analyse data ------------------------------------------------------------
# specify survey design
survey_23 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_ind_wt,
                       data = sscq23)

sscq_23 <- svymean(~topQual, survey_23, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(topQual = levels(sscq23$topQual), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(topQual) != TRUE)
sscq_23


survey_22 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_ind_wt,
                       data = sscq22)

sscq_22 <- svymean(~topQual, survey_22, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(topQual = levels(sscq22$topQual), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(topQual) != TRUE)
sscq_22


survey_19 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_ind_wt,
                       data = sscq19)

sscq_19 <- svymean(~topQual, survey_19, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(topQual = levels(sscq19$topQual), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(topQual) != TRUE)
sscq_19


survey_18 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_ind_wt,
                       data = sscq18)

sscq_18 <- svymean(~topQual, survey_18, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(topQual = levels(sscq18$topQual), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(topQual) != TRUE)
sscq_18

# Combining the datasets ------------------------------------------------------------
topQual <- bind_rows(sscq_23, sscq_22, sscq_19, sscq_18)

Year <- c("2023", "2023", "2023", "2023", "2023", "2023", "2023",
          "2022", "2022", "2022", "2022", "2022", "2022", "2022",
          "2019", "2019", "2019", "2019", "2019", "2019", "2019",
          "2018", "2018", "2018", "2018", "2018", "2018", "2018")

topQual$Year <- Year

topQual_table <- topQual %>% select(Year, topQual, perc_with_ci)
topQual_table

# Visualise estimates ------------------------------------------------------------
topQual_table <- topQual_table %>% 
  group_by(Year) %>% 
  pivot_wider(
    names_from = topQual,
    values_from = perc_with_ci)

print(topQual_table)

saveRDS(topQual_table, 
        "QA/Outputs/topQual_table.rds")

# by topQual
# Level 1 - O Grade, Standard grade or equiv (SVQ level 1 or 2)
topQual1 <- topQual %>% filter(topQual == 'Level 1 - O Grade, Standard grade or equiv (SVQ level 1 or 2)')
(topQual_1 <- ggplot(topQual1, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent Top 
    Qualification by Year
    (Level 1)"))

#ggsave(topQual_1, filename = "QA/Outputs/topQual1.pdf",
 #      width = 200, height = 120, units = "mm")

# Level 2 - Higher, A level or equivalent (SVQ Level 3)
topQual2 <- topQual %>% filter(topQual == 'Level 2 - Higher, A level or equivalent (SVQ Level 3)')
(topQual_2 <- ggplot(topQual2, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent Top 
    Qualification by Year 
    (Level 2)"))

#ggsave(topQual_2, filename = "QA/Outputs/topQual2.pdf",
 #      width = 200, height = 120, units = "mm")

# Level 3 - HNC/HND or equivalent (SVQ Level 4)
topQual3 <- topQual %>% filter(topQual == 'Level 3 - HNC/HND or equivalent (SVQ Level 4)')
(topQual_3 <- ggplot(topQual3, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent Top
    Qualification by Year 
    (Level 3)"))

#ggsave(topQual_3, filename = "QA/Outputs/topQual3.pdf",
 #      width = 200, height = 120, units = "mm")

#Level 4 - Degree, Professional qualification (Above SVQ Level 4)
topQual4 <- topQual %>% filter(topQual == 'Level 4 - Degree, Professional qualification (Above SVQ Level 4)')
(topQual_4 <- ggplot(topQual4, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent Top
    Qualification by Year 
    (Level 4)"))

#ggsave(topQual_4, filename = "QA/Outputs/topQual4.pdf",
 #      width = 200, height = 120, units = "mm")

# Other qualification
topQual5 <- topQual %>% filter(topQual == 'Other qualification')
(topQual_5 <- ggplot(topQual5, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent Top 
    Qualification by Year 
    (Other Qualifications)"))

#ggsave(topQual_5, filename = "QA/Outputs/topQual5.pdf",
 #      width = 200, height = 120, units = "mm")

# No Qualifications
topQual6 <- topQual %>% filter(topQual == 'No qualifications')
(topQual_6 <- ggplot(topQual6, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent Top 
    Qualification by Year
    (No Qualifications)"))

#ggsave(topQual_6, filename = "QA/Outputs/topQual6.pdf",
 #      width = 200, height = 120, units = "mm")

# Qualifications not known
topQual7 <- topQual %>% filter(topQual == 'Qualifications not known')
(topQual_7 <- ggplot(topQual7, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent Top 
    Qualification by Year 
    (Qualifications Not Known)"))

#ggsave(topQual_7, filename = "QA/Outputs/topQual7.pdf",
 #      width = 200, height = 120, units = "mm")
