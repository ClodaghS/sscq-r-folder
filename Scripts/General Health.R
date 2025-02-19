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
                                         genhealth, 
                                         LA,
                                         pooled_ind_wt))
nrow(sscq23)
# import cluster data
xref23 <- haven::read_sas(xref.path23,
                          col_select = c(SSCQid, cluster))

# import sscq22 data
sscq22 <- haven::read_sas(sscq.path22,
                          col_select = c(SSCQid, 
                                         genhealth, 
                                         LA,
                                         pooled_ind_wt))
nrow(sscq22)
# import cluster data
xref22 <- haven::read_sas(xref.path22,
                          col_select = c(SSCQid, cluster))

# import sscq19 data
sscq19 <- haven::read_sas(sscq.path19,
                          col_select = c(SSCQid, 
                                         genhealth, 
                                         LA,
                                         pooled_ind_wt))
nrow(sscq19)
# import cluster data
xref19 <- haven::read_sas(xref.path19,
                          col_select = c(SSCQid, cluster))

# import sscq18 data
sscq18 <- haven::read_sas(sscq.path18,
                          col_select = c(SSCQid, 
                                         genhealth, 
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
  mutate(genhealth = factor(genhealth, levels = c(-1, 1, 2, 3, 4, 5),
                            labels = c(NA,
                                       "Very Good",
                                       "Good",
                                       "Fair",
                                       "Bad",
                                       "Very Bad")))

sscq22 <- sscq22 %>% 
  left_join(xref22, by = "SSCQid") %>%
  filter(pooled_ind_wt > 0) %>% 
  mutate(genhealth = factor(genhealth, levels = c(-1, 1, 2, 3, 4, 5),
                            labels = c(NA,
                                       "Very Good",
                                       "Good",
                                       "Fair",
                                       "Bad",
                                       "Very Bad")))

sscq19 <- sscq19 %>% 
  left_join(xref19, by = "SSCQid") %>%
  filter(pooled_ind_wt > 0) %>% 
  mutate(genhealth = factor(genhealth, levels = c(-1, 1, 2, 3, 4, 5),
                            labels = c(NA,
                                       "Very Good",
                                       "Good",
                                       "Fair",
                                       "Bad",
                                       "Very Bad")))

sscq18 <- sscq18 %>% 
  left_join(xref18, by = "SSCQid") %>%
  filter(pooled_ind_wt > 0) %>% 
  mutate(genhealth = factor(genhealth, levels = c(-1, 1, 2, 3, 4, 5),
                            labels = c(NA,
                                       "Very Good",
                                       "Good",
                                       "Fair",
                                       "Bad",
                                       "Very Bad")))

# Analyse data ------------------------------------------------------------
# specify survey design
survey_23 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_ind_wt,
                       data = sscq23)

sscq_23 <- svymean(~genhealth, survey_23, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(genhealth = levels(sscq23$genhealth), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(genhealth) != TRUE)
sscq_23


survey_22 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_ind_wt,
                       data = sscq22)

sscq_22 <- svymean(~genhealth, survey_22, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(genhealth = levels(sscq22$genhealth), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(genhealth) != TRUE)
sscq_22


survey_19 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_ind_wt,
                       data = sscq19)

sscq_19 <- svymean(~genhealth, survey_19, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(genhealth = levels(sscq19$genhealth), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(genhealth) != TRUE)
sscq_19


survey_18 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_ind_wt,
                       data = sscq18)

sscq_18 <- svymean(~genhealth, survey_18, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(genhealth = levels(sscq18$genhealth), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(genhealth) != TRUE)
sscq_18

# Combining the datasets ------------------------------------------------------------
GenHealth <- bind_rows(sscq_23, sscq_22, sscq_19, sscq_18)

Year <- c("2023", "2023", "2023", "2023", "2023",
          "2022", "2022", "2022", "2022", "2022",
          "2019", "2019", "2019", "2019", "2019",
          "2018", "2018", "2018", "2018", "2018")

GenHealth$Year <- Year

GenHealth_table <- GenHealth %>% select(Year, genhealth, perc_with_ci)
GenHealth_table

# Visualise estimates ------------------------------------------------------------
GenHealth_table <- GenHealth_table %>% 
  group_by(Year) %>% 
  pivot_wider(
    names_from = genhealth,
    values_from = perc_with_ci)

print(GenHealth_table)

saveRDS(GenHealth_table, 
        "QA/Outputs/GenHealth_table.rds")

# by GenHealth
# Very Good
GenHealth1 <- GenHealth %>% filter(genhealth == 'Very Good')
(GenHealth_1 <- ggplot(GenHealth1, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent General Health 
    by Year (Very Good)"))

#ggsave(GenHealth_1, filename = "QA/Outputs/GenHealth1.pdf",
 #      width = 200, height = 120, units = "mm")

# Good
GenHealth2 <- GenHealth %>% filter(genhealth == 'Good')
(GenHealth_2 <- ggplot(GenHealth2, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent General Health 
    by Year (Good)"))

#ggsave(GenHealth_2, filename = "QA/Outputs/GenHealth2.pdf",
 #      width = 200, height = 120, units = "mm")

# Fair
GenHealth3 <- GenHealth %>% filter(genhealth == 'Fair')
(GenHealth_3 <- ggplot(GenHealth3, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent General Health 
    by Year (Fair)"))

#ggsave(GenHealth_3, filename = "QA/Outputs/GenHealth3.pdf",
 #      width = 200, height = 120, units = "mm")

# Bad
GenHealth4 <- GenHealth %>% filter(genhealth == 'Bad')
(GenHealth_4 <- ggplot(GenHealth4, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent General Health 
    by Year (Bad)"))

#ggsave(GenHealth_4, filename = "QA/Outputs/GenHealth4.pdf",
 #      width = 200, height = 120, units = "mm")

# Very Bad
GenHealth5 <- GenHealth %>% filter(genhealth == 'Very Bad')
(GenHealth_5 <- ggplot(GenHealth5, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent General Health 
    by Year (Very Bad)"))

#ggsave(GenHealth_5, filename = "QA/Outputs/GenHealth5.pdf",
 #      width = 200, height = 120, units = "mm")
