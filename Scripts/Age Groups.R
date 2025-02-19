# clear global environment
rm(list = ls())

# Load packages -----------------------------------------------------------
library(tidyverse, lib.loc="C:/Users/Public/Documents/R/win-library/4.2")
library(survey, lib.loc="C:/Users/Public/Documents/R/win-library/4.2")
library(readxl, lib.loc="C:/Users/Public/Documents/R/win-library/4.2")
library(haven, lib.loc="C:/Users/Public/Documents/R/win-library/4.2")
library(sgplot, lib.loc="C:/Users/Public/Documents/R/win-library/4.2")
library(ggbreak, lib.loc="C:/Users/Public/Documents/R/win-library/4.2")
library(svglite, lib.loc="C:/Users/Public/Documents/R/win-library/4.2")
library(cowplot, lib.loc="C:/Users/Public/Documents/R/win-library/4.2")
library(knitr, lib.loc="C:/Users/Public/Documents/R/win-library/4.2")
library(dplyr, lib.loc="C:/Users/Public/Documents/R/win-library/4.2")
library(ggplot2, lib.loc="C:/Users/Public/Documents/R/win-library/4.2")

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
                                         ageG, 
                                         LA,
                                         pooled_ind_wt))
nrow(sscq23)
# import cluster data
xref23 <- haven::read_sas(xref.path23,
                          col_select = c(SSCQid, cluster))

# import sscq22 data
sscq22 <- haven::read_sas(sscq.path22,
                          col_select = c(SSCQid, 
                                         ageG, 
                                         LA,
                                         pooled_ind_wt))
nrow(sscq22)
# import cluster data
xref22 <- haven::read_sas(xref.path22,
                          col_select = c(SSCQid, cluster))

# import sscq19 data
sscq19 <- haven::read_sas(sscq.path19,
                          col_select = c(SSCQid, 
                                         ageG, 
                                         LA,
                                         pooled_ind_wt))
nrow(sscq19)
# import cluster data
xref19 <- haven::read_sas(xref.path19,
                          col_select = c(SSCQid, cluster))

# import sscq18 data
sscq18 <- haven::read_sas(sscq.path18,
                          col_select = c(SSCQid, 
                                         ageG, 
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
  mutate(ageG = factor(ageG, levels = c(1, 2, 3, 4, 5, 6, 7),
                            labels = c("16-24",
                                       "25-34",
                                       "35-44",
                                       "45-54",
                                       "55-64",
                                       "65-74",
                                       "75+")))

sscq22 <- sscq22 %>% 
  left_join(xref22, by = "SSCQid") %>%
  filter(pooled_ind_wt > 0) %>% 
  mutate(ageG = factor(ageG, levels = c(1, 2, 3, 4, 5, 6, 7),
                       labels = c("16-24",
                                  "25-34",
                                  "35-44",
                                  "45-54",
                                  "55-64",
                                  "65-74",
                                  "75+")))

sscq19 <- sscq19 %>% 
  left_join(xref19, by = "SSCQid") %>%
  filter(pooled_ind_wt > 0) %>% 
  mutate(ageG = factor(ageG, levels = c(1, 2, 3, 4, 5, 6, 7),
                       labels = c("16-24",
                                  "25-34",
                                  "35-44",
                                  "45-54",
                                  "55-64",
                                  "65-74",
                                  "75+")))

sscq18 <- sscq18 %>% 
  left_join(xref18, by = "SSCQid") %>%
  filter(pooled_ind_wt > 0) %>% 
  mutate(ageG = factor(ageG, levels = c(1, 2, 3, 4, 5, 6, 7),
                       labels = c("16-24",
                                  "25-34",
                                  "35-44",
                                  "45-54",
                                  "55-64",
                                  "65-74",
                                  "75+")))


# Analyse data ------------------------------------------------------------
# specify survey design
survey_23 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_ind_wt,
                       data = sscq23)

sscq_23 <- svymean(~ageG, survey_23, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(ageG = levels(sscq23$ageG), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(ageG) != TRUE)
sscq_23


survey_22 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_ind_wt,
                       data = sscq22)

sscq_22 <- svymean(~ageG, survey_22, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(ageG = levels(sscq22$ageG), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(ageG) != TRUE)
sscq_22


survey_19 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_ind_wt,
                       data = sscq19)

sscq_19 <- svymean(~ageG, survey_19, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(ageG = levels(sscq19$ageG), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(ageG) != TRUE)
sscq_19


survey_18 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_ind_wt,
                       data = sscq18)

sscq_18 <- svymean(~ageG, survey_18, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(ageG = levels(sscq18$ageG), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(ageG) != TRUE)
sscq_18


# Combining the datasets ------------------------------------------------------------
AgeG <- bind_rows(sscq_23, sscq_22, sscq_19, sscq_18)

Year <- c("2023", "2023", "2023", "2023", "2023", "2023", "2023",
          "2022", "2022", "2022", "2022", "2022", "2022", "2022",
          "2019", "2019", "2019", "2019", "2019", "2019", "2019",
          "2018", "2018", "2018", "2018", "2018", "2018", "2018")

AgeG$Year <- Year

AgeG_table <- AgeG %>% select(Year, ageG, perc_with_ci)
AgeG_table

# Visualise estimates ------------------------------------------------------------
AgeG_table <- AgeG_table %>% 
  group_by(Year) %>% 
  pivot_wider(
    names_from = ageG,
    values_from = perc_with_ci)

print(AgeG_table)

saveRDS(AgeG_table, 
        "QA/Outputs/AgeG_table.rds")

#by each age group
#16-24
AgeG1 <- AgeG %>% filter(ageG == '16-24')
(Age_G1 <- ggplot(AgeG1, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent Age Group 
    by Year (16-24)"))

#ggsave(Age_G1, filename = "QA/Outputs/AgeG1.png",
 #      width = 200, height = 120, units = "mm")

#25-34
AgeG2 <- AgeG %>% filter(ageG == '25-34')
(Age_G2 <- ggplot(AgeG2, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent Age Group 
    by Year (25-34)"))

#ggsave(Age_G2, filename = "QA/Outputs/AgeG2.png",
#       width = 200, height = 120, units = "mm")

#35-44
AgeG3 <- AgeG %>% filter(ageG == '35-44')
(Age_G3 <- ggplot(AgeG3, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent Age Group 
    by Year (35-44)"))

#ggsave(Age_G3, filename = "QA/Outputs/AgeG3.png",
 #      width = 200, height = 120, units = "mm")

#45-54
AgeG4 <- AgeG %>% filter(ageG == '45-54')
(Age_G4 <- ggplot(AgeG4, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent Age Group 
    by Year (45-54)"))

#ggsave(Age_G4, filename = "QA/Outputs/AgeG4.png",
 #      width = 200, height = 120, units = "mm")

#55-64
AgeG5 <- AgeG %>% filter(ageG == '55-64')
(Age_G5 <- ggplot(AgeG1, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent Age Group 
    by Year (55-64)"))

#ggsave(Age_G5, filename = "QA/Outputs/AgeG5.png",
 #      width = 200, height = 120, units = "mm")

#65-74
AgeG6 <- AgeG %>% filter(ageG == '65-74')
(Age_G6 <- ggplot(AgeG6, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent Age Group 
    by Year (65-74)"))

#ggsave(Age_G6, filename = "QA/Outputs/AgeG6.png",
 #      width = 200, height = 120, units = "mm")

#75+
AgeG7 <- AgeG %>% filter(ageG == '75+')
(Age_G7 <- ggplot(AgeG7, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent Age Group 
    by Year (75+)"))

#ggsave(Age_G7, filename = "QA/Outputs/AgeG7.png",
 #      width = 200, height = 120, units = "mm")

