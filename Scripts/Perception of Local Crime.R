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
                                         CrimeArea, 
                                         LA,
                                         pooled_crim_wt))
nrow(sscq23)
# import cluster data
xref23 <- haven::read_sas(xref.path23,
                          col_select = c(SSCQid, cluster))

# import sscq22 data
sscq22 <- haven::read_sas(sscq.path22,
                          col_select = c(SSCQid, 
                                         CrimeArea, 
                                         LA,
                                         pooled_crim_wt))
nrow(sscq22)
# import cluster data
xref22 <- haven::read_sas(xref.path22,
                          col_select = c(SSCQid, cluster))

# import sscq19 data
sscq19 <- haven::read_sas(sscq.path19,
                          col_select = c(SSCQid, 
                                         CrimeArea, 
                                         LA,
                                         pooled_crim_wt))
nrow(sscq19)
# import cluster data
xref19 <- haven::read_sas(xref.path19,
                          col_select = c(SSCQid, cluster))

# import sscq18 data
sscq18 <- haven::read_sas(sscq.path18,
                          col_select = c(SSCQid, 
                                         CrimeArea, 
                                         LA,
                                         pooled_crim_wt))
nrow(sscq18)
# import cluster data
xref18 <- haven::read_sas(xref.path18,
                          col_select = c(SSCQid, cluster))

# Build SSCQ data --------------------------------------------------------------
sscq23 <- sscq23 %>% 
  left_join(xref23, by = "SSCQid") %>%
  filter(pooled_crim_wt > 0) %>% 
  mutate(CrimeArea = factor(CrimeArea, levels = c(-4, -1, 1, 2, 3, 4, 5),
                            labels = c(NA,
                                       NA,
                                       "A lot more",
                                       "A little more",
                                       "About the same",
                                       "A little less",
                                       "A lot less")))

sscq22 <- sscq22 %>% 
  left_join(xref22, by = "SSCQid") %>%
  filter(pooled_crim_wt > 0) %>% 
  mutate(CrimeArea = factor(CrimeArea, levels = c(-4, -1, 1, 2, 3, 4, 5),
                            labels = c(NA,
                                       NA,
                                       "A lot more",
                                       "A little more",
                                       "About the same",
                                       "A little less",
                                       "A lot less")))

sscq19 <- sscq19 %>% 
  left_join(xref19, by = "SSCQid") %>%
  filter(pooled_crim_wt > 0) %>% 
  mutate(CrimeArea = factor(CrimeArea, levels = c(-4, -1, 1, 2, 3, 4, 5),
                            labels = c(NA,
                                       NA,
                                       "A lot more",
                                       "A little more",
                                       "About the same",
                                       "A little less",
                                       "A lot less")))

sscq18 <- sscq18 %>% 
  left_join(xref18, by = "SSCQid") %>%
  filter(pooled_crim_wt > 0) %>% 
  mutate(CrimeArea = factor(CrimeArea, levels = c(-4, -1, 1, 2, 3, 4, 5),
                            labels = c(NA,
                                       NA,
                                       "A lot more",
                                       "A little more",
                                       "About the same",
                                       "A little less",
                                       "A lot less")))

# Analyse data ------------------------------------------------------------
# specify survey design
survey_23 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_crim_wt,
                       data = sscq23)

sscq_23 <- svymean(~CrimeArea, survey_23, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(CrimeArea = levels(sscq23$CrimeArea), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(CrimeArea) != TRUE)
sscq_23


survey_22 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_crim_wt,
                       data = sscq22)

sscq_22 <- svymean(~CrimeArea, survey_22, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(CrimeArea = levels(sscq22$CrimeArea), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(CrimeArea) != TRUE)
sscq_22


survey_19 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_crim_wt,
                       data = sscq19)

sscq_19 <- svymean(~CrimeArea, survey_19, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(CrimeArea = levels(sscq19$CrimeArea), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(CrimeArea) != TRUE)
sscq_19


survey_18 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_crim_wt,
                       data = sscq18)

sscq_18 <- svymean(~CrimeArea, survey_18, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(CrimeArea = levels(sscq18$CrimeArea), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(CrimeArea) != TRUE)
sscq_18

# Combining the datasets ------------------------------------------------------------
CrimeArea <- bind_rows(sscq_23, sscq_22, sscq_19, sscq_18)

Year <- c("2023", "2023", "2023", "2023", "2023",
          "2022", "2022", "2022", "2022", "2022",
          "2019", "2019", "2019", "2019", "2019", 
          "2018", "2018", "2018", "2018", "2018")

CrimeArea$Year <- Year

CrimeArea_table <- CrimeArea %>% select(Year, CrimeArea, perc_with_ci)
CrimeArea_table

# Visualise estimates ------------------------------------------------------------
CrimeArea_table <- CrimeArea_table %>% 
  group_by(Year) %>% 
  pivot_wider(
    names_from = CrimeArea,
    values_from = perc_with_ci)

print(CrimeArea_table)

saveRDS(CrimeArea_table, 
        "QA/Outputs/CrimeArea_table.rds")

# by Crime Area
# A lot more
CrimeArea1 <- CrimeArea %>% filter(CrimeArea == 'A lot more')
(CrimeArea_1 <- ggplot(CrimeArea1, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "Perception of Local Crime Rate
    by Year (A lot more)"))

#ggsave(CrimeArea_1, filename = "QA/Outputs/CrimeArea1.pdf",
 #      width = 200, height = 120, units = "mm")

# A little more
CrimeArea2 <- CrimeArea %>% filter(CrimeArea == 'A little more')
(CrimeArea_2 <- ggplot(CrimeArea2, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "Perception of Local Crime Rate 
    by Year (A little more)"))

#ggsave(CrimeArea_2, filename = "QA/Outputs/CrimeArea2.pdf",
 #      width = 200, height = 120, units = "mm")

# About the same
CrimeArea3 <- CrimeArea %>% filter(CrimeArea == 'About the same')
(CrimeArea_3 <- ggplot(CrimeArea3, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "Perception of Local Crime Rate 
    by Year (About the same)"))

#ggsave(CrimeArea_3, filename = "QA/Outputs/CrimeArea3.pdf",
 #      width = 200, height = 120, units = "mm")

# A little less
CrimeArea4 <- CrimeArea %>% filter(CrimeArea == 'A little less')
(CrimeArea_4 <- ggplot(CrimeArea4, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "Perception of Local Crime Rate
    by Year (A little less)"))

#ggsave(CrimeArea_4, filename = "QA/Outputs/CrimeArea4.pdf",
 #      width = 200, height = 120, units = "mm")

# A lot less
CrimeArea5 <- CrimeArea %>% filter(CrimeArea == 'A lot less')
(CrimeArea_5 <- ggplot(CrimeArea5, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "Perception of Local Crime Rate
    by Year (A lot less)"))

#ggsave(CrimeArea_5, filename = "QA/Outputs/CrimeArea5.pdf",
 #      width = 200, height = 120, units = "mm")
