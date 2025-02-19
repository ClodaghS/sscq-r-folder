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
                                         tenure, 
                                         LA,
                                         pooled_hh_wt))
nrow(sscq23)
# import cluster data
xref23 <- haven::read_sas(xref.path23,
                          col_select = c(SSCQid, cluster))

# import sscq22 data
sscq22 <- haven::read_sas(sscq.path22,
                          col_select = c(SSCQid, 
                                         tenure, 
                                         LA,
                                         pooled_hh_wt))
nrow(sscq22)
# import cluster data
xref22 <- haven::read_sas(xref.path22,
                          col_select = c(SSCQid, cluster))

# import sscq19 data
sscq19 <- haven::read_sas(sscq.path19,
                          col_select = c(SSCQid, 
                                         tenure, 
                                         LA,
                                         pooled_hh_wt))
nrow(sscq19)
# import cluster data
xref19 <- haven::read_sas(xref.path19,
                          col_select = c(SSCQid, cluster))

# import sscq18 data
sscq18 <- haven::read_sas(sscq.path18,
                          col_select = c(SSCQid, 
                                         tenure, 
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
  mutate(tenure = factor(tenure, levels = c(-1, 1, 2, 3, 4, 5),
                          labels = c(NA,
                                     "Buying with the help of a mortgage or loan",
                                     "Own outright (i.e. no mortgage or loan)",
                                     "Pay part rent and part mortgage (shared ownership)",
                                     "Renting (including in receipt of housing benefit)",
                                     "Living here rent free")))

sscq22 <- sscq22 %>% 
  left_join(xref22, by = "SSCQid") %>%
  filter(pooled_hh_wt > 0) %>% 
  mutate(tenure = factor(tenure, levels = c(-1, 1, 2, 3, 4, 5),
                         labels = c(NA,
                                    "Buying with the help of a mortgage or loan",
                                    "Own outright (i.e. no mortgage or loan)",
                                    "Pay part rent and part mortgage (shared ownership)",
                                    "Renting (including in receipt of housing benefit)",
                                    "Living here rent free")))

sscq19 <- sscq19 %>% 
  left_join(xref19, by = "SSCQid") %>%
  filter(pooled_hh_wt > 0) %>% 
  mutate(tenure = factor(tenure, levels = c(-1, 1, 2, 3, 4, 5),
                         labels = c(NA,
                                    "Buying with the help of a mortgage or loan",
                                    "Own outright (i.e. no mortgage or loan)",
                                    "Pay part rent and part mortgage (shared ownership)",
                                    "Renting (including in receipt of housing benefit)",
                                    "Living here rent free")))

sscq18 <- sscq18 %>% 
  left_join(xref18, by = "SSCQid") %>%
  filter(pooled_hh_wt > 0) %>% 
  mutate(tenure = factor(tenure, levels = c(-1, 1, 2, 3, 4, 5),
                         labels = c(NA,
                                    "Buying with the help of a mortgage or loan",
                                    "Own outright (i.e. no mortgage or loan)",
                                    "Pay part rent and part mortgage (shared ownership)",
                                    "Renting (including in receipt of housing benefit)",
                                    "Living here rent free")))

# Analyse data ------------------------------------------------------------
# specify survey design
survey_23 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_hh_wt,
                       data = sscq23)

sscq_23 <- svymean(~tenure, survey_23, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(tenure = levels(sscq23$tenure), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(tenure) != TRUE)
sscq_23


survey_22 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_hh_wt,
                       data = sscq22)

sscq_22 <- svymean(~tenure, survey_22, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(tenure = levels(sscq22$tenure), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(tenure) != TRUE)
sscq_22


survey_19 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_hh_wt,
                       data = sscq19)

sscq_19 <- svymean(~tenure, survey_19, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(tenure = levels(sscq19$tenure), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(tenure) != TRUE)
sscq_19


survey_18 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_hh_wt,
                       data = sscq18)

sscq_18 <- svymean(~tenure, survey_18, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(tenure = levels(sscq18$tenure), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(tenure) != TRUE)
sscq_18

# Combining the datasets ------------------------------------------------------------
Tenure <- bind_rows(sscq_23, sscq_22, sscq_19, sscq_18)

Year <- c("2023", "2023", "2023", "2023", "2023",
          "2022", "2022", "2022", "2022", "2022",
          "2019", "2019", "2019", "2019", "2019", 
          "2018", "2018", "2018", "2018", "2018")

Tenure$Year <- Year

Tenure_table <- Tenure %>% select(Year, tenure, perc_with_ci)
Tenure_table

# Visualise estimates ------------------------------------------------------------
Tenure_table <- Tenure_table %>% 
  group_by(Year) %>% 
  pivot_wider(
    names_from = tenure,
    values_from = perc_with_ci)

print(Tenure_table)

saveRDS(Tenure_table, 
        "QA/Outputs/Tenure_table.rds")

# by HH Type
# Mortgaged
Tenure1 <- Tenure %>% filter(tenure == 'Buying with the help of a mortgage or loan')
(Tenure_1 <- ggplot(Tenure1, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent Tenure 
    by Year (Mortgaged)"))

#ggsave(Tenure_1, filename = "QA/Outputs/Tenure1.pdf",
 #      width = 200, height = 120, units = "mm")

# Owned outright
Tenure2 <- Tenure %>% filter(tenure == 'Own outright (i.e. no mortgage or loan)')
(Tenure_2 <- ggplot(Tenure2, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent Tenure 
    by Year (Owned Outright)"))

#ggsave(Tenure_2, filename = "QA/Outputs/Tenure2.pdf",
 #      width = 200, height = 120, units = "mm")

# Shared Ownership
Tenure3 <- Tenure %>% filter(tenure == 'Pay part rent and part mortgage (shared ownership)')
(Tenure_3 <- ggplot(Tenure3, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent Tenure 
    by Year (Shared Ownership)"))

#ggsave(Tenure_3, filename = "QA/Outputs/Tenure3.pdf",
 #      width = 200, height = 120, units = "mm")

# Renting
Tenure4 <- Tenure %>% filter(tenure == 'Renting (including in receipt of housing benefit)')
(Tenure_4 <- ggplot(Tenure4, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent Tenure 
    by Year (Renting)"))

#ggsave(Tenure_4, filename = "QA/Outputs/Tenure4.pdf",
 #      width = 200, height = 120, units = "mm")

# Rent Free
Tenure5 <- Tenure %>% filter(tenure == 'Living here rent free')
(Tenure_5 <- ggplot(Tenure5, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent Tenure 
    by Year (Rent Free)"))

#ggsave(Tenure_5, filename = "QA/Outputs/Tenure5.pdf",
 #      width = 200, height = 120, units = "mm")
