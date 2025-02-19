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


# Data import -------------------------------------------------------------
# import sscq23 data
sscq23 <- haven::read_sas(sscq.path23,
                          col_select = c(SSCQid, 
                                         Vets, 
                                         LA,
                                         pooled_ind_wt))
nrow(sscq23)
# import cluster data
xref23 <- haven::read_sas(xref.path23,
                          col_select = c(SSCQid, cluster))

# import sscq22 data
sscq22 <- haven::read_sas(sscq.path22,
                          col_select = c(SSCQid, 
                                         Vets, 
                                         LA,
                                         pooled_ind_wt))
nrow(sscq22)
# import cluster data
xref22 <- haven::read_sas(xref.path22,
                          col_select = c(SSCQid, cluster))


# Build SSCQ data --------------------------------------------------------------
sscq23 <- sscq23 %>% 
  left_join(xref23, by = "SSCQid") %>%
  filter(pooled_ind_wt > 0) %>% 
  mutate(Vets = ifelse(Vets == -1, NA, Vets),
         Vets = factor(Vets, levels = c(-1, 0, 1),
                       labels = c(NA,
                                  "Not a UK Armed Forces veteran",
                                  "UK Armed Forces veteran")))

sscq22 <- sscq22 %>% 
  left_join(xref22, by = "SSCQid") %>%
  filter(pooled_ind_wt > 0) %>% 
  mutate(Vets = ifelse(Vets == -1, NA, Vets),
         Vets = factor(Vets, levels = c(-1, 0, 1),
                       labels = c(NA,
                                  "Not a UK Armed Forces veteran",
                                  "UK Armed Forces veteran")))


# Analyse data ------------------------------------------------------------
# specify survey design
survey_23 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_ind_wt,
                       data = sscq23)

sscq_23 <- svymean(~Vets, survey_23, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(Vets = levels(sscq23$Vets), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(Vets) != TRUE)
sscq_23


survey_22 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_ind_wt,
                       data = sscq22)

sscq_22 <- svymean(~Vets, survey_22, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(Vets = levels(sscq22$Vets), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(Vets) != TRUE)
sscq_22


# Combining the datasets ------------------------------------------------------------
Vets <- bind_rows(sscq_23, sscq_22)

Year <- c("2023", "2023",  
          "2022", "2022")

Vets$Year <- Year

Vets_table <- Vets %>% select(Year, Vets, perc_with_ci)
Vets_table

# Visualise estimates ------------------------------------------------------------
Vets_table <- Vets_table %>% 
  group_by(Year) %>% 
  pivot_wider(
    names_from = Vets,
    values_from = perc_with_ci)

print(Vets_table)

saveRDS(Vets_table, 
        "QA/Outputs/Vets_table.rds")

# by Veteran Status
# Not a UK Armed Forces veteran
Vets1 <- Vets %>% filter(Vets == 'Not a UK Armed Forces veteran')
(Vets_1 <- ggplot(Vets1, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent Veteran Status 
    by Year (Not a UK Armed 
    Forces veteran)"))

#ggsave(Vets_1, filename = "QA/Outputs/Vets1.pdf",
 #      width = 200, height = 120, units = "mm")

# UK Armed Forces veteran
Vets2 <- Vets %>% filter(Vets == 'UK Armed Forces veteran')
(Vets_2 <- ggplot(Vets2, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent Marital Status 
    by Year (UK Armed Forces veteran)"))

#ggsave(Vets_2, filename = "QA/Outputs/Vets2.pdf",
 #      width = 200, height = 120, units = "mm")
