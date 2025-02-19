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

# Path to SSCQ 2017 data
sscq.path17 <- paste0(sasdata.path, "sscq2017BC.sas7bdat")
# Path to file with cluster info 2017
xref.path17 <- paste0(sasdata.path, "xref2017BC.sas7bdat")

# Path to SSCQ 2016 data
sscq.path16 <- paste0(sasdata.path, "sscq2016.sas7bdat")
# Path to file with cluster info 2016
xref.path16 <- paste0(sasdata.path, "xref2016.sas7bdat")

# Data import -------------------------------------------------------------
# import sscq23 data
sscq23 <- haven::read_sas(sscq.path23,
                          col_select = c(SSCQid, 
                                         PolConC, 
                                         LA,
                                         pooled_crim_wt))
nrow(sscq23)
# import cluster data
xref23 <- haven::read_sas(xref.path23,
                          col_select = c(SSCQid, cluster))

# import sscq22 data
sscq22 <- haven::read_sas(sscq.path22,
                          col_select = c(SSCQid, 
                                         PolConC, 
                                         LA,
                                         pooled_crim_wt))
nrow(sscq22)
# import cluster data
xref22 <- haven::read_sas(xref.path22,
                          col_select = c(SSCQid, cluster))

# import sscq17 data
sscq17 <- haven::read_sas(sscq.path17,
                          col_select = c(SSCQid, 
                                         PolConC, 
                                         LA,
                                         pooled_crim_wt))
nrow(sscq17)
# import cluster data
xref17 <- haven::read_sas(xref.path17,
                          col_select = c(SSCQid, cluster))


# import sscq16 data
sscq16 <- haven::read_sas(sscq.path16,
                          col_select = c(SSCQid, 
                                         PolConC, 
                                         LA,
                                         pooled_crim_wt))
nrow(sscq16)
# import cluster data
xref16 <- haven::read_sas(xref.path16,
                          col_select = c(SSCQid, cluster))

# Build SSCQ data --------------------------------------------------------------
sscq23 <- sscq23 %>% 
  left_join(xref23, by = "SSCQid") %>%
  filter(pooled_crim_wt > 0) %>% 
  mutate(PolConC = ifelse(PolConC == 99, 0, PolConC),
         PolConC = factor(PolConC, levels = c(-1, 1, 2, 3, 4),
                          labels = c(NA,
                                     "Very confident",
                                     "Fairly confident",
                                     "Not very confident",
                                     "Not at all confident")))


sscq22 <- sscq22 %>% 
  left_join(xref22, by = "SSCQid") %>%
  filter(pooled_crim_wt > 0) %>% 
  mutate(PolConC = ifelse(PolConC == 99, 0, PolConC),
         PolConC = factor(PolConC, levels = c(-1, 1, 2, 3, 4),
                          labels = c(NA,
                                     "Very confident",
                                     "Fairly confident",
                                     "Not very confident",
                                     "Not at all confident")))

sscq17 <- sscq17 %>% 
  left_join(xref17, by = "SSCQid") %>%
  filter(pooled_crim_wt > 0) %>% 
  mutate(PolConC = ifelse(PolConC == 99, 0, PolConC),
         PolConC = factor(PolConC, levels = c(-1, 1, 2, 3, 4),
                          labels = c(NA,
                                     "Very confident",
                                     "Fairly confident",
                                     "Not very confident",
                                     "Not at all confident")))

sscq16 <- sscq16 %>% 
  left_join(xref16, by = "SSCQid") %>%
  filter(pooled_crim_wt > 0) %>% 
  mutate(PolConC = ifelse(PolConC == 99, 0, PolConC),
         PolConC = factor(PolConC, levels = c(-1, 1, 2, 3, 4),
                          labels = c(NA,
                                     "Very confident",
                                     "Fairly confident",
                                     "Not very confident",
                                     "Not at all confident")))

# Analyse data ------------------------------------------------------------
# specify survey design
survey_23 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_crim_wt,
                       data = sscq23)

sscq_23 <- svymean(~PolConC, survey_23, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(PolConC = levels(sscq23$PolConC), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(PolConC) != TRUE)
sscq_23


survey_22 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_crim_wt,
                       data = sscq22)

sscq_22 <- svymean(~PolConC, survey_22, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(PolConC = levels(sscq22$PolConC), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(PolConC) != TRUE)
sscq_22


survey_17 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_crim_wt,
                       data = sscq17)

sscq_17 <- svymean(~PolConC, survey_17, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(PolConC = levels(sscq17$PolConC), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(PolConC) != TRUE)
sscq_17


survey_16 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_crim_wt,
                       data = sscq16)

sscq_16 <- svymean(~PolConC, survey_16, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(PolConC = levels(sscq16$PolConC), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(PolConC) != TRUE)
sscq_16

# Combining the datasets ------------------------------------------------------------
PolConC <- bind_rows(sscq_23, sscq_22, sscq_17, sscq_16)

Year <- c("2023", "2023", "2023", "2023",
          "2022", "2022", "2022", "2022",
          "2017", "2017", "2017", "2017",  
          "2016", "2016", "2016", "2016")

PolConC$Year <- Year

PolConC_table <- PolConC %>% select(Year, PolConC, perc_with_ci)
PolConC_table

# Visualise estimates ------------------------------------------------------------
PolConC_table <- PolConC_table %>% 
  group_by(Year) %>% 
  pivot_wider(
    names_from = PolConC,
    values_from = perc_with_ci)

print(PolConC_table)

saveRDS(PolConC_table, 
        "QA/Outputs/PolConC_table.rds")

# by Police Confidence C
# Very confident
PolConC1 <- PolConC %>% filter(PolConC == 'Very confident')
(PolCon_C1 <- ggplot(PolConC1, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "Police Confidence C
    by Year (Very confident)"))

#ggsave(PolCon_C1, filename = "QA/Outputs/PolConC1.pdf",
 #      width = 200, height = 120, units = "mm")

# Fairly confident
PolConC2 <- PolConC %>% filter(PolConC == 'Fairly confident')
(PolCon_C2 <- ggplot(PolConC2, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "Police Confidence C 
    by Year (Fairly confident)"))

#ggsave(PolCon_C2, filename = "QA/Outputs/PolConC2.pdf",
 #      width = 200, height = 120, units = "mm")

# Not very confident
PolConC3 <- PolConC %>% filter(PolConC == 'Not very confident')
(PolCon_C3 <- ggplot(PolConC3, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "Police Confidence C
    by Year (Not very confident)"))

#ggsave(PolCon_C3, filename = "QA/Outputs/PolConC3.pdf",
 #      width = 200, height = 120, units = "mm")

# Not at all confident
PolConC4 <- PolConC %>% filter(PolConC == 'Not at all confident')
(PolCon_C4 <- ggplot(PolConC4, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "Police Confidence C by
    Year (Not at all confident)"))

#ggsave(PolCon_C4, filename = "QA/Outputs/PolConC4.pdf",
 #      width = 200, height = 120, units = "mm")
