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
                                         PolConF, 
                                         LA,
                                         pooled_crim_wt))
nrow(sscq23)
# import cluster data
xref23 <- haven::read_sas(xref.path23,
                          col_select = c(SSCQid, cluster))

# import sscq22 data
sscq22 <- haven::read_sas(sscq.path22,
                          col_select = c(SSCQid, 
                                         PolConF, 
                                         LA,
                                         pooled_crim_wt))
nrow(sscq22)
# import cluster data
xref22 <- haven::read_sas(xref.path22,
                          col_select = c(SSCQid, cluster))

# import sscq17 data
sscq17 <- haven::read_sas(sscq.path17,
                          col_select = c(SSCQid, 
                                         PolConF, 
                                         LA,
                                         pooled_crim_wt))
nrow(sscq17)
# import cluster data
xref17 <- haven::read_sas(xref.path17,
                          col_select = c(SSCQid, cluster))


# import sscq16 data
sscq16 <- haven::read_sas(sscq.path16,
                          col_select = c(SSCQid, 
                                         PolConF, 
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
  mutate(PolConF = ifelse(PolConF == 99, 0, PolConF),
         PolConF = factor(PolConF, levels = c(-1, 1, 2, 3, 4),
                          labels = c(NA,
                                     "Very confident",
                                     "Fairly confident",
                                     "Not very confident",
                                     "Not at all confident")))


sscq22 <- sscq22 %>% 
  left_join(xref22, by = "SSCQid") %>%
  filter(pooled_crim_wt > 0) %>% 
  mutate(PolConF = ifelse(PolConF == 99, 0, PolConF),
         PolConF = factor(PolConF, levels = c(-1, 1, 2, 3, 4),
                          labels = c(NA,
                                     "Very confident",
                                     "Fairly confident",
                                     "Not very confident",
                                     "Not at all confident")))

sscq17 <- sscq17 %>% 
  left_join(xref17, by = "SSCQid") %>%
  filter(pooled_crim_wt > 0) %>% 
  mutate(PolConF = ifelse(PolConF == 99, 0, PolConF),
         PolConF = factor(PolConF, levels = c(-1, 1, 2, 3, 4),
                          labels = c(NA,
                                     "Very confident",
                                     "Fairly confident",
                                     "Not very confident",
                                     "Not at all confident")))

sscq16 <- sscq16 %>% 
  left_join(xref16, by = "SSCQid") %>%
  filter(pooled_crim_wt > 0) %>% 
  mutate(PolConF = ifelse(PolConF == 99, 0, PolConF),
         PolConF = factor(PolConF, levels = c(-1, 1, 2, 3, 4),
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

sscq_23 <- svymean(~PolConF, survey_23, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(PolConF = levels(sscq23$PolConF), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(PolConF) != TRUE)
sscq_23


survey_22 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_crim_wt,
                       data = sscq22)

sscq_22 <- svymean(~PolConF, survey_22, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(PolConF = levels(sscq22$PolConF), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(PolConF) != TRUE)
sscq_22


survey_17 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_crim_wt,
                       data = sscq17)

sscq_17 <- svymean(~PolConF, survey_17, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(PolConF = levels(sscq17$PolConF), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(PolConF) != TRUE)
sscq_17


survey_16 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_crim_wt,
                       data = sscq16)

sscq_16 <- svymean(~PolConF, survey_16, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(PolConF = levels(sscq16$PolConF), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(PolConF) != TRUE)
sscq_16

# Combining the datasets ------------------------------------------------------------
PolConF <- bind_rows(sscq_23, sscq_22, sscq_17, sscq_16)

Year <- c("2023", "2023", "2023", "2023",
          "2022", "2022", "2022", "2022",
          "2017", "2017", "2017", "2017",  
          "2016", "2016", "2016", "2016")

PolConF$Year <- Year

PolConF_table <- PolConF %>% select(Year, PolConF, perc_with_ci)
PolConF_table

# Visualise estimates ------------------------------------------------------------
PolConF_table <- PolConF_table %>% 
  group_by(Year) %>% 
  pivot_wider(
    names_from = PolConF,
    values_from = perc_with_ci)

print(PolConF_table)

saveRDS(PolConF_table, 
        "QA/Outputs/PolConF_table.rds")

# by Police Confidence F
# Very confident
PolConF1 <- PolConF %>% filter(PolConF == 'Very confident')
(PolCon_F1 <- ggplot(PolConF1, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "Police Confidence F 
    by Year (Very confident)"))

#ggsave(PolCon_F1, filename = "QA/Outputs/PolConF1.pdf",
 #      width = 200, height = 120, units = "mm")

# Fairly confident
PolConF2 <- PolConF %>% filter(PolConF == 'Fairly confident')
(PolCon_F2 <- ggplot(PolConF2, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "Police Confidence F
    by Year (Fairly confident)"))

#ggsave(PolCon_F2, filename = "QA/Outputs/PolConF2.pdf",
 #      width = 200, height = 120, units = "mm")

# Not very confident
PolConF3 <- PolConF %>% filter(PolConF == 'Not very confident')
(PolCon_F3 <- ggplot(PolConF3, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "Police Confidence F
    by Year (Not very confident)"))

#ggsave(PolCon_F3, filename = "QA/Outputs/PolConF3.pdf",
 #      width = 200, height = 120, units = "mm")

# Not at all confident
PolConF4 <- PolConF %>% filter(PolConF == 'Not at all confident')
(PolCon_F4 <- ggplot(PolConF4, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "Police Confidence F
    by Year (Not at all confident)"))

#ggsave(PolCon_F4, filename = "QA/Outputs/PolConF4.pdf",
 #      width = 200, height = 120, units = "mm")
