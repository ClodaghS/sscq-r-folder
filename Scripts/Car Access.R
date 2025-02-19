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
                                         CarAccess, 
                                         LA,
                                         pooled_hh_wt))
nrow(sscq23)
# import cluster data
xref23 <- haven::read_sas(xref.path23,
                          col_select = c(SSCQid, cluster))

# import sscq22 data
sscq22 <- haven::read_sas(sscq.path22,
                          col_select = c(SSCQid, 
                                         CarAccess, 
                                         LA,
                                         pooled_hh_wt))
nrow(sscq22)
# import cluster data
xref22 <- haven::read_sas(xref.path22,
                          col_select = c(SSCQid, cluster))

# import sscq19 data
sscq19 <- haven::read_sas(sscq.path19,
                          col_select = c(SSCQid, 
                                         CarAccess, 
                                         LA,
                                         pooled_hh_wt))
nrow(sscq19)
# import cluster data
xref19 <- haven::read_sas(xref.path19,
                          col_select = c(SSCQid, cluster))

# import sscq18 data
sscq18 <- haven::read_sas(sscq.path18,
                          col_select = c(SSCQid, 
                                         CarAccess, 
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
  mutate(CarAccess = ifelse(CarAccess == 99, 0, CarAccess),
         CarAccess = factor(CarAccess, levels = c(0, 1, 2, 3),
                            labels = c("No car",
                                       "1 car",
                                       "2 cars",
                                       "3 or more cars")))

sscq22 <- sscq22 %>% 
  left_join(xref22, by = "SSCQid") %>%
  filter(pooled_hh_wt > 0) %>% 
  mutate(CarAccess = ifelse(CarAccess == 99, 0, CarAccess),
         CarAccess = factor(CarAccess, levels = c(0, 1, 2, 3),
                            labels = c("No car",
                                       "1 car",
                                       "2 cars",
                                       "3 or more cars")))

sscq19 <- sscq19 %>% 
  left_join(xref19, by = "SSCQid") %>%
  filter(pooled_hh_wt > 0) %>% 
  mutate(CarAccess = ifelse(CarAccess == 99, 0, CarAccess),
         CarAccess = factor(CarAccess, levels = c(0, 1, 2, 3),
                            labels = c("No car",
                                       "1 car",
                                       "2 cars",
                                       "3 or more cars")))

sscq18 <- sscq18 %>% 
  left_join(xref18, by = "SSCQid") %>%
  filter(pooled_hh_wt > 0) %>% 
  mutate(CarAccess = ifelse(CarAccess == 99, 0, CarAccess),
         CarAccess = factor(CarAccess, levels = c(0, 1, 2, 3),
                            labels = c("No car",
                                       "1 car",
                                       "2 cars",
                                       "3 or more cars")))

# Analyse data ------------------------------------------------------------
# specify survey design
survey_23 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_hh_wt,
                       data = sscq23)

sscq_23 <- svymean(~CarAccess, survey_23, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(CarAccess = levels(sscq23$CarAccess), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(CarAccess) != TRUE)
sscq_23


survey_22 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_hh_wt,
                       data = sscq22)

sscq_22 <- svymean(~CarAccess, survey_22, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(CarAccess = levels(sscq22$CarAccess), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(CarAccess) != TRUE)
sscq_22


survey_19 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_hh_wt,
                       data = sscq19)

sscq_19 <- svymean(~CarAccess, survey_19, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(CarAccess = levels(sscq19$CarAccess), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(CarAccess) != TRUE)
sscq_19


survey_18 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_hh_wt,
                       data = sscq18)

sscq_18 <- svymean(~CarAccess, survey_18, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(CarAccess = levels(sscq18$CarAccess), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         perc = mean *100,
         margin_of_error = upper_ci - perc,
         perc_with_ci = paste0(round(perc, 1), " ± ", round(margin_of_error, 1))) %>%
  select(-SE) %>%
  filter(is.na(CarAccess) != TRUE)
sscq_18

# Combining the datasets ------------------------------------------------------------
Car_Access <- bind_rows(sscq_23, sscq_22, sscq_19, sscq_18)

Year <- c("2023", "2023", "2023", "2023", 
          "2022", "2022", "2022", "2022", 
          "2019", "2019", "2019", "2019",  
          "2018", "2018", "2018", "2018")

Car_Access$Year <- Year

Car_Access_table <- Car_Access %>% select(Year, CarAccess, perc_with_ci)
Car_Access_table

# Visualise estimates ------------------------------------------------------------
Car_Access_table <- Car_Access_table %>% 
  group_by(Year) %>% 
  pivot_wider(
    names_from = CarAccess,
    values_from = perc_with_ci)

print(Car_Access_table)

saveRDS(Car_Access_table, 
        "QA/Outputs/Car_Access_table.rds")

# by Car Access
# No Car
CarAccess1 <- Car_Access %>% filter(CarAccess == 'No car')
(CarAccess_1 <- ggplot(CarAccess1, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent Car Access 
    by Year (No Car)"))

#ggsave(CarAccess_1, filename = "QA/Outputs/CarAccess1.pdf",
 #      width = 200, height = 120, units = "mm")

# 1 Car
CarAccess2 <- Car_Access %>% filter(CarAccess == '1 car')
(CarAccess_2 <- ggplot(CarAccess2, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent Car Access 
    by Year (1 Car)"))

#ggsave(CarAccess_2, filename = "QA/Outputs/CarAccess2.pdf",
 #      width = 200, height = 120, units = "mm")

# 2 Cars
CarAccess3 <- Car_Access %>% filter(CarAccess == '2 cars')
(CarAccess_3 <- ggplot(CarAccess3, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent Car Access 
    by Year (2 Cars)"))

#ggsave(CarAccess_3, filename = "QA/Outputs/CarAccess3.pdf",
 #      width = 200, height = 120, units = "mm")

# 3 Cars
CarAccess4 <- Car_Access %>% filter(CarAccess == '3 or more cars')
(CarAccess_4 <- ggplot(CarAccess4, aes(x = Year, y = perc)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "SSCQ Respondent Car Access by 
    Year (3 or More Cars)"))

#ggsave(CarAccess_4, filename = "QA/Outputs/CarAccess4.pdf",
 #      width = 200, height = 120, units = "mm")
