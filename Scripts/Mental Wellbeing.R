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
                                         swemwbs, 
                                         LA,
                                         pooled_ind_wt))
nrow(sscq23)
# import cluster data
xref23 <- haven::read_sas(xref.path23,
                          col_select = c(SSCQid, cluster))



# import sscq22 data
sscq22 <- haven::read_sas(sscq.path22,
                          col_select = c(SSCQid, 
                                         swemwbs, 
                                         LA,
                                         pooled_ind_wt))
nrow(sscq22)
# import cluster data
xref22 <- haven::read_sas(xref.path22,
                          col_select = c(SSCQid, cluster))


# import sscq17 data
sscq17 <- haven::read_sas(sscq.path17,
                          col_select = c(SSCQid, 
                                         swemwbs, 
                                         LA,
                                         pooled_ind_wt))
nrow(sscq17)
# import cluster data
xref17 <- haven::read_sas(xref.path17,
                          col_select = c(SSCQid, cluster))


# import sscq16 data
sscq16 <- haven::read_sas(sscq.path16,
                          col_select = c(SSCQid, 
                                         swemwbs, 
                                         LA,
                                         pooled_ind_wt))
nrow(sscq16)
# import cluster data
xref16 <- haven::read_sas(xref.path16,
                          col_select = c(SSCQid, cluster))

# Build SSCQ data --------------------------------------------------------------

sscq23 <- sscq23 %>% 
  left_join(xref23, by = "SSCQid") %>%
  mutate(across(where(is.numeric), ~ ifelse(. == -1, NA, .))) %>%
  filter(pooled_ind_wt > 0)

sscq22 <- sscq22 %>% 
  left_join(xref22, by = "SSCQid") %>%
  mutate(across(where(is.numeric), ~ ifelse(. == -1, NA, .))) %>%
  filter(pooled_ind_wt > 0)

sscq17 <- sscq17 %>% 
  left_join(xref17, by = "SSCQid") %>%
  mutate(across(where(is.numeric), ~ ifelse(. == -1, NA, .))) %>%
  filter(pooled_ind_wt > 0)

sscq16 <- sscq16 %>% 
  left_join(xref16, by = "SSCQid") %>%
  mutate(across(where(is.numeric), ~ ifelse(. == -1, NA, .))) %>%
  filter(pooled_ind_wt > 0)

# Analyse data ------------------------------------------------------------
# specify survey design
survey_23 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_ind_wt,
                       data = sscq23)

sscq_23 <- svymean(~swemwbs, survey_23, na.rm = TRUE)
#manually add SE output where SE is highlighted
sscq_23 <- as.numeric(sscq_23)

sscq_23 <- as_tibble(sscq_23) %>% 
  mutate(lower_ci = (value - 1.96 * 0.0417), #SE
         upper_ci = (value + 1.96 * 0.0417), #SE
         margin_of_error = upper_ci - value,
         mean_with_ci = paste0(round(value, 1), " ± ", round(margin_of_error, 1))) 

sscq_23


survey_22 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_ind_wt,
                       data = sscq22)

sscq_22 <- svymean(~swemwbs, survey_22, na.rm = TRUE)
#manually add SE output where SE is highlighted
sscq_22 <- as.numeric(sscq_22)

sscq_22 <- as_tibble(sscq_22) %>% 
  mutate(lower_ci = (value - 1.96 * 0.043), #SE
         upper_ci = (value + 1.96 * 0.043), #SE
         margin_of_error = upper_ci - value,
         mean_with_ci = paste0(round(value, 1), " ± ", round(margin_of_error, 1))) 

sscq_22



survey_17 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_ind_wt,
                       data = sscq17)

sscq_17 <- svymean(~swemwbs, survey_17, na.rm = TRUE)
#manually add SE output where SE is highlighted
sscq_17 <- as.numeric(sscq_17)

sscq_17 <- as_tibble(sscq_17) %>% 
  mutate(lower_ci = (value - 1.96 * 0.0398), #SE
         upper_ci = (value + 1.96 * 0.0398), #SE
         margin_of_error = upper_ci - value,
         mean_with_ci = paste0(round(value, 1), " ± ", round(margin_of_error, 1))) 

sscq_17



survey_16 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_ind_wt,
                       data = sscq16)

sscq_16 <- svymean(~swemwbs, survey_16, na.rm = TRUE)
#manually add SE output where SE is highlighted
sscq_16 <- as.numeric(sscq_16)

sscq_16 <- as_tibble(sscq_16) %>% 
  mutate(lower_ci = (value - 1.96 * 0.0392), #SE
         upper_ci = (value + 1.96 * 0.0392), #SE
         margin_of_error = upper_ci - value,
         mean_with_ci = paste0(round(value, 1), " ± ", round(margin_of_error, 1))) 

sscq_16


# Combining the datasets ------------------------------------------------------------
swemwbs <- bind_rows(sscq_23, sscq_22, sscq_17, sscq_16)

Year <- c("2023",
          "2022",
          "2017",
          "2016")

swemwbs$Year <- Year

swemwbs_table <- swemwbs %>% select(Year, mean_with_ci)
swemwbs_table

swemwbs_plot <- swemwbs %>% select(Year, value, lower_ci, upper_ci)


# Visualise estimates ------------------------------------------------------------
swemwbs_table <- swemwbs_table %>%
  select(Year, mean_with_ci) %>%
  group_by(Year) %>% 
  rename("Average SSCQ Respondent Mental Wellbeing Score by Year" = mean_with_ci)

print(swemwbs_table)

saveRDS(swemwbs_table, 
        "QA/Outputs/swemwbs_table.rds")


(swemwbs_plot <- ggplot(swemwbs_plot, aes(x = Year, y = value)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "Average SSCQ Respondent Mental 
    Wellbeing Score by Year"))

#ggsave(swemwbs_plot, filename = "QA/Outputs/swemwbs_plot.pdf",
 #      width = 200, height = 120, units = "mm")
