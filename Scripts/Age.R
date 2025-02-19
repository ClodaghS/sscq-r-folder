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
                                         age, 
                                         LA,
                                         pooled_ind_wt))
nrow(sscq23)
# import cluster data
xref23 <- haven::read_sas(xref.path23,
                          col_select = c(SSCQid, cluster))



# import sscq22 data
sscq22 <- haven::read_sas(sscq.path22,
                          col_select = c(SSCQid, 
                                         age, 
                                         LA,
                                         pooled_ind_wt))
nrow(sscq22)
# import cluster data
xref22 <- haven::read_sas(xref.path22,
                          col_select = c(SSCQid, cluster))


# import sscq19 data
sscq19 <- haven::read_sas(sscq.path19,
                          col_select = c(SSCQid, 
                                         age, 
                                         LA,
                                         pooled_ind_wt))
nrow(sscq19)
# import cluster data
xref19 <- haven::read_sas(xref.path19,
                          col_select = c(SSCQid, cluster))


# import sscq18 data
sscq18 <- haven::read_sas(sscq.path18,
                          col_select = c(SSCQid, 
                                         age, 
                                         LA,
                                         pooled_ind_wt))
nrow(sscq18)
# import cluster data
xref18 <- haven::read_sas(xref.path18,
                          col_select = c(SSCQid, cluster))

# Build SSCQ data --------------------------------------------------------------

sscq23 <- sscq23 %>% 
  left_join(xref23, by = "SSCQid") %>%
  filter(pooled_ind_wt > 0)

sscq22 <- sscq22 %>% 
  left_join(xref22, by = "SSCQid") %>%
  filter(pooled_ind_wt > 0)

sscq19 <- sscq19 %>% 
  left_join(xref19, by = "SSCQid") %>%
  filter(pooled_ind_wt > 0)

sscq18 <- sscq18 %>% 
  left_join(xref18, by = "SSCQid") %>%
  filter(pooled_ind_wt > 0)

# Analyse data ------------------------------------------------------------
# specify survey design
survey_23 <- svydesign(id = ~cluster, 
                        strata = ~LA,
                        weights = ~pooled_ind_wt,
                        data = sscq23)

sscq_23 <- svymean(~age, survey_23, na.rm = TRUE)
#manually add SE output where SE is highlighted
sscq_23 <- as.numeric(sscq_23)

sscq_23 <- as_tibble(sscq_23) %>% 
  mutate(lower_ci = (value - 1.96 * 0.208), #SE
         upper_ci = (value + 1.96 * 0.208), #SE
         margin_of_error = upper_ci - value,
         mean_with_ci = paste0(round(value, 1), " ± ", round(margin_of_error, 1))) 

sscq_23


survey_22 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_ind_wt,
                       data = sscq22)

sscq_22 <- svymean(~age, survey_22, na.rm = TRUE)
#manually add SE output where SE is highlighted
sscq_22 <- as.numeric(sscq_22)

sscq_22 <- as_tibble(sscq_22) %>% 
  mutate(lower_ci = (value - 1.96 * 0.208), #SE
         upper_ci = (value + 1.96 * 0.208), #SE
         margin_of_error = upper_ci - value,
         mean_with_ci = paste0(round(value, 1), " ± ", round(margin_of_error, 1))) 

sscq_22



survey_19 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_ind_wt,
                       data = sscq19)

sscq_19 <- svymean(~age, survey_19, na.rm = TRUE)
#manually add SE output where SE is highlighted
sscq_19 <- as.numeric(sscq_19)

sscq_19 <- as_tibble(sscq_19) %>% 
  mutate(lower_ci = (value - 1.96 * 0.1779), #SE
         upper_ci = (value + 1.96 * 0.1779), #SE
         margin_of_error = upper_ci - value,
         mean_with_ci = paste0(round(value, 1), " ± ", round(margin_of_error, 1))) 

sscq_19



survey_18 <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_ind_wt,
                       data = sscq18)

sscq_18 <- svymean(~age, survey_18, na.rm = TRUE)
#manually add SE output where SE is highlighted
sscq_18 <- as.numeric(sscq_18)

sscq_18 <- as_tibble(sscq_18) %>% 
  mutate(lower_ci = (value - 1.96 * 0.1842), #SE
         upper_ci = (value + 1.96 * 0.1842), #SE
         margin_of_error = upper_ci - value,
         mean_with_ci = paste0(round(value, 1), " ± ", round(margin_of_error, 1))) 

sscq_18


# Combining the datasets ------------------------------------------------------------
Age <- bind_rows(sscq_23, sscq_22, sscq_19, sscq_18)

Year <- c("2023",
            "2022",
            "2019",
            "2018")

Age$Year <- Year

Age_table <- Age %>% select(Year, mean_with_ci)
Age_table

Age_plot <- Age %>% select(Year, value, lower_ci, upper_ci)


# Visualise estimates ------------------------------------------------------------
Age_table <- Age_table %>%
  select(Year, mean_with_ci) %>%
  group_by(Year) %>% 
  rename("Average SSCQ Respondent Age by Year" = mean_with_ci)

print(Age_table)

saveRDS(Age_table, 
        "QA/Outputs/Age_table.rds")

(Mean_Age <- ggplot(Age_plot, aes(x = Year, y = value)) +
  geom_boxplot(fill = sg_colour_values["dark-blue"])+
  geom_errorbar(aes(ymin = lower_ci, ymax = upper_ci), width = 0.5,
                colour = sg_colour_values[4]) +
  labs(
    x = NULL,
    y = NULL,
    title = "Average SSCQ Respondent 
    Age by Year"))

#ggsave(Mean_Age, filename = "QA/Outputs/Age.pdf",
 #      width = 200, height = 120, units = "mm")

