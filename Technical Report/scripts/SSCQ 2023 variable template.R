# Load packages -----------------------------------------------------------

library(tidyverse)
library(survey)
library(readxl)
library(haven)
library(sgplot)
library(ggbreak)
library(svglite)

sgplot::use_sgplot()

# Setup -------------------------------------------------------------------

## will need to be changed once data is moved away from SAS database

# Path to SAS data
sasdata.path <- "//s0177a/sasdata1/ocs_pool/"

# Path to SSCQ 2023 data
sscq.path23 <- paste0(sasdata.path, "sscq2022SBW.sas7bdat")#change to this years document

# Path to file with cluster info 2023
xref.path23 <- paste0(sasdata.path, "xref2022sbw.sas7bdat")#change to this years document


# Data import -------------------------------------------------------------

# import sscq data
sscq23 <- haven::read_sas(sscq.path23,
                          col_select = c(SSCQid, 
                                         LA,
                                         pooled_hh_wt, ## may need to be changed depending on variable, indv v hh
                                         pooled_hh_wt_sc,## may need to be changed depending on variable, indv v hh
                                         IndCare))
nrow(sscq23)

# import cluster data
xref23 <- haven::read_sas(xref.path23,
                          col_select = c(SSCQid, cluster))


# Build SSCQ data --------------------------------------------------------------

analyse23_hh <- sscq23 %>% 
  
  # merge SSCQ and cluster data
  left_join(xref23, by = "SSCQid") %>%
  
  # remove observations whose household weight was 0
  filter(pooled_hh_wt > 0) %>%
  ## change if individual weight
  
  # recode variable
  mutate(IndCare = ifelse(IndCare == 99, 0, IndCare),
         
         IndCare = factor(IndCare, levels = c(1, 2),
                              labels = c("Yes",
                                         "No")))

# Analyse data ------------------------------------------------------------

# specify survey design
survey_hh <- svydesign(id = ~cluster, 
                       strata = ~LA,
                       weights = ~pooled_hh_wt, ## again change if individual
                       data = analyse23_hh)

# SSCQ by variable
sscq_estimates <- svymean(~IndCare, survey_hh, na.rm = TRUE)%>%
  as_tibble() %>% 
  mutate(IndCare = levels(analyse23_hh$IndCare), .before = mean) %>%
  mutate(lower_ci = (mean - 1.96 * SE) *100,
         upper_ci = (mean + 1.96 * SE) *100,
         mean = mean *100) %>%
  `names<-`(replace(names(.), 2, c('perc'))) %>%
  select(-SE) %>%
  filter(is.na(IndCare) != TRUE)
sscq_estimates


# Visualise estimates ------------------------------------------------------------

sscq_estimates$IndCare <- factor(sscq_estimates$IndCare, 
                                     levels = c("Yes",
                                                "No"))


## once we have decided what/if we will visualise => adapt as needed
(IndCare <- ggplot() +
    geom_point(data = sscq_estimates, aes(IndCare, perc, fill = sg_colour_values[1]), size = 2, shape = 15) +
    geom_errorbar(data = sscq_estimates, aes(x = IndCare, ymin = lower_ci, ymax = upper_ci), 
                  colour = sg_colour_values[1]) +
    geom_point(data = census_unpaid_care, aes(IndCare, perc, fill = sg_colour_values["orange"]), 
               colour = sg_colour_values["orange"], size = 2) + 
    theme_sg(legend = "bottom", grid = "xy", axis = "none") +
    scale_y_break(c(5, 10), scales = "fixed", space = .5) +
    scale_y_break(c(22, 75), scales = "fixed", space = .5) +
    scale_y_continuous(limits = c(0, 90),
                       breaks = seq(0, 97, by = 2),
                       labels = function(x) paste0(x, "%")) +
    theme(axis.text.y.right = element_blank()) +
    labs(
      x = NULL,
      y = NULL,
      title = "Provision of Unpaid Care",
      subtitle = "Percentage of Scottish Households Who Provide Unpaid Care" ,
      caption = "Source: Scottish Surveys Core Questions and Scotland's Census") +
    scale_x_discrete(labels = function(x) str_wrap(x, width = 15)) +
    scale_fill_manual(values = c("#002d54", "#e5682a"), 
                      labels = c("SSCQ", "Census")) +
    theme(legend.title = element_blank()))


ggsave(IndCare, filename = "output/IndCare.svg",
       width = 159, height = 100, units = "mm")

saveRDS(sscq_estimates, "output/IndCare_estimates.rds")


