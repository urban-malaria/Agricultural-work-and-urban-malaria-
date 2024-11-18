# Author: Grace Legris (gracebea@gmail.com)
# Created: [2024-10-03]
# Purpose: Generate Difference-in-Differences Analysis and Scatterplot and Line Plots
# ==========================================================================================================================================

# clear current workspace
rm(list = ls())

## =========================================================================================================================================
### Directory Management and File Paths
## =========================================================================================================================================

user <- Sys.getenv("USER")
if ("ozodi" %in% user) {
  Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", gsub("OneDrive", "", Sys.getenv("HOME")))))
  Drive <- file.path(gsub("[//]", "/", Drive))
  DriveDir <- file.path(Drive, "Urban Malaria Proj Dropbox", "urban_malaria")
  PopDir <- file.path(DriveDir, "data", "data_agric_analysis")
  ManDir <- file.path(DriveDir, "projects", "Manuscripts", "ongoing", "agriculture_malaria_manuscript")
  FigDir <- file.path(ManDir, "figures", "main_figures")
  SupDir <- file.path(ManDir, "figures", "supplementary", "pdf_figures")
  ExpDir <- file.path(ManDir, "figures", "exploratory")
} else if  ("CHZCHI003" %in% user) {
  Drive <- file.path("C:/Users/CHZCHI003/Dropbox")
  DriveDir <- file.path("C:/Users/CHZCHI003/OneDrive/urban_malaria")
  PopDir <- file.path(Drive)
  ManDir <- file.path(Drive, "agriculture_malaria_manuscript")
  FigDir <- file.path(ManDir, "figures", "main_figures")
  SupDir <- file.path(ManDir, "figures", "supplementary", "pdf_figures")
  ExpDir <- file.path(ManDir, "figures", "exploratory")
} else if  ("cchiz" %in% user) {
  Drive <- file.path("C:/Users/cchiz/Dropbox")
  DriveDir <- file.path("C:/Users/cchiz/OneDrive/urban_malaria")
  PopDir <- file.path(Drive)
  ManDir <- file.path(Drive, "agriculture_malaria_manuscript")
  FigDir <- file.path(ManDir, "figures", "main_figures")
  SupDir <- file.path(ManDir, "figures", "supplementary", "pdf_figures")
  ExpDir <- file.path(ManDir, "figures", "exploratory")
} else if ("grace" %in% user) {
  Drive <- "/Users/grace/Urban Malaria Proj Dropbox"
  DriveDir <- file.path(Drive, "urban_malaria")
  PopDir <- file.path(DriveDir, "data", 'data_agric_analysis')
  ManDir <- file.path(DriveDir, "projects", "Manuscripts", "ongoing", "agriculture_malaria_manuscript")
  FigDir <- file.path(ManDir, "figures", "main_figures")
  SupDir <- file.path(ManDir, "figures", "supplementary", "pdf_figures")
  ExpDir <- file.path(ManDir, "figures", "exploratory")
} else {
  Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
  DriveDir <- file.path(Drive, 'Library', 'CloudStorage', 'OneDrive-NorthwesternUniversity', "urban_malaria")
  #DriveDir <- file.path(Drive,  "OneDrive - Northwestern University", "urban_malaria")
  PopDir <- file.path(DriveDir, "data", 'data_agric_analysis')
  ManDir <- file.path(DriveDir, "projects", "Manuscripts", "agriculture_malaria_manuscript")
  FigDir <- file.path(ManDir, "figures", "main_figures")
  SupDir <- file.path(ManDir, "figures", "supplementary", "pdf_figures")
  ExpDir <- file.path(ManDir, "figures", "exploratory")
}

# note before sourcing functions and packages, run the code below to download rdhs if you don't already have it
# devtools::install_github("ropensci/rdhs")
source("functions/functions_employment.R")
options(survey.lonely.psu="adjust")  # this option allows admin units with only one cluster to be analyzed

## =========================================================================================================================================
### MALARIA DATA PREP
## =========================================================================================================================================
source("functions/functions_employment.R")

## -----------------------------------------------------------------------------------------------------------------------------------------
### Malaria Data Prep: Original data that does NOT include confidence intervals (used for DiD)
## -----------------------------------------------------------------------------------------------------------------------------------------

# read in the trend analysis dataset for malaria test positivity (this has both urban and rural data)
malaria_trend_data <- read_csv(file.path(PopDir, "analysis_dat/240606_urban_df_for_analysis_trend_malaria_chilo_created.csv"))

#******** URBAN *********
# urban analysis for malaria: reshape and clean data
# exclude Tanzania as test positivity rate is less than 1%
# exclude Mozambique 2011 as MZ 2015 and MZ 2022-23 are the two most recent surveys
urban_trend_malaria <- malaria_trend_data  %>%  
  filter(title == "Urban", cntryId...9 != "TZ", country_year.x...2 != "Mozambique 2011") %>% # filter relevant data for urban areas
  select(country_year.x...2, cntryId...5, agric_percent, non_agric_percent, diff_val_malaria, cntryId...12, id) %>% # select specific columns
  rename(country_year = country_year.x...2, country_id = cntryId...5, country_surveyyears = cntryId...12) %>% # rename columns for clarity
  mutate(id = if_else(country_year == "Mozambique 2015", "Preceding survey", id)) # update id for specific condition

# recode "Mozambique 2011 vs 2015" to "Mozambique 2015 Vs. 2022 - 2023"
urban_trend_malaria$country_surveyyears[urban_trend_malaria$country_surveyyears == "Mozambique 2011 Vs. 2015"] <- "Mozambique 2015 Vs. 2022 - 23"

# hardcode test positivity values for Mozambique 2022 based on above analysis
df <- data.frame(country_year = "Mozambique 2022 - 23", agric_percent = 27, non_agric_percent = 9, diff_val_malaria = 27 - 9, id = "Most recent survey", country_id  = "MZ", country_surveyyears = "Mozambique 2015 Vs. 2022 - 23")

# join the trend data with hardcoded values
urban_trend_malaria_updated <- rbind(urban_trend_malaria, df)

#******** RURAL *********
# rural analysis for malaria: reshape and clean data
# exclude Tanzania as test positivity rate is less than 1%
# exclude Mozambique 2011 as MZ 2015 and MZ 2022-23 are the two most recent surveys
rural_trend_malaria <- malaria_trend_data  %>%  
  filter(title == "Rural", cntryId...9 != "TZ", country_year.x...2 != "Mozambique 2011") %>% # filter relevant data for urban areas
  select(country_year.x...2, cntryId...5, agric_percent, non_agric_percent, diff_val_malaria, cntryId...12, id) %>% # select specific columns
  rename(country_year = country_year.x...2, country_id = cntryId...5, country_surveyyears = cntryId...12) %>% # rename columns for clarity
  mutate(id = if_else(country_year == "Mozambique 2015", "Preceding survey", id)) # update id for specific condition

# recode "Mozambique 2011 vs 2015" to "Mozambique 2015 Vs. 2022 - 2023"
rural_trend_malaria$country_surveyyears[rural_trend_malaria$country_surveyyears == "Mozambique 2011 Vs. 2015"] <- "Mozambique 2015 Vs. 2022 - 23"

# hardcode test positivity values for Mozambique 2022 based on above analysis
df <- data.frame(country_year = "Mozambique 2022 - 23", agric_percent = 27, non_agric_percent = 9, diff_val_malaria = 27 - 9, id = "Most recent survey", country_id  = "MZ", country_surveyyears = "Mozambique 2015 Vs. 2022 - 23")

# join the trend data with hardcoded values
rural_trend_malaria_updated <- rbind(rural_trend_malaria, df)

## -----------------------------------------------------------------------------------------------------------------------------------------
### Malaria Data Prep: New data that DOES include confidence intervals (also includes MZ2022 data so no need to hard code) (Grace)
## -----------------------------------------------------------------------------------------------------------------------------------------

# read in the trend analysis dataset for malaria test positivity - INCLUDES CONFIDENCE INTERVALS
malaria_trend_data_ci <- read_csv(file.path(PopDir, "analysis_dat/240606_df_with_ci_for_analysis_trend_malaria_grace_created.csv"))

#******** URBAN ********
# select only urban data
urban_trend_malaria_ci <- malaria_trend_data_ci  %>%  
  filter(title == "Urban")

#******** RURAL *********
# select only rural data
rural_trend_malaria_ci <- malaria_trend_data_ci  %>%  
  filter(title == "Rural")

## =========================================================================================================================================
### NET USE DATA PREP
## =========================================================================================================================================

## -----------------------------------------------------------------------------------------------------------------------------------------
### Net Use Data Prep: Original data that does NOT include confidence intervals (used for DiD)
## -----------------------------------------------------------------------------------------------------------------------------------------

# read in the trend analysis dataset for net use (this contains both urban and rural data)
net_trend_data <- read_csv(file.path(PopDir, "analysis_dat/240606_urban_df_for_analysis_trend_net_chilo_created.csv"))

#******** URBAN ********
# urban analysis for net use: reshape and clean data
# exclude Tanzania as test positivity rate is less than 1%
# exclude Mozambique 2011 as MZ 2015 and MZ 2022-23 are the two most recent surveys
urban_trend_net <- net_trend_data  %>%
  filter(title == "Rural", cntryId...9 != "TZ", country_year.x...2 != "Mozambique 2011") %>% # filter relevant data for rural areas
  select(country_year.x...2, cntryId...5, agric_percent, non_agric_percent, diff_val_net, cntryId...12, id) %>% # select specific columns
  rename(country_year = country_year.x...2, country_id = cntryId...5, country_surveyyears = cntryId...12) %>% # rename columns for clarity
  mutate(id = if_else(country_year == "Mozambique 2015", "Preceding survey", id)) # update id for specific condition

# recode "Mozambique 2011 vs 2015" to "Mozambique 2015 Vs. 2022 - 2023"
urban_trend_net$country_surveyyears[urban_trend_net$country_surveyyears == "Mozambique 2011 Vs. 2015"] <- "Mozambique 2015 Vs. 2022 - 23"

# hardcode test positivity values for urban Mozambique 2022-23 based on above analysis - I estimated these based on 2011 and 2015 surveys
df <- data.frame(country_year = "Mozambique 2022 - 23", agric_percent = 79, non_agric_percent = 68, diff_val_net = 68 - 79, id = "Most recent survey", country_id  = "MZ", country_surveyyears = "Mozambique 2015 Vs. 2022 - 23")

# join the trend data with hardcoded values
urban_trend_net_updated <- rbind(urban_trend_net, df)

#******** RURAL *********

## rural analysis for net use: reshape and clean data
# exclude Tanzania as test positivity rate is less than 1%
# exclude Mozambique 2011 as MZ 2015 and MZ 2022-23 are the two most recent surveys
rural_trend_net <- net_trend_data  %>%  
  filter(title == "Rural", cntryId...9 != "TZ", country_year.x...2 != "Mozambique 2011") %>% # filter relevant data for rural areas
  select(country_year.x...2, cntryId...5, agric_percent, non_agric_percent, diff_val_net, cntryId...12, id) %>% # select specific columns
  rename(country_year = country_year.x...2, country_id = cntryId...5, country_surveyyears = cntryId...12) %>% # rename columns for clarity
  mutate(id = if_else(country_year == "Mozambique 2015", "Preceding survey", id)) # update id for specific condition

# recode "Mozambique 2011 vs 2015" to "Mozambique 2015 Vs. 2022 - 2023"
rural_trend_net$country_surveyyears[rural_trend_net$country_surveyyears == "Mozambique 2011 Vs. 2015"] <- "Mozambique 2015 Vs. 2022 - 23"

# hardcode test positivity values for rural Mozambique 2022-23 based on above analysis - I estimated these based on 2011 and 2015 surveys
df <- data.frame(country_year = "Mozambique 2022 - 23", agric_percent = 67, non_agric_percent = 71, diff_val_net = 71 - 67, id = "Most recent survey", country_id  = "MZ", country_surveyyears = "Mozambique 2015 Vs. 2022 - 23")

# join the trend data with hardcoded values
rural_trend_net_updated <- rbind(rural_trend_net, df)

## -----------------------------------------------------------------------------------------------------------------------------------------
### Net Use Data Prep: New data that DOES include confidence intervals (also includes MZ2022 data so no need to hard code) (Grace)
## -----------------------------------------------------------------------------------------------------------------------------------------

# read in the trend analysis dataset for net use rates - INCLUDES CONFIDENCE INTERVALS
net_trend_data_ci <- read_csv(file.path(PopDir, "analysis_dat/240606_df_with_ci_for_analysis_trend_net_grace_created.csv"))

#******** URBAN ********
# select only urban data
urban_trend_net_ci <- net_trend_data_ci %>%  
  filter(title == "Urban")

#******** RURAL *********
# select only rural data
rural_trend_net_ci <- net_trend_data_ci %>%  
  filter(title == "Rural")

## -----------------------------------------------------------------------------------------------------------------------------------------
### URBAN MALARIA: Difference-in-Differences Analysis + Scatterplot
## -----------------------------------------------------------------------------------------------------------------------------------------

# calculate DiD
did_results_urban_malaria <- urban_trend_malaria_updated %>%
  group_by(country_id, country_surveyyears) %>% 
  summarise(
    B = agric_percent[id == "Most recent survey"],  # test positivity for agricultural HHs (most recent)
    A = agric_percent[id == "Preceding survey"],    # test positivity for agricultural HHs (preceding)
    D = non_agric_percent[id == "Most recent survey"],  # test positivity for non-agricultural HHs (most recent)
    C = non_agric_percent[id == "Preceding survey"],    # test positivity for non-agricultural HHs (preceding)
    DiD = (B - A) - (D - C)  # difference-in-differences calculation
  ) %>%
  ungroup()  # ensure no grouping is retained after summarization

ggplot(did_results_urban, aes(country_surveyyears, DiD))+
  geom_point(size = 5, alpha = 0.7, color = "#e07a5f")+
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50")+
  theme_manuscript()

library(stringr)

# change capital "Vs." to "vs."
did_results_urban_malaria <- did_results_urban_malaria %>%
  mutate(country_surveyyears = str_replace(country_surveyyears, "Vs.", "vs."))
# update country_surveyyears to remove spaces around dashes
did_results_urban_malaria <- did_results_urban_malaria %>%
  mutate(country_surveyyears = str_replace_all(country_surveyyears, " - ", "-"))

# create the scatter plot (urban)
did_plot_urban_malaria <- ggplot(did_results_urban_malaria, aes(x = DiD, y = country_surveyyears)) +
  geom_point(size = 5, alpha = 0.7, color = "#e07a5f") +  # Scatter points
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +  # Vertical line at x=0
  theme_minimal() +
  labs(x = "Difference-in-Differences (DD): 
       Change in Malaria Positivity Rates (Agricultural HH - Non-Agricultural HH)", 
       y = "Country", 
       title = "Difference-in-Differences Analysis Comparing Malaria Positivity 
       in Agricultural vs. Non-Agricultural Households in Urban Areas") +
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 14)) + 
  xlim(-25, 25)  # set x-axis limits

## -----------------------------------------------------------------------------------------------------------------------------------------
### RURAL MALARIA: Difference-in-Differences Analysis + Scatterplot
## -----------------------------------------------------------------------------------------------------------------------------------------

# calculate DiD
did_results_rural_malaria <- rural_trend_malaria_updated %>%
  group_by(country_id, country_surveyyears) %>% 
  summarise( 
    B = agric_percent[id == "Most recent survey"],  # test positivity for agricultural HHs (most recent)
    A = agric_percent[id == "Preceding survey"],    # test positivity for agricultural HHs (preceding)
    D = non_agric_percent[id == "Most recent survey"],  # test positivity for non-agricultural HHs (most recent)
    C = non_agric_percent[id == "Preceding survey"],    # test positivity for non-agricultural HHs (preceding)
    DiD = (B - A) - (D - C)  # difference-in-differences calculation
  ) %>%
  ungroup()  # ensure no grouping is retained after summarization

# change capital "Vs." to "vs."
did_results_rural_malaria <- did_results_rural_malaria %>%
  mutate(country_surveyyears = str_replace(country_surveyyears, "Vs.", "vs."))
# update country_surveyyears to remove spaces around dashes
did_results_rural_malaria <- did_results_rural_malaria %>%
  mutate(country_surveyyears = str_replace_all(country_surveyyears, " - ", "-"))

# create the scatter plot (rural)
did_plot_rural_malaria <- ggplot(did_results_rural_malaria, aes(x = DiD, y = country_surveyyears)) +
  geom_point(size = 5, alpha = 0.7, color = "#e07a5f") +  # Scatter points
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +  # Vertical line at x=0
  theme_minimal() +
  labs(x = "Difference-in-Differences (DD): 
       Change in Malaria Positivity Rates (Agricultural HH - Non-Agricultural HH)", 
       y = "Country", 
       title = "Difference-in-Differences Analysis Comparing Malaria Positivity 
       in Agricultural vs. Non-Agricultural Households in Rural Areas") +
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 14)) + 
  xlim(-25, 25)  # set x-axis limits

library(patchwork)

# remove individual titles and x-axis labels from the urban and rural plots
did_plot_urban_malaria <- did_plot_urban_malaria + 
  labs(title = NULL, subtitle = "Urban", x = NULL, y = NULL) + 
  theme(plot.subtitle = element_text(hjust = 0.5, size = 12))
did_plot_rural_malaria <- did_plot_rural_malaria + 
  labs(title = NULL, subtitle = "Rural", x = NULL, y = NULL) + 
  theme(plot.subtitle = element_text(hjust = 0.5, size = 12))

# combine the two plots vertically (ease of comparison)
did_malaria_plots <- (did_plot_urban_malaria + did_plot_rural_malaria) +
  plot_annotation(
    title = "Difference-in-Differences Analysis Comparing Malaria
    Positivity in Agricultural vs. Non-Agricultural Households over Time",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 16)),
    subtitle = NULL  # remove the subtitle from the overall plot
  ) +
  # add a custom label for the x-axis below the plots
  plot_layout(ncol = 1) +
  labs(x = "Difference-in-Differences: Change in Malaria Positivity\n Rates (Agricultural HH - Non-Agricultural HH)") + 
  theme(axis.title.x = element_text(hjust = 0.5, vjust = -3))

# display the combined plot and save as .png
did_malaria_plots
ggsave(paste0(FigDir, "/png_figures/", Sys.Date(),"_DiD_urban+rural_plot.png"), did_malaria_plots, width = 7, height = 7) 

## -----------------------------------------------------------------------------------------------------------------------------------------
### URBAN NET USE: Difference-in-Differences Analysis + Scatterplot
## -----------------------------------------------------------------------------------------------------------------------------------------

# calculate DiD
did_results_urban_net <- urban_trend_net_updated %>%
  group_by(country_id, country_surveyyears) %>% 
  summarise(
    B = agric_percent[id == "Most recent survey"],  # net use rate for agricultural HHs (most recent)
    A = agric_percent[id == "Preceding survey"],    # net use rate for agricultural HHs (preceding)
    D = non_agric_percent[id == "Most recent survey"],  # net use rate for non-agricultural HHs (most recent)
    C = non_agric_percent[id == "Preceding survey"],    # net use rate for non-agricultural HHs (preceding)
    DiD = (B - A) - (D - C)  # difference-in-differences calculation
  ) %>%
  ungroup()  # ensure no grouping is retained after summarization

# change capital "Vs." to "vs."
did_results_urban_net <- did_results_urban_net %>%
  mutate(country_surveyyears = str_replace(country_surveyyears, "Vs.", "vs."))
# update country_surveyyears to remove spaces around dashes
did_results_urban_net <- did_results_urban_net %>%
  mutate(country_surveyyears = str_replace_all(country_surveyyears, " - ", "-"))

# create the scatter plot (urban)
did_plot_urban_net <- ggplot(did_results_urban_net, aes(x = DiD, y = country_surveyyears)) +
  geom_point(size = 5, alpha = 0.7, color = "#e07a5f") +  # Scatter points
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +  # Vertical line at x=0
  theme_minimal() +
  labs(x = "Difference-in-Differences (DD): 
       Change in Net Use Rates (Agricultural HH - Non-Agricultural HH)",
       y = NULL,
       title = "Difference-in-Differences Analysis Comparing Net Use Rates 
       in Agricultural vs. Non-Agricultural Households in Urban Areas") +
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 14)) + 
  xlim(-50, 25)  # set x-axis limits

## -----------------------------------------------------------------------------------------------------------------------------------------
### RURAL NET USE: Difference-in-Differences Analysis + Scatterplot
## -----------------------------------------------------------------------------------------------------------------------------------------

# calculate DiD
did_results_rural_net <- rural_trend_net_updated %>%
  group_by(country_id, country_surveyyears) %>% 
  summarise(
    B = agric_percent[id == "Most recent survey"],  # test positivity for agricultural HHs (most recent)
    A = agric_percent[id == "Preceding survey"],    # test positivity for agricultural HHs (preceding)
    D = non_agric_percent[id == "Most recent survey"],  # test positivity for non-agricultural HHs (most recent)
    C = non_agric_percent[id == "Preceding survey"],    # test positivity for non-agricultural HHs (preceding)
    DiD = (B - A) - (D - C)  # difference-in-differences calculation
  ) %>%
  ungroup()  # ensure no grouping is retained after summarization

# change capital "Vs." to "vs."
did_results_rural_net <- did_results_rural_net %>%
  mutate(country_surveyyears = str_replace(country_surveyyears, "Vs.", "vs."))
# update country_surveyyears to remove spaces around dashes
did_results_rural_net <- did_results_rural_net %>%
  mutate(country_surveyyears = str_replace_all(country_surveyyears, " - ", "-"))

# create the scatter plot (rural)
did_plot_rural_net <- ggplot(did_results_rural_net, aes(x = DiD, y = country_surveyyears)) +
  geom_point(size = 5, alpha = 0.7, color = "#e07a5f") +  # scatter points
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +  # vertical line at x=0
  theme_minimal() +
  labs(x = "Difference-in-Differences (DD): 
       Change in Net Use Rates (Agricultural HH - Non-Agricultural HH)",
       y = NULL,
       title = "Difference-in-Differences Analysis Comparing Net Use Rates 
       in Agricultural vs. Non-Agricultural Households in Rural Areas") +
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(vjust = 5, size = 12),
        plot.title = element_text(hjust = 0.5, size = 14)) + 
  xlim(-50, 25)  # set x-axis limits

# remove individual titles and x-axis labels from the urban and rural plots
did_plot_urban_net <- did_plot_urban_net + 
  labs(title = NULL, subtitle = "Urban", x = NULL, y = NULL) + 
  theme(plot.subtitle = element_text(hjust = 0.5, size = 12))
did_plot_rural_net <- did_plot_rural_net + 
  labs(title = NULL, subtitle = "Rural", x = NULL, y = NULL) + 
  theme(plot.subtitle = element_text(hjust = 0.5, size = 12))

# combine the two plots vertically (ease of comparison)
did_net_plots <- (did_plot_urban_net + did_plot_rural_net) +
  plot_annotation(
    title = "Difference-in-Differences Analysis Comparing Net Use\n Rates in Agricultural vs. Non-Agricultural Households over Time",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 16)),
    subtitle = NULL  # remove the subtitle from the overall plot
  ) +
  # add a custom label for the x-axis below the plots
  plot_layout(ncol = 1) +
  labs(x = "Difference-in-Differences: Change in Malaria Net Use\n Rates (Agricultural HH - Non-Agricultural HH)") + 
  theme(axis.title.x = element_text(hjust = 0.5, vjust = -3))

# display the combined plot and save as .png
did_net_plots
ggsave(paste0(FigDir, "/png_figures/", Sys.Date(),"_DiD_urban+rural_net_plot.png"), did_net_plots, width = 7, height = 7) 


## -----------------------------------------------------------------------------------------------------------------------------------------
### Combined Scatterplots: Urban/Rural Malaria/Net Use
## -----------------------------------------------------------------------------------------------------------------------------------------

library(tidyverse)

# URBAN plot: combine malaria positivity DiD and net use DiD

#color_list <- c("forestgreen", "darkorchid4", "#d391fa", "#3e00b3", "#c55c80", "#e07a5f", "#0d47a1")

color_list <- c("#006400", "#4B0082", "#DDA0DD", "#1E90FF", "#C71585", "#FF6347", "gold")

# combine malaria and net use data for urban areas
urban_combined <- left_join(did_results_urban_malaria, did_results_urban_net, by = "country_id", suffix = c("_malaria", "_net"))

# create a scatter plot for urban areas
urban_combined_plot <- ggplot(urban_combined) +
  geom_point(aes(x = DiD_net, y = DiD_malaria, color = country_surveyyears_malaria), size = 5, alpha = 0.7) +  # points
  scale_shape_manual(values = c("Net Use" = 15, "Malaria" = 17)) +  # Different shapes: square for net use, triangle for malaria
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +  # vertical line at x=0
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +  # horizontal line at y=0
  scale_color_manual(values = color_list) +  # apply custom colors
  labs(x = "Difference-in-Differences: Net Use Rate", 
       y = "Difference-in-Differences: Malaria Positivity", 
       title = "Urban: DiD Analysis of Malaria Positivity and Net Use",
       color = "Country and Survey Years") +
  theme(panel.background = element_blank(),  # remove grey background
        panel.grid.major = element_line(color = "#e0e0e0"),  # lighter grey for major grid lines
        panel.grid.minor = element_line(color = "#f0f0f0"),  # even lighter grey for minor grid lines
        axis.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 14)) + 
  xlim(-50, 25) + 
  geom_smooth(aes(x = DiD_net, y = DiD_malaria), method = "lm", se = FALSE, color = "black")  # add linear regression line

# correlation test, extract correlation coefficient and p-value
cor_test_result_urban <- cor.test(urban_combined$DiD_net, urban_combined$DiD_malaria)
corr_coef_urban <- cor_test_result_urban$estimate
p_value_urban <- cor_test_result_urban$p.value

# print results
print(paste("Correlation Coefficient:", corr_coef_urban))
print(paste("P-value:", p_value_urban))

# display and save as .png
urban_combined_plot
ggsave(paste0(FigDir, "/png_figures/", Sys.Date(),"_DiD_urban_mal_net_combined.png"), urban_combined_plot, width = 7, height = 7) 

# RURAL plot: combine malaria positivity DiD and net use DiD

# combine malaria and net use data for rural areas
rural_combined <- left_join(did_results_rural_malaria, did_results_rural_net, by = "country_id", suffix = c("_malaria", "_net"))

# create a scatter plot for rural areas
# add a linear regression line with the geom_smooth() function
rural_combined_plot <- ggplot(rural_combined) +
  geom_point(aes(x = DiD_net, y = DiD_malaria, color = country_surveyyears_malaria), size = 5, alpha = 0.7) +  # points
  scale_shape_manual(values = c("Net Use" = 15, "Malaria" = 17)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "grey50") +  # vertical line at x = 0
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50") +  # horizontal line at y = 0
  scale_color_manual(values = color_list) +  # apply custom colors
  labs(x = "DiD: Change in Net Use Rates\n (Agricultural HH - Non-Agricultural HH)", 
       y = "Difference-in-Differences: Malaria Positivity", 
       title = "Rural: DiD Analysis of Malaria Positivity and Net Use",
       color = "Country") +
  theme(panel.background = element_blank(),  # remove grey background
        panel.grid.major = element_line(color = "#e0e0e0"),  # light grey for major grid lines
        panel.grid.minor = element_line(color = "#f0f0f0"),  # lighter grey for minor grid lines
        axis.text = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 14)) + 
  xlim(-50, 25) + 
  geom_smooth(aes(x = DiD_net, y = DiD_malaria), method = "lm", se = FALSE, color = "black")  # add linear regression line

# correlation test, extract correlation coefficient and p-value
cor_test_result_rural <- cor.test(rural_combined$DiD_net, rural_combined$DiD_malaria)
corr_coef_rural <- cor_test_result_rural$estimate
p_value_rural <- cor_test_result_rural$p.value

# print results
print(paste("Correlation Coefficient:", corr_coef_rural))
print(paste("P-value:", p_value_rural))

# display and save as .png
rural_combined_plot
ggsave(paste0(FigDir, "/png_figures/", Sys.Date(),"_DiD_rural_mal_net_combined.png"), rural_combined_plot, width = 7, height = 7) 

#### combine the urban and rural plots

# remove individual titles and x-axis labels from the urban and rural plots
urban_combined_plot <- urban_combined_plot + 
  labs(title = NULL, subtitle = "Urban", x = NULL, y = NULL) + 
  theme(plot.subtitle = element_text(hjust = 0.5, size = 12)) 
rural_combined_plot <- rural_combined_plot + 
  labs(title = NULL, subtitle = "Rural", y = NULL) + 
  theme(plot.subtitle = element_text(hjust = 0.5, size = 12)) 

# extract the legend
legend <- get_only_legend(urban_combined_plot) 

urban_combined_plot <- urban_combined_plot + theme(legend.position = "none")
rural_combined_plot <- rural_combined_plot + theme(legend.position = "none") 

combined_plot <- grid.arrange(urban_combined_plot, rural_combined_plot)

# arrange the combined plot and legend side by side
final_did_plots <- grid.arrange(
  combined_plot,
  legend,
  nrow = 1,
  ncol = 2,
  heights = c(5),
  widths = c(8, 4),
  top = textGrob("Difference-in-Differences (DiD) Analysis Comparing Malaria Positivity and Net Use \n Rates in Agricultural vs. Non-Agricultural Households over Time",
                 gp = gpar(fontsize = 12, fontface = "bold", hjust = 0.5)),  # Adjusted hjust for centering
  left = textGrob("DiD: Change in Malaria Positivity\n (Agricultural HH - Non-Agricultural HH)",
                  rot = 90,
                  gp = gpar(fontsize = 12))
)

# display the combined plot and save as .png
ggsave(paste0(FigDir, "/png_figures/", Sys.Date(),"_DiD_final_plot.png"), final_did_plots, width = 7, height = 7) 


## =========================================================================================================================================
### LINE PLOTS (Survey Year vs. Malaria TPR/Net Use)
## =========================================================================================================================================

## -----------------------------------------------------------------------------------------------------------------------------------------
### Data Prep for Line Plots (use dataframe that includes CI for line plots)
# 1) Create long-format urban malaria dataframe
# 2) Create long-format rural malaria dataframe
# 3) Create long-format urban net use dataframe
# 4) Create long-format rural net use dataframe
# 5) Read in + prep net distribution dataframe
## -----------------------------------------------------------------------------------------------------------------------------------------

# ***** 1) LONG FORMAT URBAN MALARIA DF *****
# create new variables with just country name and just survey year (first year in survey range if survey spans two years)
urban_trend_malaria_ci <- urban_trend_malaria_ci %>%
  mutate(
    # extract the country name (everything before the first numeric character)
    country = str_extract(country_year, "^[^0-9]+") %>% str_trim(),
    
    # extract the year (the first numeric part found in the string)
    year = str_extract(country_year, "\\d{4}") %>% as.numeric()
  )

# reshape the data to long format (necessary to plot both agric_percent and non_agric_percent)
urban_trend_malaria_ci_long <- urban_trend_malaria_ci %>%
  pivot_longer(
    cols = c(agric_percent, non_agric_percent),
    names_to = "household_type",
    values_to = "percent"
  )

# mali agric. lower CI is negative so cap the lower CI bound at 0 and upper at 100
urban_trend_malaria_ci_long <- urban_trend_malaria_ci_long %>%
  mutate(
    agric_lower_ci = ifelse(agric_lower_ci < 0, 0, agric_lower_ci),
    agric_upper_ci = ifelse(agric_upper_ci > 100, 100, agric_upper_ci),
    non_agric_lower_ci = ifelse(non_agric_lower_ci < 0, 0, non_agric_lower_ci),
    non_agric_upper_ci = ifelse(non_agric_upper_ci > 100, 100, non_agric_upper_ci)
  )

# ***** 2) LONG FORMAT RURAL MALARIA DF *****
# create new variable with just country name and just survey year (first year in survey range if survey spans two years)
rural_trend_malaria_ci <- rural_trend_malaria_ci %>%
  mutate(
    # extract the country name (everything before the first numeric character)
    country = str_extract(country_year, "^[^0-9]+") %>% str_trim(),
    
    # extract the year (the first numeric part found in the string)
    year = str_extract(country_year, "\\d{4}") %>% as.numeric()
  )

# reshape the data to long format (necessary to plot both agric_percent and non_agric_percent)
rural_trend_malaria_ci_long <- rural_trend_malaria_ci %>%
  pivot_longer(
    cols = c(agric_percent, non_agric_percent),
    names_to = "household_type",
    values_to = "percent"
  )

# cap the lower CI bound at 0 and upper at 100 (none should be outside but good practice)
rural_trend_malaria_ci_long <- rural_trend_malaria_ci_long %>%
  mutate(
    agric_lower_ci = ifelse(agric_lower_ci < 0, 0, agric_lower_ci),
    agric_upper_ci = ifelse(agric_upper_ci > 100, 100, agric_upper_ci),
    non_agric_lower_ci = ifelse(non_agric_lower_ci < 0, 0, non_agric_lower_ci),
    non_agric_upper_ci = ifelse(non_agric_upper_ci > 100, 100, non_agric_upper_ci)
  )

# ***** 3) LONG FORMAT URBAN NET USE DF *****
# create new variable with just country name and just survey year (first year in survey range if survey spans two years)
urban_trend_net_ci <- urban_trend_net_ci %>%
  mutate(
    # extract the country name (everything before the first numeric character)
    country = str_extract(country_year, "^[^0-9]+") %>% str_trim(),
    
    # extract the year (the first numeric part found in the string)
    year = str_extract(country_year, "\\d{4}") %>% as.numeric()
  )

# reshape the data to long format (necessary to plot both agric_percent and non_agric_percent)
urban_trend_net_ci_long <- urban_trend_net_ci %>%
  pivot_longer(
    cols = c(agric_percent, non_agric_percent),
    names_to = "household_type",
    values_to = "percent"
  )

# ***** 4) LONG FORMAT RURAL NET USE DF *****
# create new variable with just country name and just survey year (first year in survey range if survey spans two years)
rural_trend_net_ci <- rural_trend_net_ci %>%
  mutate(
    # extract the country name (everything before the first numeric character)
    country = str_extract(country_year, "^[^0-9]+") %>% str_trim(),
    
    # extract the year (the first numeric part found in the string)
    year = str_extract(country_year, "\\d{4}") %>% as.numeric()
  )

# reshape the data to long format (necessary to plot both agric_percent and non_agric_percent)
rural_trend_net_ci_long <- rural_trend_net_ci %>%
  pivot_longer(
    cols = c(agric_percent, non_agric_percent),
    names_to = "household_type",
    values_to = "percent"
  )

# ***** 5) NET DISTRIBUTION DF *****
net_distribution_df <- read_csv(file.path(DriveDir, "data/Urban_malaria_net_ownership_data/Extracted_csv/220315_ITN_distribution_data.csv"))

# select only 6 countries of interest and calculate year-to-year percent change in nets distributed (will add this to line plots)
net_distribution_df <- net_distribution_df %>%
  rename(total_net_distributed = `total net distributed`) %>%
  group_by(Country) %>%
  arrange(Country, Year) %>%
  mutate(
    # set first_year_nets to the second year's distribution for Côte d'Ivoire and Benin,
    # and to the first year's distribution for other countries
    first_year_nets = if_else(Country %in% c("Côte d'Ivoire", "Benin"),
                              nth(total_net_distributed, 2, default = first(total_net_distributed)),
                              first(total_net_distributed)),
    pct_change_nets = (total_net_distributed - first_year_nets) / first_year_nets * 100  # Calculate percentage change from the selected year
  ) %>%
  filter(Country %in% c("Benin", "Burkina Faso", "Cameroon", "Côte d'Ivoire", "Mali", "Mozambique")) %>%
  ungroup()

# replace "inf" value with NA (Cote d'Ivoire went from 0 nets to 18509750 in 2020 -> 2021) and change spelling of CI to match other dfs
net_distribution_df <- net_distribution_df %>%
  mutate(
    Country = ifelse(Country == "Côte d'Ivoire", "Cote d'Ivoire", Country),
    pct_change_nets = ifelse(is.infinite(pct_change_nets), NA, pct_change_nets)
  )

View(net_distribution_df)
## -----------------------------------------------------------------------------------------------------------------------------------------
### Generate a line plot for Malaria Test Positivity Rate (TPR) alone
# •	X-axis: Year of the survey.
# •	Y-axis: Malaria test positivity rate (TPR).
# •	Color the lines by country and include both agric and non-agric HHs for each survey.
## -----------------------------------------------------------------------------------------------------------------------------------------

# URBAN MALARIA LINE PLOT
# create the line plot (all countries so do not include confidence intervals)
urban_malaria_tpr_plot <- ggplot(urban_trend_malaria_ci_long, aes(x = year, y = percent, color = country, linetype = household_type)) +
  geom_line(size = 1) +  # line for each group
  geom_point(size = 3, alpha = 0.7) +  # add points for each data point
  theme_minimal() +
  labs(x = "Survey Year",
       y = "Malaria Test Positivity Rate (%)",
       color = "Country",
       title = "Malaria Test Positivity Rate by\n Year and Household Type: Urban") +
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 14)) +
  scale_color_manual(values = color_list) +  # use existing color list
  scale_linetype_manual(values = c("solid", "dashed"), labels = c("Agricultural  ", "Non-Agricultural   ")) +  # different line types for agric/non-agric HHs
  guides(linetype = guide_legend(title = "Household Type")) + # add legend title for line types
  scale_x_continuous(breaks = seq(min(urban_trend_malaria_ci_long$year), max(urban_trend_malaria_ci_long$year), by = 2))  # tick marks every 2 years

urban_malaria_tpr_plot

# RURAL MALARIA LINE PLOT
# create the line plot (all countries so do not include confidence intervals)
rural_malaria_tpr_plot <- ggplot(rural_trend_malaria_ci_long, aes(x = year, y = percent, color = country, linetype = household_type)) +
  geom_line(size = 1) +  # line for each group
  geom_point(size = 3, alpha = 0.7) +  # add points for each data point
  theme_minimal() +
  labs(x = "Survey Year",
       y = "Malaria Test Positivity Rate (%)",
       color = "Country",
       title = "Malaria Test Positivity Rate (TPR) by\n Year and Household Type: Rural") +
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 14)) +
  scale_color_manual(values = color_list) +  # use existing color list
  scale_linetype_manual(values = c("solid", "dashed"), labels = c("Agricultural", "Non-Agricultural")) +  # different line types for agric/non-agric HHs
  guides(linetype = guide_legend(title = "Household Type")) + # add legend title for line types
  scale_x_continuous(breaks = seq(min(urban_trend_malaria_ci_long$year), max(urban_trend_malaria_ci_long$year), by = 2))  # tick marks every 2 years

rural_malaria_tpr_plot

#### COMBINE THE PLOTS

# remove individual titles and x-axis labels from the urban and rural plots
urban_malaria_tpr_plot <- urban_malaria_tpr_plot + 
  labs(title = NULL, subtitle = "Urban", x = NULL, y = NULL) + 
  theme(plot.subtitle = element_text(hjust = 0.5, size = 12)) 
rural_malaria_tpr_plot <- rural_malaria_tpr_plot + 
  labs(title = NULL, subtitle = "Rural", y = NULL) + 
  theme(plot.subtitle = element_text(hjust = 0.5, size = 12)) 

legend <- get_only_legend(urban_malaria_tpr_plot) 

# remove legends from both plots
urban_malaria_tpr_plot <- urban_malaria_tpr_plot + theme(legend.position = "none")
rural_malaria_tpr_plot <- rural_malaria_tpr_plot + theme(legend.position = "none") 

malaria_combined_line_plot <- grid.arrange(urban_malaria_tpr_plot, rural_malaria_tpr_plot)

# arrange the combined plot and legend side by side
malaria_final_line_plots <- grid.arrange(
  malaria_combined_line_plot,
  legend,
  nrow = 1,
  ncol = 2,
  heights = c(5),
  widths = c(8, 4),
  top = textGrob("Malaria Test Positivity Rate by Survey Year and Household Type",
                 gp = gpar(fontsize = 12, fontface = "bold", hjust = 0.5)),  # Adjusted hjust for centering
  left = textGrob("Malaria Test Positivity Rate (%)",
                  rot = 90,
                  gp = gpar(fontsize = 12))
)

# display the combined plot and save as .png
ggsave(paste0(FigDir, "/png_figures/", Sys.Date(),"_malaria_line_plot_combined.png"), malaria_final_line_plots, width = 10, height = 10) 

## -----------------------------------------------------------------------------------------------------------------------------------------
### Generate a line plot for Net Use alone
# •	X-axis: Year of the survey.
# •	Y-axis: Net use rates.
# •	Color the lines by country and include both agric and non-agric HHs for each survey.
## -----------------------------------------------------------------------------------------------------------------------------------------

# URBAN NET USE PLOT
# create the line plot (all countries so does not include confidence intervals)
urban_net_plot <- ggplot(urban_trend_net_long, aes(x = year, y = percent, color = country, linetype = household_type)) +
  geom_line(size = 1) +  # line for each group
  geom_point(size = 3, alpha = 0.7) +  # add points for each data point
  theme_minimal() +
  labs(x = "Survey Year",
       y = "Net Use Rate (%)",
       color = "Country",
       title = "Net Use Rate by Year and Household Type: Urban") +
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 14)) +
  scale_color_manual(values = color_list) +  # use existing color list
  scale_linetype_manual(values = c("solid", "dashed"), labels = c("Agricultural  ", "Non-Agricultural   ")) +  # different line types for agric/non-agric HHs
  guides(linetype = guide_legend(title = "Household Type")) + # add legend title for line types
  scale_x_continuous(breaks = seq(min(urban_trend_net_long$year), max(urban_trend_net_long$year), by = 2))  # tick marks every 2 years

urban_net_plot

# RURAL NET USE PLOT
# create the line plot (all countries so does not include confidence intervals)
rural_net_plot <- ggplot(rural_trend_net_long, aes(x = year, y = percent, color = country, linetype = household_type)) +
  geom_line(size = 1) +  # line for each group
  geom_point(size = 3, alpha = 0.7) +  # add points for each data point
  theme_minimal() +
  labs(x = "Survey Year",
       y = "Net Use Rate (%)",
       color = "Country",
       title = "Net Use Rate by Year and Household Type: Rural") +
  theme(axis.text.y = element_text(size = 12),
        axis.text.x = element_text(size = 12),
        plot.title = element_text(hjust = 0.5, size = 14)) +
  scale_color_manual(values = color_list) +  # use existing color list
  scale_linetype_manual(values = c("solid", "dashed"), labels = c("Agricultural  ", "Non-Agricultural   ")) +  # different line types for agric/non-agric HHs
  guides(linetype = guide_legend(title = "Household Type")) + # add legend title for line types
  scale_x_continuous(breaks = seq(min(rural_trend_net_long$year), max(rural_trend_net_long$year), by = 2))  # tick marks every 2 years

rural_net_plot

#### COMBINE THE PLOTS

# remove individual titles and x-axis labels from the urban and rural plots
urban_net_plot <- urban_net_plot + 
  labs(title = NULL, subtitle = "Urban", x = NULL, y = NULL) + 
  theme(plot.subtitle = element_text(hjust = 0.5, size = 12)) 
rural_net_plot <- rural_net_plot + 
  labs(title = NULL, subtitle = "Rural", y = NULL) + 
  theme(plot.subtitle = element_text(hjust = 0.5, size = 12)) 

legend <- get_only_legend(urban_net_plot) 

# remove legends from both plots
urban_net_plot <- urban_net_plot + theme(legend.position = "none")
rural_net_plot <- rural_net_plot + theme(legend.position = "none") 

net_combined_line_plot <- grid.arrange(urban_net_plot, rural_net_plot)

# arrange the combined plot and legend side by side
net_final_line_plots <- grid.arrange(
  net_combined_line_plot,
  legend,
  nrow = 1,
  ncol = 2,
  heights = c(5),
  widths = c(8, 4),
  top = textGrob("Net Use Rate by Survey Year and Household Type",
                 gp = gpar(fontsize = 12, fontface = "bold", hjust = 0.5)),  # Adjusted hjust for centering
  left = textGrob("Net Use Rate (%)",
                  rot = 90,
                  gp = gpar(fontsize = 12))
)

# display the combined plot and save as .png
ggsave(paste0(FigDir, "/png_figures/", Sys.Date(),"_net_line_plot_combined.png"), net_final_line_plots, width = 10, height = 10)

# get a list of unique countries
countries <- unique(urban_trend_malaria_long$country)

# define an empty list to store the plots
country_plots <- list()

## =========================================================================================================================================
### URBAN LINE PLOTS: a) Malaria/net use combined, b) Malaria only, c) Net Use only (FACET BY COUNTRY)
## =========================================================================================================================================

# URBAN: loop through each country
for (country_name in countries) {
  
  # # get malaria TPR data for the current country
  # urban_country_data_malaria_ci <- urban_trend_malaria_ci_long %>%
  #   filter(country == country_name) %>%
  #   mutate(data_type = "Malaria TPR")
  # 
  # # get net use data for the current country
  # urban_country_data_net_ci <- urban_trend_net_ci_long %>%
  #   filter(country == country_name) %>%
  #   mutate(data_type = "Net Use Rate")
  # 
  # # combine the two datasets
  # urban_combined_data <- bind_rows(urban_country_data_malaria_ci, urban_country_data_net_ci)
  # 
  # # create plot a: combined malaria tpr and net use
  # urban_comb_plot <- ggplot(urban_combined_data, aes(x = year, y = percent, linetype = data_type, color = household_type)) +
  #   geom_line(size = 1) +  # Line for each group
  #   geom_point(size = 3, alpha = 0.7) +  # Add points for each data point
  #   geom_errorbar(data = urban_combined_data %>% filter(data_type == "Malaria TPR"), 
  #                 aes(ymin = agric_lower_ci, ymax = agric_upper_ci), 
  #                 width = 0.3, color = "#5560AB", alpha = 0.7) +  # Confidence interval for Malaria TPR (agricultural)
  #   geom_errorbar(data = urban_combined_data %>% filter(data_type == "Malaria TPR"), 
  #                 aes(ymin = non_agric_lower_ci, ymax = non_agric_upper_ci), 
  #                 width = 0.3, color = "#FAAF43", alpha = 0.7) +  # Confidence interval for Malaria TPR (non-agricultural)
  #   geom_errorbar(data = urban_combined_data %>% filter(data_type == "Net Use Rate"), 
  #                 aes(ymin = agric_lower_ci, ymax = agric_upper_ci), 
  #                 width = 0.3, color = "#5560AB", alpha = 0.7) +  # Confidence interval for Net Use Rate (agricultural)
  #   geom_errorbar(data = urban_combined_data %>% filter(data_type == "Net Use Rate"), 
  #                 aes(ymin = non_agric_lower_ci, ymax = non_agric_upper_ci), 
  #                 width = 0.3, color = "#FAAF43", alpha = 0.7) +  # Confidence interval for Net Use Rate (non-agricultural)
  #   theme_manuscript() +
  #   labs(
  #     x = "Survey Year", y = "Percent (%)", title = paste(country_name), color = "Household Type", linetype = "Data Type"  # Legend titles
  #   ) +
  #   theme(
  #     axis.text.y = element_text(size = 12),
  #     axis.text.x = element_text(size = 10),
  #     plot.title = element_text(hjust = 0.5, size = 14)
  #   ) +
  #   scale_color_manual(values = c("agric_percent" = "#5560AB", "non_agric_percent" = "#FAAF43"),  # Agricultural = blue, Non-agricultural = yellow
  #                      labels = c("Agricultural", "Non-Agricultural")) +  # Custom labels for the legend
  #   scale_linetype_manual(values = c("Malaria TPR" = "solid", "Net Use Rate" = "dashed")) +  # Line type: solid for Malaria TPR, dashed for Net Use
  #   guides(linetype = guide_legend(override.aes = list(color = "black"))) +  # Set legend line color to black
  #   scale_x_continuous(limits = c(2009.5, 2022.5), breaks = seq(2010, 2022, by = 3)) +
  #   scale_y_continuous(limits = c(0, 100))  # Set consistent y-axis limits
  
  # get malaria TPR and net use data for the current country
  urban_country_data_malaria_ci <- urban_trend_malaria_ci_long %>%
    filter(country == country_name) %>%
    mutate(data_type = "Malaria TPR")
  
  urban_country_data_net_ci <- urban_trend_net_ci_long %>%
    filter(country == country_name) %>%
    mutate(data_type = "Net Use Rate")
  
  # combine malaria and net use data
  urban_combined_data <- bind_rows(urban_country_data_malaria_ci, urban_country_data_net_ci)
  
  # get net distribution data and percentage change for the current country (compared to first year nets were distributed)
  net_data_country <- net_distribution_df %>%
    filter(Country == country_name) %>%
    select(Year, total_net_distributed, pct_change_nets)
  
  # create a new data frame for net distribution with a legend entry
  net_data_country_legend <- net_data_country %>%
    mutate(data_type = "Percentage Change in Nets Distributed")  # Add a column for the legend entry
  
  # PLOT: TPR AND NET USE AND PERCENT CHANGE IN NETS DISTRIBUTED FROM PRIOR YEAR
  urban_comb_plot <- ggplot() +
    
    # area plot for percent change in net distribution
    geom_area(data = net_data_country_legend, 
              aes(x = Year, y = scales::rescale(pct_change_nets, to = c(0, 100))),
              fill = "lightgrey", alpha = 0.5, position = 'identity') +
    
    # line plot for malaria TPR and net use
    geom_line(data = urban_combined_data, 
              aes(x = year, y = percent, linetype = data_type, color = household_type), size = 1) +
    
    # points for malaria TPR and net use
    geom_point(data = urban_combined_data, 
               aes(x = year, y = percent, color = household_type), size = 3, alpha = 0.7) +
    
    # add confidence intervals for malaria TPR (agricultural)
    geom_errorbar(data = urban_combined_data %>% filter(data_type == "Malaria TPR", household_type == "agric_percent"),
                  aes(x = year, ymin = agric_lower_ci, ymax = agric_upper_ci),
                  width = 0.3, color = "#5560AB", alpha = 0.7) +
    
    # add confidence intervals for malaria TPR (non-agricultural)
    geom_errorbar(data = urban_combined_data %>% filter(data_type == "Malaria TPR", household_type == "non_agric_percent"),
                  aes(x = year, ymin = non_agric_lower_ci, ymax = non_agric_upper_ci),
                  width = 0.3, color = "#FAAF43", alpha = 0.7) +
    
    # add confidence intervals for net use rate (agricultural)
    geom_errorbar(data = urban_combined_data %>% filter(data_type == "Net Use Rate", household_type == "agric_percent"),
                  aes(x = year, ymin = agric_lower_ci, ymax = agric_upper_ci),
                  width = 0.3, color = "#5560AB", alpha = 0.7) +
    
    # add confidence intervals for net use rate (non-agricultural)
    geom_errorbar(data = urban_combined_data %>% filter(data_type == "Net Use Rate", household_type == "non_agric_percent"),
                  aes(x = year, ymin = non_agric_lower_ci, ymax = non_agric_upper_ci),
                  width = 0.3, color = "#FAAF43", alpha = 0.7) +
    
    # set up primary and secondary y-axes
    scale_y_continuous(
      name = "Malaria TPR/Net Use Rate (%)",  # Primary y-axis label
      limits = c(0, 100),  # Primary y-axis scale from 0 to 100
      sec.axis = sec_axis(
        trans = ~ scales::rescale(., from = c(0, 100), to = c(-110, 16000)),  # Set secondary y-axis to custom range
        name = "Percentage Change in Nets Distributed (%)"
      )
    ) +
    
    # add theme, labels, and scales
    theme_manuscript() +
    labs(
      x = "Survey Year", y = "Percent (%)", title = paste(country_name), color = "Household Type", linetype = "Data Type"
    ) +
    theme(
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, size = 14)
    ) +
    scale_color_manual(values = c("agric_percent" = "#5560AB", "non_agric_percent" = "#FAAF43"),  # Agricultural = blue, Non-agricultural = yellow
                       labels = c("Agricultural", "Non-Agricultural")) +  # Custom labels for the legend
    scale_linetype_manual(values = c("Malaria TPR" = "solid", "Net Use Rate" = "dashed", "Year Percentage Change in Nets Distributed" = "dotted"), 
                          labels = c("Malaria TPR", "Net Use Rate", "Percentage Change\n in Nets Distributed"))  # Update labels
  
  # save the combined plot with the country name
  assign(paste0(country_name, "_urban_plot"), urban_comb_plot)
  
  # add the combined plot to the list
  country_plots[[country_name]] <- urban_comb_plot
  
  #------------------------------------------------------------------------------------------------------------------------------------------------#
  
  # create a new data frame for net distribution with a legend entry
  net_data_country_num_legend <- net_data_country %>%
    mutate(data_type = "Nets Distributed")  # Add a column for the legend entry
  
  # PLOT: TPR AND NET USE AND NUMBER OF NETS DISTRIBUTED
  urban_comb_num_nets_plot <- ggplot(urban_combined_data, aes(x = year, y = percent, linetype = data_type, color = household_type)) +
    geom_line(size = 1) +  # Line for malaria TPR and net use
    geom_point(size = 3, alpha = 0.7) +  # Points for TPR and net use
    
    # add confidence intervals for malaria TPR (agricultural)
    geom_errorbar(data = urban_combined_data %>% filter(data_type == "Malaria TPR", household_type == "agric_percent"),
                  aes(ymin = agric_lower_ci, ymax = agric_upper_ci),
                  width = 0.3, color = "#5560AB", alpha = 0.7) +
    
    # add confidence intervals for malaria TPR (non-agricultural)
    geom_errorbar(data = urban_combined_data %>% filter(data_type == "Malaria TPR", household_type == "non_agric_percent"),
                  aes(ymin = non_agric_lower_ci, ymax = non_agric_upper_ci),
                  width = 0.3, color = "#FAAF43", alpha = 0.7) +
    
    # add confidence intervals for net use rate (agricultural)
    geom_errorbar(data = urban_combined_data %>% filter(data_type == "Net Use Rate", household_type == "agric_percent"),
                  aes(ymin = agric_lower_ci, ymax = agric_upper_ci),
                  width = 0.3, color = "#5560AB", alpha = 0.7) +
    
    # add confidence intervals for net use rate (non-agricultural)
    geom_errorbar(data = urban_combined_data %>% filter(data_type == "Net Use Rate", household_type == "non_agric_percent"),
                  aes(ymin = non_agric_lower_ci, ymax = non_agric_upper_ci),
                  width = 0.3, color = "#FAAF43", alpha = 0.7) +
    
    # add net distribution data as points and connect with a dotted line
    geom_point(data = net_data_country_num_legend, aes(x = Year, y = scales::rescale(total_net_distributed, to = c(0, 100)), linetype = "Number of Nets Distributed"),  
               color = "darkgreen", size = 1.5, alpha = 0.7) +  # points for number nets distributed
    geom_line(data = net_data_country_num_legend, aes(x = Year, y = scales::rescale(total_net_distributed, to = c(0, 100)), linetype = "Number of Nets Distributed"),  
              color = "darkgreen", size = 0.5, linetype = "dotted") +  # dotted line for nets distributed
    
    # set up primary and secondary y-axes
    scale_y_continuous(
      name = "Malaria TPR/Net Use Rate (%)",  # Primary y-axis label
      limits = c(0, 100),  # Primary y-axis scale from 0 to 100
      sec.axis = sec_axis(
        trans = ~ scales::rescale(., from = c(0, 100), to = c(-100, 19000000)),  # Set secondary y-axis to custom range
        name = "Number of Nets Distributed"
      )
    ) +
    
    # add theme, labels, and scales
    theme_manuscript() +
    labs(
      x = "Survey Year", y = "Percent (%)", title = paste(country_name), color = "Household Type", linetype = "Data Type"
    ) +
    theme(
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, size = 14)
    ) +
    scale_color_manual(values = c("agric_percent" = "#5560AB", "non_agric_percent" = "#FAAF43"),  # Agricultural = blue, Non-agricultural = yellow
                       labels = c("Agricultural", "Non-Agricultural")) +  # Custom labels for the legend
    scale_linetype_manual(values = c("Malaria TPR" = "solid", "Net Use Rate" = "dashed", "Number of Nets Distributed" = "dotted"),  
                          labels = c("Malaria TPR", "Net Use Rate", "Number of\n Nets Distributed"))  # Update labels
  
  # save the combined plot with the country name
  assign(paste0(country_name, "_urban_num_nets_plot"), urban_comb_num_nets_plot)
  
  # # FACET: make plot b: malaria tpr by country -----------------------------------------------------------------------------------
  # urban_malaria_plot <- ggplot(urban_country_data_malaria_ci, aes(x = year, y = percent, color = household_type)) +
  #   
  #   # add confidence interval error bars with color by household_type
  #   geom_errorbar(
  #     aes(ymin = agric_lower_ci, ymax = agric_upper_ci),
  #     data = subset(urban_country_data_malaria_ci, household_type == "agric_percent"),
  #     width = 0.2, color = "#FAAF43", alpha = 0.7
  #   ) +
  #   geom_errorbar(
  #     aes(ymin = non_agric_lower_ci, ymax = non_agric_upper_ci),
  #     data = subset(urban_country_data_malaria_ci, household_type == "non_agric_percent"),
  #     width = 0.2, color = "#5560AB", alpha = 0.7
  #   ) +
  #   geom_line(size = 1) +
  #   geom_point(size = 3, alpha = 0.7, show.legend = TRUE) +
  #   theme_manuscript() +
  #   labs(
  #     x = "Survey Year", y = "Percent (%)", title = paste(country_name), color = "Household Type"
  #   ) +
  #   theme(
  #     axis.text.y = element_text(size = 12),
  #     axis.text.x = element_text(size = 10),
  #     plot.title = element_text(hjust = 0.5, size = 14)
  #   ) +
  #   scale_color_manual(
  #     values = c("agric_percent" = "#FAAF43", "non_agric_percent" = "#5560AB"),  # Agricultural = green, Non-agricultural = orange
  #     labels = c("Agricultural", "Non-Agricultural")  # Custom labels for the legend
  #   ) +
  #   scale_x_continuous(limits = c(2010, 2022), breaks = seq(2010, 2022, by = 3)) +  # Set limits and breaks for x-axis
  #   scale_y_continuous(limits = c(0, 96))
  # 
  # # save the malaria plot with the country name
  # assign(paste0(country_name, "_malaria_urban_plot"), urban_malaria_plot)
  # 
  # # FACET: c) make plot for just net use by country -------------------------------------------------------------------------------------
  # urban_net_plot <- ggplot(urban_country_data_net_ci, aes(x = year, y = percent, color = household_type)) +
  #   geom_errorbar( # add confidence interval error bars with color by household_type
  #     aes(ymin = agric_lower_ci, ymax = agric_upper_ci),
  #     data = subset(urban_country_data_net_ci, household_type == "agric_percent"),
  #     width = 0.2, color = "#FAAF43", alpha = 0.7
  #   ) +
  #   geom_errorbar(
  #     aes(ymin = non_agric_lower_ci, ymax = non_agric_upper_ci),
  #     data = subset(urban_country_data_net_ci, household_type == "non_agric_percent"),
  #     width = 0.2, color = "#5560AB", alpha = 0.7
  #   ) +
  #   # plot the lines and points with color by household_type
  #   geom_line(size = 1) +
  #   geom_point(size = 3, alpha = 0.7, show.legend = TRUE) +  # Add points for each data point, with legend
  #   theme_manuscript() +
  #   labs(
  #     x = "Survey Year",
  #     y = "Percent (%)",
  #     title = paste(country_name),
  #     color = "Household Type"  # Update the legend title to represent colors for household_type
  #   ) +
  #   theme(
  #     axis.text.y = element_text(size = 12),
  #     axis.text.x = element_text(size = 10),
  #     plot.title = element_text(hjust = 0.5, size = 14)
  #   ) +
  #   scale_color_manual(
  #     values = c("agric_percent" = "#FAAF43", "non_agric_percent" = "#5560AB"),  # Agricultural = green, Non-agricultural = orange
  #     labels = c("Agricultural", "Non-Agricultural")  # Custom labels for the legend
  #   ) +
  #   scale_x_continuous(limits = c(2010, 2022), breaks = seq(2010, 2022, by = 3)) +  # Set limits and breaks for x-axis
  #   scale_y_continuous(limits = c(0, 96))
  # 
  # # save the net plot with the country name
  # assign(paste0(country_name, "_net_urban_plot"), urban_net_plot)
}
print(Benin_urban_plot)

## -----------------------------------------------------------------------------------------------------------------------------------------
### Combine Malaria + Net Use + Percent Change in Net Distribution Urban Plots 
## -----------------------------------------------------------------------------------------------------------------------------------------

# extract the combined malaria/net use legend
legend <- get_only_legend(Benin_urban_plot) 

# remove legends from all plots
Benin_urban_plot <- Benin_urban_plot + 
  theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
`Burkina Faso_urban_plot` <- `Burkina Faso_urban_plot` + 
  theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
Cameroon_urban_plot <- Cameroon_urban_plot + 
  theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
`Cote d'Ivoire_urban_plot` <- `Cote d'Ivoire_urban_plot` + 
  theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
Mali_urban_plot <- Mali_urban_plot + 
  theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
Mozambique_urban_plot <- Mozambique_urban_plot + 
  theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())

urban_country_line_plots <- grid.arrange(Benin_urban_plot, `Burkina Faso_urban_plot`, Cameroon_urban_plot, `Cote d'Ivoire_urban_plot`, Mali_urban_plot, Mozambique_urban_plot,
                                         nrow = 2, ncol = 3)

# combine the plots and legend
urban_country_final_line_plots <- grid.arrange(
  urban_country_line_plots,
  legend,
  ncol = 2,  # legend in the second column, plots in the first column
  widths = c(10, 2),  # adjust the width to give more space to the plots
  top = textGrob(
    "Urban",
    gp = gpar(fontsize = 16, fontface = "bold", hjust = 0.5)  # center the title
  ),
  bottom = textGrob(
    "Survey Year",
    gp = gpar(fontsize = 12, hjust = 0.5)  # center the title
  )
  # left = textGrob(
  #   "Percent (%)", 
  #   rot = 90, 
  #   gp = gpar(fontsize = 12)
  # )
)

# display the combined plot and save as .png and .pdf
ggsave(paste0(FigDir, "/png_figures/", Sys.Date(),"_urban_country_final_line_plots.png"), urban_country_final_line_plots, width = 10, height = 10) 
ggsave(paste0(FigDir, "/pdf_figures/", Sys.Date(),"_urban_country_area_plots.pdf"), urban_country_final_line_plots, width = 10, height = 8) 

## -----------------------------------------------------------------------------------------------------------------------------------------
### Combine Malaria + Net Use + Number of Nets Distributed Urban Plots 
## -----------------------------------------------------------------------------------------------------------------------------------------

# extract the combined malaria/net use legend
num_nets_legend <- get_only_legend(Benin_urban_num_nets_plot) 

# remove legends from all plots
Benin_urban_num_nets_plot <- Benin_urban_num_nets_plot + 
  theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
`Burkina Faso_urban_num_nets_plot` <- `Burkina Faso_urban_num_nets_plot` + 
  theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
Cameroon_urban_num_nets_plot <- Cameroon_urban_num_nets_plot + 
  theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
`Cote d'Ivoire_urban_num_nets_plot` <- `Cote d'Ivoire_urban_num_nets_plot` + 
  theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
Mali_urban_num_nets_plot <- Mali_urban_num_nets_plot + 
  theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
Mozambique_urban_num_nets_plot <- Mozambique_urban_num_nets_plot + 
  theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())

urban_country_num_nets_line_plots <- grid.arrange(Benin_urban_num_nets_plot, `Burkina Faso_urban_num_nets_plot`, Cameroon_urban_num_nets_plot, `Cote d'Ivoire_urban_num_nets_plot`, Mali_urban_num_nets_plot, Mozambique_urban_num_nets_plot,
                                         nrow = 2, ncol = 3)

# combine the plots and legend
urban_country_final_num_nets_line_plots <- grid.arrange(
  urban_country_num_nets_line_plots,
  num_nets_legend,
  ncol = 2,  # legend in the second column, plots in the first column
  widths = c(10, 2),  # adjust the width to give more space to the plots
  top = textGrob(
    "Urban",
    gp = gpar(fontsize = 16, fontface = "bold", hjust = 0.5)  # center the title
  ),
  bottom = textGrob(
    "Survey Year",
    gp = gpar(fontsize = 12, hjust = 0.5)  # center the title
  )
  # left = textGrob(
  #   "Percent (%)", 
  #   rot = 90, 
  #   gp = gpar(fontsize = 12)
  # )
)

# display the combined plot and save as .png and .pdf
ggsave(paste0(FigDir, "/png_figures/", Sys.Date(),"_urban_country_final_num_nets_line_plots.png"), urban_country_final_num_nets_line_plots, width = 10, height = 10) 
ggsave(paste0(FigDir, "/pdf_figures/", Sys.Date(),"_urban_country_final_num_nets_line_plots.pdf"), urban_country_final_num_nets_line_plots, width = 10, height = 8) 

## -----------------------------------------------------------------------------------------------------------------------------------------
### Combine Malaria Urban Plots 
## -----------------------------------------------------------------------------------------------------------------------------------------

# extract the malaria legend
legend_malaria <- get_only_legend(Benin_malaria_urban_plot) 

# remove legends from all plots
Benin_malaria_urban_plot <- Benin_malaria_urban_plot + theme(legend.position = "none")
`Burkina Faso_malaria_urban_plot` <- `Burkina Faso_malaria_urban_plot` + theme(legend.position = "none") 
Cameroon_malaria_urban_plot <- Cameroon_malaria_urban_plot + theme(legend.position = "none")
`Cote d'Ivoire_malaria_urban_plot` <- `Cote d'Ivoire_malaria_urban_plot` + theme(legend.position = "none") 
Mali_malaria_urban_plot <- Mali_malaria_urban_plot + theme(legend.position = "none")
Mozambique_malaria_urban_plot <- Mozambique_malaria_urban_plot + theme(legend.position = "none")

urban_malaria_line_plots <- grid.arrange(Benin_malaria_urban_plot, `Burkina Faso_malaria_urban_plot`, Cameroon_malaria_urban_plot, `Cote d'Ivoire_malaria_urban_plot`, Mali_malaria_urban_plot, Mozambique_malaria_urban_plot,
                                         nrow = 2,
                                         ncol = 3
)

# combine the plots and legend
urban_malaria_final_line_plots <- grid.arrange(
  urban_malaria_line_plots,
  legend_malaria,
  ncol = 2,  # legend in the second column, plots in the first column
  widths = c(10, 4),  # adjust the width to give more space to the plots
  top = textGrob(
    "Malaria Test Positivity Rate by Country: Urban",
    gp = gpar(fontsize = 16, fontface = "bold", hjust = 0.5)  # center the title
  )
  # left = textGrob(
  #   "Percent (%)", 
  #   rot = 90, 
  #   gp = gpar(fontsize = 12)
  # )
)

# display the combined plot and save as .png
ggsave(paste0(FigDir, "/png_figures/", Sys.Date(),"_CI_urban_malaria_final_line_plots.png"), urban_malaria_final_line_plots, width = 10, height = 10) 

## -----------------------------------------------------------------------------------------------------------------------------------------
### Combine Net Use Urban Plots 
## -----------------------------------------------------------------------------------------------------------------------------------------

# extract the net use legend
legend_net <- get_only_legend(Benin_net_urban_plot) 

# remove legends from all plots
Benin_net_urban_plot <- Benin_net_urban_plot + theme(legend.position = "none")
`Burkina Faso_net_urban_plot` <- `Burkina Faso_net_urban_plot` + theme(legend.position = "none") 
Cameroon_net_urban_plot <- Cameroon_net_urban_plot + theme(legend.position = "none")
`Cote d'Ivoire_net_urban_plot` <- `Cote d'Ivoire_net_urban_plot` + theme(legend.position = "none") 
Mali_net_urban_plot <- Mali_net_urban_plot + theme(legend.position = "none")
Mozambique_net_urban_plot <- Mozambique_net_urban_plot + theme(legend.position = "none")

urban_net_line_plots <- grid.arrange(Benin_net_urban_plot, `Burkina Faso_net_urban_plot`, Cameroon_net_urban_plot, `Cote d'Ivoire_net_urban_plot`, Mali_net_urban_plot, Mozambique_net_urban_plot,
                                     nrow = 2,
                                     ncol = 3
)

# combine the plots and legend
urban_net_final_line_plots <- grid.arrange(
  urban_net_line_plots,
  legend_net,
  ncol = 2,  # legend in the second column, plots in the first column
  widths = c(10, 2),  # adjust the width to give more space to the plots
  top = textGrob(
    "Net Use Rate by Country: Urban",
    gp = gpar(fontsize = 16, fontface = "bold", hjust = 0.5)  # center the title
  )
  # left = textGrob(
  #   "Percent (%)", 
  #   rot = 90, 
  #   gp = gpar(fontsize = 12)
  # )
)

# display the combined plot and save as .png
ggsave(paste0(FigDir, "/png_figures/", Sys.Date(),"_urban_net_final_line_plots.png"), urban_net_final_line_plots, width = 10, height = 10) 

## =========================================================================================================================================
### RURAL LINE PLOTS: 1) Malaria/net use combined, 2) Malaria only, 3) Net Use only 
## =========================================================================================================================================

# RURAL: loop through each country
for (country_name in countries) {
  
  # get malaria TPR data for the current country
  rural_country_data_malaria_ci <- rural_trend_malaria_ci_long %>%
    filter(country == country_name) %>%
    mutate(data_type = "Malaria TPR")
  
  # get net use data for the current country
  rural_country_data_net_ci <- rural_trend_net_ci_long %>%
    filter(country == country_name) %>%
    mutate(data_type = "Net Use Rate")
  
  # combine the two datasets
  rural_combined_data <- bind_rows(rural_country_data_malaria_ci, rural_country_data_net_ci)
  
  # create a new data frame for net distribution with a legend entry
  net_data_country_num_legend <- net_data_country %>%
    mutate(data_type = "Nets Distributed")  # Add a column for the legend entry
  
  # # create plot a: combined malaria tpr and net use ---------------------------------------------------------------------------------
  # rural_comb_plot <- ggplot(rural_combined_data, aes(x = year, y = percent, linetype = data_type, color = household_type)) +
  #   geom_line(size = 1) +  # Line for each group
  #   geom_point(size = 3, alpha = 0.7) +  # Add points for each data point
  #   geom_errorbar(data = rural_combined_data %>% filter(data_type == "Malaria TPR"), 
  #                 aes(ymin = agric_lower_ci, ymax = agric_upper_ci), 
  #                 width = 0.3, color = "#5560AB", alpha = 0.7) +  # Confidence interval for Malaria TPR (agricultural)
  #   geom_errorbar(data = rural_combined_data %>% filter(data_type == "Malaria TPR"), 
  #                 aes(ymin = non_agric_lower_ci, ymax = non_agric_upper_ci), 
  #                 width = 0.3, color = "#FAAF43", alpha = 0.7) +  # Confidence interval for Malaria TPR (non-agricultural)
  #   geom_errorbar(data = rural_combined_data %>% filter(data_type == "Net Use Rate"), 
  #                 aes(ymin = agric_lower_ci, ymax = agric_upper_ci), 
  #                 width = 0.3, color = "#5560AB", alpha = 0.7) +  # Confidence interval for Net Use Rate (agricultural)
  #   geom_errorbar(data = rural_combined_data %>% filter(data_type == "Net Use Rate"), 
  #                 aes(ymin = non_agric_lower_ci, ymax = non_agric_upper_ci), 
  #                 width = 0.3, color = "#FAAF43", alpha = 0.7) +  # Confidence interval for Net Use Rate (non-agricultural)
  #   theme_manuscript() +
  #   labs(
  #     x = "Survey Year", y = "Percent (%)", title = paste(country_name), color = "Household Type", linetype = "Data Type"  # Legend titles
  #   ) +
  #   theme(
  #     axis.text.y = element_text(size = 12),
  #     axis.text.x = element_text(size = 10),
  #     plot.title = element_text(hjust = 0.5, size = 14)
  #   ) +
  #   scale_color_manual(values = c("agric_percent" = "#5560AB", "non_agric_percent" = "#FAAF43"),  # Agricultural = blue, Non-agricultural = yellow
  #                      labels = c("Agricultural", "Non-Agricultural")) +  # Custom labels for the legend
  #   scale_linetype_manual(values = c("Malaria TPR" = "solid", "Net Use Rate" = "dashed")) +  # Line type: solid for Malaria TPR, dashed for Net Use
  #   guides(linetype = guide_legend(override.aes = list(color = "black"))) +  # Set legend line color to black
  #   scale_x_continuous(limits = c(2009.5, 2022.5), breaks = seq(2010, 2022, by = 3)) +  # Set limits and breaks for x-axis
  #   scale_y_continuous(limits = c(0, 100))
  # 
  # # save the combined plot with the country name
  # assign(paste0(country_name, "_rural_plot"), rural_comb_plot)
  # 
  # # add the combined plot to the list
  # country_plots[[country_name]] <- rural_comb_plot
  
  # ------------------------------------------------------------------------------------------------------------------------------
  # AREA PLOTS WITH PERCENT CHANGE IN NETS DISTRIBUTED (COMPARED TO FIRST YEAR)
  
  # PLOT: TPR AND NET USE AND PERCENT CHANGE IN NETS DISTRIBUTED FROM PRIOR YEAR
  rural_area_plot <- ggplot() +
    
    # area plot for percent change in net distribution
    geom_area(data = net_data_country_legend, 
              aes(x = Year, y = scales::rescale(pct_change_nets, to = c(0, 100))),
              fill = "lightgrey", alpha = 0.5, position = 'identity') +
    
    # line plot for malaria TPR and net use
    geom_line(data = rural_combined_data, 
              aes(x = year, y = percent, linetype = data_type, color = household_type), size = 1) +
    
    # points for malaria TPR and net use
    geom_point(data = rural_combined_data, 
               aes(x = year, y = percent, color = household_type), size = 3, alpha = 0.7) +
    
    # add confidence intervals for malaria TPR (agricultural)
    geom_errorbar(data = rural_combined_data %>% filter(data_type == "Malaria TPR", household_type == "agric_percent"),
                  aes(x = year, ymin = agric_lower_ci, ymax = agric_upper_ci),
                  width = 0.3, color = "#5560AB", alpha = 0.7) +
    
    # add confidence intervals for malaria TPR (non-agricultural)
    geom_errorbar(data = rural_combined_data %>% filter(data_type == "Malaria TPR", household_type == "non_agric_percent"),
                  aes(x = year, ymin = non_agric_lower_ci, ymax = non_agric_upper_ci),
                  width = 0.3, color = "#FAAF43", alpha = 0.7) +
    
    # add confidence intervals for net use rate (agricultural)
    geom_errorbar(data = rural_combined_data %>% filter(data_type == "Net Use Rate", household_type == "agric_percent"),
                  aes(x = year, ymin = agric_lower_ci, ymax = agric_upper_ci),
                  width = 0.3, color = "#5560AB", alpha = 0.7) +
    
    # add confidence intervals for net use rate (non-agricultural)
    geom_errorbar(data = rural_combined_data %>% filter(data_type == "Net Use Rate", household_type == "non_agric_percent"),
                  aes(x = year, ymin = non_agric_lower_ci, ymax = non_agric_upper_ci),
                  width = 0.3, color = "#FAAF43", alpha = 0.7) +
    
    # set up primary and secondary y-axes
    scale_y_continuous(
      name = "Malaria TPR/Net Use Rate (%)",  # Primary y-axis label
      limits = c(0, 100),  # Primary y-axis scale from 0 to 100
      sec.axis = sec_axis(
        trans = ~ scales::rescale(., from = c(0, 100), to = c(-110, 16000)),  # Set secondary y-axis to custom range
        name = "Percentage Change in Nets Distributed (%)"
      )
    ) +
    
    # add theme, labels, and scales
    theme_manuscript() +
    labs(
      x = "Survey Year", y = "Percent (%)", title = paste(country_name), color = "Household Type", linetype = "Data Type"
    ) +
    theme(
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, size = 14)
    ) +
    scale_color_manual(values = c("agric_percent" = "#5560AB", "non_agric_percent" = "#FAAF43"),  # Agricultural = blue, Non-agricultural = yellow
                       labels = c("Agricultural", "Non-Agricultural")) +  # Custom labels for the legend
    scale_linetype_manual(values = c("Malaria TPR" = "solid", "Net Use Rate" = "dashed", "Year Percentage Change in Nets Distributed" = "dotted"), 
                          labels = c("Malaria TPR", "Net Use Rate", "Percentage Change\n in Nets Distributed"))  # Update labels
  
  # save the combined plot with the country name
  assign(paste0(country_name, "_rural_plot"), rural_area_plot)
  
  # add the combined plot to the list
  country_plots[[country_name]] <- rural_area_plot
  
  # FACET: make plot b: malaria tpr by country -----------------------------------------------------------------------------------
  rural_malaria_plot <- ggplot(rural_country_data_malaria_ci, aes(x = year, y = percent, color = household_type)) +
    
    # add confidence interval error bars with color by household_type
    geom_errorbar(
      aes(ymin = agric_lower_ci, ymax = agric_upper_ci),
      data = subset(rural_country_data_malaria_ci, household_type == "agric_percent"),
      width = 0.2, color = "#FAAF43", alpha = 0.7
    ) +
    geom_errorbar(
      aes(ymin = non_agric_lower_ci, ymax = non_agric_upper_ci),
      data = subset(rural_country_data_malaria_ci, household_type == "non_agric_percent"),
      width = 0.2, color = "#5560AB", alpha = 0.7
    ) +
    geom_line(size = 1) +
    geom_point(size = 3, alpha = 0.7, show.legend = TRUE) +
    theme_manuscript() +
    labs(
      x = "Survey Year", y = "Percent (%)", title = paste(country_name), color = "Household Type"
    ) +
    theme(
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, size = 14)
    ) +
    scale_color_manual(
      values = c("agric_percent" = "#FAAF43", "non_agric_percent" = "#5560AB"),  # Agricultural = green, Non-agricultural = orange
      labels = c("Agricultural", "Non-Agricultural")  # Custom labels for the legend
    ) +
    scale_x_continuous(limits = c(2010, 2022), breaks = seq(2010, 2022, by = 3)) +  # Set limits and breaks for x-axis
    scale_y_continuous(limits = c(0, 100))
  
  # save the malaria plot with the country name
  assign(paste0(country_name, "_malaria_rural_plot"), rural_malaria_plot)
  
  # FACET: c) make plot for just net use by country -------------------------------------------------------------------------------------
  rural_net_plot <- ggplot(rural_country_data_net_ci, aes(x = year, y = percent, color = household_type)) +
    geom_errorbar( # add confidence interval error bars with color by household_type
      aes(ymin = agric_lower_ci, ymax = agric_upper_ci),
      data = subset(rural_country_data_net_ci, household_type == "agric_percent"),
      width = 0.2, color = "#FAAF43", alpha = 0.7
    ) +
    geom_errorbar(
      aes(ymin = non_agric_lower_ci, ymax = non_agric_upper_ci),
      data = subset(rural_country_data_net_ci, household_type == "non_agric_percent"),
      width = 0.2, color = "#5560AB", alpha = 0.7
    ) +
    # plot the lines and points with color by household_type
    geom_line(size = 1) +
    geom_point(size = 3, alpha = 0.7, show.legend = TRUE) +  # Add points for each data point, with legend
    theme_manuscript() +
    labs(
      x = "Survey Year",
      y = "Percent (%)",
      title = paste(country_name),
      color = "Household Type"  # Update the legend title to represent colors for household_type
    ) +
    theme(
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, size = 14)
    ) +
    scale_color_manual(
      values = c("agric_percent" = "#FAAF43", "non_agric_percent" = "#5560AB"),  # Agricultural = green, Non-agricultural = orange
      labels = c("Agricultural", "Non-Agricultural")  # Custom labels for the legend
    ) +
    scale_x_continuous(limits = c(2010, 2022), breaks = seq(2010, 2022, by = 3)) +  # Set limits and breaks for x-axis
    scale_y_continuous(limits = c(0, 100))
  
  # save the net plot with the country name
  assign(paste0(country_name, "_net_rural_plot"), rural_net_plot)
}

## -----------------------------------------------------------------------------------------------------------------------------------------
### Combine Malaria + Net Use Rural Plots 
## -----------------------------------------------------------------------------------------------------------------------------------------

# remove legends from all plots
Benin_rural_plot <- Benin_rural_plot + 
  theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
`Burkina Faso_rural_plot` <- `Burkina Faso_rural_plot` + 
  theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
Cameroon_rural_plot <- Cameroon_rural_plot + 
  theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
`Cote d'Ivoire_rural_plot` <- `Cote d'Ivoire_rural_plot` + 
  theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
Mali_rural_plot <- Mali_rural_plot + 
  theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())
Mozambique_rural_plot <- Mozambique_rural_plot + 
  theme(legend.position = "none", axis.title.y = element_blank(), axis.title.x = element_blank())

rural_country_line_plots <- grid.arrange(Benin_rural_plot, `Burkina Faso_rural_plot`, Cameroon_rural_plot, `Cote d'Ivoire_rural_plot`, Mali_rural_plot, Mozambique_rural_plot,
                                         nrow = 2, ncol = 3)

# combine the plots and legend
rural_country_final_line_plots <- grid.arrange(
  rural_country_line_plots,
  legend,
  ncol = 2,  # legend in the second column, plots in the first column
  widths = c(10, 2),  # adjust the width to give more space to the plots
  top = textGrob(
    "Rural",
    gp = gpar(fontsize = 16, fontface = "bold", hjust = 0.5)  # center the title
  ),
  bottom = textGrob(
    "Survey Year",
    gp = gpar(fontsize = 12, hjust = 0.5)  # center the title
  )
  # left = textGrob(
  #   "Percent (%)", 
  #   rot = 90, 
  #   gp = gpar(fontsize = 12)
  # )
)

# display the combined plot and save as .png
ggsave(paste0(FigDir, "/png_figures/", Sys.Date(),"_rural_country_final_line_plots.png"), rural_country_final_line_plots, width = 10, height = 10) 
ggsave(paste0(FigDir, "/pdf_figures/", Sys.Date(),"_rural_country_area_plots.pdf"), rural_country_final_line_plots, width = 10, height = 8) 

## -----------------------------------------------------------------------------------------------------------------------------------------
### Combine Malaria Rural Plots 
## -----------------------------------------------------------------------------------------------------------------------------------------

# remove legends from all plots
Benin_malaria_rural_plot <- Benin_malaria_rural_plot + theme(legend.position = "none")
`Burkina Faso_malaria_rural_plot` <- `Burkina Faso_malaria_rural_plot` + theme(legend.position = "none") 
Cameroon_malaria_rural_plot <- Cameroon_malaria_rural_plot + theme(legend.position = "none")
`Cote d'Ivoire_malaria_rural_plot` <- `Cote d'Ivoire_malaria_rural_plot` + theme(legend.position = "none") 
Mali_malaria_rural_plot <- Mali_malaria_rural_plot + theme(legend.position = "none")
Mozambique_malaria_rural_plot <- Mozambique_malaria_rural_plot + theme(legend.position = "none")

rural_malaria_line_plots <- grid.arrange(Benin_malaria_rural_plot, `Burkina Faso_malaria_rural_plot`, Cameroon_malaria_rural_plot, `Cote d'Ivoire_malaria_rural_plot`, Mali_malaria_rural_plot, Mozambique_malaria_rural_plot,
                                         nrow = 2,
                                         ncol = 3
)

# combine the plots and legend
rural_malaria_final_line_plots <- grid.arrange(
  rural_malaria_line_plots,
  legend_malaria,
  ncol = 2,  # legend in the second column, plots in the first column
  widths = c(10, 4),  # adjust the width to give more space to the plots
  top = textGrob(
    "Malaria Test Positivity Rate by Country: Rural",
    gp = gpar(fontsize = 16, fontface = "bold", hjust = 0.5)  # center the title
  )
  # left = textGrob(
  #   "Percent (%)", 
  #   rot = 90, 
  #   gp = gpar(fontsize = 12)
  # )
)

# display the combined plot and save as .png
ggsave(paste0(FigDir, "/png_figures/", Sys.Date(),"_CI_rural_malaria_final_line_plots.png"), rural_malaria_final_line_plots, width = 10, height = 10) 

## -----------------------------------------------------------------------------------------------------------------------------------------
### Combine Net Use Rural Plots 
## -----------------------------------------------------------------------------------------------------------------------------------------

# remove legends from all plots
Benin_net_rural_plot <- Benin_net_rural_plot + theme(legend.position = "none")
`Burkina Faso_net_rural_plot` <- `Burkina Faso_net_rural_plot` + theme(legend.position = "none") 
Cameroon_net_rural_plot <- Cameroon_net_rural_plot + theme(legend.position = "none")
`Cote d'Ivoire_net_rural_plot` <- `Cote d'Ivoire_net_rural_plot` + theme(legend.position = "none") 
Mali_net_rural_plot <- Mali_net_rural_plot + theme(legend.position = "none")
Mozambique_net_rural_plot <- Mozambique_net_rural_plot + theme(legend.position = "none")

rural_net_line_plots <- grid.arrange(Benin_net_rural_plot, `Burkina Faso_net_rural_plot`, Cameroon_net_rural_plot, `Cote d'Ivoire_net_rural_plot`, Mali_net_rural_plot, Mozambique_net_rural_plot,
                                     nrow = 2,
                                     ncol = 3
)

# combine the plots and legend
rural_net_final_line_plots <- grid.arrange(
  rural_net_line_plots,
  legend_net,
  ncol = 2,  # legend in the second column, plots in the first column
  widths = c(10, 2),  # adjust the width to give more space to the plots
  top = textGrob(
    "Net Use Rate by Country: Rural",
    gp = gpar(fontsize = 16, fontface = "bold", hjust = 0.5)  # center the title
  )
  # left = textGrob(
  #   "Percent (%)", 
  #   rot = 90, 
  #   gp = gpar(fontsize = 12)
  # )
)

# display the combined plot and save as .png
ggsave(paste0(FigDir, "/png_figures/", Sys.Date(),"_rural_net_final_line_plots.png"), rural_net_final_line_plots, width = 10, height = 10) 

