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

## -----------------------------------------------------------------------------------------------------------------------------------------
### URBAN MALARIA: Difference-in-Differences Analysis + Scatterplot
## -----------------------------------------------------------------------------------------------------------------------------------------

# read in the trend analysis dataset for malaria test positivity
malaria_trend_data <- read_csv(file.path(PopDir, "analysis_dat/240606_urban_df_for_analysis_trend_malaria_chilo_created.csv"))

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

# rural analysis for malaria: reshape and clean data
# exclude Tanzania as test positivity rate is less than 1%
# exclude Mozambique 2011 as MZ 2015 and MZ 2022-23 are the two most recent surveys
rural_trend_malaria <- malaria_trend_data  %>%  
  filter(title == "Rural", cntryId...9 != "TZ", country_year.x...2 != "Mozambique 2011") %>% # filter relevant data for rural areas
  select(country_year.x...2, cntryId...5, agric_percent, non_agric_percent, diff_val_malaria, cntryId...12, id) %>% # select specific columns
  rename(country_year = country_year.x...2, country_id = cntryId...5, country_surveyyears = cntryId...12) %>% # rename columns for clarity
  mutate(id = if_else(country_year == "Mozambique 2015", "Preceding survey", id)) # update id for specific condition

# recode "Mozambique 2011 vs 2015" to "Mozambique 2015 Vs. 2022 - 2023"
rural_trend_malaria$country_surveyyears[rural_trend_malaria$country_surveyyears == "Mozambique 2011 Vs. 2015"] <- "Mozambique 2015 Vs. 2022 - 23"

# hardcode test positivity values for Mozambique 2022 based on above analysis
df <- data.frame(country_year = "Mozambique 2022 - 23", agric_percent = 27, non_agric_percent = 9, diff_val_malaria = 27 - 9, id = "Most recent survey", country_id  = "MZ", country_surveyyears = "Mozambique 2015 Vs. 2022 - 23")

# join the trend data with hardcoded values
rural_trend_malaria_updated <- rbind(rural_trend_malaria, df)

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

# read in the trend analysis dataset for net use
net_trend_data <- read_csv(file.path(PopDir, "analysis_dat/240606_urban_df_for_analysis_trend_net_chilo_created.csv"))

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

# rural analysis for net use: reshape and clean data
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

color_list <- c("#006400","#4B0082", "#DDA0DD","#1E90FF","#C71585","#FF6347")

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

# function to extract the legend
get_only_legend <- function(urban_combined_plot) { 
  
  # get tabular interpretation of plot 
  plot_table <- ggplot_gtable(ggplot_build(urban_combined_plot))  
  
  #  mark only legend in plot 
  legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box")  
  
  # extract legend 
  legend <- plot_table$grobs[[legend_plot]] 
  
  # return legend 
  return(legend)  
}

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

## -----------------------------------------------------------------------------------------------------------------------------------------
### Fit Line
# Fit a line to the existing scatter plot to examine the trend visually
# Calculate and report the correlation coefficient to quantify the relationship between malaria test positivity rates (TPR) and net use.
## -----------------------------------------------------------------------------------------------------------------------------------------


## -----------------------------------------------------------------------------------------------------------------------------------------
### Generate a line plot for Malaria Test Positivity Rate (TPR) alone
# •	X-axis: Year of the survey.
# •	Y-axis: Malaria test positivity rate (TPR).
# •	Color the lines by country and include both agric and non-agric HHs for each survey.
## -----------------------------------------------------------------------------------------------------------------------------------------

# URBAN PLOT

# create new variable with just country name and just survey year (first year in survey range if survey spans two years)
urban_trend_malaria_updated <- urban_trend_malaria_updated %>%
  mutate(
    # extract the country name (everything before the first numeric character)
    country = str_extract(country_year, "^[^0-9]+") %>% str_trim(),
    
    # extract the year (the first numeric part found in the string)
    year = str_extract(country_year, "\\d{4}") %>% as.numeric()
  )


# reshape the data to long format (necessary to plot both agric_percent and non_agric_percent)
urban_trend_malaria_long <- urban_trend_malaria_updated %>%
  pivot_longer(
    cols = c(agric_percent, non_agric_percent),
    names_to = "household_type",
    values_to = "percent"
  )

# create the line plot
urban_malaria_tpr_plot <- ggplot(urban_trend_malaria_long, aes(x = year, y = percent, color = country, linetype = household_type)) +
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
  scale_x_continuous(breaks = seq(min(urban_trend_malaria_long$year), max(urban_trend_malaria_long$year), by = 2))  # tick marks every 2 years

urban_malaria_tpr_plot

# RURAL PLOT

# create new variable with just country name and just survey year (first year in survey range if survey spans two years)
rural_trend_malaria_updated <- rural_trend_malaria_updated %>%
  mutate(
    # extract the country name (everything before the first numeric character)
    country = str_extract(country_year, "^[^0-9]+") %>% str_trim(),
    
    # extract the year (the first numeric part found in the string)
    year = str_extract(country_year, "\\d{4}") %>% as.numeric()
  )

# reshape the data to long format (necessary to plot both agric_percent and non_agric_percent)
rural_trend_malaria_long <- rural_trend_malaria_updated %>%
  pivot_longer(
    cols = c(agric_percent, non_agric_percent),
    names_to = "household_type",
    values_to = "percent"
  )

# create the line plot
rural_malaria_tpr_plot <- ggplot(rural_trend_malaria_long, aes(x = year, y = percent, color = country, linetype = household_type)) +
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
  scale_x_continuous(breaks = seq(min(urban_trend_malaria_long$year), max(urban_trend_malaria_long$year), by = 2))  # tick marks every 2 years

rural_malaria_tpr_plot

#### COMBINE THE PLOTS

# remove individual titles and x-axis labels from the urban and rural plots
urban_malaria_tpr_plot <- urban_malaria_tpr_plot + 
  labs(title = NULL, subtitle = "Urban", x = NULL, y = NULL) + 
  theme(plot.subtitle = element_text(hjust = 0.5, size = 12)) 
rural_malaria_tpr_plot <- rural_malaria_tpr_plot + 
  labs(title = NULL, subtitle = "Rural", y = NULL) + 
  theme(plot.subtitle = element_text(hjust = 0.5, size = 12)) 

# function to extract the legend
get_only_legend <- function(urban_malaria_tpr_plot) { 
  
  # get tabular interpretation of plot 
  plot_table <- ggplot_gtable(ggplot_build(urban_malaria_tpr_plot))  
  
  #  mark only legend in plot 
  legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box")  
  
  # extract legend 
  legend <- plot_table$grobs[[legend_plot]] 
  
  # return legend 
  return(legend)  
}

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

# URBAN PLOT

# create new variable with just country name and just survey year (first year in survey range if survey spans two years)
urban_trend_net_updated <- urban_trend_net_updated %>%
  mutate(
    # extract the country name (everything before the first numeric character)
    country = str_extract(country_year, "^[^0-9]+") %>% str_trim(),
    
    # extract the year (the first numeric part found in the string)
    year = str_extract(country_year, "\\d{4}") %>% as.numeric()
  )


# reshape the data to long format (necessary to plot both agric_percent and non_agric_percent)
urban_trend_net_long <- urban_trend_net_updated %>%
  pivot_longer(
    cols = c(agric_percent, non_agric_percent),
    names_to = "household_type",
    values_to = "percent"
  )

# create the line plot
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

# RURAL PLOT

# create new variable with just country name and just survey year (first year in survey range if survey spans two years)
rural_trend_net_updated <- rural_trend_net_updated %>%
  mutate(
    # extract the country name (everything before the first numeric character)
    country = str_extract(country_year, "^[^0-9]+") %>% str_trim(),
    
    # extract the year (the first numeric part found in the string)
    year = str_extract(country_year, "\\d{4}") %>% as.numeric()
  )


# reshape the data to long format (necessary to plot both agric_percent and non_agric_percent)
rural_trend_net_long <- rural_trend_net_updated %>%
  pivot_longer(
    cols = c(agric_percent, non_agric_percent),
    names_to = "household_type",
    values_to = "percent"
  )

# create the line plot
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

# function to extract the legend
get_only_legend <- function(urban_net_plot) { 
  
  # get tabular interpretation of plot 
  plot_table <- ggplot_gtable(ggplot_build(urban_net_plot))  
  
  #  mark only legend in plot 
  legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box")  
  
  # extract legend 
  legend <- plot_table$grobs[[legend_plot]] 
  
  # return legend 
  return(legend)  
}

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

## -----------------------------------------------------------------------------------------------------------------------------------------
### Create Separate Line Plots for each Country - Both Malaria TPR and Net Use
## -----------------------------------------------------------------------------------------------------------------------------------------

# get a list of unique countries
countries <- unique(urban_trend_malaria_long$country)

# define an empty list to store the plots
country_plots <- list()

# URBAN: loop through each country
for (country_name in countries) {
  
  # get malaria TPR data for the current country
  urban_country_data_malaria <- urban_trend_malaria_long %>%
    filter(country == country_name) %>%
    mutate(data_type = "Malaria TPR")  # Add a column to distinguish the data type
  
  # get net use data for the current country
  urban_country_data_net <- urban_trend_net_long %>%
    filter(country == country_name) %>%
    mutate(data_type = "Net Use Rate")  # Add a column to distinguish the data type
  
  # combine the two datasets
  urban_combined_data <- bind_rows(urban_country_data_malaria, urban_country_data_net)
  
  # create the plot
  plot <- ggplot(urban_combined_data, aes(x = year, y = percent, linetype = household_type, color = data_type)) +
    geom_line(size = 1) +  # Line for each group
    geom_point(size = 3, alpha = 0.7) +  # Add points for each data point
    theme_minimal() +
    labs(
      x = "Survey Year",
      y = "Percent (%)",
      title = paste(country_name),
      color = "Data Type"  # Legend title for color
    ) +
    theme(
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, size = 14)
    ) +
    scale_color_manual(values = c("Malaria TPR" = "#5560AB", "Net Use Rate" = "#FAAF43")) +  # Use different colors for malaria and net use
    scale_linetype_manual(
      values = c("solid", "dashed"),
      labels = c("Agricultural", "Non-Agricultural")
    ) +  # Different line types for agric/non-agric HHs
    guides(linetype = guide_legend(title = "Household Type")) +  # Add legend title for line types
    scale_x_continuous(breaks = seq(min(urban_combined_data$year), max(urban_combined_data$year), by = 3))  # Tick marks every 3 years
  
  # save the plot and with the country name
  assign(paste0(country_name, "_urban_plot"), plot)
  
  # add the plot to the list
  country_plots[[country_name]] <- plot
}

#### COMBINE THE PLOTS

# function to extract the legend
get_only_legend <- function(Benin_urban_plot) { 
  
  # get tabular interpretation of plot 
  plot_table <- ggplot_gtable(ggplot_build(Benin_urban_plot))  
  
  #  mark only legend in plot 
  legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box")  
  
  # extract legend 
  legend <- plot_table$grobs[[legend_plot]] 
  
  # return legend 
  return(legend)  
}

legend <- get_only_legend(Benin_urban_plot) 

# remove legends from all plots
Benin_urban_plot <- Benin_urban_plot + theme(legend.position = "none")
`Burkina Faso_urban_plot` <- `Burkina Faso_urban_plot` + theme(legend.position = "none") 
Cameroon_urban_plot <- Cameroon_urban_plot + theme(legend.position = "none")
`Cote d'Ivoire_urban_plot` <- `Cote d'Ivoire_urban_plot` + theme(legend.position = "none") 
Mali_urban_plot <- Mali_urban_plot + theme(legend.position = "none")
Mozambique_urban_plot <- Mozambique_urban_plot + theme(legend.position = "none")

urban_country_line_plots <- grid.arrange(Benin_urban_plot, `Burkina Faso_urban_plot`, Cameroon_urban_plot, `Cote d'Ivoire_urban_plot`, Mali_urban_plot, Mozambique_urban_plot,
                                         nrow = 2,
                                         ncol = 3
)

# combine the plots and legend
urban_country_final_line_plots <- grid.arrange(
  urban_country_line_plots,
  legend,
  ncol = 2,  # legend in the second column, plots in the first column
  widths = c(10, 2),  # adjust the width to give more space to the plots
  top = textGrob(
    "Malaria Test Positivity Rate and Net Use by Country: Urban",
    gp = gpar(fontsize = 16, fontface = "bold", hjust = 0.5)  # center the title
  ),
  left = textGrob(
    "Percent (%)", 
    rot = 90, 
    gp = gpar(fontsize = 12)
  )
)

# display the combined plot and save as .png
ggsave(paste0(FigDir, "/png_figures/", Sys.Date(),"_urban_country_final_line_plots.png"), urban_country_final_line_plots, width = 10, height = 10) 


### RURAL

# RURAL: loop through each country
for (country_name in countries) {
  
  # get malaria TPR data for the current country
  rural_country_data_malaria <- rural_trend_malaria_long %>%
    filter(country == country_name) %>%
    mutate(data_type = "Malaria TPR")  # Add a column to distinguish the data type
  
  # get net use data for the current country
  rural_country_data_net <- rural_trend_net_long %>%
    filter(country == country_name) %>%
    mutate(data_type = "Net Use Rate")  # Add a column to distinguish the data type
  
  # combine the two datasets
  rural_combined_data <- bind_rows(rural_country_data_malaria, rural_country_data_net)
  
  # create the plot
  plot <- ggplot(rural_combined_data, aes(x = year, y = percent, linetype = household_type, color = data_type)) +
    geom_line(size = 1) +  # Line for each group
    geom_point(size = 3, alpha = 0.7) +  # Add points for each data point
    theme_minimal() +
    labs(
      x = "Survey Year",
      y = "Percent (%)",
      title = paste(country_name),
      color = "Data Type"  # Legend title for color
    ) +
    theme(
      axis.text.y = element_text(size = 12),
      axis.text.x = element_text(size = 10),
      plot.title = element_text(hjust = 0.5, size = 14)
    ) +
    scale_color_manual(values = c("Malaria TPR" = "#5560AB", "Net Use Rate" = "#FAAF43")) +  # Use different colors for malaria and net use
    scale_linetype_manual(
      values = c("solid", "dashed"),
      labels = c("Agricultural", "Non-Agricultural")
    ) +  # Different line types for agric/non-agric HHs
    guides(linetype = guide_legend(title = "Household Type")) +  # Add legend title for line types
    scale_x_continuous(breaks = seq(min(rural_combined_data$year), max(rural_combined_data$year), by = 3))  # Tick marks every 3 years
  
  # save the plot and with the country name
  assign(paste0(country_name, "_rural_plot"), plot)
  
  # add the plot to the list
  country_plots[[country_name]] <- plot
}

#### COMBINE THE PLOTS

# remove legends from all plots
Benin_rural_plot <- Benin_rural_plot + theme(legend.position = "none")
`Burkina Faso_rural_plot` <- `Burkina Faso_rural_plot` + theme(legend.position = "none") 
Cameroon_rural_plot <- Cameroon_rural_plot + theme(legend.position = "none")
`Cote d'Ivoire_rural_plot` <- `Cote d'Ivoire_rural_plot` + theme(legend.position = "none") 
Mali_rural_plot <- Mali_rural_plot + theme(legend.position = "none")
Mozambique_rural_plot <- Mozambique_rural_plot + theme(legend.position = "none")

rural_country_line_plots <- grid.arrange(Benin_rural_plot, `Burkina Faso_rural_plot`, Cameroon_rural_plot, `Cote d'Ivoire_rural_plot`, Mali_rural_plot, Mozambique_rural_plot,
                                         nrow = 2,
                                         ncol = 3
)

# combine the plots and legend
rural_country_final_line_plots <- grid.arrange(
  rural_country_line_plots,
  legend,
  ncol = 2,  # legend in the second column, plots in the first column
  widths = c(10, 2),  # adjust the width to give more space to the plots
  top = textGrob(
    "Malaria Test Positivity Rate and Net Use by Country: Rural",
    gp = gpar(fontsize = 16, fontface = "bold", hjust = 0.5)  # center the title
  ),
  left = textGrob(
    "Percent (%)", 
    rot = 90, 
    gp = gpar(fontsize = 12)
  )
)

# display the combined plot and save as .png
ggsave(paste0(FigDir, "/png_figures/", Sys.Date(),"_rural_country_final_line_plots.png"), rural_country_final_line_plots, width = 10, height = 10) 