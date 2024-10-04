# Author: Grace Legris (gracebea@gmail.com)
# Created: [2024-10-03]
# Purpose: Generate Difference-in-Differences Analysis and Scatterplot
# ==========================================================================================================================================

# clear current workspace
rm(list = ls())

## =========================================================================================================================================
### Directory Management and File Paths
## =========================================================================================================================================

user <- Sys.getenv("USERNAME")
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


source("functions/functions_employment.R")

## -----------------------------------------------------------------------------------------------------------------------------------------
### URBAN: Difference-in-Differences Analysis + Scatterplot
## -----------------------------------------------------------------------------------------------------------------------------------------

# read in the trend analysis datasets for malaria test positivity
df_trend_malaria <- read_csv(file.path(PopDir, "analysis_dat/240606_urban_df_for_analysis_trend_malaria_chilo_created.csv"))

# urban analysis for malaria
urban_trend <- df_trend_malaria  %>%  
  filter(title == "Urban", cntryId...9 != "TZ", country_year.x...2 != "Mozambique 2011") %>% # filter relevant data for rural areas
  select(country_year.x...2, cntryId...5, agric_percent, non_agric_percent, diff_val_malaria, cntryId...12, id) %>% # select specific columns
  rename(country_year = country_year.x...2, country_id = cntryId...5, country_surveyyears = cntryId...12) %>% # rename columns for clarity
  mutate(id = if_else(country_year == "Mozambique 2015", "Preceding survey", id)) # update id for specific condition

# calculate DiD
did_results_urban <- urban_trend %>%
  group_by(country_id, country_surveyyears) %>% 
  summarise(
    B = agric_percent[id == "Most recent survey"],  # test positivity for agricultural HHs (most recent)
    A = agric_percent[id == "Preceding survey"],    # test positivity for agricultural HHs (preceding)
    D = non_agric_percent[id == "Most recent survey"],  # test positivity for non-agricultural HHs (most recent)
    C = non_agric_percent[id == "Preceding survey"],    # test positivity for non-agricultural HHs (preceding)
    DiD = (B - A) - (D - C)  # Difference-in-Differences calculation
  ) %>%
  ungroup()  # ensure no grouping is retained after summarization

ggplot(did_results_urban, aes(country_surveyyears, DiD))+
  geom_point(size = 5, alpha = 0.7, color = "#e07a5f")+
  coord_flip() +
  geom_hline(yintercept = 0, linetype = "dashed", color = "grey50")+
  theme_manuscript()

library(stringr)

# change capital "Vs." to "vs."
did_results_urban <- did_results_urban %>%
  mutate(country_surveyyears = str_replace(country_surveyyears, "Vs.", "vs."))
# update country_surveyyears to remove spaces around dashes
did_results_urban <- did_results_urban %>%
  mutate(country_surveyyears = str_replace_all(country_surveyyears, " - ", "-"))

# create the scatter plot (urban)
did_plot_urban <- ggplot(did_results_urban, aes(x = DiD, y = country_surveyyears)) +
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
### RURAL: Difference-in-Differences Analysis + Scatterplot
## -----------------------------------------------------------------------------------------------------------------------------------------

# rural analysis for malaria
rural_trend <- df_trend_malaria  %>%  
  filter(title == "Rural", cntryId...9 != "TZ", country_year.x...2 != "Mozambique 2011") %>% # filter relevant data for rural areas
  select(country_year.x...2, cntryId...5, agric_percent, non_agric_percent, diff_val_malaria, cntryId...12, id) %>% # select specific columns
  rename(country_year = country_year.x...2, country_id = cntryId...5, country_surveyyears = cntryId...12) %>% # rename columns for clarity
  mutate(id = if_else(country_year == "Mozambique 2015", "Preceding survey", id)) # update id for specific condition

# calculate DiD
did_results_rural <- rural_trend %>%
  group_by(country_id, country_surveyyears) %>% 
  summarise( 
    B = agric_percent[id == "Most recent survey"],  # test positivity for agricultural HHs (most recent)
    A = agric_percent[id == "Preceding survey"],    # test positivity for agricultural HHs (preceding)
    D = non_agric_percent[id == "Most recent survey"],  # test positivity for non-agricultural HHs (most recent)
    C = non_agric_percent[id == "Preceding survey"],    # test positivity for non-agricultural HHs (preceding)
    DiD = (B - A) - (D - C)  # Difference-in-Differences calculation
  ) %>%
  ungroup()  # ensure no grouping is retained after summarization

# change capital "Vs." to "vs."
did_results_rural <- did_results_rural %>%
  mutate(country_surveyyears = str_replace(country_surveyyears, "Vs.", "vs."))
# update country_surveyyears to remove spaces around dashes
did_results_rural <- did_results_rural %>%
  mutate(country_surveyyears = str_replace_all(country_surveyyears, " - ", "-"))

# create the scatter plot (rural)
did_plot_rural <- ggplot(did_results_rural, aes(x = DiD, y = country_surveyyears)) +
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
did_plot_urban <- did_plot_urban + 
  labs(title = NULL, subtitle = "Urban", x = NULL, y = NULL) + 
  theme(plot.subtitle = element_text(hjust = 0.5, size = 12))
did_plot_rural <- did_plot_rural + 
  labs(title = NULL, subtitle = "Rural", x = NULL, y = NULL) + 
  theme(plot.subtitle = element_text(hjust = 0.5, size = 12))

# combine the two plots vertically (ease of comparison)
did_plots <- (did_plot_urban + did_plot_rural) +
  plot_annotation(
    title = "Difference-in-Differences Analysis Comparing Malaria
    Positivity in Agricultural vs. Non-Agricultural Households over Time",
    theme = theme(plot.title = element_text(hjust = 0.5, size = 16)),
    subtitle = NULL  # remove the subtitle from the overall plot
  ) +
  # add a custom label for the x-axis below the plots
  plot_layout(ncol = 1) +
    labs(x = "Difference-in-Differences: Change in Malaria Positivity\n Rates (Agricultural HH - Non-Agricultural HH") + 
     theme(axis.title.x = element_text(hjust = 0.5, vjust = -3))


# display the combined plot and save as .png
did_plots
ggsave(paste0(FigDir, "/png_figures/", Sys.Date(),"_DiD_urban+rural_plot.png"), did_plots, width = 7, height = 7) 
