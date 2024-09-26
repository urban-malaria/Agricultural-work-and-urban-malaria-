# ==========================================================================================================================================
# Script Name: Assessing All Country Data
# Author: Dr. Ifeoma Ozodiegwu
# Edited by: Grace Legris, Research Data Analyst (gracebea@gmail.com)
# Edited: [2024-09-25]
# Purpose: To identify surveys that have the variables that we need for the analysis. 
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
    FigDir <- file.path(DriveDir, "projects", "Manuscripts", "agriculture_malaria_manuscript", "figures", "220623_new_figures")
} else if ("CHZCHI003" %in% user) {
  Drive <- file.path("C:/Users/CHZCHI003/OneDrive")
  DriveDir <- file.path(Drive, "urban_malaria")
  PopDir <- file.path(DriveDir, "data", 'Urban_malaria_net_ownership_data')
  ManDir <- file.path(DriveDir, "projects", "Manuscripts", "agriculture_malaria_manuscript")
  FigDir <- file.path(ManDir, "figures", "220623_new_figures")
} else if ("grace" %in% user) {
  Drive <- "/Users/grace/Urban Malaria Proj Dropbox"
  DriveDir <- file.path(Drive, "urban_malaria")
  PopDir <- file.path(DriveDir, "data", 'data_agric_analysis')
  ManDir <- file.path(DriveDir, "projects", "Manuscripts", "ongoing", "agriculture_malaria_manuscript")
  FigDir <- file.path(ManDir, "figures", "exploratory")
}else {
  Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
  DriveDir <- file.path(Drive, "OneDrive - Northwestern University", "urban_malaria")
  #DriveDir <- file.path(user_path, "OneDrive - Northwestern University", "urban_malaria")
  #DriveDir <- file.path(Drive, '', 'Northwestern University', 'Ifeoma Doreen Ozodiegwu - urban_malaria')
  #DriveDir <- file.path(Drive, '', 'Library', 'CloudStorage', "OneDrive-NorthwesternUniversity", "urban_malaria")
  DriveDir <- file.path(Drive, "OneDrive - Personal", "urban_malaria")
  PopDir <- file.path(DriveDir, "data", 'Urban_malaria_net_ownership_data')
  ManDir <- file.path(DriveDir, "projects", "Manuscripts", "agriculture_malaria_manuscript")
  FigDir <- file.path(ManDir, "figures", "220623_new_figures")
}
 

## =========================================================================================================================================
### Data Preparation: Loading Functions and Retrieving DHS Survey Indicators
## =========================================================================================================================================

# load custom functions from the specified R script
source("functions/functions_employment.R")

# fetch survey data related to various malaria and employment indicators from the DHS API:

# get malaria rapid diagnostic test (RDT) survey data for all of Africa
rdt_df <- dhs_surveys(indicatorIds="ML_PMAL_C_RDT") # indicator for malaria RDT results

# get malaria microscopy survey data for all of Africa
mal_df <- dhs_surveys(indicatorIds="ML_PMAL_C_MSY") # indicator for malaria microscopy results

# get survey data for women engaged in agriculture in Sub-Saharan Africa
occp_df <- dhs_surveys(indicatorIds="EM_OCCP_W_AGR") %>% 
  filter(RegionName == "Sub-Saharan Africa") # indicator for women in agriculture

# get survey data for men engaged in agriculture in Sub-Saharan Africa
occp_m_df <- dhs_surveys(indicatorIds="EM_OCCP_M_AGR") %>% 
  filter(RegionName == "Sub-Saharan Africa") # indicator for men in agriculture

# optional: set a longer timeout for API requests
#set_rdhs_config(timeout = 600)

# retrieve API-based estimates for RDT testing across all surveys in Africa (rdt_df dataframe created above)
rdt_dt <- dhs_data(surveyIds = rdt_df$SurveyId, indicatorIds="ML_PMAL_C_RDT") %>%
  rename(R = Value) %>%  # rename Value column to R for RDT estimates
  dplyr::select(SurveyId, R, SurveyYear) # select relevant columns

# retrieve microscopy testing estimates, filtered to only include surveys present in RDT data
mal_dt <- dhs_data(surveyIds = mal_df$SurveyId, indicatorIds="ML_PMAL_C_MSY") %>%
  filter(SurveyId %in% rdt_dt$SurveyId) %>%  # ensure matching surveys with RDT data
  rename(M = Value) %>%  # rename Value column to M for microscopy estimates
  dplyr::select(SurveyId, M, SurveyYear) # select relevant columns

# retrieve data on the proportion of women engaged in agriculture, filtered to RDT matching surveys
occp_w_dt <- dhs_data(surveyIds = occp_df$SurveyId, indicatorIds="EM_OCCP_W_AGR") %>%
  filter(SurveyId %in% rdt_dt$SurveyId) %>% # ensure matching surveys with RDT data
  rename(Wo = Value) %>%  # rename Value column to Wo for women in agriculture estimates
  dplyr::select(SurveyId, Wo, SurveyYear) # select relevant columns

# retrieve data on the proportion of men engaged in agriculture, filtered to RDT matching surveys
occp_m_dt <- dhs_data(surveyIds = occp_m_df$SurveyId, indicatorIds="EM_OCCP_M_AGR") %>%
  filter(SurveyId %in% rdt_dt$SurveyId) %>%  # ensure matching surveys with RDT data
  rename(Mo = Value) %>%  # rename Value column to Mo for men in agriculture estimates
  dplyr::select(SurveyId, Mo, SurveyYear) # select relevant columns

# create a single data frame by left joining all estimates on SurveyId variable
df <- rdt_dt %>% 
  left_join(mal_dt, by = "SurveyId") %>% 
  left_join(occp_w_dt, by = "SurveyId") %>% 
  left_join(occp_m_dt, by = "SurveyId")


## =========================================================================================================================================
### Data Visualization: Plotting Malaria and Agricultural Work Estimates
## =========================================================================================================================================

# define labels and colors for the plot
label <- c("Tested positive by microscopy", "Tested positive by RDT", "Agricultural work (Male)", "Agricultural work (Female)")
color <- c("deeppink4", "#e8a6bd", "#afb2cb", "#a7cece")

# transform the dataset for plotting by pivoting longer
plot_dat <- df %>%
  pivot_longer(cols = c("M", "R", "Wo", "Mo"), names_to = "name", values_to = "value")

# reorder SurveyId by SurveyYear and set the order of the names for the plot
plot_dat$SurveyId <- fct_reorder(plot_dat$SurveyId, plot_dat$SurveyYear)
plot_dat$name <- fct_relevel(plot_dat$name, "M", "R")

# create bar plot for countries with agricultural data and malaria testing results, save as png
barplot <- ggplot(plot_dat, aes(x= name, y = as.numeric(value), fill=name)) +
  geom_bar(stat = "identity")+ 
  facet_wrap(~SurveyId) +
  scale_fill_manual(values = color, label =label) +
  theme_manuscript() +
  theme(legend.title=element_blank(), legend.position = "top") +
  labs(x = "", y = "Percentage")
  ggsave(paste0(FigDir,"/", Sys.Date(),"_country_datasets_malaria_agric_all_data.png"), p, width = 13, height = 14)

# display the plot  
print(barplot)

# filter the dataset to remove NA values (all agric and parasitemia data present) and pivot longer, plot again for comprehensive data analysis
# resultant df_com includes only complete cases (it only contains countries that have valid data for all four indicators: 
# microscopy (M), RDT (R), agricultural work by women (Wo), and agricultural work by men (Mo))
df_com <- df %>% 
  drop_na() %>%
  pivot_longer(cols = c("M", "R", "Wo", "Mo"), names_to = "name", values_to = "value")

barplot_no_missing <- ggplot(df_com, aes(x= name, y = as.numeric(value), fill=name)) +
  geom_bar(stat = "identity")+ 
  facet_wrap(~SurveyId) +
  scale_fill_manual(values = color, label =label) +
  #theme_manuscript() +
  theme(legend.title=element_blank(), legend.position = "top") +
  labs(x = "", y = "Percentage")
  ggsave(paste0(FigDir,"/", Sys.Date(),"_country_datasets_malaria_agric_with_all_needed_analysis_data.png"), p, width = 13, height = 13)

# display the plot
print(barplot_no_missing)  
  
# filter out countries without agricultural data and plot the remaining data
# include only rows where at least one of the agricultural data columns (Wo for women and Mo for men) is not NA
df_com <- df %>% 
  filter_at(vars(Wo, Mo), any_vars(!is.na(.))) %>%
  pivot_longer(cols = c("M", "R", "Wo", "Mo"), names_to = "name", values_to = "value")

barplot_all_agric_data <- ggplot(df_com, aes(x= name, y = as.numeric(value), fill=name)) +
  geom_bar(stat = "identity")+ 
  facet_wrap(~SurveyId) +
  scale_fill_manual(values = color, label =label) +
  #theme_manuscript() +
  theme(legend.title=element_blank(), legend.position = "top") +
  labs(x = "", y = "Percentage")
  ggsave(paste0(FigDir,"/", Sys.Date(),"_country_datasets_for_analysis_data.png"), p, width = 13, height = 13)

# display the plot  
print(barplot_all_agric_data)

# save the filtered dataset for future analysis, excluding countries with insufficient data
df_com <- df %>%
  filter_at(vars(Wo, Mo), any_vars(!is.na(.))) %>%
  filter(SurveyId != "GA2019DHS")  # remove Gabon due to small estimates of agricultural work

# write the filtered data to a CSV file for further analysis
write_csv(df_com, file.path(ManDir, "csv", "surveyids_for_analysis.csv"))


## =========================================================================================================================================
### Visualize Indoor Residual Spraying (IRS) Data Across Sub-Saharan African Countries
## =========================================================================================================================================

# fetch surveys that contain IRS data for Sub-Saharan Africa
irs_df <- dhs_surveys(indicatorIds = "ML_IRSM_H_IRS") %>% 
  filter(RegionName == "Sub-Saharan Africa")

# retrieve the IRS data associated with the survey IDs, renaming the value column to "IRS" for clarity
irs_dt <- dhs_data(surveyIds = irs_df$SurveyId, indicatorIds = "ML_IRSM_H_IRS") %>%
  rename(IRS = Value) %>%
  dplyr::select(SurveyId, IRS, SurveyYear)  # select only the relevant columns

# reshape the data to a long format for plotting
plot_dat <- irs_dt %>%
  pivot_longer(cols = c("IRS"), names_to = "name", values_to = "value")

# create a bar plot to visualize the IRS data
barplot_IRS <- ggplot(plot_dat, aes(x = name, y = as.numeric(value), fill = name)) +
  geom_bar(stat = "identity") +  # Use bars to represent IRS values
  facet_wrap(~SurveyId) +  # Create separate panels for each survey
  #theme_manuscript() +  # Uncomment if a custom theme is defined
  theme(legend.title = element_blank(), legend.position = "top") +  # Customize legend appearance
  labs(x = "", y = "Percentage")  # Label the axes

# display the plot
print(barplot_IRS)
