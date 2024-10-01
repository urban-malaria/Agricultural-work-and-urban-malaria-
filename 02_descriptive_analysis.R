# ==========================================================================================================================================
# Script Name: Descriptive Analysis
# Author: Dr. Ifeoma Ozodiegwu
# Edited by: Grace Legris, Research Data Analyst (gracebea@gmail.com)
# Edited: [2024-09-30]
# Purpose: Generate Figures 1, 4, and 5
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
  FigDir <- file.path(ManDir, "figures", "main_figures","pdf_figures")
  SupDir <- file.path(ManDir, "figures", "supplementary", "pdf_figures")
  ExpDir <- file.path(ManDir, "figures", "exploratory")
} else if  ("CHZCHI003" %in% user) {
  Drive <- file.path("C:/Users/CHZCHI003/Dropbox")
  DriveDir <- file.path("C:/Users/CHZCHI003/OneDrive/urban_malaria")
  PopDir <- file.path(Drive)
  ManDir <- file.path(Drive, "agriculture_malaria_manuscript")
  FigDir <- file.path(ManDir, "figures", "main_figures","pdf_figures")
  SupDir <- file.path(ManDir, "figures", "supplementary", "pdf_figures")
  ExpDir <- file.path(ManDir, "figures", "exploratory")
} else if  ("cchiz" %in% user) {
  Drive <- file.path("C:/Users/cchiz/Dropbox")
  DriveDir <- file.path("C:/Users/cchiz/OneDrive/urban_malaria")
  PopDir <- file.path(Drive)
  ManDir <- file.path(Drive, "agriculture_malaria_manuscript")
  FigDir <- file.path(ManDir, "figures", "main_figures","pdf_figures")
  SupDir <- file.path(ManDir, "figures", "supplementary", "pdf_figures")
  ExpDir <- file.path(ManDir, "figures", "exploratory")
} else if ("grace" %in% user) {
  Drive <- "/Users/grace/Urban Malaria Proj Dropbox"
  DriveDir <- file.path(Drive, "urban_malaria")
  PopDir <- file.path(DriveDir, "data", 'data_agric_analysis')
  ManDir <- file.path(DriveDir, "projects", "Manuscripts", "ongoing", "agriculture_malaria_manuscript")
  FigDir <- file.path(ManDir, "figures", "main_figures","pdf_figures")
  SupDir <- file.path(ManDir, "figures", "supplementary", "pdf_figures")
  ExpDir <- file.path(ManDir, "figures", "exploratory")
} else {
  Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
  DriveDir <- file.path(Drive, 'Library', 'CloudStorage', 'OneDrive-NorthwesternUniversity', "urban_malaria")
  #DriveDir <- file.path(Drive,  "OneDrive - Northwestern University", "urban_malaria")
  PopDir <- file.path(DriveDir, "data", 'data_agric_analysis')
  ManDir <- file.path(DriveDir, "projects", "Manuscripts", "agriculture_malaria_manuscript")
  FigDir <- file.path(ManDir, "figures", "main_figures","pdf_figures")
  SupDir <- file.path(ManDir, "figures", "supplementary", "pdf_figures")
  ExpDir <- file.path(ManDir, "figures", "exploratory")
}


## =========================================================================================================================================
### Required Functions, Settings, and Processing
## =========================================================================================================================================

# note before sourcing functions and packages, run the code below to download rdhs if you don't already have it
# devtools::install_github("ropensci/rdhs")
source("functions/functions_employment.R")
options(survey.lonely.psu="adjust")  # this option allows admin units with only one cluster to be analyzed

## -----------------------------------------------------------------------------------------------------------------------------------------
### Read in Analysis Datasets
## -----------------------------------------------------------------------------------------------------------------------------------------

# load urban dataset and add a 'type' column to indicate urban
urban_df <- read_csv(file.path(PopDir, "analysis_dat/240729_urban_df_for_analysis.csv")) %>%  
  mutate(type ="Urban") %>% 
  transmute(country_year.x, country_year.x, id, strat, wt, type) %>% # select and retain only these columns
  name_clean_fun() # clean column names using custom function (see functions_employment.R script)

# load rural dataset and add a 'type' column to indicate rural
rural_df <- read_csv(file.path(PopDir,"analysis_dat/240729_rural_df_for_analysis.csv")) %>%  
  mutate(type ="Rural") %>% 
  transmute(country_year.x, country_year.x, id, strat, wt, type)  %>% # select and retain only these columns
  name_clean_fun() # clean column names using custom function

# extract unique country-year values from the urban dataset
recent_to_remove <- urban_df$country_year.x %>% unique()

# load urban trend dataset, filter out recent records, and add a 'type' column for urban
urban_trend_df <- read_csv(file.path(PopDir, "analysis_dat/urban_df_for_analysis_trend.csv")) %>%  
  mutate(type ="Urban")  %>% 
  filter(!country_year.x %in% recent_to_remove) %>% # remove recent entries from urban dataset
  transmute(country_year.x, country_year.x, id, strat, wt, type)  %>% # select and retain only these columns
  name_clean_fun() # clean column names using custom function

# load rural trend dataset, filter out recent records, and add a 'type' column for rural
rural_trend_df <- read_csv(file.path(PopDir,"analysis_dat/rural_df_for_analysis_trend.csv")) %>%
  mutate(type ="Rural") %>%
  filter(!country_year.x %in% recent_to_remove) %>% # remove recent entries from rural dataset
  transmute(country_year.x, country_year.x, id, strat, wt, type)  %>% # select and retain only these columns
  name_clean_fun() # clean column names using custom function


## =========================================================================================================================================
### Create Plots
## =========================================================================================================================================

## -----------------------------------------------------------------------------------------------------------------------------------------
### Figure 1
## -----------------------------------------------------------------------------------------------------------------------------------------
#totals by country 
# all_df <- rbind(urban_df, rural_df) %>% mutate(country_year.x = ifelse(country_year.x == "Congo Democratic Republic 2013 - 14", "DRC 2013 - 14",
#                                                                        ifelse(country_year.x == "Cameroon 2018", "Cameroon 2018 - 19",country_year.x))) %>% 
#   as_survey_design(ids= id,strata=strat,nest=T,weights= wt)%>% 
#   group_by(country_year.x) %>%  summarize(total = round(survey_total(),0)) %>%  ungroup() 


# figure 1 sample description 

# combine urban and rural data frames (both current and trend data) into one
all_df <- rbind(urban_df, rural_df, urban_trend_df, rural_trend_df) %>%
  
  # create a survey design object with id, strata, and weights, setting nest to true
  as_survey_design(ids= id,strata=strat,nest=T,weights= wt)%>% 
  
  # group by country and urban/rural type, then calculate total population using survey weights
  group_by(country_year.x, type) %>%
  summarize(total = round(survey_total(),0)) %>%
  ungroup() 

# calculate the percentage of the total for each group within a country/year
df <- all_df %>% 
  group_by(country_year.x) %>%  
  summarise(percent = round(total / sum(total) * 100, 0)) # calculate percentage of total population

# merge the calculated percentages with the original data frame and prepare for plotting
all <- cbind(all_df, df) %>% 
  select(-c("country_year.x")) %>% # remove duplicate country_year.x column
  mutate(plot_label = ifelse(type == "Rural", percent, NA)) %>% # create plot labels only for rural areas (to avoid label clutter)
  mutate(survey = ifelse(country_year.x %in% recent_to_remove, "Recent Survey", "Preceding Survey")) # categorize surveys into recent vs. preceding surveys based on the country year

# create the plot, excluding NA% labels
generate_survey_plot <- function(data, survey_type, titles, remove_x_axis = FALSE) {
  # filter data for the specified survey type and create the plot
  p <- ggplot(data %>% filter(survey == survey_type), aes(x = reorder(country_year.x, -total), y = total, fill = type, label = total)) +
    geom_bar(stat = "identity", alpha = 0.6) +
    scale_fill_manual(values = c("#E07A5F", "darkorchid")) +
    geom_text(aes(label = ifelse(!is.na(plot_label), paste0(plot_label, "%"), "")), 
              position = position_stack(vjust = 0.5),
              color = "black") +
    coord_flip() +
    theme_manuscript() +
    labs(x = "", y = "") +
    scale_y_continuous(position = "right") +
    ylim(0, 11500) +
    theme(legend.position = "none") +  # Remove the legend
    annotate("text", x = 8, y = 7000, label = "Urban", color = "darkorchid", hjust = 0) +
    annotate("text", x = 9, y = 7000, label = "Rural", color = "#E07A5F", hjust = 0)
  
  if (remove_x_axis) {
    p <- p + theme(axis.text.x = element_blank(), axis.ticks.x = element_blank())
  }
  
  return(p) # return the created plot
}

# generates plots for both surveys
p1bc_survey1 <- generate_survey_plot(all, "Preceding Survey", "Preceding Survey", remove_x_axis = TRUE) # plots for preceding surveys
p1bc_survey2 <- generate_survey_plot(all, "Recent Survey", "Recent Survey") + # plots for recent surveys
  labs(x = "", y = str_wrap("Number of children, 6 - 59 months tested for malaria by RDT or 
                            microscopy in urban and rural clusters, combined", width = 60))

# reduces the space between plots
p1bc_combined_plot <- p1bc_survey1 / plot_spacer() / p1bc_survey2 + 
  plot_layout(heights = c(1, -0.1, 2)) # adjust heights; second value controls the spacing

# displays the combined plot
p1bc_combined_plot

# save the combined plot as a PDF
ggsave(paste0(FigDir,"/", Sys.Date(),"_figure_1bc.pdf"), p1bc_combined_plot, width = 4.5, height = 6) 


## -----------------------------------------------------------------------------------------------------------------------------------------
### Figure 1a - Map
## -----------------------------------------------------------------------------------------------------------------------------------------

# read the shapefile for Africa boundaries
afr.shp.base<- st_read(file.path(DriveDir, "data", "Urban_malaria_net_ownership_data",
                                 "shapefiles", "africa_baundaries", "afr_g2014_2013_0.shp"))

# extract distinct country codes and names from the urban dataframe
DHS_country_codes <- urban_df %>%  select(DHS_CountryCode,CountryName) %>%  distinct(DHS_CountryCode, CountryName) %>%  mutate(data_available = 1)

# fix country codes for specific countries
DHS_country_codes$DHS_CountryCode[which(DHS_country_codes$CountryName=='Madagascar')]<-'MG'
DHS_country_codes$DHS_CountryCode[which(DHS_country_codes$CountryName=='Burundi')]<-'BI'

# join the shapefile data with country codes
afr_shape_dat <- afr.shp.base %>%
  left_join(DHS_country_codes, by = c("ISO2" = "DHS_CountryCode")) %>%  
  mutate(data_com = if_else(is.na(data_available), 0, data_available)) # create a binary indicator for data availability

# display the count of data availability
table(afr_shape_dat$data_com)

# create the map using ggplot2
p1a=ggplot() +
  geom_sf(data = afr_shape_dat , aes(geometry = geometry, fill = data_available)) +
  scale_fill_continuous(low ="#ffd5c6", high= "#d08288",  na.value = "white") +
  map_theme() +
  theme(legend.position="none")

# display the map and save as a pdf
p1a
ggsave(paste0(FigDir,"/", Sys.Date(),"_figure_1a.pdf"), p1a, width = 4, height = 8) 

## -----------------------------------------------------------------------------------------------------------------------------------------
### Figure 1c - Table
## -----------------------------------------------------------------------------------------------------------------------------------------

# figure 1c - load and prepare data
urban_df <- read_csv(file.path(PopDir, "analysis_dat/240729_urban_df_for_analysis.csv")) %>%
  mutate(type ="Urban") # add a column to indicate urban type

rural_df <- read_csv(file.path(PopDir,"analysis_dat/240729_rural_df_for_analysis.csv")) %>%
  mutate(type ="Rural") # add a column to indicate rural type

# combine urban and rural data
df <- rbind(urban_df, rural_df)

# quick chi-squared test to assess relationship between home type and interview month
table(df$home_type2, df$interview_month) # create a contingency table
test<- chisq.test(df$home_type2, df$interview_month) # perform chi-squared test
# extract test statistic and p-value
test$statistic 
test$p.value

# set up survey design object
svyd_df <- svydesign.fun(df)

# create a table of counts and percentages
table_df <- svytable(~home_type2 + interview_month, svyd_df)%>% 
  as.data.frame() %>% 
  mutate(interview_month = as.numeric(interview_month)) # convert interview_month to numeric

# recode home type and calculate percentage
table_df <- table_df %>%  
  mutate(home_type2 = ifelse(home_type2 == 'A', 'Agricultural worker Household (HH)', "Non-Agricultural worker HH")) %>% 
  group_by(home_type2) %>%  
  mutate(percent = round(Freq/sum(Freq)*100), 0) %>%  
  ungroup() # reset grouping

# create bar plot of percentages by interview month
p3 <- table_df %>% 
  ggplot(aes(x = interview_month, y = percent, label =percent)) +
  geom_col(fill = "forestgreen", alpha=0.8) + # bar plot
  geom_smooth(se = FALSE, color = "goldenrod2")+ # smooth line
  # geom_text(position = position_stack(vjust = 0.5),
  #           color = "black") +
  facet_wrap(vars(home_type2)) + # separate plots by home type
  theme_manuscript()+
  labs(y = "Percentage of children, 6 - 59 months 
       tested for malaria by RDT or microscopy in urban and rural clusters", x = "Interview month") +
  #theme(strip.background = element_blank(), strip.text.x = element_blank())+
  scale_x_continuous(breaks = scales::pretty_breaks(n=12), expand = expansion(mult = c(0.02, 0.02)))

# combine plots if needed
#p <- (p2 + p1)/p3

# save plot as a pdf
#ggsave(paste0(FigDir,"/", Sys.Date(),"_figure_1.pdf"), p, width = 8, height = 7) 

## -----------------------------------------------------------------------------------------------------------------------------------------
### Figure 2
## -----------------------------------------------------------------------------------------------------------------------------------------

#overall
color = c("#f2a5a1", "#c55c80")
# plot_over <- df %>%  dplyr::select(country_year.x, home_type2, code_year, test_result)
# plot_over2 = plot_over %>%  group_by(home_type2, test_result) %>%  summarise(value= n()) %>% mutate(percent = round(value/sum(value) *100, 0))
# 
# p <-ggplot(plot_over2, aes(fill=test_result, x= home_type2)) + 
#   geom_bar(aes(y = value), position="stack", stat = "identity")+
#   theme_manuscript()+
#   scale_x_discrete(labels = c("Agricultural worker \n household (HH)", "Non-agricultural \n worker HH"))+
#   scale_fill_manual(name = "Malaria test result", labels= c("Negative", "positive"), values= color)+
#   geom_text(aes(label = paste0(value, " ", "(", percent, "%", ")"), y = value),
#             position = position_stack(vjust = 0.5),
#             color = "white") +
#   labs(x = "", y  = "Number of children, 6 - 59 months,
#   tested positive for malaria in 16 DHS datasets") +
#   theme(strip.text.x = element_text(
#     size = 12, color = "black")) 
# 
# ggsave(paste0(SupDir,"/", Sys.Date(),"_sup_figure_1.pdf"), p, width = 8.5, height = 6) 


## -----------------------------------------------------------------------------------------------------------------------------------------
### Figure 2 - Urban Plot
## -----------------------------------------------------------------------------------------------------------------------------------------

# create a dataframe for urban data with selected columns
plot_df_um<- urban_df %>% dplyr::select(country_year.x, home_type2, code_year, test_result, id, strat, wt) 

# prepare data for plotting with survey design
plot_overall <- plot_df_um %>% 
  as_survey_design(ids = id, strata = strat, nest = TRUE, weights = wt) %>%
  group_by(home_type2, test_result) %>% # group data by home type and test result  
  summarise(
    value = survey_mean(vartype = "se") # calculate survey mean with standard error
  ) %>%
  mutate(lower_ci = value - (1.96 * value_se),  # calculate lower bound of 95% CI
         upper_ci = value + (1.96 * value_se),  # calculate lower bound of 95% CI
         percent = round(value * 100, 0))       # convert mean to percentage

# set title for the plot
plot_overall$title = "Urban"

# create bar plot for urban data
p_urban<-ggplot(plot_overall, aes(fill = test_result, x = home_type2)) + 
  geom_bar(aes(y = percent), position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin = lower_ci * 100, ymax = upper_ci * 100), 
                colour = "darkgray", 
                position = position_dodge(width = 0.9), 
                width = 0.25) +  # add error bars
  theme_manuscript() +
  scale_x_discrete(labels = c("Agricultural worker \n household (HH)", "Non-agricultural \n worker HH")) +
  scale_fill_manual(name = "Malaria test result", 
                    labels = c("Negative", "Positive"), 
                    values = color) + # set fill colors for test results
  geom_text(aes(label = paste0(percent, "%", " ", "(", round(value * 100, 0), ")"), y = percent),
            position = position_dodge(width = 0.9),
            color = "black", vjust = 2) + # add text labels to bars
  labs(x = "", y = "") +
  facet_wrap(vars(title)) + # create facets by title
  theme(strip.text.x = element_text(size = 12, color = "black")) +
  coord_cartesian(ylim = c(0, 90)) 

# display the urban plot
p_urban

## -----------------------------------------------------------------------------------------------------------------------------------------
### Figure 2 - Rural Plot
## -----------------------------------------------------------------------------------------------------------------------------------------

plot_df_rm<- rural_df %>% dplyr::select(country_year.x, home_type2, code_year, test_result, id, strat, wt) 

plot_overall <- plot_df_rm %>% 
  as_survey_design(ids = id, strata = strat, nest = TRUE, weights = wt) %>%
  group_by(home_type2, test_result) %>%   
  summarise(
    value = survey_mean(vartype = "se")
  ) %>%
  mutate(
    lower_ci = value - (1.96 * value_se),  # lower bound of 95% CI
    upper_ci = value + (1.96 * value_se),  # upper bound of 95% CI
    percent = round(value * 100, 0)        # convert to percentages
  )
plot_overall$title <- "Rural"

p_rural <- ggplot(plot_overall, aes(fill = test_result, x = home_type2)) + 
  geom_bar(aes(y = percent), position = "dodge", stat = "identity") +
  geom_errorbar(aes(ymin = lower_ci * 100, 
                    ymax = upper_ci * 100), 
                colour = "darkgray",
                position = position_dodge(width = 0.9), width = 0.25) +  # add error bars
  theme_manuscript() +
  scale_x_discrete(labels = c("Agricultural worker \n household (HH)", "Non-agricultural \n worker HH")) +
  scale_fill_manual(name = "Malaria test result", 
                    labels = c("Negative", "Positive"), 
                    values = color) +
  geom_text(aes(label = paste0(percent, "%", " ", "(", round(value * 100, 0), ")"), y = percent),
            position = position_dodge(width = 0.9),
            color = "black", vjust = 2) + 
  labs(x = "", y = "") +
  facet_wrap(vars(title)) +
  theme(strip.text.x = element_text(size = 12, color = "black")) +
  coord_cartesian(ylim = c(0, 80)) +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.y = element_blank())

# display the rural plot
p_rural

# combine the urban and rural plots, display and save as pdf
p_2a = p_urban +p_rural+ plot_layout(guides = "collect") & theme(legend.position = 'none')
p_2a
ggsave(paste0(FigDir,"/", Sys.Date(),"malaria_prevalence_HH_occupation_exposure_urban_rural.pdf"), p_2a, width = 8, height = 3)


## -----------------------------------------------------------------------------------------------------------------------------------------
### Plot: Net Use vs Occupation Category
## -----------------------------------------------------------------------------------------------------------------------------------------

# define colors for the plot
color = c( "#621244", "#efeddb")

# create a dataframe for urban data with selected columns
plot_df_un<- urban_df %>% dplyr::select(country_year.x, home_type2, code_year, u5_net_use, id, strat, wt) 

# prepare urban data for plotting with survey design
plot_urban = plot_df_un %>% as_survey_design(ids= id,strata=strat,nest=T,weights= wt) %>% 
  mutate(net_use = ifelse(u5_net_use == 1, "Slept under \n a net", "Did not sleep \n under a net")) %>% 
  group_by(home_type2, net_use) %>% # group by home type and net use 
  summarise(value = round(survey_total(),0)) %>% # calculate total net use
  mutate(percent = round(value/sum(value) *100, 0)) # calculate percentage of net use

# set title for urban plot
plot_urban$title = "Urban"

# create bar plot for urban data
p_net_u<-ggplot(plot_urban, aes(fill=net_use, x= home_type2)) + 
  geom_bar(aes(y = value), position="stack", stat = "identity")+ # stacked bar chart
  theme_manuscript()+
  scale_x_discrete(labels = c("Agricultural worker \n household (HH)", "Non-agricultural \n worker HH"))+
  scale_fill_manual(name = "", values= color)+ # set custom fill colors
  geom_text(aes(label = paste0(value, " ", "(", percent, "%", ")"), y = value),
            position = position_stack(vjust = 0.5),
            color = "white") +
  labs(x = "", y  = "Number of children, 6 - 59 months,
  tested positive for malaria in 22 DHS datasets") +
  facet_wrap(vars(title))+
  theme(strip.text.x = element_text(
    size = 12, color = "black")) +
  coord_cartesian(ylim = c(0, 44000))

# create a dataframe for rural data with selected columns
plot_df_rn<- rural_df %>% 
  dplyr::select(country_year.x, home_type2, code_year, u5_net_use, id, strat, wt)

# prepare rural data for plotting with survey design
plot_rural = plot_df_rn %>% as_survey_design(ids= id,strata=strat,nest=T,weights= wt) %>% 
  mutate(net_use = ifelse(u5_net_use == 1, "Slept under \n a net", "Did not sleep \n under a net")) %>% 
  group_by(home_type2, net_use) %>% # group by home type and net use 
  summarise(value = round(survey_total(),0)) %>% # calculate total net use
  mutate(percent = round(value/sum(value) *100, 0)) # calculate percentage of net use

# set title for rural plot
plot_rural$title = "Rural"

# create bar plot for rural data
p_net_r<-ggplot(plot_rural, aes(fill=net_use, x= home_type2)) + 
  geom_bar(aes(y = value), position="stack", stat = "identity")+
  theme_manuscript()+
  scale_x_discrete(labels = c("Agricultural worker \n household (HH)", "Non-agricultural \n worker HH"))+
  scale_fill_manual(name = "", values= color)+
  geom_text(aes(label = paste0(value, " ", "(", percent, "%", ")"), y = value),
            position = position_stack(vjust = 0.5),
            color = "white") +
  labs(x = "", y  = "Number of children, 6 - 59 months,
  tested positive for malaria in 22 DHS datasets") +
  facet_wrap(vars(title))+
  theme(strip.text.x = element_text(
    size = 12, color = "black")) +
  coord_cartesian(ylim = c(0, 44000)) + 
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.y = element_blank())

# combine urban and rural plots
p_net_u + p_net_r

# create a combined plot with legend positioned at the bottom
p_2b = p_net_u + p_net_r + plot_layout(guides = "collect") & theme(legend.position = 'bottom')

# combine malaria positivity and net use plots and save the combined plot as pdf
p_malaria_p_net = p_2a / p_2b
ggsave(paste0(FigDir,"/", Sys.Date(),"malaria_test_positivity_netuse_agric_urban_rural.pdf"),p_malaria_p_net, width = 8, height= 7) 

## -----------------------------------------------------------------------------------------------------------------------------------------
### Figure 3
## -----------------------------------------------------------------------------------------------------------------------------------------

## -----------------------------------------------------------------------------------------------------------------------------------------
# calculate the difference in MALARIA POSITIVITY between agricultural and non-agricultural workers in URBAN areas
# prepare the country-level data for plotting
plot_country = plot_df_um %>% as_survey_design(ids= id,strata=strat,nest=T,weights= wt) %>% 
  group_by(country_year.x,home_type2, test_result) %>% # group by country, home type, and test result
  summarise(value = round(survey_total(),0))%>% # calculate total values
  mutate(percent = round(value/sum(value) *100, 0)) %>% # calculate percentage
  mutate(country_year.x = ifelse(country_year.x == "Congo Democratic Republic 2013 - 14", "DRC 2013 - 14",country_year.x),
         home_type2 = ifelse(home_type2 == 'A', 'Agricultural worker Household (HH)', "Non-Agricultural worker HH"))

# df_u <- all_df %>% group_by(country_year.x) %>%  summarise(total2 = sum(total))
# 
# plot_country <- plot_country  %>%  left_join(df_u, by = "country_year.x")  %>% mutate(plot_label = ifelse(test_result == "-ve", percent, NA))

# filter data for positive test results
plot_country = plot_country %>% filter(test_result == "+ve")

# calculate the difference in malaria test positivity rates
diff_d_u_malaria <- plot_country %>% group_by(country_year.x) %>%  
  mutate(diff_val_urban_malaria = percent[home_type2 == "Agricultural worker Household (HH)"] - percent) %>% # calculate the difference
  filter(home_type2 == "Non-Agricultural worker HH") # filter for non-agricultural households

# visualize the differences
diff_d_u_malaria$title = "Urban" # set title for the plot
p_diff_u_malaria<-ggplot(diff_d_u_malaria , aes(x = reorder(country_year.x, -diff_val_urban_malaria), y = diff_val_urban_malaria, fill)) +
  geom_bar(stat = "identity",  width = 0.7, fill = "#c55c80") +
  geom_text(aes(label= diff_val_urban_malaria), position = position_stack(vjust = 0.5),
            color = "black") +
  coord_flip() +
  theme(legend.title = element_blank()) +
  labs(x = "", y = "Percentage difference in malaria test positivity rates
      between agricultural worker HH and non-agricultural worker HH")+
  facet_wrap(vars(title))+
  theme_manuscript()+
  theme(legend.position = "bottom") + 
  ylim(0, 28)

## -----------------------------------------------------------------------------------------------------------------------------------------
# calculate the difference in NET USE between agricultural and non-agricultural workers in URBAN areas
# prepare the country-level data for plotting
plot_country = plot_df_un %>% as_survey_design(ids= id,strata=strat,nest=T,weights= wt) %>% 
  group_by(country_year.x,home_type2, u5_net_use) %>% # group by country, home type, and net use
  summarise(value = round(survey_total(),0))%>% # calculate total values
  mutate(percent = round(value/sum(value) *100, 0)) %>% # calculate percentage
  mutate(country_year.x = ifelse(country_year.x == "Congo Democratic Republic 2013 - 14", "DRC 2013 - 14",country_year.x),
         home_type2 = ifelse(home_type2 == 'A', 'Agricultural worker Household (HH)', "Non-Agricultural worker HH"))

# filter data for households using nets
plot_country = plot_country %>% filter( u5_net_use == "1")

# calculate the difference in net use rates
diff_d_u_nets <- plot_country %>% 
  group_by(country_year.x) %>%  mutate(diff_val_urban_nets = percent[home_type2 == "Agricultural worker Household (HH)"] - percent) %>% # calculate the difference
  filter(home_type2 == "Non-Agricultural worker HH") # filter for non-agricultural households


# visualize the differences
diff_d_u_nets$title = "Urban"
p_diff_u_nets<-ggplot(diff_d_u_nets , aes(x = reorder(country_year.x, -diff_val_urban_nets), y = diff_val_urban_nets, fill)) +
  geom_bar(stat = "identity",  width = 0.7, fill = "#c55c80") +
  geom_text(aes(label= diff_val_urban_nets), position = position_stack(vjust = 0.5),
            color = "black") +
  coord_flip() +
  theme(legend.title = element_blank()) +
  labs(x = "", y = "Percentage difference in net use rates
      between agricultural worker HH and non-agricultural worker HH")+
  facet_wrap(vars(title))+
  theme_manuscript()+
  theme(legend.position = "bottom") 

# join both dataframes (malaria and net use difference) to create a scatterplot
diff_d_u_malaria <-   diff_d_u_malaria %>%  
  select(country_year.x , diff_val_urban_malaria) # select relevant columns for malaria data

diff_d_u_nets <- diff_d_u_nets %>%  
  select(country_year.x, diff_val_urban_nets) # select relevant columns for net use data

# merge the two dataframes and categorize net use
df_m_n_country <- left_join(diff_d_u_malaria, diff_d_u_nets) %>% 
  mutate(net_category = if_else(diff_val_urban_nets<0, "lower_agric_coverage", "higher_agric_coverage")) # create a new category based on net use difference

# retrieve unique code_years from the original dataframe
code_year <- plot_df_um %>%  
  select(country_year.x, code_year) %>%  
  distinct(country_year.x,code_year) # keep distinct combinations of country and code_year

# join the code_year data and handle missing values
df_m_n_country <- df_m_n_country %>%  
  left_join(code_year) %>%  
  mutate(code_year = if_else(is.na(code_year) & country_year.x== 'DRC 2013 - 14',"CD2013", code_year )) # replace NA for DRC with a specific code

# set title for the plot
df_m_n_country$title = "Urban"

# create the scatterplot
country_m_n_urban <- ggplot(df_m_n_country , aes(x=diff_val_urban_nets, y=diff_val_urban_malaria, color = net_category, label=code_year)) +
  geom_point(shape=19, size= 5, alpha = 0.7) +
  geom_text(hjust=0, vjust=0) +
  scale_color_manual(values = c("#622c88", "#622c88")) +
  theme_manuscript() +
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  facet_wrap(vars(title)) +
  labs(x = "Difference in net use between agricultural worker\n HHs and non-agricultural worker HHs", y = "Difference in malaria test positivity rate between \n agricultural worker HHs and non-agricultural worker HHs") +
  theme(legend.position = "none") +
  ylim(-3, 28) +
  xlim(-20, 20) 

## -----------------------------------------------------------------------------------------------------------------------------------------
# calculate the difference in MALARIA POSITIVITY between agricultural and non-agricultural workers in RURAL areas
# prepare the country-level data for plotting
plot_country = plot_df_rm %>% as_survey_design(ids= id,strata=strat,nest=T,weights= wt) %>% 
  group_by(country_year.x,home_type2, test_result) %>% # group by country, home type, and test result
  summarise(value = round(survey_total(),0)) %>% # calculate total values and round them
  mutate(percent = round(value/sum(value) *100, 0)) %>% # calculate percentage of each group
  mutate(country_year.x = ifelse(country_year.x == "Congo Democratic Republic 2013 - 14", "DRC 2013 - 14",country_year.x), # standardize country name
         home_type2 = ifelse(home_type2 == 'A', 'Agricultural worker Household (HH)', "Non-Agricultural worker HH")) # label home types

# filter for positive test results
plot_country = plot_country %>% filter(test_result == "+ve")

# calculate difference in malaria positivity rates for rural households
diff_d_r_malaria <- plot_country %>% 
  group_by(country_year.x) %>% # group by country and year
  mutate(diff_val_rural_malaria = percent[home_type2 == "Agricultural worker Household (HH)"] - percent) %>% # calculate the difference
  filter(home_type2 == "Non-Agricultural worker HH") # filter for non-agricultural households

# set title for the plot
diff_d_r_malaria$title = "rural"

# create plot
p_diff_r_malaria <- ggplot(diff_d_r_malaria , aes(x = reorder(country_year.x, -diff_val_rural_malaria), y = diff_val_rural_malaria, fill)) +
  geom_bar(stat = "identity",  width = 0.7, fill = "#c55c80") +
  geom_text(aes(label= diff_val_rural_malaria), position = position_stack(vjust = 0.5),
            color = "black") +
  coord_flip() +
  theme(legend.title = element_blank()) +
  labs(x = "", y = "Percentage difference in malaria test positivity rates
      between agricultural worker HH and non-agricultural worker HH")+
  facet_wrap(vars(title))+
  theme_manuscript()+
  theme(legend.position = "bottom") 

## -----------------------------------------------------------------------------------------------------------------------------------------
# calculate the difference in NET USE between agricultural and non-agricultural workers in RURAL areas
# prepare the country-level data for plotting
plot_country = plot_df_rn %>% as_survey_design(ids= id,strata=strat,nest=T,weights= wt) %>% 
  group_by(country_year.x,home_type2, u5_net_use) %>% # group by country, home type, and net use
  summarise(value = round(survey_total(),0)) %>% # calculate total values and round them
  mutate(percent = round(value/sum(value) *100, 0)) %>% # calculate percentage of each group
  mutate(country_year.x = ifelse(country_year.x == "Congo Democratic Republic 2013 - 14", "DRC 2013 - 14",country_year.x), # standardize country name
         home_type2 = ifelse(home_type2 == 'A', 'Agricultural worker Household (HH)', "Non-Agricultural worker HH")) # label home types

# filter for households with net use
plot_country = plot_country %>% filter( u5_net_use == "1")

# calculate difference in net use rates for rural households
diff_d_r_nets <- plot_country %>% 
  group_by(country_year.x) %>% # group by country and year
  mutate(diff_val_rural_nets = percent[home_type2 == "Agricultural worker Household (HH)"] - percent) %>% # calculate the difference
  filter(home_type2 == "Non-Agricultural worker HH") # filter for non-agricultural households


# set title for the plot
diff_d_r_nets$title = "Urban"

# create the plot
p_diff_r_nets<-ggplot(diff_d_r_nets , aes(x = reorder(country_year.x, -diff_val_rural_nets), y = diff_val_rural_nets, fill)) +
  geom_bar(stat = "identity",  width = 0.7, fill = "#c55c80") +
  geom_text(aes(label= diff_val_rural_nets), position = position_stack(vjust = 0.5),
            color = "black") +
  coord_flip() +
  theme(legend.title = element_blank()) +
  labs(x = "", y = "Percentage difference in net use rates
      between agricultural worker HH and non-agricultural worker HH")+
  facet_wrap(vars(title))+
  theme_manuscript()+
  theme(legend.position = "bottom") 

## -----------------------------------------------------------------------------------------------------------------------------------------
# join both dataframes (malaria and net use difference) to create a scatterplot for RURAL areas

# select relevant columns from the malaria and nets dataframes
diff_d_r_malaria <- diff_d_r_malaria %>%
  select(country_year.x , diff_val_rural_malaria) # keep only country-year and malaria difference

diff_d_r_nets <- diff_d_r_nets %>%
  select(country_year.x, diff_val_rural_nets) # keep only country-year and nets difference

# perform a left join to combine the dataframes
df_m_n_country_rural <- left_join(diff_d_r_malaria, diff_d_r_nets) %>% 
  mutate(net_category = if_else(diff_val_rural_nets<0, "lower_agric_coverage", "higher_agric_coverage")) # categorize net use

# extract unique country-year and code_year combinations from the plot dataframe
code_year <- plot_df_rm %>%  
  select(country_year.x, code_year) %>%  
  distinct(country_year.x,code_year) # ensure distinct values

# join the code_year data and handle missing values
df_m_n_country_rural <- df_m_n_country_rural %>%  
  left_join(code_year) %>%  
  mutate(code_year = if_else(is.na(code_year) & country_year.x== 'DRC 2013 - 14',"CD2013", code_year )) # fill missing code_year

# add title for the rural data
df_m_n_country_rural$title = "Rural"

# create scatter plot comparing net use and malaria positivity rates
country_m_n_rural <- ggplot(df_m_n_country_rural , aes(x=diff_val_rural_nets, y=diff_val_rural_malaria, color = net_category, label=code_year))+
  geom_point(shape=19, size= 5, alpha = 0.7)+
  geom_text(hjust=0, vjust=0) +
  scale_color_manual(values =c("#e07a5f", "#e07a5f"))+
  theme_manuscript()+
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  facet_wrap(vars(title))+
  labs(x = "Difference in net use between agricultural worker\n HHs and non-agricultural worker HHs", y = "Difference in malaria test positivity rate between \n agricultural worker HHs and non-agricultural worker HHs")+
  theme(legend.position = "none")+
  ylim(-3,28) + 
  xlim(-20, 20) +
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.y = element_blank())

# combine urban and rural plots and save combined plot as pdf
p_country_malaria_nets <- country_m_n_urban + country_m_n_rural

ggsave(paste0(FigDir,"/", Sys.Date(),"malaria_test_positivity_netuse_agric_urban_rural_by_country.pdf"),p_country_malaria_nets, width = 8, height= 5) 

## =========================================================================================================================================
### Trend Analysis
## =========================================================================================================================================

## -----------------------------------------------------------------------------------------------------------------------------------------
### Read in analysis datasets from Chilo's extraction in file called 02b_descriptive_trend_analysis   
## -----------------------------------------------------------------------------------------------------------------------------------------

# read in the trend analysis datasets for malaria and net use
df_trend_malaria <- read_csv(file.path(PopDir, "analysis_dat/240606_urban_df_for_analysis_trend_malaria_chilo_created.csv"))
df_trend_net<- read_csv(file.path(PopDir, "analysis_dat/240606_urban_df_for_analysis_trend_net_chilo_created.csv"))

# urban analysis for malaria
urban_trend <- df_trend_malaria  %>%  
  filter(title == "Urban", cntryId...9 != "TZ", country_year.x...2 != "Mozambique 2011") %>% # filter relevant data 
  select(country_year.x...2, cntryId...5, agric_percent, non_agric_percent, diff_val_malaria, id) %>% # select specific columns
  rename(country_year = country_year.x...2, country_id = cntryId...5) %>% # rename columns for clarity
  mutate(id = if_else(country_year == "Mozambique 2015", "Preceding survey", id)) # update id for specific condition

# hardcode test positivity values for Mozambique 2022 based on above analysis
df <- data.frame(country_year = "Mozambique 2022 - 2023", agric_percent = 27, non_agric_percent = 9, diff_val_malaria = 27 - 9, id = "Most recent survey", country_id  = "MZ")

# join the trend data with hardcoded values
urban_malaria_trend <- rbind(urban_trend, df) %>%  
  select(country_year, diff_val_malaria, id, country_id) # select relevant columns

# urban analysis for nets
urban_net_trend <- df_trend_net %>% 
  filter(title == "Urban", cntryId...9 != "TZ", country_year.x...2 != "Mozambique 2011") %>% # filter relevant data
  select(country_year.x...2, cntryId...5, agric_percent, non_agric_percent, diff_val_net, id) %>% # select specific columns
  rename(country_year = country_year.x...2, country_id = cntryId...5) %>% # rename columns for clarity
  mutate(id = if_else(country_year == "Mozambique 2015", "Preceding survey", id)) # update id for specific condition

# hardcode test positivity values for Mozambique 2022 based on above analysis
df <- data.frame(country_year = "Mozambique 2022 - 2023", agric_percent = 50, non_agric_percent = 55, diff_val_net = 50 - 55, id = "Most recent survey", country_id  = "MZ")

# join the net trend data with hardcoded values
urban_net_trend <- rbind(urban_net_trend, df) %>%  
  select(country_year, diff_val_net, id, country_id)

# perform a left join to combine malaria and net trend data
urban_mal_net_trend <- left_join(urban_malaria_trend, urban_net_trend)

# add title for the urban data
urban_mal_net_trend$title = "Urban"

# create a scatter plot comparing net use and malaria positivity rates for urban areas
country_m_n_urban_trend <- ggplot(urban_mal_net_trend, aes(x=diff_val_net, y=diff_val_malaria, label=country_id))+
  geom_point(shape=19, size= 5, alpha = 0.7, aes(color =id))+
  geom_line(aes(group = country_id))+
  geom_text(hjust=0, vjust=0) +
  scale_color_manual(values =c("#622c88", "#ffc3f7"))+
  theme_manuscript()+
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  facet_wrap(vars(title))+
  labs(x = "Difference in net use between agricultural worker\n HHs and non-agricultural worker HHs", y = "Difference in malaria test positivity rate between \n agricultural worker HHs and non-agricultural worker HHs")+
  theme(legend.position = "bottom", legend.title = element_blank())+
  ylim(-4,27)+
  xlim(-38, 20) 

# rural analysis for malaria
rural_mal_trend <- df_trend_malaria  %>%  
  filter(title == "Rural", cntryId...9 != "TZ", country_year.x...2 != "Mozambique 2011") %>% # filter relevant data for rural areas
  select(country_year.x...2, cntryId...5, agric_percent, non_agric_percent, diff_val_malaria, id) %>% # select specific columns
  rename(country_year = country_year.x...2, country_id = cntryId...5) %>% # rename columns for clarity
  mutate(id = if_else(country_year == "Mozambique 2015", "Preceding survey", id)) # update id for specific condition

# hardcode test positivity values for Mozambique 2022 based on prior analysis
df <- data.frame(country_year = "Mozambique 2022 - 2023", agric_percent = 38, non_agric_percent = 41, diff_val_malaria = 38 - 41, id = "Most recent survey", country_id  = "MZ")

# join the rural trend data with hardcoded values
rural_malaria_trend <- rbind(rural_mal_trend, df)%>%  select(country_year, diff_val_malaria, id, country_id)

# rural analysis for nets
rural_net_trend <- df_trend_net %>% 
  filter(title == "Rural", cntryId...9 != "TZ", country_year.x...2 != "Mozambique 2011") %>% # filter relevant data for rural areas
  select(country_year.x...2, cntryId...5, agric_percent, non_agric_percent, diff_val_net, id) %>% # select specific columns
  rename(country_year = country_year.x...2, country_id = cntryId...5) %>% # rename columns for clarity
  mutate(id = if_else(country_year == "Mozambique 2015", "Preceding survey", id)) # update id for specific condition

# hardcode test positivity values for Mozambique 2022 based on above analysis
df <- data.frame(country_year = "Mozambique 2022 - 2023", agric_percent = 36, non_agric_percent = 37, diff_val_net = 36 - 37, id = "Most recent survey", country_id  = "MZ")

# join the net trend data with hardcoded values
rural_net_trend <- rbind(rural_net_trend, df) %>%  
  select(country_year, diff_val_net, id, country_id) # select relevant columns

# left join to combine rural malaria and net trend data
rural_mal_net_trend <- left_join(rural_malaria_trend, rural_net_trend)

# add title for the rural data
rural_mal_net_trend$title = "Rural"

# create a scatter plot comparing net use and malaria positivity rates for rural areas
country_m_n_rural_trend <- ggplot(rural_mal_net_trend, aes(x=diff_val_net, y=diff_val_malaria, label=country_id))+
  geom_point(shape=19, size= 5, alpha = 0.7, aes(color =id))+
  geom_line(aes(group = country_id))+
  geom_text(hjust=0, vjust=0) +
  scale_color_manual(values =c("#e07a5f", "#efdcac"))+
  theme_manuscript()+
  geom_vline(xintercept = 0) +
  geom_hline(yintercept = 0) +
  facet_wrap(vars(title))+
  labs(x = "Difference in net use between agricultural worker\n HHs and non-agricultural worker HHs", y = "Difference in malaria test positivity rate between \n agricultural worker HHs and non-agricultural worker HHs")+
  theme(legend.position = "bottom", legend.title = element_blank())+
  ylim(-4, 27)+
  xlim(-38, 20)+
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.y = element_blank())

# combine the urban and rural trend plots
p_trend_m_nets <- country_m_n_urban_trend + country_m_n_rural_trend

# add country plots and trend plots and save combined plots as pdf
all_country_descriptive <- p_country_malaria_nets / p_trend_m_nets + plot_annotation(tag_levels = 'A')
ggsave(paste0(FigDir,"/", Sys.Date(),"malaria_test_positivity_netuse_agric_urban_rural_by_country_with_trends.pdf"),all_country_descriptive, width = 8.1, height=9) 


## =========================================================================================================================================
### Supplement Figures - Differences by Variable Type
## =========================================================================================================================================

## -----------------------------------------------------------------------------------------------------------------------------------------
### Household Size
## -----------------------------------------------------------------------------------------------------------------------------------------

# combine urban and rural data frames into a single data frame
all_df <- rbind(urban_df, rural_df) %>%  
  mutate(home_type3 = ifelse(home_type2 == "A", "Agricultural worker \n Household (HH)", 
                             "Non-Agricultural \n worker HH")) # categorize home type based on home_type2

# convert type variable to a factor with specified levels for plotting
all_df$type_f <- factor(all_df$type, levels=c("Urban", "Rural"))

# perform t-test for urban household size by home type
test_df <- all_df %>%  
  filter(type == "Urban") %>%  # filter for urban data
  select(hh_size, home_type3) %>% 
  drop_na # remove missing values
t.test(test_df$hh_size ~ test_df$home_type3) # conduct t-test

# calculate Cohen's d for effect size
test_df2 <- test_df %>% 
  group_by(home_type3) %>%
  group_split() # split data by home type for separate calculations

# calculate Cohen's d
cohen_d <- (mean(test_df2[[1]]$hh_size) - mean(test_df2[[2]]$hh_size)) / 
  sqrt(((sd(test_df2[[1]]$hh_size)^2) + (sd(test_df2[[2]]$hh_size)^2)) / 2) # compute Cohen's d formula

# perform t-test for rural household size by home type
test_df <- all_df %>%  
  filter(type == "Rural") %>%  # filter for rural data
  select(hh_size, home_type3)  %>% 
  drop_na
t.test(test_df$hh_size ~ test_df$home_type3) # conduct t-test

# create a boxplot of household size by home type
p1 <- ggplot(all_df, aes(x = home_type3, y = hh_size, fill = home_type3)) +
  scale_fill_manual(name = '', values =c('#5560AB','#FAAF43'))+
  geom_boxplot(outlier.size = -1, color="black", alpha = 0.7) +
  #geom_point(aes(fill = home_type3), shape = 21,size=2, alpha=0.6, stroke=0, color = '#979797')+
  labs(x = "", y = "Household size", fill = "home type") +
  theme_manuscript()+
  theme(legend.position = 'none')+
  facet_wrap(vars(type_f)) +
  theme(strip.text.x = element_text(size = 12))+
  ylim(0, 16)

# display the plot and save as pdf
p1
ggsave(paste0(FigDir,"/", Sys.Date(),"agric_paper_covariate_plot_HH_size.pdf"), p1 , width = 4, height = 4)

## -----------------------------------------------------------------------------------------------------------------------------------------
### Roof Type
## -----------------------------------------------------------------------------------------------------------------------------------------

# categorize roof type and prepare survey design
all_df2 <- all_df %>%
  as_survey_design(ids = id, strata = strat, nest = T, weights = wt) %>%  # create a survey design object
  mutate(roof = ifelse(roof_type == 1, "Low-risk roof", "High-risk roof")) %>%  # categorize roof based on roof_type
  drop_na(roof) %>%  # remove rows with missing roof values
  mutate(roof_f = factor(roof, levels = c("Low-risk roof", "High-risk roof"))) %>%  # convert roof to a factor
  group_by(type_f, home_type3, roof_f) %>%  # group by type, home type, and roof type
  summarise(value = round(survey_total(), 0)) %>%  # calculate total survey values, rounded to nearest integer
  mutate(percent = round(value / sum(value) * 100, 0))  # calculate percentage of total values

# perform chi-squared test for urban households
test_df <- all_df %>%  
  filter(type == "Urban") %>%  # filter for urban data
  select(roof_type, home_type3) %>% 
  drop_na()  # remove missing values
table(test_df$home_type3, test_df$roof_type) # create contingency table
test <- chisq.test(test_df$home_type3, test_df$roof_type) # conduct chi-squared test
test$statistic # extract chi-squared statistic
test$p.value # extract p-value

# perform chi-squared test for rural households
test_df <- all_df %>%  
  filter(type == "Rural") %>%  # filter for rural data
  select(roof_type, home_type3) %>% 
  drop_na()  # remove missing values
table(test_df$home_type3, test_df$roof_type)  # create contingency table
test <- chisq.test(test_df$home_type3, test_df$roof_type)  # conduct chi-squared test
test$statistic  # extract chi-squared statistic
test$p.value  # extract p-value

# create a bar plot of roof type by home type
p2 <- ggplot(all_df2, aes(fill=roof_f, x= home_type3)) + 
  geom_bar(aes(y = percent), position="stack", stat = "identity", show.legend = F)+
  scale_fill_manual(name = "", values = c("bisque", "forestgreen"))+
  theme_manuscript() +
  facet_wrap(vars(type_f)) +
  labs(x = "Roof")+
  theme(strip.text.x = element_text(size = 12))

## -----------------------------------------------------------------------------------------------------------------------------------------
### Wealth
## -----------------------------------------------------------------------------------------------------------------------------------------

# convert wealth to a factor with specified levels and labels
all_df$wealth_f <- factor(all_df$wealth, levels=c("5", "4", "3", "2", "1"), 
                          labels =c("Richest", "Rich", "Middle", "Poor", "Poorest"))

# create a survey design object and summarize wealth data
all_df2 <- all_df %>%
  as_survey_design(ids= id,strata=strat,nest=T,weights= wt) %>% # create survey design
  group_by(type_f, home_type3, wealth_f) %>% # group by type, home type, and wealth category
  summarise(value = round(survey_total(),0)) %>% # calculate total values, rounded to nearest integer
  mutate(percent = round(value/sum(value) *100, 0)) # calculate percentage of total values

# perform chi-squared test for urban households
test_df <- all_df %>%  
  filter(type == "Urban") %>%  # filter for urban data
  select(wealth, home_type3) %>% 
  drop_na()  # remove missing values
table(test_df$home_type3, test_df$wealth)  # create contingency table
test <- chisq.test(test_df$home_type3, test_df$wealth)  # conduct chi-squared test
test$statistic  # extract chi-squared statistic
test$p.value  # extract p-value

# perform chi-squared test for rural households
test_df <- all_df %>%  
  filter(type == "Rural") %>%  # filter for rural data
  select(wealth, home_type3) %>% 
  drop_na()  # remove missing values
table(test_df$home_type3, test_df$wealth)  # create contingency table
test <- chisq.test(test_df$home_type3, test_df$wealth)  # conduct chi-squared test
test$statistic  # extract chi-squared statistic
test$p.value  # extract p-value

# create a bar plot of wealth distribution by home type  
p3 <- ggplot(all_df2, aes(fill=wealth_f, x= home_type3)) + 
  geom_bar(aes(y = percent), position="stack", stat = "identity",show.legend = F)+
  scale_fill_manual(name = "Wealth Quintiles", values= colorRampPalette(c("#bbdefb", "#0d47a1"))(5))+
  theme_manuscript() +
  facet_wrap(vars(type_f)) +
  theme(strip.text.x = element_text(size = 12), legend.title = element_blank())+
  labs(x= "")


## =========================================================================================================================================
### Reading in Environmental Data
## =========================================================================================================================================

# read in environmental data from a CSV file and create a new variable for code and year
df_env <- read.csv(file.path(PopDir, "analysis_dat/all_geospatial_monthly_DHS.csv")) %>% 
  mutate(code_year = paste(stringr::str_extract(.id, "^.{2}"), dhs_year, sep = "")) %>% # extract the first two characters from .id and concatenate with dhs_year
  select(-dhs_year) %>% # drop the original dhs_year column
  mutate(EVI_2000m_new = if_else(EVI_2000m < 0, 0, EVI_2000m)) # replace negative EVI values with 0

# display the structure of the dataframe
glimpse(df_env)

# add environmental data to urban and rural datasets
urban_df2 <- urban_df  %>% 
  left_join(df_env, by = c("code_year",  "hv001")) # join urban data with environmental data based on code_year and hv001

rural_df2 <- rural_df  %>% 
  left_join(df_env, by = c("code_year",   "hv001")) # join rural data with environmental data based on code_year and hv001

# combine urban and rural datasets and create a new home type variable
all_df <- rbind(urban_df2, rural_df2) %>%  
  mutate(home_type3 = ifelse(home_type2 == "A", "Agricultural worker \n Household (HH)", 
                             "Non-Agricultural \n worker HH")) # label home types based on home_type2

all_df$type_f <- factor(all_df$type, levels=c("Urban", "Rural")) # create a factor for type with specified levels

# write the final analysis dataset to a CSV file
write.csv(all_df, file.path(PopDir, "analysis_dat/urban_rural_analysis_data_for_modeling.csv"))

# check the number of missing values for selected environmental variables
check <- all_df %>%  
  select(code_year, hv001, EVI_2000m_new, preci_monthly_2000m, RH_monthly_2000m, temp_monthly_2000m) %>%  
  filter(temp_monthly_2000m < 0) # filter for cases where temperature values are below 0

# check the structure of the urban dataset with added environmental data
glimpse(urban_df2)

# calculate mean EVI by code year and interview month for urban data
EVI_df <- urban_df2 %>%  
  group_by(code_year, interview_month) %>% 
  summarise(mean_EVI = mean(EVI_2000m_new, na.rm = T)) # compute mean EVI, removing NA values

# create a scatter plot of mean EVI by interview month, faceted by code year
ggplot(EVI_df,  aes(x= interview_month, y = mean_EVI)) +
  geom_point()+ 
  facet_wrap(vars(code_year)) 

# perform t-test on urban data
test_df <- all_df %>%  
  filter(type == "Urban") %>% # filter for urban data
  select(home_type3, EVI_2000m_new) %>% # select relevant columns
  drop_na # remove rows with NA values

#result<- t.test(log(test_df$EVI_2000m_new) ~ test_df$home_type3)
#log_conf_int <- result$conf.int
#exp(log_conf_int)

# calculate Cohen's d for urban data
test_df2 <- test_df %>% 
  group_by(home_type3) %>% # calculate Cohen's d for urban data
  group_split() # split the grouped data into a list

# calculate Cohen's d
cohen_d <- (mean(test_df2[[1]]$EVI_2000m_new) - mean(test_df2[[2]]$EVI_2000m_new)) / 
  sqrt(((sd(test_df2[[1]]$EVI_2000m_new)^2) + (sd(test_df2[[2]]$EVI_2000m_new)^2)) / 2)

# perform t-test on rural data
test_df <- all_df %>%  
  filter(type == "Rural") %>% # filter for rural data
  select(home_type3, EVI_2000m_new) %>% # select relevant columns
  drop_na # remove rows with NA values

# perform t-test on rural data by home type
t.test(test_df$EVI_2000m_new ~ test_df$home_type3)

# calculate Cohen's d for rural data
test_df2 <- test_df %>% 
  group_by(home_type3) %>% # group by home type
  group_split() # split the grouped data into a list

# calculate Cohen's d
cohen_d <- (mean(test_df2[[1]]$EVI_2000m_new) - mean(test_df2[[2]]$EVI_2000m_new)) / 
  sqrt(((sd(test_df2[[1]]$EVI_2000m_new)^2) + (sd(test_df2[[2]]$EVI_2000m_new)^2)) / 2)

## -----------------------------------------------------------------------------------------------------------------------------------------
### Enhanced Vegetation Index (EVI)
## -----------------------------------------------------------------------------------------------------------------------------------------

# create box plot for Enhanced Vegetation Index (EVI)
p4 <- ggplot(all_df, aes(x = home_type3, y = EVI_2000m_new, fill = home_type3)) +
  scale_fill_manual(name = '', values =c('#5560AB','#FAAF43'))+
  geom_boxplot(outlier.size = -1, color="black", alpha = 0.7) +
  #geom_point(aes(fill = home_type3), shape = 21,size=2, alpha=0.6, stroke=0, color = '#979797')+
  labs(x = "", y = "Enhanced Vegetation Index", fill = "home type") +
  theme_manuscript()+
  theme(legend.position = 'none')+
  facet_wrap(vars(type_f)) +
  theme(strip.text.x = element_text(size = 12))
p4 # display the plot

# t-test on urban data
test_df <- all_df %>%  
  filter(type == "Urban") %>%  # filter for urban data
  select(home_type3, preci_monthly_2000m) %>%  # select relevant columns
  drop_na  # remove rows with NA values

# perform t-test on log-transformed precipitation data by home type
result<- t.test(log(test_df$preci_monthly_2000m) ~ test_df$home_type3)
log_conf_int <- result$conf.int # extract confidence interval from result
exp(log_conf_int) # exponentiate the confidence interval

# t-test on rural data
test_df <- all_df %>%  
  filter(type == "Rural") %>%  # filter for rural data
  select(home_type3, preci_monthly_2000m) %>%  # select relevant columns
  drop_na  # remove rows with NA values

# perform t-test on rural data by home type
t.test(test_df$preci_monthly_2000m ~ test_df$home_type3)

# perform t-test on log-transformed precipitation data for rural data
result<- t.test(log(test_df$preci_monthly_2000m) ~ test_df$home_type3)
log_conf_int <- result$conf.int # extract confidence interval from result
exp(log_conf_int) # exponentiate the confidence interval

# calculate Cohen's d for rural data
test_df2 <- test_df %>% 
  group_by(home_type3) %>%  # group by home type
  group_split()  # split the grouped data into a list

# calculate Cohen's d
cohen_d <- (mean(test_df2[[1]]$preci_monthly_2000m) - mean(test_df2[[2]]$preci_monthly_2000m)) / 
  sqrt(((sd(test_df2[[1]]$preci_monthly_2000m)^2) + (sd(test_df2[[2]]$preci_monthly_2000m)^2)) / 2)

## -----------------------------------------------------------------------------------------------------------------------------------------
### Precipitation
## -----------------------------------------------------------------------------------------------------------------------------------------

# create box plot for monthly precipitation
p5 <- ggplot(all_df, aes(x = home_type3, y = preci_monthly_2000m, fill = home_type3)) +
  scale_fill_manual(name = '', values =c('#5560AB','#FAAF43'))+
  geom_boxplot(outlier.size = -1, color="black", alpha = 0.7) +
  #geom_point(aes(fill = home_type3), shape = 21,size=2, alpha=0.6, stroke=0, color = '#979797')+
  labs(x = "", y = "Precipitation (mm)", fill = "home type") +
  theme_manuscript()+
  theme(legend.position = 'none')+
  facet_wrap(vars(type_f)) +
  theme(strip.text.x = element_text(size = 12))+
  ylim(0, 510)

# t-test on urban data for relative humidity **** ALL THIS CODE IS FOR RELATIVE HUMIDITY - SWITCH TO PRECIPITATION????? ****
test_df <- all_df %>%  
  filter(type == "Urban") %>%  # filter for urban data
  select(home_type3, RH_monthly_2000m) %>%  # select relevant columns
  drop_na  # remove rows with NA values

result <- t.test(log(test_df$RH_monthly_2000m) ~ test_df$home_type3)  # perform t-test on log-transformed RH data by home type
log_conf_int <- result$conf.int  # extract confidence interval from result
exp(log_conf_int)  # exponentiate the confidence interval

# calculate Cohen's d for urban data
test_df2 <- test_df %>% 
  group_by(home_type3) %>%  # group by home type
  group_split()  # split the grouped data into a list

# calculate Cohen's d
cohen_d <- (mean(test_df2[[1]]$RH_monthly_2000m) - mean(test_df2[[2]]$RH_monthly_2000m)) / 
  sqrt(((sd(test_df2[[1]]$RH_monthly_2000m)^2) + (sd(test_df2[[2]]$RH_monthly_2000m)^2)) / 2)

# t-test on rural data for relative humidity
test_df <- all_df %>%  
  filter(type == "Rural") %>%  # filter for rural data
  select(home_type3, RH_monthly_2000m) %>%  # select relevant columns
  drop_na  # remove rows with NA values

# perform t-test on log-transformed RH data by home type
result <- t.test(log(test_df$RH_monthly_2000m) ~ test_df$home_type3)
log_conf_int <- result$conf.int  # extract confidence interval from result
exp(log_conf_int)  # exponentiate the confidence interval

# calculate Cohen's d for rural data
test_df2 <- test_df %>% 
  group_by(home_type3) %>%  # group by home type
  group_split()  # split the grouped data into a list

# calculate Cohen's d
cohen_d <- (mean(test_df2[[1]]$RH_monthly_2000m) - mean(test_df2[[2]]$RH_monthly_2000m)) / 
  sqrt(((sd(test_df2[[1]]$RH_monthly_2000m)^2) + (sd(test_df2[[2]]$RH_monthly_2000m)^2)) / 2)

## -----------------------------------------------------------------------------------------------------------------------------------------
### Relative Humidity
## -----------------------------------------------------------------------------------------------------------------------------------------

# create box plot for relative humidity
p6 <- ggplot(all_df, aes(x = home_type3, y = RH_monthly_2000m, fill = home_type3)) +
  scale_fill_manual(name = '', values =c('#5560AB','#FAAF43'))+
  geom_boxplot(outlier.size = -1, color="black", alpha = 0.7) +
  #geom_point(aes(fill = home_type3), shape = 21,size=2, alpha=0.6, stroke=0, color = '#979797')+
  labs(x = "", y = "Relative Humidity (%)", fill = "home type") +
  theme_manuscript()+
  theme(legend.position = 'none')+
  facet_wrap(vars(type_f)) +
  theme(strip.text.x = element_text(size = 12)) +
  ylim(0, 80)

# t-test on urban data for temperature
test_df <- all_df %>%  
  filter(type == "Urban") %>%  # filter for urban data
  select(home_type3, temp_monthly_2000m) %>%  # select relevant columns
  drop_na  # remove rows with NA values

# perform t-test on log-transformed temperature data by home type
result <- t.test(log(test_df$temp_monthly_2000m) ~ test_df$home_type3)
log_conf_int <- result$conf.int  # extract confidence interval from result
exp(log_conf_int)  # exponentiate the confidence interval

# calculate Cohen's d for urban data
test_df2 <- test_df %>% 
  group_by(home_type3) %>%  # group by home type
  group_split()  # split the grouped data into a list

# calculate Cohen's d
cohen_d <- (mean(test_df2[[1]]$temp_monthly_2000m) - mean(test_df2[[2]]$temp_monthly_2000m)) / 
  sqrt(((sd(test_df2[[1]]$temp_monthly_2000m)^2) + (sd(test_df2[[2]]$temp_monthly_2000m)^2)) / 2)

# t-test on rural data for temperature
test_df <- all_df %>%  
  filter(type == "Rural") %>%  # filter for rural data
  select(home_type3, temp_monthly_2000m) %>%  # select relevant columns
  drop_na  # remove rows with NA values

# perform t-test on log-transformed temperature data by home type
result <- t.test(log(test_df$temp_monthly_2000m) ~ test_df$home_type3)
log_conf_int <- result$conf.int  # extract confidence interval from result
exp(log_conf_int)  # exponentiate the confidence interval

# calculate Cohen's d for rural data
test_df2 <- test_df %>% 
  group_by(home_type3) %>%  # group by home type
  group_split()  # split the grouped data into a list

# calculate Cohen's d
cohen_d <- (mean(test_df2[[1]]$temp_monthly_2000m) - mean(test_df2[[2]]$temp_monthly_2000m)) / 
  sqrt(((sd(test_df2[[1]]$temp_monthly_2000m)^2) + (sd(test_df2[[2]]$temp_monthly_2000m)^2)) / 2)

# create box plot for temperature
p7 <- ggplot(all_df, aes(x = home_type3, y = temp_monthly_2000m, fill = home_type3)) +
  scale_fill_manual(name = '', values =c('#5560AB','#FAAF43'))+
  geom_boxplot(outlier.size = -1, color="black", alpha = 0.7) +
  #geom_point(aes(fill = home_type3), shape = 21,size=2, alpha=0.6, stroke=0, color = '#979797')+
  labs(x = "", y = "Temperature (\u00B0C)", fill = "home type") +
  theme_manuscript()+
  theme(legend.position = 'none')+
  facet_wrap(vars(type_f)) +
  theme(strip.text.x = element_text(size = 12))+
  ylim(0, 35)

## -----------------------------------------------------------------------------------------------------------------------------------------
### Age
## -----------------------------------------------------------------------------------------------------------------------------------------

# create box plot for age
p8 <- ggplot(all_df, aes(x = home_type3, y = hc1, fill = home_type3)) +
  scale_fill_manual(name = '', values =c('#5560AB','#FAAF43'))+
  geom_boxplot(outlier.size = -1, color="black", alpha = 0.7) +
  #geom_point(aes(fill = home_type3), shape = 21,size=2, alpha=0.6, stroke=0, color = '#979797')+
  labs(x = "", y = "Age (months)", fill = "home type") +
  theme_manuscript()+
  theme(legend.position = 'none')+
  facet_wrap(vars(type_f)) +
  theme(strip.text.x = element_text(size = 12))+
  ylim(0, 59)
p8 # display the box plot

# wrap multiple plots into a single plot object and save the combined plot as a pdf
all_p1 <- wrap_plots(p1,p4, p5, p6, p7, p8) 
all_p1 # display the combined plot
ggsave(paste0(FigDir,"/", Sys.Date(),"agric_paper_covariate_plots.pdf"), all_p1, width = 8.5, height = 6)

# wrap another set of plots into a single plot object and save the combined plot as a pdf
all_p2 <- wrap_plots(p2, p3) 
ggsave(paste0(FigDir,"/", Sys.Date(),"agric_paper_covariate_plots_2.pdf"), all_p2, width = 8.5, height = 4)

## -----------------------------------------------------------------------------------------------------------------------------------------
### Gender
## -----------------------------------------------------------------------------------------------------------------------------------------

# mutate data frame to create new variables for gender and stunting status
all_df <- all_df %>% 
  mutate(gender = ifelse(hc27 == 2, "Female", "Male"), # assign gender based on hc27 value
         stunting_new = ifelse(hc70 < -300, "stunted", ifelse(hc70 > 8000, NA, "Not stunted"))) # assign stunting status based on hc70 value

# create a bar plot for gender distribution
p_gender <- ggplot(all_df, aes(x = home_type3, fill = gender)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format(scale = 100), limits = c(0, 1)) +
  # geom_text(aes(label = scales::percent(..count../sum(..count..), accuracy = 0.1), group = gender),
  #           stat = "count",
  #           position = position_fill(vjust = 0.5),
  #           color = "black") + 
  scale_fill_manual(values =c("#efdcac", "#e07a5f"))+
  labs(x = "", y = "Gender", fill = "home type") +
  theme_manuscript()+
  theme(legend.position = 'none')+
  facet_wrap(vars(type_f)) +
  theme(strip.text.x = element_text(size = 12))

p_gender # display the gender distribution plot

## -----------------------------------------------------------------------------------------------------------------------------------------
### Stunting
## -----------------------------------------------------------------------------------------------------------------------------------------

# create a bar plot for stunting distribution, filtering out missing values in stunting_new
p_stunting <- ggplot(all_df %>% drop_na(stunting_new), aes(x = home_type3, fill = stunting_new)) +
  geom_bar(position = "fill") +
  scale_y_continuous(labels = scales::percent_format(scale = 100), limits = c(0, 1)) +
  # geom_text(aes(label = scales::percent(..count../sum(..count..), accuracy = 0.1), group = stunting_new),
  #           stat = "count",
  #           position = position_fill(vjust = 0.5),
  #           color = "white") +
  scale_fill_manual(values =c("#d391fa", "#3e00b3"))+
  labs(x = "", y = "Stunting", fill = "home type") +
  theme_manuscript()+
  theme(legend.position = 'none')+
  facet_wrap(vars(type_f)) +
  theme(strip.text.x = element_text(size = 12))
p_stunting # display the stunting distribution plot

# combine various plots into a single figure layout
figure4_bottom <- p2 + p3 + p_gender + p_stunting 
figure4_bottom

# create a final figure layout with all_p1 above figure4_bottom and annotate with tag levels and save final figure as pdf
p_figure_4 <- all_p1 / figure4_bottom + plot_annotation(tag_levels = 'A')
p_figure_4
ggsave(paste0(FigDir,"/", Sys.Date(),"figure_4.pdf"), p_figure_4, width = 8.5, height = 11) 


# ###############################################################
# #old
# #plot by country 
# 
# #supplement figure 2
# p<-ggplot(plot_country , aes(x = reorder(country_year.x, -total2), y = percent, fill = test_result)) +
#   geom_bar(stat = "identity", position = "stack", width = 0.7) +
#   coord_flip() +
#   scale_fill_manual(name = "Malaria test result", labels= c("Negative", "positive"), values= color)+
#   facet_grid(. ~ home_type2, scales = "free", space = "free") +
#   geom_text(aes(label = paste0(plot_label, "%"), y = plot_label),
#             position = position_stack(vjust = 0.5),
#             color = "black") +
#   theme(legend.title = element_blank()) +
#   labs(x = "", y = "Percentage of children, 6 - 59 months 
#        tested for malaria in urban clusters per country")+
#   theme_manuscript()+
#   theme(legend.position = "bottom")
# p
# ggsave(paste0(SupDir,"/", Sys.Date(),"_sup_figure_2.pdf"), p, width = 8.5, height = 6) 
# 
# 
# 
# #diff figure 2 urban
# 
# 
# 
# 
# #rural
# 
# 
# 
# #rural by country figure for supplement 
# plot_country = plot_u_df %>%  group_by(country_year.x,home_type2, test_result) %>%  summarise(value= n()) %>% mutate(percent = round(value/sum(value) *100, 0)) %>% 
#   mutate(country_year.x = ifelse(country_year.x == "Congo Democratic Republic 2013 - 14", "DRC 2013 - 14",country_year.x),
#          home_type2 = ifelse(home_type2 == 'A', 'Agricultural worker Household (HH)', "Non-Agricultural worker HH")) %>% 
#   mutate(country_year.x = ifelse(country_year.x == "Cameroon 2018", "Cameroon 2018 - 19",country_year.x)) %>% mutate(plot_label = ifelse(test_result == "-ve", percent, NA))
# 
# df_u <- all_df %>% group_by(country_year.x) %>%  summarise(total2 = sum(total))
# 
# plot_country <- plot_country  %>%  left_join(df_u, by = "country_year.x") 
# 
# p_sup3 <-ggplot(plot_country , aes(x = reorder(country_year.x, -total2), y = percent, fill = test_result)) +
#   geom_bar(stat = "identity", position = "stack", width = 0.7) +
#   coord_flip() +
#   scale_fill_manual(name = "Malaria test result", labels= c("Negative", "positive"), values= color)+
#   facet_grid(. ~ home_type2, scales = "free", space = "free") +
#   geom_text(aes(label = paste0(plot_label, "%"), y = plot_label),
#             position = position_stack(vjust = 0.5),
#             color = "black") +
#   theme(legend.title = element_blank()) +
#   labs(x = "", y = "Percentage of children, 6 - 59 months 
#        tested for malaria in rural clusters per country")+
#   theme_manuscript()+
#   theme(legend.position = "bottom")
# p_sup3
# 
# ggsave(paste0(SupDir,"/", Sys.Date(),"_sup_figure_3.pdf"), p_sup3, width = 8.5, height= 6) 
# 
# 
# 
# #diff figure 2 rural
# plot_country = plot_country %>% filter(test_result == "+ve")
# diff_d_r_malaria <- plot_country %>% group_by(country_year.x) %>%  mutate(diff_val_rural_malaria = percent[home_type2 == "Agricultural worker Household (HH)"] - percent) %>% 
#   filter(home_type2 == "Non-Agricultural worker HH")
# diff_d_r$title = "Rural"
# p_diff_r<-ggplot(diff_d_r , aes(x = reorder(country_year.x, -diff_val_rural_malaria), y = diff_val_rural_malaria, fill)) +
#   geom_bar(stat = "identity",  width = 0.7, fill = "#c55c80") +
#   geom_text(aes(label= diff_val), position = position_stack(vjust = 0.5),
#                       color = "black") +
#   coord_flip() +
#   theme(legend.title = element_blank()) +
#   labs(x = "", y = "Percentage difference in malaria test positivity rates
#       between agricultural worker HH and non-agricultural worker HH")+
#   facet_wrap(vars(title))+
#   theme_manuscript()+
#   theme(legend.position = "bottom") +
#   ylim(-2, 28)
# 
# 
# all_diff_p <- p_diff_u + p_diff_r
# 
# ggsave(paste0(ExpDir,"/", Sys.Date(),"malaria_prevalence_diff_rural_urban_by_country.png"),all_diff_p, width = 8.5, height= 5) 
# 
# #new test positivity difference plot 
# 
# diff_u <- diff_d_u %>%  select(country_year.x, diff_val, title)
# diff_r <- diff_d_r %>%  select(country_year.x, diff_val, title)
# 
# all_diff <- rbind(diff_u, diff_r) %>%  group_by(country_year.x) 
# 
# p_dat <- all_diff%>% 
#   summarise(start = range(diff_val)[1], end = range(diff_val)[2]) %>% 
#   ungroup()
# 
# 
# p_2b<-ggplot(p_dat, aes(x = start, y = reorder(country_year.x, end)))+
#   geom_segment(aes(xend = end, yend = country_year.x)) +
#   
#   geom_point(
#     data = all_diff,
#     aes(diff_val, country_year.x, color = title), 
#     size = 4, alpha =0.7
#   ) +
#   
#   scale_color_manual(name= "", values=c( "darkgoldenrod1", "darkviolet")) +
#   scale_x_continuous(breaks = seq(-2, 28, by = 2))+
#   theme_manuscript() +
#   theme(legend.position = "bottom") +
#   labs(y = "", x = "Percentage difference in malaria test positivity rates
#       between agricultural worker HH and non-agricultural worker HH")
# p_2b
# #ggsave(paste0(FigDir,"/", Sys.Date(),"_new_malaria_prevalence_diff_rural__by_country.pdf"),p_2b, width = 8.5, height= 5) 
# 
# p_figure2 <- p_2a/p_2b
# ggsave(paste0(FigDir,"/", Sys.Date(),"_Figure_2.pdf"),p_figure2, width = 8.5, height= 9.5) 
# 
# 
# 
# #figure 3
# 
# #net use vs occupation category 
# color = c( "#621244", "#efeddb")
# plot_urban = urban_df %>%  
#   mutate(net_use = ifelse(u5_net_use == 1, "Slept under \n a net", "Did not sleep \n under a net")) %>% 
#   group_by(home_type2, net_use) %>%  
#   summarise(value= n()) %>% mutate(percent = round(value/sum(value) *100, 0))
# plot_urban$title = "Urban"
# 
# p<-ggplot(plot_urban, aes(fill=net_use, x= home_type2)) + 
#   geom_bar(aes(y = value), position="stack", stat = "identity")+
#   theme_manuscript()+
#   scale_x_discrete(labels = c("Agricultural worker \n household (HH)", "Non-agricultural \n worker HH"))+
#   scale_fill_manual(name = "", values= color)+
#   geom_text(aes(label = paste0(value, " ", "(", percent, "%", ")"), y = value),
#             position = position_stack(vjust = 0.5),
#             color = "white") +
#   labs(x = "", y  = "Number of children, 6 - 59 months,
#   tested positive for malaria in 22 DHS datasets") +
#   facet_wrap(vars(title))+
#   theme(strip.text.x = element_text(
#     size = 12, color = "black")) +
#   coord_cartesian(ylim = c(0, 44000))
# 
# 
# 
# 
# ggsave(paste0(ExpDir,"/", Sys.Date(),"netuse_agric_rural_urban.pdf"),p_net, width = 7.5, height= 5) 
# 
# 
# #urban 
# 
# plot_country = urban_df %>%  group_by(country_year.x,home_type2, u5_net_use) %>%  summarise(value= n()) %>% mutate(percent = round(value/sum(value) *100, 0)) %>% 
#   mutate(country_year.x = ifelse(country_year.x == "Congo Democratic Republic 2013 - 14", "DRC 2013 - 14",country_year.x),
#          home_type2 = ifelse(home_type2 == 'A', 'Agricultural worker Household (HH)', "Non-Agricultural worker HH"))
# 
# df_u <- all_df %>% group_by(country_year.x) %>%  summarise(total2 = sum(total))
# 
# plot_country <- plot_country  %>%  left_join(df_u, by = "country_year.x") 
# 
# #plot by country urban 
# 
# #supplement figure 4
# plot_country = plot_country  %>%  
#   mutate(net_use = ifelse(u5_net_use == 1, "Slept under \n a net", "Did not sleep \n under a net")) %>% mutate(plot_label = ifelse(net_use == "Did not sleep \n under a net", percent, NA)) 
# p_sup4 <-ggplot(plot_country , aes(x = reorder(country_year.x, -total2), y = percent, fill = net_use)) +
#   geom_bar(stat = "identity", position = "stack", width = 0.7) +
#   coord_flip() +
#   scale_fill_manual(name = "",  values= color)+
#   facet_grid(. ~ home_type2, scales = "free", space = "free") +
#   geom_text(aes(label = paste0(plot_label, "%"), y = plot_label),
#             position = position_stack(vjust = 0.5),
#             color = "white") +
#   theme(legend.title = element_blank()) +
#   labs(x = "", y = "Percent of children, 6 - 59 months \n tested for malaria in urban clusters per country")+
#   theme_manuscript()+
#   theme(legend.position = "bottom")
# p_sup4
# ggsave(paste0(SupDir,"/", Sys.Date(),"_sup_figure_4.pdf"), p_sup4, width = 8.5, height = 6) 
# 
# 
# 
# #diff 
# plot_country = plot_country %>% filter(u5_net_use == 1)
# diff_d_u <- plot_country %>% group_by(country_year.x) %>%  mutate(diff_val = percent[home_type2== "Agricultural worker Household (HH)"] - percent) %>% 
#   filter(home_type2 == "Non-Agricultural worker HH") 
# 
# diff_d_u$title = "Urban"
# p_diff_u<-ggplot(diff_d_u, aes(x = reorder(country_year.x, -diff_val), y = diff_val, fill)) +
#   geom_bar(stat = "identity",  width = 0.7, fill = "#c55c80") +
#   geom_text(aes(label= diff_val), position = position_stack(vjust = 0.5),
#             color = "black") +
#   coord_flip() +
#   theme(legend.title = element_blank()) +
#   labs(x = "", y = "Percentage difference in net use
#       between agricultural worker HH and non-agricultural worker HH")+
#   facet_wrap(vars(title))+
#   theme_manuscript()+
#   theme(legend.position = "bottom") 
# 
# 
# 
# #rural 
# 
# plot_country = rural_df %>%  group_by(country_year.x,home_type2, u5_net_use) %>%  summarise(value= n()) %>% mutate(percent = round(value/sum(value) *100, 0)) %>% 
#   mutate(country_year.x = ifelse(country_year.x == "Congo Democratic Republic 2013 - 14", "DRC 2013 - 14",country_year.x),
#          home_type2 = ifelse(home_type2 == 'A', 'Agricultural worker Household (HH)', "Non-Agricultural worker HH")) %>% 
#   mutate(country_year.x = ifelse(country_year.x == "Cameroon 2018", "Cameroon 2018 - 19",country_year.x))
# 
# 
# df_u <- all_df %>% group_by(country_year.x) %>%  summarise(total2 = sum(total))
# 
# plot_country <- plot_country  %>%  left_join(df_u, by = "country_year.x") 
# 
# 
# #plot by country rural
# 
# #supplement figure
# plot_country = plot_country %>% mutate(net_use = ifelse(u5_net_use == 1, "Slept under \n a net", "Did not sleep \n under a net")) %>% 
#   mutate(plot_label = ifelse(net_use == "Did not sleep \n under a net", percent, NA)) 
# p_sup5 <-ggplot(plot_country , aes(x = reorder(country_year.x, -total2), y = percent, fill = net_use)) +
#   geom_bar(stat = "identity", position = "stack", width = 0.7) +
#   coord_flip() +
#   scale_fill_manual(name = "",  values= color)+
#   facet_grid(. ~ home_type2, scales = "free", space = "free") +
#   geom_text(aes(label = paste0(plot_label, "%"), y = plot_label),
#             position = position_stack(vjust = 0.5),
#             color = "white") +
#   theme(legend.title = element_blank()) +
#   labs(x = "", y = "Percent of children, 6 - 59 months \n tested for malaria in rural clusters per country")+
#   theme_manuscript()+
#   theme(legend.position = "bottom")
# p_sup5
# ggsave(paste0(SupDir,"/", Sys.Date(),"_sup_figure_5.pdf"), p_sup5, width = 8.5, height = 6) 
# 
# #diff 
# 
# plot_country = plot_country %>% filter(u5_net_use == 1)
# diff_d_r <- plot_country %>% group_by(country_year.x) %>%  mutate(diff_val = percent[home_type2== "Agricultural worker Household (HH)"] - percent) %>% 
#   filter(home_type2 == "Non-Agricultural worker HH") 
# 
# diff_d_r$title = "Rural"
# p_diff_r<-ggplot(diff_d_r , aes(x = reorder(country_year.x, -diff_val), y = diff_val, fill)) +
#   geom_bar(stat = "identity",  width = 0.7, fill = "#c55c80") +
#   geom_text(aes(label= diff_val), position = position_stack(vjust = 0.5),
#             color = "black") +
#   coord_flip() +
#   theme(legend.title = element_blank()) +
#   facet_wrap(vars(title))+
#   theme_manuscript()+
#   labs(x = "", y = "Percentage difference in net use
#       between agricultural worker HH and non-agricultural worker HH")+
#   theme(legend.position = "bottom") 
# 
# all_diff_p <- p_diff_u + p_diff_r
# 
# ggsave(paste0(ExpDir,"/", Sys.Date(),"net_use_diff_rural_urban_by_country.pdf"),all_diff_p, width = 8.5, height= 5) 
# 
# 
# #new net difference plot 
# 
# diff_u <- diff_d_u %>%  select(country_year.x, diff_val, title)
# diff_r <- diff_d_r %>%  select(country_year.x, diff_val, title)
# 
# all_diff <- rbind(diff_u, diff_r) %>%  group_by(country_year.x) 
# 
# p_dat <- all_diff%>% 
#   summarise(start = range(diff_val)[1], end = range(diff_val)[2]) %>% 
#   ungroup()
# 
# 
# p_3b <-ggplot(p_dat, aes(x = start, y = reorder(country_year.x, end)))+
#   geom_segment(aes(xend = end, yend = country_year.x)) +
#   
#   geom_point(
#     data = all_diff,
#     aes(diff_val, country_year.x, color = title), 
#     size = 4, alpha =0.7
#   ) +
#   
#   scale_color_manual(name= "", values=c( "darkorange", "darkolivegreen")) +
#   scale_x_continuous(breaks = seq(-15, 10, by = 3))+
#   theme_manuscript() +
#   theme(legend.position = "bottom") + 
#   labs(y = "", x = "Percentage difference in net use 
#       between agricultural worker HH and non-agricultural worker HH")
# p_3b
# p_figure3 <- p_net/p_3b
# ggsave(paste0(FigDir,"/", Sys.Date(),"_Figure_3.pdf"),p_figure3, width = 8.5, height= 9.5) 
# 
# ############################################################################
# #does agricultural worker HHs have greater diarrheal disease burden? 23 vs 20%, not much of a difference there
# ############################################################################
# 
# df3 <- urban_df %>% mutate(diarrhea_grp = ifelse(total_diarrhea >= 1, "Had diarrhea", "No diarrhea")) %>% 
#   select(home_type2, diarrhea_grp) %>% drop_na() %>% 
#   group_by(home_type2, diarrhea_grp) %>%   summarise(value= n()) %>% 
#   mutate(percent = round(value/sum(value) *100, 0)) %>%  ungroup() %>%  drop_na(diarrhea_grp)
# 
# 
# ggplot(df3, aes(fill=diarrhea_grp, x= home_type2)) + 
#   geom_bar(aes(y = value), position="stack", stat = "identity")+
#   #scale_fill_manual(name = "", label = c(" House was sprayed", "House was not sprayed"), values = c("#d391fa", "#3e00b3"))+
#   theme_manuscript() +
#   geom_text(aes(label = paste0(value, " ", "(", percent, "%", ")"), y = value),
#             position = position_stack(vjust = 0.5),
#             color = "white")+ 
#   labs(x = "")+
#   scale_x_discrete(labels = c("Agricultural worker \n household (HH)", "Non-agricultural \n worker HH"))
# 
# ############################################################################
# #does agricultural worker HHs have greater malaria environmental drivers
# ############################################################################
# 
# df_env <- read.csv(file.path(PopDir, "analysis_dat/all_geospatial_monthly_DHS.csv")) %>% 
#   mutate(code_year = paste(stringr::str_extract(.id, "^.{2}"), dhs_year, sep = ""))
# 
# all_env_df <- rbind(urban_df, rural_df) %>% left_join(df_env, by = c("code_year", "hv001")) %>%
#   mutate(hv025 = ifelse(hv025 == 1, "urban", "rural"))
# 
# 
# p_evi <- box_plot_fun(all_env_df, "EVI_2000m", "home_type2") + labs(title = "Enhanced Vegetation Index (EVI)", y = "EVI")
# p_prec <- box_plot_fun(all_env_df, "preci_monthly_2000m", "home_type2")  + labs(title = "Precipitation", y = "precipitation")
# p_RH <- box_plot_fun(all_env_df, "RH_monthly_2000m", "home_type2")  + labs(title = "Relative humidity", y = "relative humidity") + theme(legend.position = "bottom")
# p_temp <- box_plot_fun(all_env_df, "temp_monthly_2000m", "home_type2")  + labs(title = "Temperature", y = "temperature")
# 
# P_all_env <- p_evi + p_prec + p_RH + p_temp
# P_all_env
# 
# ggsave(paste0(SupDir,"/", Sys.Date(),"_sup_fig_6.pdf"), P_all_env, width = 8.5, height= 6) 
# 
# #mean difference test for environmental variables per household type. 
# 
# df_list <-split(all_env_df, all_env_df$type)
# 
# ttest_fun <- function(var1, dataset){
#   ttest_result <- t.test(var1 ~ home_type2, data = dataset, var.equal = TRUE)
#   
#   return(c(ttest_result$p.value))
# }
# 
# vars <- c("EVI", "Precipiatation", "R. Humidity", "Temperature")
# #rural
# rural_urban_fun <- function(subscrp) {
#   ttest_evi_r <- ttest_fun(df_list[[subscrp]]$EVI_2000m, df_list[[subscrp]])
#   ttest_prec_r <- ttest_fun(df_list[[subscrp]]$preci_monthly_2000m, df_list[[subscrp]])
#   ttest_RH_r <- ttest_fun(df_list[[subscrp]]$RH_monthly_2000m, df_list[[subscrp]])
#   ttest_temp_r <- ttest_fun(df_list[[subscrp]]$temp_monthly_2000m, df_list[[subscrp]])
#   
#   return(data.frame(c(ttest_evi_r, ttest_prec_r, ttest_RH_r, ttest_temp_r), vars))
# }
# 
# rural_pvalue <- rural_urban_fun(1)
# urban_pvalue <- rural_urban_fun(2)
# 
# 
