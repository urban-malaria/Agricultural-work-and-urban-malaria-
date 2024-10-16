# ==========================================================================================================================================
# Script Name: Descriptive Trend Analysis
# Author: Dr. Ifeoma Ozodiegwu
# Edited by: Grace Legris, Research Data Analyst (gracebea@gmail.com)
# Edited: [2024-10-08]
# Purpose: Generate Figures 1, 4, and 5
# ==========================================================================================================================================

rm(list = ls())

## =========================================================================================================================================
### Directory Management and File Paths
## =========================================================================================================================================

user <- Sys.getenv("USER")
if ("ido0493" %in% user) {
  user_path <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("OneDrive"))))
  DriveDir <- file.path(user_path, "urban_malaria")
  PopDir <- file.path(DriveDir, "data", "data_agric_analysis")
  ManDir <- file.path(DriveDir, "projects", "Manuscripts", "agriculture_malaria_manuscript")
  FigDir <- file.path(ManDir, "figures", "main_figures","pdf_figures")
  SupDir <- file.path(ManDir, "figures", "supplementary", "pdf_figures")
  ExpDir <- file.path(ManDir, "figures", "exploratory")
} else if  ("CHZCHI003" %in% user) {
  Drive <- file.path("C:/Users/CHZCHI003/OneDrive")
  DriveDir <- file.path(Drive, "urban_malaria")
  PopDir <- file.path(DriveDir, "data", 'data_agric_analysis')
  ManDir <- file.path(DriveDir, "projects", "Manuscripts", "agriculture_malaria_manuscript")
  FigDir <- file.path(ManDir, "figures", "main_figures","pdf_figures")
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
  FigDir <- file.path(ManDir, "figures", "main_figures","pdf_figures")
  SupDir <- file.path(ManDir, "figures", "supplementary", "pdf_figures")
  ExpDir <- file.path(ManDir, "figures", "exploratory")
}


## =========================================================================================================================================
### Required Functions and Settings
## =========================================================================================================================================

#note before sourcing functions and packages, run the code below to download rdhs if you don't already have it
#devtools::install_github("ropensci/rdhs")
source("functions/functions_employment.R")
options(survey.lonely.psu="adjust")  # this option allows admin units with only one cluster to be analyzed


## =========================================================================================================================================
### Read in Analysis Datasets
## =========================================================================================================================================

urban_df <- read_csv(file.path(PopDir, "analysis_dat/urban_df_for_analysis_trend.csv")) %>%  mutate(type ="urban_data") 
rural_df <- read_csv(file.path(PopDir,"analysis_dat/rural_df_for_analysis_trend.csv")) %>%  mutate(type ="rural_data")


## =========================================================================================================================================
### Plots
## =========================================================================================================================================

#totals by country 
# all_df <- rbind(urban_df, rural_df) %>% mutate(country_year.x = ifelse(country_year.x == "Congo Democratic Republic 2013 - 14", "DRC 2013 - 14",
#                                                                        ifelse(country_year.x == "Cameroon 2018", "Cameroon 2018 - 19",country_year.x))) %>% 
#   as_survey_design(ids= id,strata=strat,nest=T,weights= wt)%>% 
#   group_by(country_year.x) %>%  summarize(total = round(survey_total(),0)) %>%  ungroup() 


# combine urban and rural datasets and clean the country-year names
# adjust country-year names to ensure consistency across datasets
all_df <- rbind(urban_df, rural_df) %>% 
  mutate(country_year.x = ifelse(country_year.x == "Congo Democratic Republic 2013 - 14", "DRC 2013 - 14",
                                 ifelse(country_year.x == "Uganda 2009", "Uganda 2009 - 10",
                                        ifelse(country_year.x == "Cameroon 2018", "Cameroon 2018 - 19", country_year.x)))) %>%
  
  # convert the dataframe into a survey design object with stratified sampling
  as_survey_design(ids = id, strata = strat, nest = TRUE, weights = wt) %>% 
  
  # group by country and type (urban/rural), then calculate the total using survey weights
  group_by(country_year.x, type) %>%
  summarize(total = round(survey_total(), 0)) %>%
  ungroup()

# calculate the percentage for each type within each country
df <- all_df%>% 
  group_by(country_year.x) %>%  
  summarise(percent =  round(total/sum(total) * 100, 0))

# merge the percentage data back with the original dataset and create plot labels for rural data
all <- cbind(all_df, df) %>%
  dplyr::select(-c("country_year.x")) %>%
  mutate(plot_label = ifelse(type == "rural_data", percent, NA))

# create the base data for figure 1b by combining urban and rural datasets
df <- rbind(urban_df, rural_df)

# select relevant columns for further analysis or visualization of urban data
plot_u_df2 <- urban_df %>% dplyr::select(country_year.x, home_type2, code_year, test_result, id, strat, wt) 

# summarize overall test results by home type and test outcome, calculate percentages
plot_overall <- plot_u_df2 %>% 
  group_by(home_type2, test_result) %>% 
  summarise(value= n()) %>% 
  mutate(percent = round(value/sum(value) *100, 0))
plot_overall$title = "Urban"

# test: make plot_u_df2 a survey design object to enable extraction of confidence intervals
plot_u_df2_survey <- plot_u_df2 %>%
  as_survey_design(ids = id, strata = strat, nest = TRUE, weights = wt)

# test: summarize test results by country, year, home type, and test outcome, calculate percentages with CIs
plot_country_urban_ci <- plot_u_df2_survey %>%
  group_by(country_year.x, code_year, home_type2, test_result) %>%
  summarize(
    percent = survey_mean(vartype = "ci") * 100,  # calculate percentage and confidence intervals
    total = survey_total()  # total count of observations
  ) %>%
  mutate(
    #country_year.x = ifelse(country_year.x == "Congo Democratic Republic 2013 - 14", 
                            #"DRC 2013 - 14", country_year.x),  # rename long names for clarity
    home_type2 = ifelse(home_type2 == 'A', 
                        'Agricultural worker Household (HH)', 
                        "Non-Agricultural worker HH")
  )

# # summarize test results by country, year, home type, and test outcome, calculate percentages - DOES NOT INCLUDE CI
# plot_country <- plot_u_df2 %>%
#   group_by(country_year.x, code_year, home_type2, test_result) %>%
#   summarise(value = n()) %>%                                        # count occurrences for each group
#   mutate(percent = round(value / sum(value) * 100, 0)) %>%          # calculate the percentage for each test result
#   # Rename certain values for clarity in the plot
#   mutate(country_year.x = ifelse(country_year.x == "Congo Democratic Republic 2013 - 14", 
#                                  "DRC 2013 - 14", country_year.x),   # shorten long country-year names for DRC
#          home_type2 = ifelse(home_type2 == 'A', 
#                              'Agricultural worker Household (HH)', 
#                              "Non-Agricultural worker HH"))          # replace 'A' with a descriptive label for home types

# summarize the total number of records for each country-year
df_u <- all_df %>%
  group_by(country_year.x) %>%
  summarise(total2 = sum(total))                                      # calculate total count for each country-year

# merge the summarized total count with the country plot data
plot_country_urban_ci <- plot_country_urban_ci %>%
  left_join(df_u, by = "country_year.x") %>%                          # left join to bring in the total count for each country-year
  # create plot labels and extract country codes for further analysis
  mutate(plot_label = ifelse(test_result == "-ve", percent, NA),      # create a label for negative test results
         cntryId = stringr::str_extract(code_year, "^.{2}"))          # extract first two characters of code_year as the country ID

# filter plot_country to only include positive test results and rename CI variables
plot_country_urban_ci <- plot_country_urban_ci %>%
  filter(test_result == "+ve") %>%
  rename(lower_ci = percent_low, upper_ci = percent_upp)

# calculate the difference in percentages for agricultural vs. non-agricultural households
diff_d_u <- plot_country_urban_ci %>%
  group_by(country_year.x) %>%  # group by country-year for comparison
  mutate(diff_val = percent[home_type2 == "Agricultural worker Household (HH)"] - percent) %>%  # calculate difference
  filter(home_type2 == "Non-Agricultural worker HH")  # retain only non-agricultural households

diff_d_u$title <- "Urban"  # assign title for urban data

## -----------------------------------------------------------------------------------------------------------------------------------------
### Rural Plot
## -----------------------------------------------------------------------------------------------------------------------------------------

# process rural data
plot_r_df2 <- rural_df %>%
  dplyr::select(country_year.x, home_type2, code_year, test_result, id, strat, wt)  # select relevant columns from rural_df

# summarize overall test results for rural areas by home type and test outcome
plot_overall <- plot_r_df2 %>%
  group_by(home_type2, test_result) %>%  # group by home type and test result
  summarise(value = n()) %>%  # count occurrences for each group
  mutate(percent = round(value / sum(value) * 100, 0))  # calculate the percentage for each test result

plot_overall$title <- "Rural"  # assign title for rural data

# test: make plot_u_df2 a survey design object to enable extraction of confidence intervals
plot_r_df2_survey <- plot_r_df2 %>%
  as_survey_design(ids = id, strata = strat, nest = TRUE, weights = wt)

# test: summarize test results by country, year, home type, and test outcome, calculate percentages with CIs
plot_country_rural_ci <- plot_r_df2_survey %>%
  group_by(country_year.x, code_year, home_type2, test_result) %>%
  summarize(
    percent = survey_mean(vartype = "ci") * 100,  # calculate percentage and confidence intervals
    total = survey_total()  # total count of observations
  ) %>%
  mutate(
    #country_year.x = case_when(
      #country_year.x == "Congo Democratic Republic 2013 - 14" ~ "DRC 2013 - 14",  # rename DRC
      #country_year.x == "Cameroon 2018" ~ "Cameroon 2018 - 19",  # correct Cameroon years
      #TRUE ~ country_year.x  # keep the rest unchanged
    #),
    home_type2 = ifelse(home_type2 == 'A', 
                        'Agricultural worker Household (HH)', 
                        "Non-Agricultural worker HH")
  )

# # summarize rural data by country, home type, and test result for the supplement figure - DOES NOT INCLUDE CIs
# plot_country <- plot_r_df2 %>%
#   group_by(country_year.x, home_type2, test_result) %>%  # group by country-year, home type, and test result
#   summarise(value = n()) %>%  # count occurrences for each group
#   mutate(percent = round(value / sum(value) * 100, 0)) %>%  # calculate percentage for each test result
#   mutate(country_year.x = ifelse(country_year.x == "Congo Democratic Republic 2013 - 14", "DRC 2013 - 14", country_year.x),  # rename country-year for consistency
#          home_type2 = ifelse(home_type2 == 'A', 'Agricultural worker Household (HH)', "Non-Agricultural worker HH")) %>%  # rename home type
#   mutate(country_year.x = ifelse(country_year.x == "Cameroon 2018", "Cameroon 2018 - 19", country_year.x)) %>%  # correct Cameroon year
#   mutate(plot_label = ifelse(test_result == "-ve", percent, NA))  # create plot label for negative test results

# calculate total counts for each country
df_r <- all_df %>%
  group_by(country_year.x) %>%  # group by country-year
  summarise(total2 = sum(total))  # sum total counts

# join the total counts with the plot_country data
plot_country_rural_ci <- plot_country_rural_ci %>%
  left_join(df_r, by = "country_year.x") %>%                          # left join to bring in the total count for each country-year
  # create plot labels and extract country codes for further analysis
  mutate(plot_label = ifelse(test_result == "-ve", percent, NA),      # create a label for negative test results
         cntryId = stringr::str_extract(code_year, "^.{2}"))          # extract first two characters of code_year as the country ID

# filter plot_country to only include positive test results and rename CI variables
plot_country_rural_ci <- plot_country_rural_ci %>%
  filter(test_result == "+ve") %>%
  rename(lower_ci = percent_low, upper_ci = percent_upp)

# calculate differences in percentages for agricultural vs. non-agricultural households
diff_d_r <- plot_country_rural_ci %>%
  group_by(country_year.x) %>%  # group by country-year
  mutate(diff_val = percent[home_type2 == "Agricultural worker Household (HH)"] - percent) %>%  # calculate difference
  filter(home_type2 == "Non-Agricultural worker HH")  # retain only non-agricultural households

diff_d_r$title <- "Rural"  # assign title for rural data

## -----------------------------------------------------------------------------------------------------------------------------------------
### Plot: New Test Positivity Difference Plot
## -----------------------------------------------------------------------------------------------------------------------------------------

# select relevant columns from urban and rural difference datasets
diff_u <- diff_d_u %>% 
  dplyr::select(country_year.x, diff_val, title)

diff_r <- diff_d_r %>%  
  dplyr::select(country_year.x, diff_val, title)

# combine urban and rural differences into a single dataframe
all_diff <- rbind(diff_u, diff_r) %>%  
  group_by(country_year.x)  # group by country-year

# summarize the range of difference values for each country-year
p_dat <- all_diff %>%  
  summarise(start = range(diff_val)[1], end = range(diff_val)[2]) %>%  
  ungroup()  # remove grouping

# factor country-year for proper ordering in plots
p_dat$country_year.x <- factor(p_dat$country_year.x, levels = rev(unique(p_dat$country_year.x)))  

# create the ggplot for differences in malaria test positivity rates
p_2b <- ggplot(p_dat, aes(x = start, y = country_year.x, end)) +  
  geom_segment(aes(xend = end, yend = country_year.x)) +  # draw segments for ranges
  geom_point(  # add points for difference values
    data = all_diff,  
    aes(diff_val, country_year.x, color = title),  
    size = 4, alpha = 0.7  # set point size and transparency
  ) +  
  scale_color_manual(name = "", values = c("darkgoldenrod1", "darkviolet")) +
  scale_x_continuous(breaks = seq(-2, 28, by = 2)) +  # set x-axis breaks
  theme_manuscript() +  # apply custom theme
  theme(legend.position = "bottom") +
  labs(y = "", x = "Percentage difference in malaria test positivity rates\nbetween agricultural worker HH and non-agricultural worker HH")

# display the plot
p_2b


## =========================================================================================================================================
### Country Level - Difference BETWEEN Household Type
## =========================================================================================================================================

## -----------------------------------------------------------------------------------------------------------------------------------------
### Urban
## -----------------------------------------------------------------------------------------------------------------------------------------

# prepare urban data for plotting
plot_u_df2 <- urban_df %>% 
  dplyr::select(country_year.x, home_type2, code_year, test_result)

# summarize overall results by home type and test result
plot_overall <- plot_u_df2 %>%  
  group_by(home_type2, test_result) %>%  
  summarise(value = n()) %>%  # count occurrences
  mutate(percent = round(value / sum(value) * 100, 0))  # calculate percentage

plot_overall$title <- "Urban"  # assign title for urban data

# create country-level summary of test results
plot_country <- plot_u_df2 %>%  
  group_by(country_year.x, code_year, home_type2, test_result) %>%  
  summarise(value = n()) %>%  # count occurrences
  mutate(percent = round(value / sum(value) * 100, 0)) %>%  # calculate percentage
  mutate(
    country_year.x = ifelse(country_year.x == "Congo Democratic Republic 2013 - 14", "DRC 2013 - 14", country_year.x),  # rename DRC
    home_type2 = ifelse(home_type2 == 'A', 'Agricultural worker Household (HH)', "Non-Agricultural worker HH")  # rename household types
  )

# calculate total for each country
df_u <- all_df %>% 
  group_by(country_year.x) %>%  
  summarise(total2 = sum(total))  # sum total

# join the total data with the country-level summary
plot_country <- plot_country %>%  
  left_join(df_u, by = "country_year.x") %>%  
  mutate(
    plot_label = ifelse(test_result == "-ve", percent, NA),  # create labels for negative test results
    cntryId = stringr::str_extract(code_year, "^.{2}")  # extract country ID
  )

# prepare difference figure for urban data
plot_country <- plot_country %>% 
  filter(test_result == "+ve") %>%  # filter for positive test results
  group_by(home_type2, cntryId) %>%
  mutate(id = as.character(row_number())) %>%  # create unique ID for each row
  mutate(cntryId_id = paste(cntryId, id))  # create combined country ID and row number

# calculate difference for non-agricultural worker households
diff_d_u_a <- plot_country %>% 
  filter(home_type2 != "Non-Agricultural worker HH") %>%  # filter out non-agricultural households
  group_by(cntryId) %>%  
  mutate(diff_val = percent[id == "2"] - percent) %>%  # calculate difference
  filter(id != "2")  # exclude id "2"

# calculate difference for agricultural worker households
diff_d_u <- plot_country %>% 
  filter(home_type2 != "Agricultural worker Household (HH)") %>%  # filter out agricultural households
  group_by(cntryId) %>%  
  mutate(diff_val = percent[id == "2"] - percent) %>%  # calculate difference
  filter(id != "2") %>%  # exclude id "2"
  rbind(diff_d_u_a)  # combine results

diff_d_u$title <- "Urban"  # assign title for urban data differences

## -----------------------------------------------------------------------------------------------------------------------------------------
### Rural
## -----------------------------------------------------------------------------------------------------------------------------------------

# select relevant columns from the rural dataframe
plot_r_df2 <- rural_df %>%
  dplyr::select(country_year.x, home_type2, code_year, test_result)

# summarize overall test results by home type and test outcome, calculate percentages
plot_overall <- plot_r_df2 %>%
  group_by(home_type2, test_result) %>%
  summarise(value = n(), .groups = 'drop') %>%  # count occurrences for each group
  mutate(percent = round(value / sum(value) * 100, 0))  # calculate percentages

# assign a title to the overall plot
plot_overall$title <- "Urban"

# summarize test results by country, year, home type, and test outcome, calculate percentages
plot_country = plot_r_df2 %>% 
  group_by(country_year.x, code_year, home_type2, test_result) %>%
  summarise(value= n()) %>% 
  mutate(percent = round(value/sum(value) *100, 0)) %>% 
  mutate(country_year.x = ifelse(country_year.x == "Congo Democratic Republic 2013 - 14", "DRC 2013 - 14",country_year.x),
         home_type2 = ifelse(home_type2 == 'A', 'Agricultural worker Household (HH)', "Non-Agricultural worker HH"))

# calculate total test results by country_year.x
df_r <- all_df %>%
  group_by(country_year.x) %>%
  summarise(total2 = sum(total), .groups = 'drop')

# join total results with the country-specific plot
plot_country2 <- plot_country %>%
  left_join(df_r, by = "country_year.x") %>%
  mutate(plot_label = ifelse(test_result == "-ve", percent, NA),  # assign label for negative results
         cntryId = stringr::str_extract(code_year, "^.{2}"))  # extract country ID from code_year

# difference figure 2 urban: create a filtered plot for positive test results
plot_country <- plot_country2 %>%
  filter(test_result == "+ve") %>%
  group_by(home_type2, cntryId) %>%
  mutate(id = as.character(row_number()),  # create a row ID for each group
         cntryId_id = paste(cntryId, id))  # create combined country ID

# calculate the difference for agricultural households (excluding non-agricultural)
diff_d_r_a <- plot_country %>%
  filter(home_type2 != "Non-Agricultural worker HH") %>%
  group_by(cntryId) %>%
  mutate(diff_val = percent[id == "2"] - percent) %>%  # calculate the difference based on row ID
  filter(id != "2")  # exclude the row with ID 2

# calculate the difference for non-agricultural households (excluding agricultural)
diff_d_r <- plot_country %>%
  filter(home_type2 != "Agricultural worker Household (HH)") %>%
  group_by(cntryId) %>%
  mutate(diff_val = percent[id == "2"] - percent) %>%  # calculate the difference based on row ID
  filter(id != "2") %>%  # exclude the row with ID 2
  rbind(diff_d_r_a)  # combine with the previous difference dataframe

# assign a title to the final difference plot
diff_d_r $title = "Rural"

## -----------------------------------------------------------------------------------------------------------------------------------------
### New Test Positivity Difference Plot
## -----------------------------------------------------------------------------------------------------------------------------------------

# select relevant columns from the difference dataframes for urban and rural
diff_u <- diff_d_u %>%
  dplyr::select(cntryId, diff_val, home_type2, title)

diff_r <- diff_d_r %>%
  dplyr::select(cntryId, diff_val, home_type2, title)

# combine urban and rural differences into one dataframe
all_diff <- rbind(diff_u, diff_r) %>% 
  mutate(cntryId =  str_replace_all(cntryId, c("BF" = "Burkina Faso 2010 Vs. 2021",  # replace country IDs with descriptive names
                                               "BJ" = "Benin 2011 - 12 Vs. 2017 - 18",
                                               "CM" = "Cameroon 2011 Vs. 2018",
                                               "CI" = "Cote d'Ivoire 2011 - 12 Vs. 2021",
                                               "ML" = "Mali 2012 - 13 Vs. 2018",
                                               "MZ" = "Mozambique 2011 Vs. 2015",
                                               "TZ" = " Tanzania 2011 - 12 Vs. 2015 - 16"))) %>% 
  group_by(cntryId, title) 

# summarize the start and end of the difference values for each country
p_dat <- all_diff %>%
  summarise(start = range(diff_val)[1],  # get the minimum value of diff_val
            end = range(diff_val)[2]) %>%  # get the maximum value of diff_val
  ungroup()

# convert country ID to a factor with levels in reverse order for plotting
p_dat$cntryId <- factor(p_dat$cntryId,levels=rev(unique(p_dat$cntryId)))

# difference plot
p_2b <- ggplot(p_dat, aes(x = start, y = cntryId, end))+
  geom_segment(aes(xend = end, yend = cntryId)) +
  
  geom_point(
    data = all_diff,
    aes(diff_val, cntryId, color = home_type2), 
    size = 4, alpha =0.7
  ) +
  scale_color_manual(name= "", values=c( "darkgoldenrod1", "darkviolet")) +
  scale_x_continuous(breaks = seq(-65, 20, by = 10))+
  theme_manuscript() +
  theme(legend.position = "bottom") +
  labs(y = "", x = "Percentage difference in malaria test\n positivity rates by home type") +
  facet_wrap(~title)

p_2b


## =========================================================================================================================================
### Country level - Difference AMONG Household Type: URBAN
## =========================================================================================================================================

# select relevant columns from the urban dataframe
plot_u_df2 <- urban_df %>%
  dplyr::select(id, strat, wt, country_year.x, home_type2, code_year, test_result)

# summarize overall test results by home type and test outcome
plot_overall <- plot_u_df2 %>%
  group_by(home_type2, test_result) %>%
  summarise(value = n()) %>%  # count occurrences for each group
  mutate(percent = round(value / sum(value) * 100, 0))  # calculate the percentage for each home type

plot_overall$title = "Urban" # assign title for the urban plot

# summarize test results by country, year, home type, and test outcome
plot_country <- plot_u_df2 %>%
  as_survey_design(ids = id, strata = strat, nest = TRUE, weights = wt) %>%
  group_by(country_year.x, code_year, home_type2, test_result) %>%
  summarise(value = round(survey_total(), 0)) %>%  # calculate total using survey weights
  mutate(percent = round(value / sum(value) * 100, 0)) %>%  # calculate percentages for each result
  # rename certain values for clarity in the plot
  mutate(country_year.x = ifelse(country_year.x == "Congo Democratic Republic 2013 - 14", 
                                 "DRC 2013 - 14", country_year.x),
         home_type2 = ifelse(home_type2 == 'A', 
                             'Agricultural worker Household (HH)', 
                             "Non-Agricultural worker HH"))  # replace 'A' with descriptive label

# summarize total values by country year for further calculations
df_u <- all_df %>%
  group_by(country_year.x) %>%
  summarise(total2 = sum(total))  # sum total values for each country year

# join the total summary with country data and create a label for plotting
plot_country <- plot_country %>%
  left_join(df_u, by = "country_year.x") %>%
  mutate(plot_label = ifelse(test_result == "-ve", percent, NA),  # create label for negative results
         cntryId = stringr::str_extract(code_year, "^.{2}"))  # extract country ID from code_year


## =========================================================================================================================================
### Final Compilation of Datasets
## =========================================================================================================================================

## -----------------------------------------------------------------------------------------------------------------------------------------
### Difference Figure 2: Urban
## -----------------------------------------------------------------------------------------------------------------------------------------

# filter data for positive test results and group by home type and country ID --- MODIFIED TO INCLUDE CONFIDENCE INTERVALS
plot_country_urban_ci <- plot_country_urban_ci %>%
  filter(test_result == "+ve") %>%  # keep only positive test results
  group_by(home_type2, cntryId) %>%
  mutate(id = as.character(row_number())) %>%  # create an ID for each row within groups
  mutate(cntryId_id = paste(cntryId, id))  # concatenate country ID and row ID

# select relevant columns for agricultural worker households
df1 <- plot_country_urban_ci %>%
  filter(home_type2 == 'Agricultural worker Household (HH)') %>%  # filter for agricultural households
  dplyr::select(country_year.x, home_type2, percent, lower_ci, upper_ci, total, total_se, cntryId)  # select relevant columns

# select relevant columns for non-agricultural worker households
df2 <- plot_country_urban_ci %>%
  filter(home_type2 != 'Agricultural worker Household (HH)') %>%  # filter for non-agricultural households
  dplyr::select(country_year.x, home_type2, percent, lower_ci, upper_ci, total, total_se, cntryId)  # select relevant columns

# bind the two data frames and calculate the difference in percentages
diff_d_u <- bind_cols(df1, df2) %>%
  mutate(diff_val = percent...3 - percent...11)

diff_d_u$title = "Urban" # assign title for the urban plot

## -----------------------------------------------------------------------------------------------------------------------------------------
### Rural
## -----------------------------------------------------------------------------------------------------------------------------------------

# select relevant columns from the rural data frame
plot_r_df2 <- rural_df %>%
  dplyr::select(id, strat, wt, country_year.x, home_type2, code_year, test_result)  # select columns of interest

# summarize overall test results by home type and test outcome, calculate percentages
plot_overall <- plot_r_df2 %>%
  group_by(home_type2, test_result) %>%  # group by home type and test result
  summarise(value = n()) %>%  # count occurrences for each group
  mutate(percent = round(value / sum(value) * 100, 0))  # calculate the percentage for each test result

plot_overall$title = "Rural" # assign title for the rural plot

# convert the rural data frame into a survey design object with stratified sampling
plot_country <- plot_r_df2 %>%
  as_survey_design(ids = id, strata = strat, nest = TRUE, weights = wt) %>%  # create survey design
  group_by(country_year.x, code_year, home_type2, test_result) %>%  # group by country, year, home type, and test result
  summarise(value = round(survey_total(), 0)) %>%  # calculate total values with survey weights
  mutate(percent = round(value / sum(value) * 100, 0)) %>%  # calculate percentage for each test result
  mutate(country_year.x = ifelse(country_year.x == "Congo Democratic Republic 2013 - 14", "DRC 2013 - 14", country_year.x),  # shorten long country-year names for DRC
         home_type2 = ifelse(home_type2 == 'A', 'Agricultural worker Household (HH)', "Non-Agricultural worker HH"))  # replace 'A' with a descriptive label for home types

# calculate total values for each country-year combination
df_r <- all_df %>%
  group_by(country_year.x) %>%  # group by country-year
  summarise(total2 = sum(total))  # calculate total for each country-year

# join the summary totals with the main plot country data frame
plot_country <- plot_country %>%
  left_join(df_r, by = "country_year.x") %>%  # join on country-year
  mutate(plot_label = ifelse(test_result == "-ve", percent, NA),  # assign labels based on test result
         cntryId = stringr::str_extract(code_year, "^.{2}"))  # extract country ID from code_year

# filter for positive test results and group by home type and country ID --- MODIFIED TO INCLUDE CONFIDENCE INTERVALS
plot_country_rural_ci <- plot_country_rural_ci %>%
  filter(test_result == "+ve") %>%  # keep only positive test results
  group_by(home_type2, cntryId) %>%  # group by home type and country ID
  mutate(id = as.character(row_number())) %>%  # create a unique ID for each row
  mutate(cntryId_id = paste(cntryId, id))  # concatenate country ID and row ID

# separate data for agricultural worker households
df1 <- plot_country_rural_ci %>%
  filter(home_type2 == 'Agricultural worker Household (HH)') %>%  # filter for agricultural households
  dplyr::select(country_year.x, home_type2, percent, lower_ci, upper_ci, total, total_se, cntryId)  # select relevant columns

# separate data for non-agricultural worker households
df2 <- plot_country_rural_ci %>%
  filter(home_type2 != 'Agricultural worker Household (HH)') %>%  # filter for non-agricultural households
  dplyr::select(country_year.x, home_type2, percent, lower_ci, upper_ci, total, total_se, cntryId)  # select relevant columns

# bind the two data frames and calculate the difference in percent values
diff_d_r <- bind_cols(df1, df2) %>%
  mutate(diff_val = percent...3 - percent...11)  # calculate difference between specified percent columns

diff_d_r$title = "Rural" # assign a title for the difference data frame

## -----------------------------------------------------------------------------------------------------------------------------------------
### Combine the Difference Data Frames for Urban and Rural
## -----------------------------------------------------------------------------------------------------------------------------------------

# combine the two difference data frames and replace country IDs with descriptive names
all_diff <- rbind(diff_d_u, diff_d_r) %>% 
  mutate(cntryId =  str_replace_all(cntryId...8, c("BF" = "Burkina Faso 2010 Vs. 2021",  # replace abbreviations with full country names + survey years
                                               "BJ" = "Benin 2011 - 12 Vs. 2017 - 18",
                                               "CM" = "Cameroon 2011 Vs. 2018",
                                               "CI" = "Cote d'Ivoire 2011 - 12 Vs. 2021",
                                               "ML" = "Mali 2012 - 13 Vs. 2018",
                                               "MZ" = "Mozambique 2011 Vs. 2015",
                                               "TZ" = "Tanzania 2011 - 12 Vs. 2015 - 16"))) %>%
  group_by(cntryId...8, title) %>% 
  mutate(id = as.character(row_number())) %>% # create a unique ID for each row
  mutate(id = ifelse(id == "1", "Preceding survey", "Most recent survey")) %>% # label the IDs
  group_by(cntryId, title) # re-group by country ID and title
 
# rename the columns for clarity
all_diff2 <- all_diff %>%
  rename(agric_percent = percent...3,  # rename agricultural percent column
         non_agric_percent = percent...11,  # rename non-agricultural percent column
         agric_lower_ci = lower_ci...4,
         agric_upper_ci = upper_ci...5,
         agric_total = total...6,
         agric_total_se = total_se...7,
         non_agric_lower_ci = lower_ci...12,
         non_agric_upper_ci = upper_ci...13,
         non_agric_total = total...14,
         non_agric_total_se = total_se...15,
         diff_val_malaria = diff_val)  

# save as .csv
#write.csv(all_diff2_test, file.path(PopDir, "analysis_dat/240606_urban_df_for_analysis_trend_malaria_chilo_created.csv"))

## =========================================================================================================================================
### MALARIA - Modify Code to Include Survey Design in "240606_urban_df_for_analysis_trend_malaria_chilo_created.csv"
## =========================================================================================================================================

## -----------------------------------------------------------------------------------------------------------------------------------------
### Create Urban Trend Analysis DF
## -----------------------------------------------------------------------------------------------------------------------------------------

# # Convert the rural data frame into a survey design object and calculate the percentages
# plot_country3 <- plot_u_df2 %>%
#   as_survey_design(ids = id, strata = strat, nest = TRUE, weights = wt) %>%
#   filter(test_result == "+ve") %>%  # keep only positive test results
#   group_by(country_year.x, code_year, home_type2, test_result) %>%
#   summarise(value = round(survey_total(), 0), 
#             value_se_calc = survey_total(se = TRUE)) %>%  # Rename value_se to avoid duplication
#   mutate(percent = round(value / sum(value) * 100, 0)) %>%
#   mutate(country_year.x = ifelse(country_year.x == "Congo Democratic Republic 2013 - 14", "DRC 2013 - 14", country_year.x),
#          home_type2 = ifelse(home_type2 == 'A', 'Agricultural worker Household (HH)', "Non-Agricultural worker HH"))
# 
# # Calculate the difference in percent for agricultural and non-agricultural households
# df1_test2 <- plot_country3 %>%
#   filter(home_type2 == 'Agricultural worker Household (HH)') %>% # agricultural
#   dplyr::select(country_year.x, home_type2, percent, value, value_se)
# 
# df2_test2 <- plot_country3 %>%
#   filter(home_type2 != 'Agricultural worker Household (HH)') %>% # non-agricultural
#   dplyr::select(country_year.x, home_type2, percent, value, value_se)
# 
# diff_d_u_test <- bind_cols(df1_test2, df2_test2) %>%
#   mutate(diff_val = percent...4 - percent...10)
# 
# diff_d_u_test$title = "Urban"
# 
# ## -----------------------------------------------------------------------------------------------------------------------------------------
# ### Create Rural Trend Analysis DF
# ## -----------------------------------------------------------------------------------------------------------------------------------------
# 
# # Convert the rural data frame into a survey design object and calculate the percentages
# plot_country2 <- plot_r_df2 %>%
#   as_survey_design(ids = id, strata = strat, nest = TRUE, weights = wt) %>%
#   filter(test_result == "+ve") %>%  # keep only positive test results
#   group_by(country_year.x, code_year, home_type2, test_result) %>%
#   summarise(value = round(survey_total(), 0), 
#             value_se_calc = survey_total(se = TRUE)) %>%  # Rename value_se to avoid duplication
#   mutate(percent = round(value / sum(value) * 100, 0)) %>%
#   mutate(country_year.x = case_when(
#     country_year.x == "Cameroon 2018" ~ "Cameroon 2018 - 19",  # Update Cameroon 2018
#     country_year.x == "Congo Democratic Republic 2013 - 14" ~ "DRC 2013 - 14",  # Update DRC
#     TRUE ~ country_year.x  # Keep all other values unchanged
#   )) %>%
#   mutate(home_type2 = ifelse(home_type2 == 'A', 'Agricultural worker Household (HH)', "Non-Agricultural worker HH"))
# 
# # Calculate the difference in percent for agricultural and non-agricultural households
# df1_test <- plot_country2 %>%
#   filter(home_type2 == 'Agricultural worker Household (HH)') %>%
#   dplyr::select(country_year.x, home_type2, percent, value, value_se)
# 
# df2_test <- plot_country2 %>%
#   filter(home_type2 != 'Agricultural worker Household (HH)') %>%
#   dplyr::select(country_year.x, home_type2, percent, value, value_se)
# 
# diff_d_r_test <- bind_cols(df1_test, df2_test) %>%
#   mutate(diff_val = percent...4 - percent...10)
# 
# diff_d_r_test$title = "Rural"

## -----------------------------------------------------------------------------------------------------------------------------------------
### Combine the Difference Data Frames for Urban and Rural
## -----------------------------------------------------------------------------------------------------------------------------------------

# # extract country ID from code_year...1 variable in both dataframes
# diff_d_r <- diff_d_r %>%
#   mutate(country_id = str_extract(code_year...1, "^[A-Z]{2}"))
# diff_d_u <- diff_d_u %>%
#   mutate(country_id = str_extract(code_year...1, "^[A-Z]{2}"))

# combine the difference data frames for urban and rural
all_diff_ci <- rbind(diff_d_u, diff_d_r) %>%
  mutate(country_surveyyears = str_replace_all(cntryId...8, 
                                   c("BF" = "Burkina Faso 2010 Vs. 2021", 
                                     "BJ" = "Benin 2011 - 12 Vs. 2017 - 18", 
                                     "CM" = "Cameroon 2011 Vs. 2018", 
                                     "CI" = "Cote d'Ivoire 2011 - 12 Vs. 2021", 
                                     "ML" = "Mali 2012 - 13 Vs. 2018", 
                                     "MZ" = "Mozambique 2015 Vs. 2022 - 23", 
                                     "TZ" = "Tanzania 2011 - 12 Vs. 2015 - 16"))) %>%
  group_by(cntryId...8, title) %>%
  mutate(id = as.character(row_number())) %>%
  mutate(id = ifelse(id == "1", "Preceding survey", "Most recent survey")) %>%
  mutate(country_surveyyears = ifelse(cntryId...8 == "GH", "Ghana 2014 Vs. 2022 - 23", country_surveyyears)) %>%
  group_by(cntryId...8, title)

# Rename columns for clarity and include survey design variables
all_diff2_ci <- all_diff_ci %>%
  rename(agric_percent = percent...3, 
         agric_lower_ci = lower_ci...4,
         agric_upper_ci = upper_ci...5,
         agric_total = total...6,
         agric_total_se = total_se...7,
         country_id = cntryId...8,
         country_year = country_year.x...9,
         non_agric_percent = percent...11,
         non_agric_lower_ci = lower_ci...12,
         non_agric_upper_ci = upper_ci...13,
         non_agric_total = total...14,
         non_agric_total_se = total_se...15,
         diff_val_malaria = diff_val)

#### WRITE TREND ANALYSIS DATASET HERE
write.csv(all_diff2_ci, file.path(PopDir, "analysis_dat/240606_df_with_ci_for_analysis_trend_malaria_grace_created.csv"))


## =========================================================================================================================================
### NET USE - Modify Code to Include Survey Design in "240606_urban_df_for_analysis_trend_net_chilo_created.csv"
## =========================================================================================================================================

## -----------------------------------------------------------------------------------------------------------------------------------------
### URBAN
## -----------------------------------------------------------------------------------------------------------------------------------------

plot_u_df2_net <- urban_df %>% 
  dplyr::select(id, strat, wt, country_year.x, home_type2, code_year, u5_net_use) 

plot_u_df2_net_survey <- plot_u_df2_net %>%
  as_survey_design(ids = id, strata = strat, nest = TRUE, weights = wt)

# summarize test results by country, year, home type, and test outcome, calculate percentages with CIs
plot_country_urban_net_ci <- plot_u_df2_net_survey %>%
  group_by(country_year.x, code_year, home_type2, u5_net_use) %>%
  summarize(
    percent = survey_mean(vartype = "ci") * 100,  # calculate percentage and confidence intervals
    total = survey_total()  # total count of observations
  ) %>%
  mutate(
    home_type2 = ifelse(home_type2 == 'A', 
                        'Agricultural worker Household (HH)', 
                        "Non-Agricultural worker HH")
  )

# calculate the difference in percent for agricultural and non-agricultural households
df1_urban_net <- plot_country_urban_net_ci %>%
  filter(home_type2 == 'Agricultural worker Household (HH)') # agricultural

df2_urban_net <- plot_country_urban_net_ci %>%
  filter(home_type2 != 'Agricultural worker Household (HH)') # non-agricultural

diff_d_u_net <- bind_cols(df1_urban_net, df2_urban_net) %>% 
  mutate(diff_val_net = percent...5 - percent...14)

# filter plot_country to only include those that used net (net use = 1)
diff_d_u_net <- diff_d_u_net %>%
  filter(u5_net_use...4 == 1)

diff_d_u_net$title = "Urban"

## -----------------------------------------------------------------------------------------------------------------------------------------
### RURAL
## -----------------------------------------------------------------------------------------------------------------------------------------

plot_r_df2_net <- rural_df %>% 
  dplyr::select(id, strat, wt, country_year.x, home_type2, code_year, u5_net_use) 

plot_r_df2_net_survey <- plot_r_df2_net %>%
  as_survey_design(ids = id, strata = strat, nest = TRUE, weights = wt)

# summarize test results by country, year, home type, and test outcome, calculate percentages with CIs
plot_country_rural_net_ci <- plot_r_df2_net_survey %>%
  group_by(country_year.x, code_year, home_type2, u5_net_use) %>%
  summarize(
    percent = survey_mean(vartype = "ci") * 100,  # calculate percentage and confidence intervals
    total = survey_total()  # total count of observations
  ) %>%
  mutate(
    home_type2 = ifelse(home_type2 == 'A', 
                        'Agricultural worker Household (HH)', 
                        "Non-Agricultural worker HH")
  )

# calculate the difference in percent for agricultural and non-agricultural households
df1_rural_net <- plot_country_rural_net_ci %>%
  filter(home_type2 == 'Agricultural worker Household (HH)') # agricultural

df2_rural_net <- plot_country_rural_net_ci %>%
  filter(home_type2 != 'Agricultural worker Household (HH)') # non-agricultural

diff_d_r_net <- bind_cols(df1_rural_net, df2_rural_net) %>% 
  mutate(diff_val_net = percent...5 - percent...14)

# filter plot_country to only include those that used nets (net use = 1)
diff_d_r_net <- diff_d_r_net %>%
  filter(u5_net_use...4 == 1)

diff_d_r_net$title = "Rural"

## -----------------------------------------------------------------------------------------------------------------------------------------
### Compile Urban and Rural Net Use Trend Datasets
## -----------------------------------------------------------------------------------------------------------------------------------------

# extract country ID from code_year...2 variable in both dataframes
diff_d_r_net <- diff_d_r_net %>%
  mutate(country_id = str_extract(code_year...2, "^[A-Z]{2}"))
diff_d_u_net <- diff_d_u_net %>%
  mutate(country_id = str_extract(code_year...2, "^[A-Z]{2}"))

# combine the two difference data frames and replace country IDs with descriptive names
all_diff_net <- rbind(diff_d_u_net, diff_d_r_net) %>% 
  mutate(country_surveyyears =  str_replace_all(country_id, c("BF" = "Burkina Faso 2010 vs. 2021",  # replace abbreviations with full country names + survey years
                                                   "BJ" = "Benin 2011-12 vs. 2017-18",
                                                   "CM" = "Cameroon 2011 vs. 2018",
                                                   "CI" = "Cote d'Ivoire 2011-12 vs. 2021",
                                                   "ML" = "Mali 2012-13 vs. 2018",
                                                   "MZ" = "Mozambique 2015 vs. 2022 - 23",
                                                   "TZ" = "Tanzania 2011-12 vs. 2015-16"))) %>%
  group_by(country_id, title) %>% 
  mutate(id = as.character(row_number())) %>% # create a unique ID for each row
  mutate(id = ifelse(id == "1", "Preceding survey", "Most recent survey")) %>% # label the IDs
  mutate(country_surveyyears = ifelse(country_id == "GH", "Ghana 2014 Vs. 2022 - 23", country_surveyyears)) %>%
  group_by(country_id, title) # re-group by country ID and title

# rename the columns for clarity
all_diff_net <- all_diff_net %>%
  rename(country_year = country_year.x...1,
         code_year = code_year...2,
         agric_percent = percent...5, 
         non_agric_percent = percent...14,
         agric_lower_ci = percent_low...6,
         agric_upper_ci = percent_upp...7,
         agric_total = total...8,
         agric_total_se = total_se...9,
         non_agric_lower_ci = percent_low...15,
         non_agric_upper_ci = percent_upp...16,
         non_agric_total = total...17,
         non_agric_total_se = total_se...18)  

# save as .csv
write.csv(all_diff_net, file.path(PopDir, "analysis_dat/240606_df_with_ci_for_analysis_trend_net_grace_created.csv"))

## =========================================================================================================================================
### 
## =========================================================================================================================================

p_dat <- all_diff%>% 
  summarise(start = range(diff_val)[1], end = range(diff_val)[2]) %>% 
  ungroup() 

p_dat$cntryId <- factor(p_dat$cntryId,levels=rev(unique(p_dat$cntryId)))


#diff plot

p_figure_2_2 <-ggplot(p_dat, aes(x = start, y = cntryId, end))+
  geom_segment(aes(xend = end, yend = cntryId)) +
  
  geom_point(
    data = all_diff,
    aes(diff_val, cntryId, color = id), 
    size = 4, alpha =0.7
  ) +
  
  scale_color_manual(name= "", values=c( "darkgoldenrod1", "darkviolet")) +
  scale_x_continuous(breaks = seq(-65, 20, by = 10))+
  theme_manuscript() +
  theme(legend.position = "bottom") +
  labs(y = "", x = "Percentage difference in malaria test positivity rates
      between agricultural worker HH and non-agricultural worker HH over time") +
  facet_wrap(~factor(title, levels = c("Urban", "Rural")))

p_figure_2_2 

ggsave(paste0(FigDir,"/", Sys.Date(),"_Figure_2_2.png"),p_figure_2_2, width = 8.5, height= 3.5) 



##############################################################################
##combined difference
#################################################################################

#urban
plot_u_df2<- urban_df %>% dplyr::select(country_year.x, home_type2, code_year, test_result) 



plot_overall = plot_u_df2 %>%  group_by(code_year, test_result) %>%  summarise(value= n()) %>% 
  mutate(percent = round(value/sum(value) *100, 0)) %>%  filter(test_result == "+ve")

diff_u <- plot_overall %>% 
  mutate(cntryId = stringr::str_extract(code_year, "^.{2}")) %>% group_by(cntryId)  %>%
  mutate(id = as.character(row_number())) %>% 
  mutate(diff_val = percent[id == "2"] - percent) %>% filter(id != "2")

diff_u$title = "Urban"


#rural

plot_r_df2<- rural_df %>% dplyr::select(country_year.x, home_type2, code_year, test_result) 



plot_overall = plot_r_df2 %>%  group_by(code_year, test_result) %>%  summarise(value= n()) %>% 
  mutate(percent = round(value/sum(value) *100, 0)) %>%  filter(test_result == "+ve")

diff_r <- plot_overall %>% 
  mutate(cntryId = stringr::str_extract(code_year, "^.{2}")) %>% group_by(cntryId)  %>%
  mutate(id = as.character(row_number())) %>% 
  mutate(diff_val = percent[id == "2"] - percent) %>% filter(id != "2")

diff_r$title = "Rural"


all_diff <- rbind(diff_u, diff_r) %>% 
  mutate(cntryId =  str_replace_all(cntryId, c("BF" = "Burkina Faso 2010 Vs. 2021", 
                                                   "BJ" = "Benin 2011 - 12 Vs. 2017 - 18",
                                                   "CM" = "Cameroon 2011 Vs. 2018",
                                                   "CI" = "Cote d'Ivoire 2011 - 12 Vs. 2021",
                                                   "ML" = "Mali 2012 - 13 Vs. 2018",
                                                   "MZ" = "Mozambique 2011 Vs. 2015",
                                                   "TZ" = "Tanzania 2011 - 12 Vs. 2015 - 16"))) %>%
  group_by(cntryId) 


p_dat <- all_diff%>% 
  summarise(start = range(diff_val)[1], end = range(diff_val)[2]) %>% 
  ungroup() 

p_dat$cntryId <- factor(p_dat$cntryId,levels=rev(unique(p_dat$cntryId)))

#diff plot

p_2b<-ggplot(p_dat, aes(x = start, y = cntryId, end))+
  geom_segment(aes(xend = end, yend = cntryId)) +
  
  geom_point(
    data = all_diff,
    aes(diff_val, cntryId, color = title), 
    size = 4, alpha =0.7
  ) +
  
  scale_color_manual(name= "", values=c( "darkgoldenrod1", "darkviolet")) +
  scale_x_continuous(breaks = seq(-65, 20, by = 10))+
  theme_manuscript() +
  theme(legend.position = "bottom") +
  labs(y = "", x = "Percentage difference in malaria test positivity rates over time") 

p_2b + labs(title = "Overall difference in malaria passivity between the last and most recent survey with Agric data
")


###############################################
#column plots
####################################################

#urban
plot_u_df2<- urban_df %>% dplyr::select(country_year.x, home_type2, code_year, test_result) 



plot_overall = plot_u_df2 %>%  group_by(code_year, home_type2,test_result) %>%  summarise(value= n()) %>% 
  mutate(percent = round(value/sum(value) *100, 0)) %>%  filter(test_result == "+ve"  ) %>%
  mutate(cntryId = stringr::str_extract(code_year, "^.{2}")) %>% mutate(year = parse_number(code_year))


ggplot() + 
  geom_col(data = plot_overall, aes(x = year, y = percent, fill = home_type2)) + 
  facet_wrap(~cntryId + home_type2) + 
  labs(title = "Malaria positivity trends for countries with multiple surveys with agric data")  + 
  theme(legend.position = "none")
  
##############################################################################
#                              NET USE 
##############################################################################


#urban
plot_u_df2<- urban_df %>% dplyr::select(id, strat, wt, country_year.x, home_type2, code_year, u5_net_use) 



plot_overall = plot_u_df2 %>%  group_by(home_type2, u5_net_use) %>%  summarise(value= n()) %>% mutate(percent = round(value/sum(value) *100, 0))
plot_overall$title = "Urban"


#plot by country 
plot_country = plot_u_df2 %>% as_survey_design(ids= id,strata=strat,nest=T,weights= wt) %>%
  group_by(country_year.x, code_year, home_type2, u5_net_use) %>%  
  summarise(value = round(survey_total(),0))%>% mutate(percent = round(value/sum(value) *100, 0)) %>% 
  mutate(country_year.x = ifelse(country_year.x == "Congo Democratic Republic 2013 - 14", "DRC 2013 - 14",country_year.x),
         home_type2 = ifelse(home_type2 == 'A', 'Agricultural worker Household (HH)', "Non-Agricultural worker HH"))

df_u <- all_df %>% group_by(country_year.x) %>%  summarise(total2 = sum(total))

plot_country <- plot_country  %>%  left_join(df_u, by = "country_year.x")  %>% 
  mutate(plot_label = ifelse(u5_net_use == 0, percent, NA), cntryId = stringr::str_extract(code_year, "^.{2}")) 



#diff figure 2 urban
plot_country = plot_country %>% filter(u5_net_use == 1) %>% group_by(home_type2, cntryId) %>% mutate(id = as.character(row_number())) %>% 
  mutate(cntryId_id = paste(cntryId, id))

df1 <- plot_country %>% filter(home_type2 == 'Agricultural worker Household (HH)') %>% dplyr::select(country_year.x, home_type2, percent, cntryId)
df2 <- plot_country %>% filter(home_type2 != 'Agricultural worker Household (HH)') %>% dplyr::select(country_year.x, home_type2, percent, cntryId)

diff_d_u <- bind_cols(df1, df2) %>% mutate(diff_val = percent...3 - percent...7)

diff_d_u $title = "Urban"

#rural

plot_r_df2<- rural_df %>% dplyr::select(country_year.x, home_type2, code_year, u5_net_use) 



plot_overall = plot_r_df2 %>%  group_by(home_type2, u5_net_use) %>%  summarise(value= n()) %>% mutate(percent = round(value/sum(value) *100, 0))
plot_overall$title = "Rural"


#plot by country 
plot_country = plot_r_df2 %>%  
  as_survey_design(ids= id,strata=strat,nest=T,weights= wt) %>%
  group_by(country_year.x, code_year, home_type2, u5_net_use) %>%  summarise(value= n()) %>% mutate(percent = round(value/sum(value) *100, 0)) %>% 
  mutate(country_year.x = ifelse(country_year.x == "Congo Democratic Republic 2013 - 14", "DRC 2013 - 14",country_year.x),
         home_type2 = ifelse(home_type2 == 'A', 'Agricultural worker Household (HH)', "Non-Agricultural worker HH"))

df_r <- all_df %>% group_by(country_year.x) %>%  summarise(total2 = sum(total))

plot_country <- plot_country  %>%  left_join(df_r, by = "country_year.x")  %>% 
  mutate(plot_label = ifelse(u5_net_use == 0, percent, NA), cntryId = stringr::str_extract(code_year, "^.{2}")) 



#diff figure 2 urban
plot_country = plot_country %>% filter(u5_net_use == 1) %>% group_by(home_type2, cntryId) %>% mutate(id = as.character(row_number())) %>% 
  mutate(cntryId_id = paste(cntryId, id))

df1 <- plot_country %>% filter(home_type2 == 'Agricultural worker Household (HH)') %>% dplyr::select(country_year.x, home_type2, percent, cntryId)
df2 <- plot_country %>% filter(home_type2 != 'Agricultural worker Household (HH)') %>% dplyr::select(country_year.x, home_type2, percent, cntryId)

diff_d_r <- bind_cols(df1, df2) %>% mutate(diff_val = percent...3 - percent...7)

diff_d_r$title = "Rural"


all_diff <- rbind(diff_d_u, diff_d_r) %>% 
  mutate(cntryId =  str_replace_all(cntryId...4, c("BF" = "Burkina Faso 2010 Vs. 2021", 
                                                   "BJ" = "Benin 2011 - 12 Vs. 2017 - 18",
                                                   "CM" = "Cameroon 2011 Vs. 2018",
                                                   "CI" = "Cote d'Ivoire 2011 - 12 Vs. 2021",
                                                   "ML" = "Mali 2012 - 13 Vs. 2018",
                                                   "MZ" = "Mozambique 2011 Vs. 2015",
                                                   "TZ" = "Tanzania 2011 - 12 Vs. 2015 - 16"))) %>%
  group_by(cntryId...4, title) %>% mutate(id = as.character(row_number())) %>% 
  mutate(id = ifelse(id == "1", "Preceding survey", "Most recent survey")) %>% group_by(cntryId, title) 



all_diff = all_diff %>%  rename(agric_percent = percent...3, non_agric_percent = percent...7, diff_val_net = diff_val)

## -----------------------------------------------------------------------------------------------------------------------------------------
### WRITE TO .CSV FILE (used for DiD and line plots)
## -----------------------------------------------------------------------------------------------------------------------------------------
write.csv(all_diff, file.path(PopDir, "analysis_dat/240606_urban_df_for_analysis_trend_net_chilo_created.csv"))


p_dat <- all_diff%>% 
  summarise(start = range(diff_val)[1], end = range(diff_val)[2]) %>% 
  ungroup()

p_dat$cntryId <- factor(p_dat$cntryId,levels=rev(unique(p_dat$cntryId)))

#diff plot

p_figure_3_2 <-ggplot(p_dat, aes(x = start, y = cntryId, end))+
  geom_segment(aes(xend = end, yend = cntryId)) +
  
  geom_point(
    data = all_diff,
    aes(diff_val, cntryId, color = id), 
    size = 4, alpha =0.7
  ) +
  
  scale_color_manual(name= "", values=c( "darkorange", "darkolivegreen")) +
  scale_x_continuous(breaks = seq(-65, 20, by = 10))+
  theme_manuscript() +
  theme(legend.position = "bottom") +
  labs(y = "", x = "Percentage difference in net use
      between agricultural worker HH and non-agricultural worker HH over time") +
  facet_wrap(~factor(title, levels = c("Urban", "Rural")))

p_figure_3_2 

ggsave(paste0(FigDir,"/", Sys.Date(),"_Figure_3_2.png"),p_figure_3_2, width = 8.5, height= 3.5) 


#combined, positivity and net use. 

diffs <- p_figure_2_2 / p_figure_3_2 + plot_annotation(tag_levels = 'A')
diffs
ggsave(paste0(FigDir,"/", Sys.Date(),"_Figure_4.pdf"),diffs, width = 8.5, height= 7) 

