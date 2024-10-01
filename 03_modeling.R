# ==========================================================================================================================================
# Script Name: Descriptive Analysis
# Author: Dr. Ifeoma Ozodiegwu
# Edited by: Grace Legris, Research Data Analyst (gracebea@gmail.com)
# Edited: [2024-09-30]
# Purpose: Create Models that Explore the Relationship Between Agricultural Worker HH and Malaria Test Positivity
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
  FigDir <- file.path(ManDir, "figures", "main_figures","pdf_figures")
  SupDir <- file.path(ManDir, "figures", "supplementary", "pdf_figures")
  ExpDir <- file.path(ManDir, "figures", "exploratory")
} else if  ("cchiz" %in% user) {
  Drive <- file.path("C:/Users/cchiz/OneDrive")
  DriveDir <- file.path(Drive, "urban_malaria")
  PopDir <- file.path(DriveDir, "data", 'data_agric_analysis')
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

#note before sourcing functions and packages, run the code below to download rdhs if you don't already have it
#devtools::install_github("ropensci/rdhs")
source("functions/functions_employment.R")
options(survey.lonely.psu="adjust")  # this option allows admin units with only one cluster to be analyzed
library(svylme) #use glmer package instead 

## -----------------------------------------------------------------------------------------------------------------------------------------
### Read in Analysis Datasets
## -----------------------------------------------------------------------------------------------------------------------------------------

# read data from CSV file into all_df and apply transformations
all_df <- read_csv(file.path(PopDir, "analysis_dat/urban_rural_analysis_data_for_modeling.csv")) %>%  
  mutate(malaria_result = ifelse(test_result == "+ve", 1, 0),  # create binary malaria_result based on test_result
         EVI_2000m_new = case_when(is.na(EVI_2000m_new) ~ NA,  # handle missing values in EVI_2000m_new
                                   TRUE ~ EVI_2000m_new * 10),  # scale EVI_2000m_new by a factor of 10
         home_type_factor = ifelse(home_type2 == "A", "Z_Agric", "Non-agric"),  # categorize home type
         wealth_index = as.factor(wealth),  # convert wealth index to a factor
         dhs_year_factor = as.factor(dhs_year),  # convert DHS year to a factor
         sex = ifelse(hc27 == 2, "Female", "Male"),  # recode sex variable
         u5_net_use_factor = ifelse(u5_net_use == 1, "Z_Used", "not_used"),  # categorize net use among under-fives
         roof_type_factor = ifelse(roof_type == 1, "Z_Improved", "poor"),  # categorize roof type
         stunting = ifelse(hc70 < -300, "Stunted", ifelse(hc70 > 8000, NA, "Not stunted")))  # categorize stunting status

# filter for urban location types into urban_df
urban_df <-all_df %>%  filter(type == "Urban")  

# display the structure of urban_df
glimpse(urban_df)


# define variables of interest for analysis
var <- list("home_type_factor", "hc1", "sex", "stunting", "u5_net_use_factor", "hh_size", "roof_type_factor", 
            "wealth_index", "dhs_year_factor", "EVI_2000m_new", "preci_monthly_2000m", "RH_monthly_2000m", "temp_monthly_2000m")

# define descriptive table names for corresponding variables
table_names <- c("Household occupation category: agricultural", "Age", "Gender: male", "Stunting: stunted", 
                 "Net use among children under the age of five years: use", "Household size", "Roof type: improved", 
                 "Wealth: poor", "Wealth: middle", "Wealth: rich", "Wealth: richest",
                 "DHS Year: 2013", "DHS Year: 2014", "DHS Year: 2015", "DHS Year: 2016", "DHS Year: 2017", 
                 "DHS Year: 2018", "DHS Year: 2019", "DHS Year: 2021", "DHS Year: 2022", "DHS Year: 2023", 
                 "Enhanced vegetation index", "Precipitation", "Relative humidity (%)", "Temperature")

# create a copy of all_df for further modifications if needed
all_df_renamed_vars <- all_df
#all_df_renamed_vars$

# define location types for analysis
location_types <- c("Urban", "Rural")

# initialize an empty list to store results for each location type
all_results <- list() # To store results for each location type

# loop through each location type defined in location_types (Urban or Rural)
for (location in location_types) {
  # filter the data based on location type
  df <- all_df %>%
    filter(type == location)
  
  # drop NA values
  df_new <- df %>% drop_na(EVI_2000m_new)
  unadj_df <- list()
  
  # loop through each variable of interest
  for (i in 1:length(var)) {
    svy_design <- svydesign.fun(df_new) # create a survey design object for the filtered data
    
    # construct the formula dynamically for the model
    formula <- as.formula(paste("malaria_result ~", var[[i]], "+ (1|hv001)"))
    
    # fit the generalized linear model using the survey design
    result <- svyglm(formula, design = svy_design, family = binomial(link = "logit"))
    
    # summarize and tidy the model results
    df_result <- tidy(result)
    
    # filter out the intercept and calculate additional metrics
    df_result <- df_result %>% 
      filter(term != "(Intercept)") %>%  # exclude the intercept from the results
      rename_at(3, ~"SE") %>%  # rename the third column to "SE"
      mutate(odds = exp(estimate)) %>%  # calculate odds from the estimates
      mutate(lower_ci = exp(-1.96 * SE + estimate)) %>%  # calculate lower confidence interval
      mutate(upper_ci = exp(1.96 * SE + estimate)) %>%  # calculate upper confidence interval
      tibble::rownames_to_column() %>%  # convert row names to a column
      mutate(type = "unadjusted", location = location)  # add type and location information
    
    # store the results for the current variable
    unadj_df[[i]] <- df_result
  }
  
  # combine results for the current location type into all_results
  all_results[[location]] <- bind_rows(unadj_df)
}

# combine results for all location types into a single dataframe and format the output
all_results_combined <- bind_rows(all_results) %>%
  transmute(
    term, # keep the term column
    estimate = sprintf("%.3f (%.3f – %.3f)", # format the estimates and confidence intervals
                       round(odds, 3), round(lower_ci, 3), round(upper_ci, 3)), # Round and format odds and CIs
    location # keep the location column
  ) %>%
  pivot_wider( # reshape the dataframe to wide format based on location
    names_from = location, # use the location names as column headers
    values_from = estimate # fill with the formatted estimates
  )

# display the combined results
all_results_combined

# assign descriptive table names to the term column in the combined results
all_results_combined$term <- table_names 

# create a reference dataframe with fixed values for certain terms
ref_df <- data.frame(term = c("Gender: female", "Stunting: not stunted", 
                              "Household occupation category: none agricultural worker HH",
                              "Net use among children under the age of five years: did not use", 
                              "Roof type: poor","Wealth: poorest", "DHS Year: 2012"), 
                     Urban = c("1.000","1.000","1.000","1.000","1.000","1.000","1.000"), 
                     Rural = c("1.000","1.000","1.000","1.000","1.000","1.000","1.000"))

# combine the formatted results with the reference dataframe and arrange by term, write to an Excel file
all_results_final <- all_results_combined %>% bind_rows(ref_df) %>% arrange(term)
write_xlsx(all_results_final, file.path(PopDir, "analysis_dat", "single_reg_results.xlsx"))


## =========================================================================================================================================
### Mediation Analysis - Phase 1
## =========================================================================================================================================

# define a function to create the survey design object
svydesign_fun <- function(df) {
  # assuming df contains appropriate id, strata, and weights columns
  svydesign(ids = ~hv001, strata = ~strat, weights = ~wt, data = df, nest = TRUE)
}

# define a function to run survey-weighted logistic or linear regression
run_svyglm <- function(formula, data) {
  # create survey design object using the helper function (see functions_employment.R)
  svy_design <- svydesign_fun(data)
  
  # extract the outcome variable from the formula
  outcome_var <- all.vars(formula)[1]
  
  # check if the outcome variable is binary (for logistic regression)
  if (is.factor(data[[outcome_var]]) || length(unique(data[[outcome_var]])) == 2) {
    # logistic regression using survey design using survey design if outcome is binary
    model <- svyglm(formula, design = svy_design, family = binomial(link = "logit"))
    
    # tidy the logistic regression model and calculate odds ratios (OR) and confidence intervals (CI)
    tidy_model <- tidy(model) %>%
      filter(term != "(Intercept)") %>%  # exclude the intercept term from results
      rename(SE = std.error) %>%         # rename standard error column to 'SE'
      mutate(OR = exp(estimate),         # calculate odds ratio (exponentiate the estimate)
             lower_ci = exp(estimate - 1.96 * SE),  # calculate lower bound of 95% CI
             upper_ci = exp(estimate + 1.96 * SE))  # calculate upper bound of 95% CI
    
    # select relevant columns to return for logistic regression
    tidy_model <- tidy_model %>%
      select(term, OR, lower_ci, upper_ci, p.value)
    
  } else {
    # perform linear regression using survey design if outcome is continuous
    model <- svyglm(formula, design = svy_design)
    
    # tidy the linear regression model and extract coefficients and confidence intervals
    tidy_model <- tidy(model) %>%
      filter(term != "(Intercept)") %>% # exclude the intercept term
      rename_at(3, ~"SE") %>% # rename standard error column to 'SE'
      #mutate(odds = exp(estimate)) %>%
      mutate(lower_ci = exp(-1.96 * SE + estimate)) %>% # calculate lower bound of CI
      mutate(upper_ci = exp(1.96 * SE + estimate)) %>% # calculate upper bound of CI
      tibble::rownames_to_column() %>% # add rownames as a column
      mutate(type = "unadjusted", location = location) # add additional details: type and location
  }
  
  return(tidy_model)
}

# assuming your dataset is named 'all_df2', and 'typem' contains "Urban" and "Rural"
all_df2 <- all_df %>% 
  # create a binary variable for malaria result (1 for positive, 0 for negative)
  mutate(malaria_result = ifelse(test_result =="+ve", 1,0), 
         # multiply the 'EVI_2000m_new' variable by 10, but keep missing values as NA
         EVI_2000m_new = case_when(is.na(EVI_2000m_new) ~ NA,
                                   TRUE ~ EVI_2000m_new * 10),
         # create a binary variable for home type (1 for agricultural, 0 for non-agricultural)
         home_type_dep = ifelse(home_type2 =="A", 1, 0),
         home_type_dep = as.factor(home_type_dep), # convert to a factor
         wealth_index = as.factor(wealth), # convert to a factor
         dhs_year_factor = as.factor(dhs_year), # convert to a factor
         # create a binary variable for sex (Female or Male)
         sex = ifelse(hc27 == 2, "Female", "Male"),
         u5_net_use_dep = as.factor(u5_net_use), # convert to a factor
         roof_type_dep = as.factor(roof_type), # convert to a factor
         # create a binary variable for stunting (1 for stunted, 0 for not stunted)
         # if the value of 'hc70' is greater than 8000, mark it as NA
         stunting_dep = ifelse(hc70 < -300, 1, ifelse(hc70 > 8000, NA, 0)),
         stunting_dep = as.factor(stunting_dep)) # convert to a factor

# filter the data to separate urban and rural models
urban_df <- all_df2 %>% filter(type == "Urban")
rural_df <- all_df2 %>% filter(type == "Rural")

# define the formulas for the regression models to be run
formulas <- list(
  stunting_dep  ~ wealth_index,         # stunting based on wealth index
  home_type_dep ~ stunting,             # home type based on stunting status
  home_type_dep ~ wealth_index,         # home type based on wealth index
  roof_type_dep ~ home_type_factor,     # roof type based on home type
  u5_net_use_dep ~ temp_monthly_2000m,  # net use based on monthly temperature
  hh_size ~ wealth_index,               # household size based on wealth index
  u5_net_use_dep ~ RH_monthly_2000m,    # net use based on relative humidity
  temp_monthly_2000m ~ RH_monthly_2000m # temperature based on relative humidity
)

# run the regressions separately for urban and rural datasets
urban_results <- lapply(formulas, function(f) run_svyglm(f, urban_df))
rural_results <- lapply(formulas, function(f) run_svyglm(f, rural_df)) 

# bind the results into a single dataframe with an indicator for urban and rural
urban_results_df <- bind_rows(urban_results, .id = "model_number") %>%
  mutate(location = "Urban") # add a column to specify urban location
rural_results_df <- bind_rows(rural_results, .id = "model_number") %>%
  mutate(location = "Rural") # add a column to specify rural location

# combine the results from urban and rural datasets into one dataframe
final_results <- bind_rows(urban_results_df, rural_results_df)  %>% 
  mutate(p.value = round(p.value, 4)) %>% # round p-values to four decimal places
  mutate(model_number = case_when(
    model_number == 1 ~ "stunting_dep ~ wealth_index",             # model 1: stunting based on wealth index
    model_number == 2 ~ "home_type_dep ~ stunting",                # model 2: home type based on stunting status
    model_number == 3 ~ "home_type_dep ~ wealth_index",            # model 3: home type based on wealth index
    model_number == 4 ~ "roof_type_dep ~ home_type_factor",        # model 4: roof type based on home type
    model_number == 5 ~ "u5_net_use_dep ~ temp_monthly_2000m",     # model 5: net use based on temperature
    model_number == 6 ~ "hh_size ~ wealth_index",                  # model 6: household size based on wealth index
    model_number == 7 ~ "u5_net_use_dep ~ RH_monthly_2000m",       # model 7: net use based on relative humidity
    model_number == 8 ~ "temp_monthly_2000m ~ RH_monthly_2000m",   # model 8: temperature based on relative humidity
    TRUE ~ model_number ))

# display the final results
final_results


## =========================================================================================================================================
### Mediation Analysis - Phase 2
## =========================================================================================================================================

# define a list of formulas for the models to be run
formulas <- list(
  malaria_result ~ stunting_dep  + wealth_index,          # model 1: malaria result based on stunting and wealth index
  malaria_result ~ home_type_dep + stunting,              # model 2: malaria result based on home type and stunting
  malaria_result ~ home_type_dep + wealth_index,          # model 3: malaria result based on home type and wealth index
  malaria_result ~ roof_type_dep + home_type_factor,      # model 4: malaria result based on roof type and home type factor
  malaria_result ~ u5_net_use_dep + temp_monthly_2000m,   # model 5: malaria result based on net use and temperature
  malaria_result ~ hh_size + wealth_index,                # model 6: malaria result based on household size and wealth index
  malaria_result ~ u5_net_use_dep + RH_monthly_2000m,     # model 7: malaria result based on net use and relative humidity
  malaria_result ~ temp_monthly_2000m + RH_monthly_2000m  # model 8: malaria result based on temperature and relative humidity
)

# run the regressions separately for urban and rural data
urban_results <- lapply(formulas, function(f) run_svyglm(f, urban_df))
rural_results <- lapply(formulas, function(f) run_svyglm(f, rural_df)) 

# bind the results for urban and rural into separate dataframes and add a location indicator
urban_results_df <- bind_rows(urban_results, .id = "model_number") %>%
  mutate(location = "Urban") # add a column to specify urban location
rural_results_df <- bind_rows(rural_results, .id = "model_number") %>%
  mutate(location = "Rural") # add a column to specify rural location

# combine the urban and rural results into one dataframe
final_results <- bind_rows(urban_results_df, rural_results_df)  %>% 
  mutate(p.value = round(p.value, 4)) %>% # round p-values to four decimal places
  mutate(model_number = case_when( # map model numbers to their corresponding formula
    model_number == 1 ~ "stunting_dep ~ wealth_index",
    model_number == 2 ~ "home_type_dep ~ stunting",
    model_number == 3 ~ "home_type_dep ~ wealth_index",
    model_number == 4 ~ "roof_type_dep ~ home_type_factor",
    model_number == 5 ~ "u5_net_use_dep ~ temp_monthly_2000m",
    model_number == 6 ~ "hh_size ~ wealth_index",
    model_number == 7 ~ "u5_net_use_dep ~ RH_monthly_2000m",
    model_number == 8 ~ "temp_monthly_2000m ~ RH_monthly_2000m",
    TRUE ~ model_number ))

# display the final results
final_results

## -----------------------------------------------------------------------------------------------------------------------------------------
### Data Preparation and Merging: Read Urban/Rural Datasets, Integrate Geospatial/Environmental Data, Obtain Country Identifiers
## -----------------------------------------------------------------------------------------------------------------------------------------

# read the urban dataset and add a column to indicate it is urban data
urban_df <- read_csv(file.path(PopDir, "analysis_dat/urban_df_for_analysis.csv")) %>%  mutate(type ="urban_data") 

# read the rural dataset and add a column to indicate it is rural data
rural_df <- read_csv(file.path(PopDir,"analysis_dat/rural_df_for_analysis.csv")) %>%  mutate(type ="rural_data")

# read the environmental data and create a new variable combining id and year, then remove dhs_year column
df_env <- read.csv(file.path(PopDir, "analysis_dat/all_geospatial_monthly_DHS.csv")) %>% 
  mutate(code_year = paste(stringr::str_extract(.id, "^.{2}"), dhs_year, sep = "")) %>% select(-dhs_year)

# obtain country ids with country names, DHS codes, and subregion names
ids <- dhs_countries(returnFields=c("CountryName", "DHS_CountryCode", "SubregionName"))

# join the urban dataset with country ids and environmental data by matching code_year and hv001
urban_df <- urban_df %>%  
  left_join(ids, by = ("DHS_CountryCode")) %>% 
  left_join(df_env, by = c("code_year", "hv001")) 

# join the rural dataset with country ids and environmental data by matching code_year and hv001
rural_df <- rural_df %>%  
  left_join(ids, by = ("DHS_CountryCode")) %>% 
  left_join(df_env, by = c("code_year", "hv001")) 

table(rural_df$u5_net_use) # display the frequency table for u5 net use in the rural dataset
table(urban_df$home_type2) # display the frequency table for home type in the urban dataset
table(urban_df$hv253) # display the frequency table for variable hv253 in the urban dataset
glimpse(urban_df) # get a quick overview of the structure of the urban dataset

## =========================================================================================================================================
### Fit Unadjusted Model for Urban and Rural Datasets
## =========================================================================================================================================

# create a list of urban and rural datasets
dat <- list(urban_df, rural_df)

# initialize an empty list to store the unadjusted results
unadj_df <- list()

# define the location names for the datasets
location <- c("Urban", "Rural")

# loop over the datasets to fit the unadjusted models
for (i in 1:length(dat)) {
  
  # create a modified dataframe with binary malaria result and home type variable
  mod_df <- dat[[i]] %>%  
    mutate(malaria_result = ifelse(test_result == "+ve", 1, 0)) %>% 
    mutate(work_HH = ifelse(home_type2 == "A", "1", "0")) %>%  
    rename(agric_home = work_HH)
  
  # create survey design object using svydesign.fun function
  svy_design <- svydesign.fun(mod_df)
  
  # fit an unadjusted logistic regression model (no additional covariates)
  result <- svyglm(malaria_result ~ agric_home, design = svy_design, family = binomial(link = "logit"))
  
  # summarize the model results
  res_sum <- summary(result)
  
  # tidy the result and filter out the intercept
  df <- tidy(result) %>%  
    filter(term != "(Intercept)") %>% 
    rename_at(3, ~ "SE")
  
  # calculate odds ratios and confidence intervals
  df <- data.frame(df) %>% 
    mutate(odds = exp(estimate)) %>%  # odds ratio estimation (exponentiate estimate)
    mutate(lower_ci = exp(-1.96 * SE + estimate)) %>% 
    mutate(upper_ci = exp(1.96 * SE + estimate)) %>% 
    tibble::rownames_to_column() %>%  # turn row names into a column?
    mutate(location = location[[i]], type = "unadjusted")  # add location and type columns
  
  # store the results in the list
  unadj_df[[i]] <- df
}

# combine the unadjusted results from both locations into a single dataframe and display
unadj_df <- bind_rows(unadj_df)
unadj_df

## =========================================================================================================================================
### Fit Adjusted Model
# exploring other potential predictors to understand their relationship with agricultural households (agric HH)
# generating categorical variable comparisons for urban and rural data
## =========================================================================================================================================

# title for the plots
title<- c("Urban", "Rural")

# initializing a list to store the plots
plots <- list()

# loop through both datasets (urban and rural)
for (i in 1:length(dat)){
  
  # --- plot 1: roof type comparison ---
  df <- dat[[i]] %>%  select(home_type2,roof_type) %>%                 # select relevant columns  
    drop_na() %>%                                                      # remove rows with NA values
    mutate(roof = ifelse(roof_type == 1, "Low-risk", "High-risk")) %>% # classify roof type into "Low-risk" and "High-risk"
    group_by(roof,home_type2) %>%                                      # group by roof type and home type (agricultural vs non-agricultural)   
    summarise(value= n()) %>%                                          # count occurrences in each group
    mutate(percent = round(value/sum(value) *100, 0)) %>%              # calculate percentages
    ungroup() 
  
  p1 <- ggplot(df, aes(fill=home_type2, x= roof)) + 
    geom_bar(aes(y = percent), position="stack", stat = "identity")+   # create stacked bar plot
    scale_fill_manual(name = "", label = c("Agricultural worker Household (HH)", "Non-Agricultural worker HH"), values = c("bisque", "forestgreen"))+
    theme_manuscript() +
    labs(x = "Roof", title = title[[i]] )
  
  # --- plot 2: wealth quintile comparison ---
  df2 <- dat[[i]] %>%  
    select(home_type2,wealth) %>%                                      # select home type and wealth columns
    drop_na() %>% 
    group_by(wealth,home_type2) %>%                                    # group by wealth and home type
    summarise(value= n()) %>% 
    mutate(percent = round(value/sum(value) *100, 0)) %>%  
    ungroup() 
  
  p2 <- ggplot(df2, aes(fill=home_type2, x= wealth)) + 
    geom_bar(aes(y = percent), position="stack", stat = "identity") +  # stacked bar plot
    scale_fill_manual(name = "", label = c("Agricultural worker Household (HH)", "Non-Agricultural worker HH"), values = c("bisque", "forestgreen"))+
    theme_manuscript() +
    labs(x = "Wealth Quintile", title = title[[i]])+
    theme(axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(), 
          axis.title.y = element_blank())
  
  # --- plot 3: net use comparison ---
  df3 <- dat[[i]] %>%  
    select(home_type2,u5_net_use) %>%  
    drop_na() %>% 
    mutate(net_use = ifelse(u5_net_use == 1, "Slept under a net", "Did not sleep under a net")) %>% 
    group_by(net_use,home_type2) %>%
    summarise(value= n()) %>% 
    mutate(percent = round(value/sum(value) *100, 0)) %>%
    ungroup() 
  
  p3 <- ggplot(df3, aes(fill=home_type2, x= net_use)) + 
    geom_bar(aes(y = percent), position="stack", stat = "identity")+
    scale_fill_manual(name = "", label = c("Agricultural worker Household (HH)", "Non-Agricultural worker HH"), values = c("bisque", "forestgreen"))+
    theme_manuscript() +
    labs(x = "Net use", title = title[[i]])+
    theme(axis.text.y = element_blank(), 
          axis.ticks.y = element_blank(), 
          axis.title.y = element_blank())
  
  # combine the three plots (roof, wealth, net use) in a single layout with shared legend at the bottom
  p <- p1 + p2 + p3 + plot_layout(guides = "collect") & theme(legend.position = 'bottom')
  
  # store the combined plot in the list
  plots[[i]] <- p
}

# p_cov contains the combined plot layout of urban and rural comparisons for confounders. Print and save as pdf
p_cov <- plots[[1]]/ plots[[2]]+ plot_layout(guides = "collect")
p_cov
ggsave(paste0(SupDir,"/", Sys.Date(),"likely_confounder_distribution.pdf"), p_cov, width = 8.5, height = 5) 

## -----------------------------------------------------------------------------------------------------------------------------------------
### IRS (indoor residual spraying) comparison for urban data - no difference between the two groups found
## -----------------------------------------------------------------------------------------------------------------------------------------

# selecting home type and IRS variables from the urban dataset
df3 <- urban_df %>%  
  select(home_type2,hv253) %>%                                           # select relevant columns: home type and IRS indicator
  drop_na() %>%                                                          # remove rows with missing values
  # create IRS variable: if hv253 is 1, mark as "House was sprayed", else "Not sprayed"
  mutate(IRS = ifelse(hv253 == 1, "House was sprayed", ifelse(hv253 == 0, "Not sprayed", NA))) %>% 
  group_by(home_type2, IRS) %>%
  summarise(value= n()) %>%                                              # count occurrences in each group
  mutate(percent = round(value/sum(value) *100, 0)) %>%
  ungroup() %>%
  drop_na(IRS)                                                           # remove rows with NA IRS values

# create the bar plot for IRS distribution and save as a pdf
ggplot(df3, aes(fill=IRS, x= home_type2)) + 
  geom_bar(aes(y = value), position="stack", stat = "identity")+
  scale_fill_manual(name = "", label = c(" House was sprayed", "House was not sprayed"), values = c("#d391fa", "#3e00b3"))+
  theme_manuscript() +
  geom_text(aes(label = paste0(value, " ", "(", percent, "%", ")"), y = value),
            position = position_stack(vjust = 0.5),
            color = "white") + 
  labs(x = "") +
  scale_x_discrete(labels = c("Agricultural worker \n household (HH)", "Non-agricultural \n worker HH"))
ggsave(paste0(ExpDir,"/", Sys.Date(),"IRS_distribution_urban.pdf"), width = 6, height = 5) 

## -----------------------------------------------------------------------------------------------------------------------------------------
### IRS (indoor residual spraying) comparison for rural data
## -----------------------------------------------------------------------------------------------------------------------------------------

# selecting home type and IRS variables from the rural dataset
df3 <- rural_df %>%  select(home_type2,hv253) %>%  drop_na() %>% 
  mutate(IRS = ifelse(hv253 == 1, "House was sprayed", ifelse(hv253 == 0, "Not sprayed", NA)))%>% 
  group_by(home_type2, IRS) %>%   summarise(value= n()) %>% 
  mutate(percent = round(value/sum(value) *100, 0)) %>%  ungroup() %>%  drop_na(IRS)

# create the bar plot for IRS distribution in rural areas and save as a pdf
ggplot(df3, aes(fill=IRS, x= home_type2)) + 
  geom_bar(aes(y = value), position="stack", stat = "identity")+
  scale_fill_manual(name = "", label = c(" House was sprayed", "House was not sprayed"), values = c("#d391fa", "#3e00b3"))+
  theme_manuscript() +
  geom_text(aes(label = paste0(value, " ", "(", percent, "%", ")"), y = value),
            position = position_stack(vjust = 0.5),
            color = "white")+ 
  labs(x = "")+
  scale_x_discrete(labels = c("Agricultural worker \n household (HH)", "Non-agricultural \n worker HH"))
ggsave(paste0(ExpDir,"/", Sys.Date(),"IRS_distribution_rural.pdf"), width = 6, height = 5) 


## =========================================================================================================================================
### Discrete and Categorical Variable Comparisons
## =========================================================================================================================================

# create boxplots to compare household size between agricultural and non-agricultural worker households in urban and rural areas

# prepare urban data for plotting
plot_df <- urban_df %>%
  mutate(home_type3 = ifelse(home_type2 == "A", "Agricultural worker \n Household (HH)", 
                             "Non-Agricultural \n worker HH"),
         type = "Urban")

# create boxplot for household size in urban areas
p1 <- ggplot(plot_df, aes(x = home_type3, y = hh_size, fill = home_type3)) +
  scale_fill_manual(name = '', values =c('#5560AB','#FAAF43'))+
  geom_boxplot(outlier.size = -1, color="black", alpha = 0.7) + # don't display outliers
  geom_point(aes(fill = home_type3), shape = 21,size=2, alpha=0.6, stroke=0, color = '#979797')+
  labs(x = "", y = "hh_size", fill = "home type") +
  theme_manuscript()+
  theme(legend.position = 'none')+
  labs(y = "Household size")+
  facet_wrap(vars(type)) +
  theme(strip.text.x = element_text(size = 12))

# prepare rural data for plotting
plot_df <- rural_df %>% 
  mutate(home_type3 = ifelse(home_type2 == "A", "Agricultural worker \n Household (HH)", 
                             "Non-Agricultural \n worker HH"),
         type = "Rural")

# create boxplot for household size in rural areas
p2 <- ggplot(plot_df, aes(x = home_type3, y = hh_size, fill = home_type3)) +
  scale_fill_manual(name = '', values =c('#5560AB','#FAAF43'))+
  geom_boxplot(outlier.size = -1, color="black", alpha = 0.7) +
  geom_point(aes(fill = home_type3), shape = 21,size=2, alpha=0.6, stroke=0, color = '#979797')+
  labs(x = "", y = "hh_size", fill = "home type") +
  theme_manuscript()+
  theme(legend.position = 'none')+
  labs(y = "Household size")+
  facet_wrap(vars(type))+ 
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.y = element_blank()) +
  theme(strip.text.x = element_text(size = 12))

# combine urban and rural household size plots, display, and save as pdf
p_hh_size = p1 + p2
p_hh_size
ggsave(paste0(SupDir,"/", Sys.Date(),"HH_size_distribution.pdf"), p_hh_size, width = 8.5, height = 4) 


## =========================================================================================================================================
### Adjusted Model Fit: Urban and Rural Data
## =========================================================================================================================================

# assessing environmental covariates for correlation
ev_cor <- urban_df %>% 
  select(EVI_2000m, preci_monthly_2000m, RH_monthly_2000m, temp_monthly_2000m) %>% # select relevant environmental covariates
  cor() # calculate correlation matrix for urban data

# plot correlation matrix for urban data
corr = ggcorrplot(ev_cor, lab = TRUE, legend.title = "Correlation coefficient")+ theme_corr()
corr

# calculate correlation for rural data
ev_cor_rural <- rural_df %>% 
  select(EVI_2000m, preci_monthly_2000m, RH_monthly_2000m, temp_monthly_2000m) %>% 
  cor() # calculate correlation matrix for rural data

# plot correlation matrix for rural data
corr_rural= ggcorrplot(ev_cor_rural, lab = TRUE, legend.title = "Correlation coefficient") + theme_corr()
corr_rural

# note: correlation is very low with highest of -0.19

# combine urban and rural data into a list for further analysis
dat <- list(urban_df, rural_df)

# initialize lists for adjusted dataframes and model results
adj_df <- list()
mod_list <- list()

# specify locations for modeling
location <- c("Urban", "Rural")

# loop through each dataset in the list
for (i in 1:length(dat)) {
  # prepare the data for modeling
  mod_df <- dat[[i]] %>%
    mutate(malaria_result = ifelse(test_result == "+ve", 1, 0)) %>%  # create binary outcome variable for malaria result
    mutate(work_HH = ifelse(home_type2 == "A", "1", "0")) %>%  # create binary variable for agricultural home type
    rename(agric_home = work_HH)  # rename the variable for clarity
  
  # define the survey design object
  svy_design <- svydesign.fun(mod_df)  # create a survey design object
  
  # fit the generalized linear model
  result <- svyglm(malaria_result ~ agric_home + u5_net_use + roof_type + wealth + hh_size + dhs_year + DHS_CountryCode + 
                     EVI_2000m + preci_monthly_2000m + RH_monthly_2000m + temp_monthly_2000m, 
                   design = svy_design, family = binomial(link = "logit"))  # fits logistic regression model
  
  # store the model result
  mod_list[[i]] <- result  # save the model output in a list
  res_sum <- summary(result)  # summarize the model results
  
  # tidy the model results for easier interpretation
  df <- tidy(result)  # create a tidy dataframe of model results
  df <- df %>% 
    filter(term != "(Intercept)") %>%  # remove the intercept from the results
    rename_at(3, ~"SE")  # rename the third column to "SE" for standard error
  
  # calculate odds ratios and confidence intervals
  df <- data.frame(df) %>%
    mutate(odds = exp(estimate)) %>%  # calculate odds ratio from the estimate
    mutate(lower_ci = exp(-1.96 * SE + estimate)) %>%  # calculate lower confidence interval
    mutate(upper_ci = exp(1.96 * SE + estimate)) %>%  # calculate upper confidence interval
    tibble::rownames_to_column() %>%  # convert row names to a column
    mutate(location = location[[i]], type = "adjusted")  # add location and type information
  
  # store the adjusted results
  adj_df[[i]] <- df  # save the tidy dataframe in a list
}

# combine all adjusted results into a single dataframe
adj_df <- bind_rows(adj_df)
adj_df

# bind adjusted and unadjusted estimates into a single dataframe
all_est <- rbind(unadj_df, adj_df)
all_est

# predicted probability for urban households
effct_df_urban <- effect_df_fun(mod_list[[1]]) %>%  
  mutate(rowname_new = ifelse(rowname==0, "Non-agricultural \n worker HH", "Agricultural \n worker HH"))

# predicted probability for rural households
effct_df_rural <- effect_df_fun(mod_list[[2]]) %>%  
  mutate(rowname_new = ifelse(rowname==0, "Non-agricultural \n worker HH", "Agricultural \n worker HH"))

# prepare data for country-adjusted odds calculation
mod_df <- urban_df %>%  
  mutate(malaria_result = ifelse(test_result =="+ve", 1,0)) %>% 
  mutate(work_HH =  ifelse(home_type2 =="A", "1","0")) %>% 
  rename(agric_home = work_HH)

# create survey design object
svy_design <- svydesign.fun(mod_df)

# run survey regression by country
result <- svyby(formula = malaria_result ~ agric_home + u5_net_use + roof_type + wealth + hh_size +
                  EVI_2000m + preci_monthly_2000m + RH_monthly_2000m + temp_monthly_2000m, 
                by = ~ DHS_CountryCode, design = svy_design, FUN = svyglm)

# calculate odds ratios and confidence intervals
df <- result%>% mutate(odds = (exp(agric_home1))) %>% # odds ratio estimation 
  mutate(lower_ci = (exp(-1.96*se.agric_home1+agric_home1))) %>% 
  mutate(upper_ci = (exp(1.96*se.agric_home1+agric_home1))) %>% 
  select(DHS_CountryCode, agric_home1, se.agric_home1, odds, lower_ci, upper_ci) # select relevant columns


## =========================================================================================================================================
### Plots
## =========================================================================================================================================

## -----------------------------------------------------------------------------------------------------------------------------------------
### Forest Plots
## -----------------------------------------------------------------------------------------------------------------------------------------

# forest plots for odds ratios
df <- all_est %>%  
  filter(term == "agric_home1") %>% 
  mutate(loc_type = paste0(location, " ", "(", type, ")"))

# convert location to factor
df$location <- factor(df$location)

library(scales)

# determine the number of colors needed
numColors <- length(levels(df$location)) # how many colors you need
getColors <- scales::brewer_pal('qual') # create a function that takes a number and returns a qualitative palette of that length (from the scales package)
myPalette <- getColors(numColors)
names(myPalette) <- levels(df$location) # give every color an appropriate name

# create the forest plot
forest_b <- ggplot(df, aes(x = odds, y = loc_type)) + 
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
  geom_errorbarh(aes(xmax = lower_ci, xmin = upper_ci, color = location), size = .5, height = 
                   .2) + 
  geom_point(aes(color = location), size = 2.5)+
  scale_color_manual(name ="", values =c("forestgreen", "darkorchid4")) +
  theme_bw()+
  theme(panel.grid.minor = element_blank())+ 
  theme(panel.border = element_blank())+
  ylab("")+
  xlab("Odds ratio for testing positive for malaria with RDT
        or microscopy among children, 6 - 59 months,
        in an agricultural worker HH compared to a 
        compared to a non-agricultural worker HH")+
  theme_manuscript()+
  theme(legend.position = "none")+
  xlim(0.5,3.7) + 
  theme(axis.text.y = element_text(colour=c("forestgreen", "forestgreen",  "darkorchid4", "darkorchid4")))
forest_b


## -----------------------------------------------------------------------------------------------------------------------------------------
### Predicted Probability
## -----------------------------------------------------------------------------------------------------------------------------------------

# create predicted probability plot
pred_p <- ggplot() + 
  geom_errorbar(data = effct_df_urban, aes(y = effect, x = rowname_new, ymax = lower, ymin = upper), size = .5, width = .2, color = "darkorchid4") + 
  geom_errorbar(data = effct_df_rural, aes(y = effect, x = rowname_new, ymax = lower, ymin = upper), size = .5, width = .2, color = "forestgreen") + 
  geom_point(data = effct_df_urban, aes(y = effect, x = rowname_new), size = 2.5, color = "darkorchid4")+
  geom_point(data = effct_df_rural, aes(y = effect, x =rowname_new), size = 2.5, color = "forestgreen")+
  theme(panel.grid.minor = element_blank())+ 
  theme(panel.border = element_blank(), axis.title.x=element_blank(),
        main.title.x=element_blank())+
  theme_manuscript()+
  labs(x = "", y= "Adjusted predicted probability of testing positive for 
       malaria with RDT or microscopy among children, 
       6 - 59 months") +
  ylim(0,0.5)

# combine forest plot and predicted probability plot and save as pdf
all_p <- forest_b + pred_p
ggsave(paste0(FigDir,"/", Sys.Date(),"_Figure_4_odds_pred_prob.pdf"), all_p, width = 8.5, height = 5) 


## -----------------------------------------------------------------------------------------------------------------------------------------
### Environmental Variable Effects
## -----------------------------------------------------------------------------------------------------------------------------------------

# calculate effect of EVI_2000m on the model
eff <- Effect("EVI_2000m", m1)

# function to extract effect estimates and confidence intervals
effect_df_fun <- function(model_, co_var){
  effect_list_est <- summary(Effect(co_var, model_)) 
  effect_list_est$effect %>% as.data.frame() %>% 
    bind_cols(effect_list_est$lower %>% as.data.frame()) %>% 
    bind_cols(effect_list_est$upper %>% as.data.frame()) %>% 
    rename(effect = ....1, lower = ....2, upper = ....3) %>% 
    tibble::rownames_to_column()
}

# predicted probability for urban and rural
effct_df_urban <- effect_df_fun(mod_list[[1]], "EVI_2000m") 
effct_df_rural <- effect_df_fun(mod_list[[2]], "EVI_2000m")

# set variable names and limits for predictions
vars <- c("EVI_2000m" , "preci_monthly_2000m" , "RH_monthly_2000m" , "temp_monthly_2000m")
y_lim <- list(0.5, 0.5 ,0.5, 0.5)
lables <- list("enhanced vegetation index", "precipitation", "relative humidity", "temperature")

# function to create predicted probability plots
pred_fun <- function(model_number){
  p <- list()
  for (i in 1:length(vars)) { 
    eff <- Effect(vars[[i]], mod_list[[model_number]])
    eff_dt <- data.frame(eff)
    eff_dt$fit <- (eff_dt$fit) #/2 # we are scaling by deviding by mean number of children tested in a cluster
    eff_dt$lower <- (eff_dt$lower)#/2
    eff_dt$upper <- (eff_dt$upper)#/2
    
    # create ggplot for each variable
    pt = ggplot(eff_dt,aes_string(vars[[i]], 'fit'))+ 
      geom_ribbon(aes(ymin=lower, ymax=upper), alpha =0.2, fill = "#666699")+
      geom_line(color = "#666699", size = 1)+ theme_manuscript()+ 
      ylim(0, y_lim[[1]]) + 
      labs(x = paste0(as.character(lables[[i]]), ' ', as.character('')), y ='predicted probability to test positive')
    p[[i]]<- pt
    
  }
  
  # combine all plots into one
  y <- p[[1]] + p[[2]] + p[[3]] + p[[4]]
  return(y) 
}

# create predicted probability plots for urban and rural
pred_fun(1) # urban
pred_fun(2) # rural

## =========================================================================================================================================
### Model Tests
## =========================================================================================================================================

# prepare datasets by selecting relevant columns and dropping NA values
dat <- list(urban_df, rural_df) %>% 
  map(~select(.,(c(home_type2, u5_net_use, roof_type, wealth, hh_size, EVI_2000m, preci_monthly_2000m, dhs_year,
                   RH_monthly_2000m, temp_monthly_2000m, strat, wt, id, test_result, DHS_CountryCode)))) %>% map(~drop_na(.,))

mod_list2 <- list()

# loop through each dataset to fit models
for (i in 1:length(dat)) {
  # create a modified dataframe with binary outcome for malaria result
  mod_df <- dat[[i]] %>% 
    mutate(malaria_result = ifelse(test_result == "+ve", 1, 0)) %>% 
    mutate(work_HH = ifelse(home_type2 == "A", "1", "0")) %>% 
    rename(agric_home = work_HH)
  
  # create survey design object
  svy_design <- svydesign.fun(mod_df)
  
  # fit models with different predictors
  result <- svyglm(malaria_result ~ agric_home + u5_net_use + roof_type + wealth + 
                     hh_size + dhs_year + DHS_CountryCode + 
                     EVI_2000m + preci_monthly_2000m + 
                     RH_monthly_2000m + temp_monthly_2000m, 
                   design = svy_design, family = binomial(link = "logit"))
  
  result_evi <- svyglm(malaria_result ~ agric_home + u5_net_use + roof_type + wealth + 
                         hh_size + dhs_year + DHS_CountryCode + 
                         EVI_2000m, 
                       design = svy_design, family = binomial(link = "logit"))
  
  result_preci <- svyglm(malaria_result ~ agric_home + u5_net_use + roof_type + wealth + 
                           hh_size + dhs_year + DHS_CountryCode + 
                           preci_monthly_2000m, 
                         design = svy_design, family = binomial(link = "logit"))
  
  result_RH <- svyglm(malaria_result ~ agric_home + u5_net_use + roof_type + wealth + 
                        hh_size + dhs_year + DHS_CountryCode + 
                        RH_monthly_2000m, 
                      design = svy_design, family = binomial(link = "logit"))
  
  result_temp <- svyglm(malaria_result ~ agric_home + u5_net_use + roof_type + wealth + 
                          hh_size + dhs_year + DHS_CountryCode + 
                          temp_monthly_2000m, 
                        design = svy_design, family = binomial(link = "logit"))
  
  # store the results for the current model in the list
  mod_list2[[i]] <- result
}

# perform Wald tests for rural models
regTermTest(result_evi, test.terms = ~ EVI_2000m,
            df = degf(result_evi$survey.design), method = "LRT")

regTermTest(result_temp, test.terms = ~ temp_monthly_2000m,
            df = degf(result_temp$survey.design), method = "LRT")

regTermTest(result_RH, test.terms = ~ RH_monthly_2000m,
            df = degf(result_RH$survey.design), method = "LRT")

regTermTest(result_preci, test.terms = ~ preci_monthly_2000m,
            df = degf(result_preci$survey.design), method = "LRT")


# perform Wald tests for urban models
urban_model <- mod_list2[[1]]
regTermTest(urban_model, test.terms = ~ EVI_2000m,
            df = degf(urban_model$survey.design), method = "LRT")

regTermTest(urban_model, test.terms = ~  temp_monthly_2000m,
            df = degf(urban_model$survey.design), method = "LRT")

regTermTest(urban_model, test.terms = ~ RH_monthly_2000m,
            df = degf(urban_model$survey.design), method = "LRT")

regTermTest(urban_model, test.terms = ~ preci_monthly_2000m,
            df = degf(urban_model$survey.design), method = "LRT")

#### testing 2LogLR percentage difference

# initialize list to store survey designs
design_list <- list()

# loop through each dataset
for (i in 1:length(dat)) {
  mod_df <- dat[[i]] %>%  
    mutate(malaria_result = ifelse(test_result == "+ve", 1, 0)) %>% # convert test results to binary
    mutate(work_HH = ifelse(home_type2 == "A", "1","0")) %>% # create binary variable for agricultural home
    rename(agric_home = work_HH) # rename variable
  
  # create survey design
  svy_design <- svydesign.fun(mod_df)
  
  # store survey design in the list
  design_list[[i]] <- svy_design
  
}

# baseline model
result <- svyglm(malaria_result ~ agric_home,
                 design = desin_list[[1]], family = binomial(link ="logit"))

# perform Wald test for the baseline model
regTermTest(result_evi, test.terms = ~ agric_home,
            df = degf(result_evi$survey.design), method = "LRT") # test significance of agric_home

# urban wald test

# fit logistic regression for EVI
result_evi <- svyglm(malaria_result ~ EVI_2000m,
                     design = svy_design, family = binomial(link ="logit"))

# test significance of EVI
regTermTest(result_evi, test.terms = ~ EVI_2000m,
            df = degf(result_evi$survey.design), method = "LRT")

# fit logistic regression for TEMPERATURE
result_temp <- svyglm(malaria_result ~ temp_monthly_2000m,
                      design = svy_design, family = binomial(link ="logit"))

# test significance of TEMPERATURE
regTermTest(result_temp, test.terms = ~ temp_monthly_2000m,
            df = degf(result_temp$survey.design), method = "LRT")

# fit logistic regression for RELATIVE HUMIDITY
result_RH <- svyglm(malaria_result ~ RH_monthly_2000m,
                    design = svy_design, family = binomial(link ="logit"))

# test significance of RELATIVE HUMIDITY
regTermTest(result_RH, test.terms = ~ RH_monthly_2000m,
            df = degf(result_RH$survey.design), method = "LRT")

# fit logistic regression for PRECIPITATION
result_preci <- svyglm(malaria_result ~ preci_monthly_2000m,
                       design = svy_design, family = binomial(link ="logit"))

# test significance of PRECIPITATION
regTermTest(result_preci, test.terms = ~ preci_monthly_2000m,
            df = degf(result_preci$survey.design), method = "LRT")
