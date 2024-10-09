# ==========================================================================================================================================
# Script Name: Compiling Analysis Dataset Trend Analysis
# Author: Dr. Ifeoma Ozodiegwu
# Edited by: Grace Legris, Research Data Analyst (gracebea@gmail.com)
# Edited: [2024-09-26]
# Purpose: recode variables in the ir, mr and pr datasets
#          summarize the ir and mr script at the household level
#          merge them for urban and rural areas 
# ==========================================================================================================================================

#remove.packages("rdhs")
#learn about RDHS, used for downloading the DHS data here https://github.com/ropensci/rdhs

# clear current workspace
rm(list = ls())

#devtools::install_github("ropensci/rdhs")

library(rdhs)

## =========================================================================================================================================
### Directory Management and File Paths
## =========================================================================================================================================

user <- Sys.getenv("USER")
if ("ido0493" %in% user) {
    user_path <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("OneDrive"))))
    DriveDir <- file.path(user_path, "urban_malaria")
    PopDir <- file.path(DriveDir, "data", "data_agric_analysis")
    ManDir <- file.path(DriveDir, "projects", "Manuscripts", "agriculture_malaria_manuscript")
    FigDir <- file.path(ManDir, "figures", "exploratory")
} else if  ("CHZCHI003" %in% user) {
    Drive <- file.path("C:/Users/CHZCHI003/OneDrive")
    DriveDir <- file.path(Drive, "urban_malaria")
    PopDir <- file.path(DriveDir, "data", 'data_agric_analysis')
    ManDir <- file.path(DriveDir, "projects", "Manuscripts", "agriculture_malaria_manuscript")
    FigDir <- file.path(ManDir, "figures", "exploratory")
} else if ("grace" %in% user) {
    Drive <- "/Users/grace/Urban Malaria Proj Dropbox"
    DriveDir <- file.path(Drive, "urban_malaria")
    PopDir <- file.path(DriveDir, "data", 'data_agric_analysis')
    ManDir <- file.path(DriveDir, "projects", "Manuscripts", "ongoing", "agriculture_malaria_manuscript")
    FigDir <- file.path(ManDir, "figures", "exploratory")
} else {
    Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
    DriveDir <- file.path(Drive, 'Library', 'CloudStorage', 'OneDrive-NorthwesternUniversity', "urban_malaria")
    #DriveDir <- file.path(Drive,  "OneDrive - Northwestern University", "urban_malaria")
    PopDir <- file.path(DriveDir, "data", 'data_agric_analysis')
    ManDir <- file.path(DriveDir, "projects", "Manuscripts", "agriculture_malaria_manuscript")
    FigDir <- file.path(ManDir, "figures", "exploratory")
}


## =========================================================================================================================================
### Required Functions and Settings
## =========================================================================================================================================

# note: before sourcing functions and packages, run the code below to download rdhs if you don't already have it
# devtools::install_github("ropensci/rdhs", force = T)
source("functions/functions_employment.R")
options(survey.lonely.psu="adjust")  # this option allows admin units with only one cluster to be analyzed

## -----------------------------------------------------------------------------------------------------------------------------------------
### Read in Survey IDs and DHS Login
## -----------------------------------------------------------------------------------------------------------------------------------------

# If files need to be updated, log in to your DHS account to download new datasets.
# Replace email and password with your credentials when running the script locally.
# Be sure to remove personal credentials before making the script public.

# log in into dhs
my_config <- set_rdhs_config(email = "gracebea@gmail.com", #cchiziba@gmail.com",#"ozodiegwui@gmail.com",
                             project = "Agricultural Malaria Project", #"Net ownership by individual", # "Association of household member engagement in agricultural work and malaria",
                             config_path = "rdhs.json",
                             cache_path = "data",
                             password_prompt = TRUE,
                             global = FALSE, 
                             timeout = 600)

# read in the survey IDs from a CSV file for the surveys to be analyzed
survs <- read.csv(file.path(ManDir, "csv", "surveyids_for_analysis.csv")) #%>%
  #mutate(cntryId = stringr::str_extract(SurveyId, "^.{2}")) %>% group_by(cntryId) %>% 
  #slice_max(SurveyYear) %>% ungroup()
 
# obtain country IDs and metadata for countries using the DHS API
ids <- dhs_countries(returnFields=c("CountryName", "DHS_CountryCode", "SubregionName"))
 

## -----------------------------------------------------------------------------------------------------------------------------------------
### Data Loading and Transforming IR and MR Datasets (no need to run commented out script unless dhs files need an update)
## -----------------------------------------------------------------------------------------------------------------------------------------

# download individual recode (IR) datasets for the survey IDs specified in the 'survs' object
# this queries the DHS API for IR datasets in DT format based on the survey IDs.
ir_datasets <- dhs_datasets(surveyIds = survs$SurveyId, fileFormat = "DT", fileType = "IR") #%>%
  #group_by(CountryName) %>% slice_max(SurveyYear)

# initialize an empty list to store downloaded IR datasets
ir_downloads <- list()

# loop through each dataset in 'ir_datasets' and download them
for (i in 1:nrow(ir_datasets)) {
    # the following block handles any errors during the dataset download process.
    # for some reason, I am not able to download this file with my credentials so downloading manually:
    # if (ir_datasets$FileName[i] == "MDIR81DT.ZIP") {
    #   df <- list("MDIR81FL" = "project_one/datasets/MDIR81FL.ZIP")  # Manually create a link for the dataset
    #   ir_downloads <- append(ir_downloads, df)
    #   next  # Skip to the next iteration of the loop
    # }
    
    # attempt to download the dataset and handle any errors encountered
    tryCatch({
      # download the dataset and save it to the specified directory ('PopDir')
      df <- get_datasets(ir_datasets$FileName[i], output_dir_root = PopDir, download_option = "zip", verbose_argument = TRUE)
      
      # append the downloaded dataset to the 'ir_downloads' list
      ir_downloads <- append(ir_downloads, df)
      
    }, error = function(e) {
      # in case of an error, print a message and continue to the next dataset
      cat("ERROR:", conditionMessage(e), "\n")
    })
}
  

## -----------------------------------------------------------------------------------------------------------------------------------------
### Processing and Saving IR Dataset Downloads: Urban and Rural Data Transformation and Export
## -----------------------------------------------------------------------------------------------------------------------------------------

# save the downloaded IR datasets as RDS files to avoid repeated sign-ins for future use
saveRDS(ir_downloads, file.path(PopDir, "analysis_dat/ir_downloads.rds"))

# load the previously saved IR downloads if needed
ir_downloads<- readRDS(file.path(PopDir, "analysis_dat/ir_downloads.rds"))
print(ir_downloads)

# check for any surveys that failed to download and need manual intervention
data.frame(SurveyId = setdiff(survs$SurveyId, ir_datasets$SurveyId))

# unzip the downloaded datasets to the specified directory 'PopDir' for further use
for (i in 1:length(ir_downloads)){
  unzip(ir_downloads[[i]],  exdir = file.path(PopDir, "data/opened/IR"))
}

# initialize lists to store transformed urban and rural IR datasets
# creates lists of dataframes and cleans it up, adding NA columns where mutated variables are absent 
dhs_ir_urban <- list()
dhs_ir_rural <- list()

# get the paths to the unzipped DTA files for the IR datasets
link_ir <- list.files(path = file.path(PopDir, "data", "opened", "IR"), pattern = "DTA", full.names = TRUE, recursive = F)

# process each IR dataset file
for (i in 1:length(link_ir)){
    # read the IR dataset into R
    dhs_ir <- read_dta(link_ir[[i]])
    
    # handle missing v701 (edu_man) - it is missing from TZIR6AFL.DTA and UGIR5AFL.DTA
    if (!("v701" %in% names(dhs_ir))) {
      dhs_ir$v701 <- NA  # create v701 as NA if it doesn't exist
    }
    
    graces_test <- dhs_ir %>% 
      select(matches("v106|v701"))
    
    # select relevant columns and create new variables for analysis
    df <- dhs_ir %>%
      dplyr::select(matches("v000|v001|v002|v003|v005|v006|v007|v012|v021|v022|v025|v106|v157|v158|v159|v168|v190|v501|v505|v701|v704|v704a|v705|v716|v717|v731|v732|v743a|s1108ai|s1108ba|s1108bc|s1108bd|s1108bf|b19|h11_1|h11_2|h11_3|h11_4|h11_5|h11_6")) %>%
      mutate(
             # create household occupation variables for women and partners
             agri_partner=tryCatch(ifelse(v705 %in% c(4, 5), "A",ifelse(v705  %in% c(1, 2, 3, 6, 7,8, 9,96), "O", ifelse(v705 == 0, "U", NA))), error =function(e) return(NA)),
             agri_woman = tryCatch(ifelse(v717 %in% c(4, 5), "A", ifelse(v717 %in% c(1, 2, 3, 6, 7, 8, 9, 11), "O", ifelse(v717 == 0, "U", NA))), error =function(e) return(NA))) %>%
             #duration_travel_woman = tryCatch(ifelse(v168 >=9, NA, v168), error =function(e) return(NA)), #Away for more than one month in the last 12 months
             #last_work_partner = tryCatch(ifelse(v704a >=8, NA, ifelse(v704a==1, 1, 0)), error = function(e) return(NA)),# if husband/partner has worked in the last 7 days or in the last 12 months)
             #last_work_woman = tryCatch(ifelse(v731 ==2, 1, ifelse(v731 >=9, NA, 0)), error = function(e) return(NA)), #if respondent has worked in the last past year, is currently working, has a job or is on leave in the last 7 days
             #trans_work_partner = tryCatch(ifelse(v704 ==9, 1,ifelse(v704 ==99998, NA, 0)),error = function(e) return(NA)),#if husband works in transportation and material moving
             #trans_work_woman = tryCatch(ifelse(v716 ==9, 1, 0), error = function(e) return(NA)), #if woman works in transportation and material moving
             #media_exposure = tryCatch(ifelse(v157 %in% c(2, 3) | (v159 %in% c(2, 3)) | (v158 %in% c(2, 3)), 0, 1), error = function(e) return(NA)),#access to at least one for a week
             #co_wives = tryCatch(ifelse(v505 ==98,NA, v505), error = function(e) return(NA)),
             #health_dec = tryCatch(ifelse(v743a == 1, 1, 0), error = function(e) return(NA)),
             #seasonal_work_woman = tryCatch(ifelse(v732 == 2, 1, ifelse(v732 == 1, 0, NA)), error = function(e)return(NA))) %>%   #if respondent or woman is a seasonal worker or not)
      
      mutate(kap_weak= tryCatch(ifelse(s1108bf == 0, 1, 0), error = function(e) return(NA))) %>%
      # adjust missing values in specific variables
      mutate(across(contains("s1108"), ~ifelse(.x >=8, NA, .x), .names = "new_{.col}")) %>%
      # create an index based on specific 's1108' variables
      mutate(kap_index = if("new_s1108ai" %in% colnames(.)) (new_s1108ai+ new_s1108ba + new_s1108bc + new_s1108bd + kap_weak)/5 else NA) %>%
      # rename some variables for clarity and consistency
      dplyr::rename(strat=v022, id=v021, dhs_year = v007, edu_woman  = v106, edu_man = v701, age_woman = v012,wealth = v190) %>%
      dplyr::select(-c(starts_with("s1108"))) %>%
      
      # create a composite household occupation variable and categorize it
      unite(col = "HH_occ", agri_partner, agri_woman, na.rm=TRUE, sep="", remove = FALSE) %>%
      mutate(HH_occ = ifelse(HH_occ == "", "M", HH_occ)) %>%
      mutate(occ_val = ifelse(grepl("A", HH_occ), "A", HH_occ)) %>%
      mutate(occ_val = ifelse(grepl("O", occ_val), "O", occ_val)) %>%
      mutate(occ_val = ifelse(grepl("U", occ_val), "U", occ_val)) %>%
      mutate(occ_val = factor(occ_val, levels = c("A", "O", "U", "M")), wt=v005/1000000)
    
      # add the country code and merge with an additional 'ids' dataset
      df <- df %>%  mutate(DHS_CountryCode = str_sub(v000, 1, 2)) %>%  left_join(ids) %>%
      mutate( min_year =min(dhs_year), code_year = paste0(DHS_CountryCode, min_year))
    
    # filter the dataset into urban and rural subsets
    df_urban <- df %>%  filter(v025 == 1) # urban data
    df_rural <- df %>%  filter(v025 == 2) # rural data
    
    # append the urban data to the list
    print(paste("appending urban data from", unique(df_urban[["CountryName"]]), "to list of data frames"))
    dhs_ir_urban <- append(dhs_ir_urban, list(df_urban))
    
    # append the rural data to the list
    print(paste("appending rural data from", unique(df_urban[["CountryName"]]), "to list of data frames"))
    dhs_ir_rural <- append(dhs_ir_rural, list(df_rural))
}

# verify specific variables within the urban and rural datasets
lapply(dhs_ir_urban, function(x) table(x$h11_6))
lapply(dhs_ir_rural, function(x) table(x$h11_1))

# save the urban datasets as an RDS file for future use
saveRDS(dhs_ir_urban, file.path(PopDir, "analysis_dat/dhs_ir_urban.rds"))

# load the urban datasets if needed
dhs_ir_urban<- readRDS(file.path(PopDir, "analysis_dat/dhs_ir_urban.rds"))

# filter list to keep only datasets with agriculture workers that are partners as this eliminates countries with no agricultural data 
#dhs_ir_urban <- dhs_ir_urban %>%purrr::discard(~all(is.na(.x$agri_partner)))
#dhs_ir_rural <- dhs_ir_rural %>%purrr::discard(~all(is.na(.x$agri_partner)))



## -----------------------------------------------------------------------------------------------------------------------------------------
### Exploring data to see if it is fit for purpose
## -----------------------------------------------------------------------------------------------------------------------------------------

# plot to view which countries have sufficient agricultural worker data 
dhs_ir_urban <- dhs_ir_urban %>% map(~mutate(., max_year = max(dhs_year), year_combo = ifelse(max_year == min_year, max_year, paste(min_year, "-",str_sub(max_year, -2))),
                                             country_year = paste0(CountryName, " ", year_combo), category = occ_val))
dhs_ir_rural <- dhs_ir_rural %>% map(~mutate(., max_year = max(dhs_year), min_year = min(as.numeric(dhs_year)), year_combo = ifelse(max_year == min_year, max_year, paste(min_year, "-", str_sub(max_year, -2))),
                                             country_year = paste0(CountryName, " ", year_combo), category = occ_val))

plot_u_df<- dhs_ir_urban %>% map(~dplyr::select(., country_year, category)) %>%  bind_rows(.id = "column_label")
plot_r_df <- dhs_ir_rural %>% map(~dplyr::select(., country_year,category)) %>%  bind_rows(.id = "column_label")



# do we have enough data for this analysis? we have 20 datasets to work with now. Reduced it from the initial 35
# xlabel = "Occupation (A - Agricultural work, O - other, U - unemployed, M - Missing)"
# color = c("#a7cece", "#e8a6bd", "#afb2cb", "#aaa3a2")
# p <- bar_fun(plot_u_df, "category" ,"category", "DHS datasets with agricultural worker data (urban areas)", xlabel)+
#    scale_fill_manual(values= color)+
#    facet_wrap(vars(country_year), scales="free")
# ggsave(paste0(FigDir,"/", Sys.Date(),"_urban_DHS_datasets_agric_data.png"), p, width = 13, height = 13)
# p 
# p <- bar_fun(plot_r_df, "category" , "category","DHS datasets with agricultural worker data (rural areas)", xlabel)+
#    scale_fill_manual(values=color)+
#    facet_wrap(vars(country_year), scales="free")
# ggsave(paste0(FigDir,"/", Sys.Date(),"_rural_DHS_datasets_agric_data.png"), p, width = 13, height = 13)



# Change to weighted percentage to compare across countries 
# urban
# plot_u_df <- dhs_ir_urban  %>% map(~as_survey_design(., ids= id,strata=strat,nest=T,weights= wt))%>% map(~drop_na(.,category)) %>%
#  map(~group_by(., category)) %>%  map(~summarize(., across(country_year), percent = survey_mean() *100,
#                                             total = survey_total())) %>%  map(~distinct(., category, .keep_all = TRUE))
# plot_u_df <- plyr::ldply(plot_u_df) %>%  mutate(category = factor(category, levels = c("M", "U", "O", "A")))
# 
# label = c("Missing","Unemployed","Other work", "Agricultural worker")
# color = c("#aaa3a2", "#afb2cb", "#e8a6bd", "#a7cece")
# p <- col_fun(plot_u_df, "country_year", "percent", "category", "Weighted Percentage (urban areas)", color, label)
# ggsave(paste0(FigDir,"/", Sys.Date(),"_urban_DHS_datasets_agric_data_weighted_percent.png"), p, width = 13, height = 13)
# p
# # rural
# plot_r_df <- dhs_ir_rural  %>% map(~as_survey_design(., ids= id,strata=strat,nest=T,weights= wt))%>% map(~drop_na(.,category)) %>%
#   map(~group_by(., category)) %>%  map(~summarize(., across(country_year), percent = survey_mean() *100,
#                                                   total = survey_total())) %>%  map(~distinct(., category, .keep_all = TRUE))
# plot_r_df  <- plyr::ldply(plot_r_df) %>%  mutate(category = factor(category, levels = c("M", "U", "O", "A")))
# p <- col_fun(plot_r_df, "country_year", "percent", "category", "Weighted Percentage (rural areas)", color, label)
# ggsave(paste0(FigDir,"/", Sys.Date(),"_rural_DHS_datasets_agric_data_weighted_percent.png"), p, width = 13, height = 13)
# p
# 


## =========================================================================================================================================
### Downloading, Processing, and Saving MR Datasets
## =========================================================================================================================================

# download MR datasets based on survey IDs
mr_datasets <- dhs_datasets(surveyIds = survs$SurveyId, fileFormat = "DT", fileType = "MR") #%>%
   #group_by(CountryName) %>%
   #slice_max(SurveyYear)

# initialize an empty list to store MR downloads 
mr_downloads <- list()

# loop through the datasets and download each, handling any errors that arise
for (i in 1:nrow(mr_datasets)){
    tryCatch({
      df <- get_datasets(mr_datasets$FileName[i], download_option = "zip", verbose_argument = TRUE)
      mr_downloads <- append(mr_downloads, df)
    }, error = function(e){cat("ERROR:", conditionMessage(e), "\n")})
}

# save the MR downloads to an RDS file for easy loading in future sessions 
saveRDS(mr_downloads, file.path(PopDir, "analysis_dat/mr_downloads.rds"))
mr_downloads<- readRDS(file.path(PopDir, "analysis_dat/mr_downloads.rds"))
 
# check if any datasets still need to be manually downloaded
data.frame(SurveyId = setdiff(survs$SurveyId, mr_datasets$SurveyId))

# initialize lists to store urban and rural datasets
dhs_mr_urban <- list()
dhs_mr_rural <- list()

# loop through the MR downloads to unzip and process each dataset
for (i in 1:length(mr_downloads)){
    unzip(mr_downloads[[i]],  exdir = file.path(PopDir, "data/opened/MR"))
    
    # locate the .dta files and load them
    link_mr <- list.files(path = file.path(PopDir, "data", "opened", "MR"), pattern = "DTA", full.names = TRUE, recursive = F)
    dhs_mr <- read_dta(link_mr[[i]])
    
    # select relevant variables and create a new variable for agricultural work responses
    df <- dhs_mr %>%
        dplyr::select(matches("mv000|mv001|mv002|mv005|mv007|mv012|mv022|mv021|mv025|mv106|mv190|mv717|v106|v701")) %>%
        mutate(agric_work_man_response = tryCatch(ifelse(mv717 %in% c(4, 5), "A", ifelse(mv717 %in% c(1, 2, 3, 6, 7, 8, 9, 11), "O", ifelse(mv717 == 0, "U", NA))), error =function(e) return(NA))) %>%
        dplyr::rename(strat=mv022, id=mv021, dhs_year = mv007, edu_woman  = mv106, age_woman = mv012,wealth = mv190) %>%
        mutate(wt=mv005/1000000)
  
    # add DHS country code, link to IDs, and other transformations
    df <- df %>%  
        mutate(DHS_CountryCode = str_sub(mv000, 1, 2)) %>%  
        left_join(ids) %>%
        mutate( min_year =min(dhs_year), code_year = paste0(DHS_CountryCode, min_year)) %>%
        mutate(agric_work_man_response2= ifelse(is.na(agric_work_man_response), "M", agric_work_man_response),
               agric_work_man_response2 = factor(agric_work_man_response2, levels = c("A", "O", "U", "M")))
  
    # filter for urban and rural datasets and append them to respective lists
    df_urban <- df %>%  filter(mv025 == 1)
    print(paste("appending urban data from", unique(df_urban[["CountryName"]]), "to list of data frames"))
    dhs_mr_urban <- append(dhs_mr_urban, list(df_urban))
    
    df_rural <- df %>%  filter(mv025 == 2)
    print(paste("appending rural data from", unique(df_urban[["CountryName"]]), "to list of data frames"))
    dhs_mr_rural <- append(dhs_mr_rural, list(df_rural))
}

# save the processed urban and rural datasets for future use
saveRDS(dhs_mr_urban, file.path(PopDir, "analysis_dat/dhs_mr_urban.rds"))
#dhs_mr_urban <- readRDS("dhs_mr_urban.rds")

saveRDS(dhs_mr_rural, file.path(PopDir, "analysis_dat/dhs_mr_rural.rds"))
#dhs_mr_rural <- readRDS("dhs_mr_rural.rds")


## =========================================================================================================================================
### Exploring data to see if it is fit for purpose
## =========================================================================================================================================

# plot to view agric data 
# dhs_mr_urban <- dhs_mr_urban %>% map(~mutate(., max_year = max(dhs_year), year_combo = ifelse(max_year == min_year, max_year, paste(min_year, "-",str_sub(max_year, -2))),
#                                              country_year = paste0(CountryName, " ", year_combo)))
# dhs_mr_rural <- dhs_mr_rural %>% map(~mutate(., max_year = max(dhs_year), min_year = min(as.numeric(dhs_year)), year_combo = ifelse(max_year == min_year, max_year, paste(min_year, "-", str_sub(max_year, -2))),
#                                              country_year = paste0(CountryName, " ", year_combo)))
# 
# plot_u_df<- dhs_mr_urban %>% map(~dplyr::select(., country_year, agric_work_man_response2, code_year)) %>%  bind_rows(.id = "column_label") 
# plot_r_df <- dhs_mr_rural %>% map(~dplyr::select(., country_year, agric_work_man_response2, code_year)) %>%  bind_rows(.id = "column_label") 
# 
# table(plot_u_df$agric_work_man_response2)

# do we have enough data for this analysis? we have 20 datasets to work with now. Reduced it from the initial 35
# xlabel = "Occupation (A - Agricultural work, O - other, U - unemployed, M - Missing)"
# color = c("#a7cece", "#e8a6bd", "#afb2cb", "#aaa3a2")
# p <- bar_fun(plot_u_df, "agric_work_man_response2" ,"agric_work_man_response2", "DHS datasets with agricultural worker data based on responses from the men's survey (urban areas)", xlabel)+
#   scale_fill_manual(values= color)+
#   facet_wrap(vars(country_year), scales="free")
# ggsave(paste0(FigDir,"/", Sys.Date(),"_men_survey_urban_DHS_datasets_agric_data.png"), p, width = 13, height = 13)
# 
# p <- bar_fun(plot_r_df, "agric_work_man_response2" ,"agric_work_man_response2","DHS datasets with agricultural worker data based on responses from the men's survey (rural areas)", xlabel)+
#   scale_fill_manual(values=color)+
#   facet_wrap(vars(country_year), scales="free")
# ggsave(paste0(FigDir,"/", Sys.Date(),"men_survey_rural_DHS_datasets_agric_data.png"), p, width = 13, height = 13)

# Change to weighted percentage to compare across countries 
# urban
# plot_u_df <- dhs_mr_urban  %>% map(~as_survey_design(., ids= id,strata=strat,nest=T,weights= wt))%>% map(~drop_na(.,agric_work_man_response2)) %>%  
#   map(~group_by(., agric_work_man_response2)) %>%  map(~summarize(., across(country_year), percent = survey_mean() *100,
#                                                   total = survey_total())) %>%  map(~distinct(., agric_work_man_response2, .keep_all = TRUE))
# plot_u_df <- plyr::ldply(plot_u_df) %>%  mutate(agric_work_man_response2 = factor(agric_work_man_response2, levels = c("M", "U", "O", "A")))
# 
# label = c("Missing","Unemployed","Other work", "Agricultural worker")
# color = c("#aaa3a2", "#afb2cb", "#e8a6bd", "#a7cece")
# p <- col_fun(plot_u_df, "country_year", "percent", "agric_work_man_response2", "Weighted Percentage (urban areas)", color, label)
# ggsave(paste0(FigDir,"/", Sys.Date(),"men_survey_urban_DHS_datasets_agric_data_weighted_percent.png"), p, width = 13, height = 13)
# 
# # rural 
# plot_r_df <- dhs_mr_rural  %>% map(~as_survey_design(., ids= id,strata=strat,nest=T,weights= wt))%>% map(~drop_na(.,agric_work_man_response2)) %>%  
#   map(~group_by(., agric_work_man_response2)) %>%  map(~summarize(., across(country_year), percent = survey_mean() *100,
#                                                   total = survey_total())) %>%  map(~distinct(., agric_work_man_response2, .keep_all = TRUE))
# plot_r_df  <- plyr::ldply(plot_r_df) %>%  mutate(agric_work_man_response2 = factor(agric_work_man_response2, levels = c("M", "U", "O", "A")))
# p <- col_fun(plot_r_df, "country_year", "percent", "agric_work_man_response2", "Weighted Percentage (rural areas)", color, label)
# ggsave(paste0(FigDir,"/", Sys.Date(),"men_survey_rural_DHS_datasets_agric_data_weighted_percent.png"), p, width = 13, height = 13)
# 


## =========================================================================================================================================
### Data Loading and Transforming PR Datasets
## =========================================================================================================================================

# download PR datasets (household malaria parasitemia data)
# retrieves datasets for the survey IDs and downloads them in the desired format ("DT" for Stata format).
pr_datasets <- dhs_datasets(surveyIds = survs$SurveyId, fileFormat = "DT", fileType = "PR") #%>%
  #group_by(CountryName) %>%
  #slice_max(SurveyYear)

# initialize an empty list to store downloaded datasets
pr_downloads <- list()

# loop through each row of the PR datasets to download the corresponding file
for (i in 1:nrow(pr_datasets)) {
    # note: there's an issue with downloading specific files (commented out code),
    # manually adding data for Madagascar 2021 if needed:
    # if (pr_datasets$FileName[i] == "MDIR81DT.ZIP") {
      #   df <- list("MDIR81FL"="project_one/datasets/MDIR81FL.ZIP") 
      #   pr_downloads <- append(pr_downloads, df)
      #   next
    # }
    
    # attempt to download the dataset and handle potential download errors
    tryCatch({
        # downloads the dataset as a zip file
        df <- get_datasets(pr_datasets$FileName[i], download_option = "zip", verbose_argument = TRUE)
        
        # append the downloaded dataset to the list
        pr_downloads <- append(pr_downloads, df)
    }, 
    # if there's an error during the download process, print the error message
    error = function(e) {
       cat("ERROR:", conditionMessage(e), "\n")
    })
}

# save the list of downloaded datasets as an RDS file for future use
saveRDS(pr_downloads, file.path(PopDir, "analysis_dat/pr_downloads.rds"))

# load the saved RDS file (list of downloaded datasets)
pr_downloads<- readRDS(file.path(PopDir, "analysis_dat/pr_downloads.rds"))

## =========================================================================================================================================
### Data Loading and Transforming MR Datasets
## =========================================================================================================================================

# identify survey IDs that are present in the 'survs' dataset but missing from the PR datasets
# this helps ensure that all required MR datasets are downloaded later
missing_mr_surveys <- data.frame(SurveyId = setdiff(survs$SurveyId, pr_datasets$SurveyId))

# initialize empty lists for storing processed urban and rural PR data
dhs_pr_urban <- list()
dhs_pr_rural <- list()

# loop through the downloaded PR datasets and unzip each file
for (i in 1:length(pr_downloads)) {
    # unzips each downloaded file into the "opened/PR" folder within the specified directory
    unzip(pr_downloads[[i]], exdir = file.path(PopDir, "data/opened/PR"))
}

# create a list of paths to the unzipped PR datasets (in .DTA format) for further processing
# 'link_pr' will store the file paths of the unzipped .DTA files
link_pr <- list.files(
    path = file.path(PopDir, "data", "opened", "PR"),   # specify the directory where files were unzipped
    pattern = "DTA",                                    # match only .DTA files (Stata format)
    full.names = TRUE,                                  # return the full path to each file
    recursive = FALSE                                   # search only in the specified directory (non-recursive)
)


## -----------------------------------------------------------------------------------------------------------------------------------------
### Process PR Datasets
## -----------------------------------------------------------------------------------------------------------------------------------------

# loop through each link in 'link_pr' to process datasets
for (i in 1:length(link_pr)) {
  
    # read in DHS PR dataset for the current country
    dhs_pr <- read_dta(link_pr[[i]])
    
    # select relevant columns for analysis (e.g., demographic, household, malaria data)
    df <- dhs_pr %>%
      dplyr::select(matches("hc1|hc60|hml12|hml16a|hml32|hml35|hv000|hv001|hv002|hv003|hv005|hv006|hv007|hv009|hv021|hv025|hv022|hv042|hv103|hv213|hv214|hv215|hv253|hv270|hvidx|sh418|sh511|v106|v701"))
    
    # special case for Cameroon 2011 (identified by unique DHS year and country code)
    if (paste0(unique(df[["hv000"]]), unique(df[["hv007"]]))[1] == "CM62011") {
      
        # create malaria test result based on 'sh418' variable for Cameroon
        df <- df %>% mutate(test_result = ifelse(sh418 %in% c(1, 2, 3), "+ve", ifelse(sh418 == 4, "-ve", NA)))
      
    # special case for Tanzania 2007 (identified by unique DHS year and country code)
    } else if (paste0(unique(df[["hv000"]]), unique(df[["hv007"]]))[1] == "TZ52007") {
        
        # create malaria test result based on 'sh511' variable for Tanzania
        df <- df %>% mutate(test_result = ifelse(sh511 == 1, "+ve", ifelse(sh511 == 0, "-ve", NA)))
      
    } else {
      
        # general case for other countries: process malaria test results using 'hml32' or 'hml35'
        df <- df %>%
            mutate(
              hml32 = tryCatch(ifelse(hml32 > 1, NA, hml32), error = function(e) return(NA)),  # handle invalid 'hml32' values
              hml35 = tryCatch(ifelse(hml35 > 1, NA, hml35), error = function(e) return(NA)),  # handle invalid 'hml35' values
              test_result = tryCatch(ifelse(!is.na(hml32), hml32, hml35), error = function(e) return(NA)),  # use 'hml32' if available, else 'hml35'
              test_result = ifelse(test_result == 1, "+ve", ifelse(test_result == 0, "-ve", NA)),  # convert test results to +ve/-ve format
              test_result = as.character(test_result)  # ensure 'test_result' is a character variable
            )
    }
    
    #child_age =tryCatch(ifelse(!is.na(hc1), hc1, hml16a), error = function(e) return(NA)),
    
    # additional variables and transformations for the dataset
    df <- df %>%
        mutate(
            # calculate net use for under-5 children
            u5_net_use = tryCatch(ifelse(hml12 %in% c(1, 2), 1, 0), error = function(e) return(NA)),
            
            # determine household characteristics (floor, wall, roof type)
            floor_type = ifelse(hv213 %in% c(30, 31, 33, 34, 35), 1, 0),
            wall_type = ifelse(hv214 %in% c(30, 31, 33, 34), 0, 1),
            roof_type = ifelse(hv215 %in% c(30, 31, 33, 34, 35, 36), 1, 0),
            
            # other variables: wealth index, interview month, household weight
            wealth = hv270,
            interview_month = hv006,
            wt = hv005 / 1000000
        ) %>%
            dplyr::rename(
              strat = hv022,        # survey stratification variable
              id = hv021,           # cluster or PSU ID
              dhs_year = hv007,     # year of the DHS survey
              hh_size = hv009       # household size
        )
    
    # create country code and append year for analysis
    df <- df %>%
      mutate(DHS_CountryCode = str_sub(hv000, 1, 2)) %>%
      left_join(ids) %>%
      mutate(min_year = min(dhs_year), code_year = paste0(DHS_CountryCode, min_year))
    
    # filter for urban children aged 6-59 months who were tested for malaria
    df_urban <- df %>%
      filter(hv042 == 1, hv103 == 1, hc1 %in% 6:59, hv025 == 1)  # hv025 == 1 indicates urban, hc1 is age in months
    
    # print message indicating that urban data for the country is being appended
    print(paste("Appending urban data from", unique(df_urban[["CountryName"]]), "to list of data frames"))
    
    # append filtered urban data to the list 'dhs_pr_urban'
    dhs_pr_urban <- append(dhs_pr_urban, list(df_urban))
    
    # filter for rural children aged 6-59 months who were tested for malaria
    df_rural <- df %>%
      filter(hv042 == 1, hv103 == 1, hc1 %in% 6:59, hv025 == 2)  # hv025 == 2 indicates rural, hc1 is age in months
    
    # print message indicating that rural data for the country is being appended
    print(paste("Appending rural data from", unique(df_urban[["CountryName"]]), "to list of data frames"))
    
    # append filtered rural data to the list 'dhs_pr_rural'
    dhs_pr_rural <- append(dhs_pr_rural, list(df_rural))
}

# summarize hv253 variable (household owns a mosquito net) for each urban dataset
lapply(dhs_pr_urban, function(x) table(x$hv253))

# summarize hv253 variable (household owns a mosquito net) for each rural dataset
lapply(dhs_pr_rural, function(x) table(x$hv253))

# filter list to keep only datasets with non-missing malaria test results
dhs_pr_urban1 <- dhs_pr_urban %>%purrr::discard(~all(is.na(.x$test_result)))
dhs_pr_rural1 <- dhs_pr_rural %>%purrr::discard(~all(is.na(.x$test_result)))

# save the filtered urban datasets
saveRDS(dhs_pr_urban1, file.path(PopDir, "analysis_dat/dhs_pr_urban.rds"))

# read back the saved urban datasets
dhs_pr_urban<-readRDS(file.path(PopDir, "analysis_dat/dhs_pr_urban.rds"))

# save the filtered rural datasets
saveRDS(dhs_pr_rural1, file.path(PopDir, "analysis_dat/dhs_pr_rural.rds"))

# read back the saved rural datasets
dhs_pr_rural<-readRDS(file.path(PopDir, "analysis_dat/dhs_pr_rural.rds"))


## =========================================================================================================================================
### Data Exploration
## =========================================================================================================================================

# add max year, year combo (e.g. 2015 - 16), and country-year info (e.g. Angola 2015 - 16) to each urban dataset
# plot to view urban malaria data 
dhs_pr_urban <- dhs_pr_urban1 %>%
  map(~mutate(., 
              max_year = max(dhs_year), 
              year_combo = ifelse(max_year == min_year, max_year, paste(min_year, "-", str_sub(max_year, -2))), 
              country_year = paste0(CountryName, " ", year_combo)))


# add max year, min year, year combo, and country-year info to each rural dataset
# plot to view rural malaria data 
dhs_pr_rural <- dhs_pr_rural1 %>%
  map(~mutate(., 
              max_year = max(dhs_year), 
              min_year = min(as.numeric(dhs_year)), 
              year_combo = ifelse(max_year == min_year, max_year, paste(min_year, "-", str_sub(max_year, -2))), 
              country_year = paste0(CountryName, " ", year_combo)))

# select relevant columns for urban datasets and bind them into a single dataframe
plot_u_df <- dhs_pr_urban %>%
  map(~dplyr::select(., country_year, test_result, code_year)) %>%
  bind_rows(.id = "column_label")


# select relevant columns for rural datasets and bind them into a single dataframe
plot_r_df <- dhs_pr_rural %>%
  map(~dplyr::select(., country_year, test_result, code_year)) %>%
  bind_rows(.id = "column_label")


# # do we have enough data for this analysis?
# label = "malaria_test results"
# color = c("darkslategray2", "deeppink3", "#aaa3a2") #"#967cb9"
# 
# # plot: URBAN DHS datasets with malaria test results among 6 - 59 years
# p <- bar_fun(plot_u_df, "test_result" , "test_result", "DHS datasets with malaria test results among 6 - 59 years (urban areas)", label)+
#    scale_fill_manual(values= color)+
#    xlab("Malaria test results by Microscopy or RDT")+
#    facet_wrap(vars(country_year), scales="free")
# p
# ggsave(paste0(FigDir,"/", Sys.Date(),"_urban_DHS_datasets_malaria_test_data.png"), p, width = 13, height = 13)
# 
# # plot: RURAL DHS datasets with malaria test results among 6 - 59 years
# p <- bar_fun(plot_r_df, "test_result" , "test_result",  "DHS datasets with malaria test results among 6 - 59 years (rural areas)", label)+
#  scale_fill_manual(values= color)+
#  xlab("Malaria test results by Microscopy or RDT")+
#  facet_wrap(vars(country_year), scales="free")
# p
# ggsave(paste0(FigDir,"/", Sys.Date(),"_rural_DHS_datasets_malaria_test_data.png"), p, width = 13, height = 13)


# change to survey weighted percentage to compare across countries
plot_u_df <- dhs_pr_urban %>%
  # convert each urban dataset to a survey design object
  map(~as_survey_design(., ids = id, strata = strat, nest = TRUE, weights = wt)) %>%

  # mutate test_result to handle missing values
  map(~mutate(., test_result = ifelse(is.na(test_result), "missing", test_result))) %>%

  # group by test_result for aggregation
  map(~group_by(., test_result)) %>%

  browser()

  # summarize to calculate weighted percentages and totals
  map(~summarize(.,
                 across(c(country_year),
                        percent = survey_mean() * 100,
                        total = survey_total()))) %>%

  # keep distinct test results while preserving other columns
  map(~distinct(., test_result, .keep_all = TRUE))

# edit to above commented out code so it runs: took survey_mean and survey_total out of across() function
plot_u_df <- dhs_pr_urban %>%
  # convert each urban dataset to a survey design object
  map(~as_survey_design(., ids = id, strata = strat, nest = TRUE, weights = wt)) %>%

  # mutate test_result to handle missing values
  map(~mutate(., test_result = ifelse(is.na(test_result), "missing", test_result))) %>%

  # group by test_result for aggregation
  map(~group_by(., test_result)) %>%

  # summarize to calculate weighted percentages and totals
  map(~summarize(.,
                 percent = survey_mean(vartype = ("ci")) * 100,    # calculate weighted percentage
                 total = survey_total())) %>%      # calculate weighted total

  # keep distinct test results while preserving other columns
  map(~distinct(., test_result, .keep_all = TRUE))

# bind the results into a single dataframe, set factor levels for test_result, and rename CI columns for clarity
plot_u_df <- plyr::ldply(plot_u_df) %>%
  mutate(test_result = factor(test_result, levels = c("missing", "-ve", "+ve"))) %>%
  rename(lower_ci = percent_low, upper_ci = percent_upp)

# label = c("Missing","Negative test","Positive test")
# color = c("#aaa3a2",  "darkslategray2", "deeppink3")
# p <- col_fun(plot_u_df, "country_year", "percent", "test_result", "Weighted Percentage (urban areas)", color, label)
# ggsave(paste0(FigDir,"/", Sys.Date(),"_urban_DHS_datasets_malaria_data_weighted_percent.png"), p, width = 13, height = 13)

# plot_r_df <- dhs_pr_rural  %>% map(~as_survey_design(., ids= id,strata=strat,nest=T,weights= wt))%>% map(~mutate(., test_result= ifelse(is.na(test_result), "missing", test_result))) %>%  
#   map(~group_by(., test_result)) %>%  map(~summarize(., across(c(country_year, code_year)), percent = survey_mean() *100,
#                                                      total = survey_total())) %>%  map(~distinct(., test_result, .keep_all = TRUE))
# plot_r_df <- plyr::ldply(plot_r_df) %>%mutate(test_result = factor(test_result, levels = c("missing", "-ve", "+ve")))
# #label = c("Missing","Negative test","Positive test")
# #color = c("#aaa3a2",  "#967cb9", "#f04a4c")
# p <- col_fun(plot_r_df, "country_year", "percent", "test_result", "Weighted Percentage (rural areas)", color, label)
# ggsave(paste0(FigDir,"/", Sys.Date(),"_rural_DHS_datasets_malaria_data_weighted_percent.png"), p, width = 13, height = 13)


## =========================================================================================================================================
### Analysis of Recent Malaria Survey Trends by Country
## =========================================================================================================================================

# get countries with recent survey positives greater than 1%
df_recent <- read.csv(file.path(PopDir, "analysis_dat/final_surveys.csv")) %>%
  # extract country ID from the code_year variable
  mutate(cntryId = stringr::str_extract(code_year, "^.{2}"))

# process plot_u_df to analyze trends
df_trend <- plot_u_df %>%
  mutate(cntryId = stringr::str_extract(code_year, "^.{2}")) %>% # extract country ID from the code_year variable
  mutate(year = parse_number(code_year)) %>% # parse the year from code_year
  filter(year > 2009) %>% # filter for years greater than 2009
  group_by(cntryId) %>% # group by country ID and select the most recent 4 years
  slice_max(year, n = 4) %>%
  ungroup() %>%
  select(code_year) %>% # select only the unique code_year values
  distinct() %>%
  mutate(cntryId = stringr::str_extract(code_year, "^.{2}")) %>% # extract country ID again for filtering
  filter(cntryId %in% c(df_recent$cntryId)) # filter for countries present in the recent survey data
  
# create a subset of df_trend for countries with multiple recent entries
df <- subset(df_trend,duplicated(cntryId) | duplicated(cntryId, fromLast=TRUE))

# write the final trend data to a CSV file
write_csv(df, file.path(PopDir, "analysis_dat/final_surveys_trend.csv"))


## =========================================================================================================================================
### Create Analysis Datasets
## =========================================================================================================================================

# filter urban and rural IR datasets to only include records with matching code_years
ir_urban <- dhs_ir_urban %>%purrr::keep(~all(.x$code_year %in% df$code_year))
ir_rural <- dhs_ir_rural %>%purrr::keep(~all(.x$code_year %in% df$code_year))

# filter urban and rural MR datasets to only include records with matching code_years
mr_urban <- dhs_mr_urban %>%purrr::keep(~all(.x$code_year %in% df$code_year))
mr_rural <- dhs_mr_rural %>%purrr::keep(~all(.x$code_year %in% df$code_year))

# filter urban and rural PR datasets to only include records with matching code_years
pr_urban <- dhs_pr_urban %>%purrr::keep(~all(.x$code_year %in% df$code_year))
pr_rural <- dhs_pr_rural %>%purrr::keep(~all(.x$code_year %in% df$code_year))

# combine all urban IR datasets by selecting columns that match the smallest column length
ir_urban %>% map(~dim(.x)[[2]]) # check column dimensions to identify smallest column length
ir_urban <- ir_urban %>% map(~dplyr::select(., colnames(ir_urban[[13]]))) # select consistent columns
ir_urban <- plyr::ldply(ir_urban) # bind all urban IR datasets into one data frame

# combine all rural IR datasets
ir_rural %>% map(~dim(.x)[[2]]) # check column dimensions - get smallest column length in list and position
ir_rural <- ir_rural %>% map(~dplyr::select(., colnames(ir_rural[[13]]))) # select consistent columns
ir_rural <- plyr::ldply(ir_rural) # bind all rural IR datasets into one data frame

# combine all urban MR datasets
mr_urban %>% map(~dim(.x)[[2]]) # check column dimensions - get smallest column length in list and position
mr_urban <- mr_urban %>% map(~dplyr::select(., colnames(mr_urban[[5]]))) # select consistent columns
mr_urban <- plyr::ldply(mr_urban) # bind all urban MR datasets into one data frame

# combine all rural MR datasets
mr_rural %>% map(~dim(.x)[[2]]) # check column dimensions - get smallest column length in list and position
mr_rural <- mr_rural %>% map(~dplyr::select(., colnames(mr_rural[[5]]))) # select consistent columns
mr_rural <- plyr::ldply(mr_rural) # bind all rural MR datasets into one data frame

# combine all urban PR datasets
pr_urban %>% map(~dim(.x)[[2]]) # check column dimensions - get smallest column length in list and position
pr_urban <- pr_urban %>% map_if(~all(c('sh511') %in% colnames(.x)), ~dplyr::select(., -sh511)) # drop 'sh511' if present
#pr_urban <- pr_urban %>% map(~dplyr::select(., colnames(pr_urban[[13]]) %>% discard(~ .x %in% c("hc11", "hc10", "hc12")))) # select consistent columns, excluding specific ones
pr_urban <- plyr::ldply(pr_urban) # bind all urban PR datasets into one data frame

# combine all rural PR datasets
pr_rural %>% map(~dim(.x)[[2]]) # check column dimensions - get smallest column length in list and position
pr_rural <- pr_rural %>% map_if(~all(c('sh511') %in% colnames(.x)), ~dplyr::select(., -sh511)) # drop 'sh511' if present
#pr_rural <- pr_rural %>% map(~dplyr::select(., colnames(pr_rural[[13]]) %>% discard(~ .x %in% c("hc11", "hc10", "hc12")))) # select consistent columns, excluding specific ones
pr_rural <- plyr::ldply(pr_rural) # bind all rural PR datasets into one data frame

# save the combined datasets for future analysis
saveRDS(ir_rural, file.path(PopDir, "analysis_dat/ir_rural.rds"))
ir_rural <- readRDS(file.path(PopDir,"analysis_dat/ir_rural.rds"))

saveRDS(mr_urban, file.path(PopDir, "analysis_dat/mr_urban.rds"))
mr_urban <- readRDS(file.path(PopDir,"analysis_dat/mr_urban.rds"))

saveRDS(mr_rural, file.path(PopDir, "analysis_dat/mr_rural.rds"))
mr_rural <- readRDS(file.path(PopDir,"analysis_dat/mr_rural.rds"))

saveRDS(pr_urban, file.path(PopDir, "analysis_dat/pr_urban.rds"))
pr_urban <- readRDS(file.path(PopDir,"analysis_dat/pr_urban.rds"))

saveRDS(pr_rural, file.path(PopDir, "analysis_dat/pr_rural.rds"))
pr_rural <- readRDS(file.path(PopDir,"analysis_dat/pr_rural.rds"))




###################################################################combine data##################################################
#combine ir  and mr(at hh level) and pr datasets 
#some households in the ir dataset is not present in the pr dataset and I am not sure why 


## =========================================================================================================================================
### Urban Data Processing and Analysis
## =========================================================================================================================================

## -----------------------------------------------------------------------------------------------------------------------------------------
### 1. Combine Urban Data: IR and MR Data Processing
## -----------------------------------------------------------------------------------------------------------------------------------------

# summarize MR data for urban households by grouping and counting the number of men
hh_mr_urban <- mr_urban %>%  
  group_by(code_year, mv001, mv002) %>%  
  summarise(agric_man = paste0(agric_work_man_response2, collapse = ""), n_men = n())

# merge IR data with summarized MR data, creating a new variable for agricultural partners
ir_mr_urban <- ir_urban %>%    
  left_join(hh_mr_urban, by = c("code_year", "v001" = "mv001", "v002" = "mv002")) %>%  
  mutate(agric_partner_other = ifelse(is.na(agri_partner), agric_man, agri_partner)) %>%  
  
  # unite agricultural partner and woman’s occupation into a new column
  unite(col = "HH_occ2", agric_partner_other, agri_woman, na.rm = TRUE, sep = "", remove = FALSE) %>%  
  
  # replace empty values in HH_occ2 with "M" for missing
  mutate(HH_occ3 = ifelse(HH_occ2 == "", "M", HH_occ2)) %>%  
  
  # determine occupation values based on patterns in HH_occ3
  mutate(occ_val3 = ifelse(grepl("A", HH_occ3), "A", HH_occ3)) %>%  
  mutate(occ_val4 = ifelse(grepl("O", occ_val3), "O", occ_val3)) %>%  
  mutate(occ_val5 = ifelse(grepl("U", occ_val4), "U", occ_val4)) 

# final adjustments for occupation values
ir_mr_urban <- ir_mr_urban %>%  
  mutate(occ_val6 = ifelse(is.na(occ_val5), "M", occ_val5), # fill NAs with "M"
         occ_val7 = ifelse(grepl("M", occ_val6), "M", occ_val6)) %>%  # mark as "M" if "M" is present
  mutate(occ_val7 = factor(occ_val7, levels = c("A", "O", "U", "M")), # convert to factor with specified levels
         wt = v005 / 1000000) # weight calculation

#pick up diarrheal disease values 
# ir_mr_urban <- ir_mr_urban %>%  dplyr::mutate(across(c(h11_1:h11_6), ~ dplyr::case_when(. %in% c("2") ~ "1",
#                                                                                          . %in% c("8", "9") ~ NA,
#                                                                                          TRUE ~ as.character(.)), .names = "rec_{col}")) %>%
#   mutate(across(starts_with("rec"), ~as.numeric(.))) %>%
#   mutate(sum_dia = rowSums(select(., contains("rec_")), na.rm =TRUE))

# plot to visualize urban agricultural data and save the dataset  
dhs_ir_mr_urban <- ir_mr_urban %>% 
  group_by(code_year) %>% # group data by year and calculate the maximum year
  mutate(max_year = max(dhs_year)) %>% 
  ungroup() %>%  # ungroup the data for further operations
  mutate(
    # create a year combo string based on min and max years
    year_combo = ifelse(max_year == min_year, max_year, 
                        paste(min_year, "-", str_sub(max_year, -2))),
    # create a combined string of country and year
    country_year = paste0(CountryName, " ", year_combo)
  )

# select relevant columns for plotting
plot_u_df <- dhs_ir_mr_urban %>% 
  dplyr::select(country_year, occ_val7, code_year)

# define x-axis label for the plot
xlabel = "Occupation (A - Agricultural work, O - other, U - unemployed, M - Missing)"

# define color palette for the plot
color = c("#a7cece", "#e8a6bd", "#afb2cb", "#aaa3a2")

# create the bar plot using the defined function (see bar_fun function in functions_employment.R script)
p <- bar_fun(
  plot_u_df, 
  "occ_val7", 
  "occ_val7", 
  "DHS datasets with agricultural worker data based on responses from the women and men's survey (urban areas)", 
  xlabel
) +
  scale_fill_manual(values = color) + # customize the fill colors for the plot
  facet_wrap(vars(country_year), scales = "free") # facet the plot by country_year with free scales
p
# save the plot to a PDF file with the current date in the filename
ggsave(paste0(FigDir,"/", Sys.Date(),"_women_men_survey_urban_DHS_datasets_agric_data.pdf"), p, width = 8.5, height = 8.5)


## -----------------------------------------------------------------------------------------------------------------------------------------
### 2. Create Survey Weighted Plots (Urban)
## -----------------------------------------------------------------------------------------------------------------------------------------

# create survey design object for urban data
plot_u_df <- dhs_ir_mr_urban %>% 
  as_survey_design(ids = id, strata = strat, nest = TRUE, weights = wt) %>% 
  # group by country and occupation value, summarizing percentages and totals
  group_by(country_year, occ_val7) %>% 
  summarize(
    percent = survey_mean() * 100,  # calculate weighted percentage
    total = survey_total()          # calculate total observations
  )

# convert occ_val7 to a factor with specified levels for proper ordering
plot_u_df <- plot_u_df %>% 
  mutate(occ_val7 = factor(occ_val7, levels = c("M", "U", "O", "A")))

# define labels for plot legend
label = c("Missing","Unemployed","Other work", "Agricultural worker")

# define color palette for the plot
color = c("#aaa3a2", "#afb2cb", "#e8a6bd", "#a7cece")

# create the plot using the custom column function (see col_fun function in functions_employment.R script)
p <- col_fun(plot_u_df, 
             "country_year", 
             "percent", 
             "occ_val7", 
             "Weighted Percentage (urban areas)", 
             color, 
             label)

# display the plot
p

# save the plot to a PDF file with the current date in the filename
ggsave(paste0(FigDir,"/", Sys.Date(),"_women_men_survey_urban_DHS_datasets_agric_data_weighted_percent.pdf"), p, width = 5.5, height = 3)

## -----------------------------------------------------------------------------------------------------------------------------------------
### 3. Household Exposure Data for Urban Areas
## -----------------------------------------------------------------------------------------------------------------------------------------

# create household exposure data for urban areas
hh_ir_urban <- ir_mr_urban %>%
  group_by(code_year) %>%  # group data by code_year to determine the maximum year for each group
  mutate(max_year = max(dhs_year)) %>%  # calculate maximum year in the group
  ungroup() %>%  # remove grouping
  
  # create year_combo and country_year identifiers
  mutate(
    year_combo = ifelse(max_year == min_year, max_year, paste(min_year, "-", str_sub(max_year, -2))),
    country_year = paste0(CountryName, " ", year_combo)
  ) %>%  
  
  # group by country and household identifiers
  group_by(country_year, code_year, v001, v002) %>%  
  summarise(
    n = n(),  # count the number of entries in each group
    home_type = paste0(occ_val7, collapse = "")  # combine occupation values into a single string
  ) %>%     #, total_diarrhea = sum(sum_dia, na.rm =TRUE)) %>% 
  
  # classify home_type into "A" (Agricultural work) or "O" (Other work)
  mutate(home_type2 = ifelse(grepl("A", home_type), "A", "O")) %>%  
  drop_na(home_type2) %>%  # remove rows with NA in home_type2
  ungroup()  # remove grouping again

# select relevant columns for plotting
plot_u_df <- hh_ir_urban %>% 
  dplyr::select(country_year, home_type2, code_year)

# define the x-axis label for the plot
xlabel = "Occupation (A - Agricultural work, O - other occupation, unemployed or occupation is missing)"

# define colors for the plot
color = c("#a7cece", "#e8a6bd")

# create a bar plot to visualize household exposure to agricultural occupation (see bar_fun custom function in functions_employment.R script)
p <- bar_fun(
  plot_u_df,  # data for the plot
  "home_type2",  # variable for x-axis
  "home_type2",  # variable for fill color
  "Household exposure to agricultural occupation based on responses from the women and men's survey (urban areas). Households labeled as A have at least one adult woman or man engaged in agricultural work",  # plot title
  xlabel  # x-axis label
) + 
  scale_fill_manual(values = color) +  # apply custom colors to the fill
  facet_wrap(vars(country_year), scales = "free")  # create separate plots for each country
p

# save the plot as a PDF file
ggsave(paste0(FigDir,"/", Sys.Date(),"agric_HH_exposure_women_men_survey_urban.pdf"), p, width = 6.8, height = 5.5)

## -----------------------------------------------------------------------------------------------------------------------------------------
### 4. Join PR Dataset with Household IR Dataset
## -----------------------------------------------------------------------------------------------------------------------------------------

# join the PR dataset with the household IR dataset
# note: the PR dataset (pr_urban) has 29,394 observations, and the household IR dataset (hh_ir_urban) has 64,727 observations
# after filtering out missing test results and home type data, the joined dataset contains 26,843 observations
urban_df <- left_join(pr_urban, hh_ir_urban, by = c("code_year", "hv001" = "v001", "hv002" = "v002")) %>%
  filter(!is.na(test_result)) %>%  # filter out rows with missing test results
  filter(!is.na(home_type2))  # filter out rows with missing home type information


# create a data frame for plotting the joined data by selecting relevant columns
plot_u_df <- urban_df %>% 
  dplyr::select(country_year.x, home_type2, code_year, test_result) 

# define the x-axis label for the plot
xlabel = "Occupation (A - Agricultural work, O - other occupation, unemployed or occupation is missing)"

# define colors for the plot
color = c("#a7cece", "#e8a6bd")

# create a bar plot to visualize the agricultural occupation based on the joined data
p <- bar_fun(
  plot_u_df,  # data for the plot (relevant columns selected)
  "home_type2",  # variable for x-axis
  "home_type",  # variable for fill color
  "Occupation based on responses from the women and men's survey (urban areas) after combining with the DHS PR dataset",  # plot title
  xlabel  # x-axis label
) + 
  scale_fill_manual(values = color) +  # apply custom colors to the fill
  facet_wrap(vars(country_year.x), scales = "free")  # create separate plots for each country
p

# save the plot as a PDF file
ggsave(paste0(FigDir,"/", Sys.Date(),"agric_HH_exposure_joined_pr_data_women_men_survey_urban.pdf"), p, width = 6.8, height = 5.5)

#### PLOT THE MALARIA VARIABLES FOR ALL DATA (FIGURE 2) ####
# this will be combined with rural area data below

# define colors for the plot
color = c("#f2a5a1", "#c55c80")

# summarize data for overall malaria test results by home type
plot_overall <- plot_u_df %>%  
  group_by(home_type2, test_result) %>%  # group by home type and test result
  summarise(value = n()) %>%  # count number of occurrences
  mutate(percent = round(value / sum(value) * 100, 0))  # calculate percentage of total

# assign title for the urban data
plot_overall$title = "Urban"

# create a bar plot for urban malaria test results
p_urban <- ggplot(plot_overall, aes(fill = test_result, x = home_type2)) + 
  geom_bar(aes(y = value), position = "stack", stat = "identity") +  # stacked bar plot
  theme_manuscript() +  # apply custom theme for manuscripts (see custom function in functions_employment.R script)
  scale_x_discrete(labels = c("Agricultural worker \n household (HH)", "Non-agricultural \n worker HH")) +  # customize x-axis labels
  scale_fill_manual(name = "Malaria test result", labels = c("Negative", "Positive"), values = color) +  # custom fill colors and labels
  geom_text(aes(label = paste0(value, " ", "(", percent, "%", ")"), y = value),  # add labels to bars
            position = position_stack(vjust = 0.5),  # position labels in the middle of the stacked bars
            color = "white") +  # label color
  labs(x = "", y = "Number of children, 6 - 59 months,\ntested positive for malaria in 22 DHS datasets") +  # axis labels
  facet_wrap(vars(title)) +  # create separate plots for each title
  theme(strip.text.x = element_text(size = 12, color = "black")) +  # customize facet label appearance
  coord_cartesian(ylim = c(0, 44000))  # set y-axis limits
p_urban

# plot by country (figure for supplement)
# summarize data for malaria test results by country and home type
plot_country <- plot_u_df %>%  
  group_by(country_year.x, home_type2, test_result) %>%  # group by country, home type, and test result
  summarise(value = n()) %>%  # count number of occurrences
  mutate(percent = round(value / sum(value) * 100, 0)) %>%  # calculate percentage of total
  mutate(country_year.x = ifelse(country_year.x == "Congo Democratic Republic 2013 - 14", "DRC 2013 - 14", country_year.x),  # rename country for consistency
         home_type2 = ifelse(home_type2 == 'A', 'Agricultural worker Household (HH)', "Non-Agricultural worker HH"))

# create a bar plot for malaria test results by country
p <- ggplot(plot_country, aes(x = reorder(country_year.x, -percent), y = percent, fill = test_result)) + 
  geom_bar(stat = "identity", position = "stack", width = 0.7) +  # stacked bar plot
  coord_flip() +  # flip coordinates for better readability
  scale_fill_manual(name = "Malaria test result", labels = c("Negative", "Positive"), values = color) +  # custom fill colors and labels
  facet_grid(. ~ home_type2, scales = "free", space = "free") +  # create separate panels for each home type
  theme(legend.title = element_blank()) +  # remove legend title for clarity
  labs(x = "", y = "Percentage of children, 6 - 59 months\n tested for malaria in urban clusters per country") +  # axis labels
  theme_manuscript() +  # apply custom theme for manuscripts
  theme(legend.position = "bottom")  # position legend at the bottom

# save the plot as a PDF file and data as a .csv file
ggsave(paste0(FigDir,"/", Sys.Date(),"malaria_prevalence_by agric_exposure_urban_by_country.pdf"), p, width = 8.5, height = 6) 
write_csv(urban_df, file.path(PopDir, "analysis_dat/urban_df_for_analysis_trend.csv"))

# p<-ggplot(plot_country, aes(fill=test_result, x= home_type2)) + 
#   geom_bar(aes(y = value), position="stack", stat = "identity")+
#   theme_manuscript(theme(axis.text.x = element_text(size = 8), 
#                          axis.text.y = element_text(size = 8),
#                          axis.title.x = element_text(size = 8),
#                          axis.title.y = element_text(size = 8)))+
#   scale_x_discrete(labels = c("Agric worker \n HH", "Non-agric \n worker HH"))+
#   scale_fill_manual(name = "Malaria test result", labels= c("Negative", "positive"), values= color)+
#   geom_text(aes(label = paste0(value, " ", "(", percent, "%", ")"), y = value),
#             position = position_stack(vjust = 0.5),
#             color = "white", size =8) +
#   labs(x = "", y  = "Number of children, 6 - 59 months, tested positive for malaria 
#        in 22 DHS datasets") +
#   facet_wrap(vars(country_year.x), scales="free") +
#   theme(legend.position = 'bottom')
  

## -----------------------------------------------------------------------------------------------------------------------------------------
### 5. Create Survey Weighted Plots (Rural)
## -----------------------------------------------------------------------------------------------------------------------------------------

# calculate household malaria rates for rural areas
hh_mr_rural <- mr_rural %>%
  group_by(code_year, mv001, mv002) %>%
  summarise(
    # concatenate agricultural work responses for men
    agric_man = paste0(agric_work_man_response2, collapse = ""),
    n_men = n()  # count the number of men
  )

# join individual responses with household data and create new variables
ir_mr_rural <- ir_rural %>%
  left_join(hh_mr_rural, by = c("code_year", "v001" = "mv001", "v002" = "mv002")) %>%
  mutate(
    # use household agricultural work data if individual data is missing
    agric_partner_other = ifelse(is.na(agri_partner), agric_man, agri_partner)
  ) %>%
  unite(col = "HH_occ2", agric_partner_other, agri_woman, na.rm = TRUE, sep = "", remove = FALSE) %>%
  mutate(
    # assign "M" if HH_occ2 is empty
    HH_occ3 = ifelse(HH_occ2 == "", "M", HH_occ2),
    # create occupation value indicators based on agricultural work
    occ_val3 = ifelse(grepl("A", HH_occ3), "A", HH_occ3),
    occ_val4 = ifelse(grepl("O", occ_val3), "O", occ_val3),
    occ_val5 = ifelse(grepl("U", occ_val4), "U", occ_val4)
  )

# finalize occupation values and create weighted variable
ir_mr_rural <- ir_mr_rural %>%
  mutate(
    occ_val6 = ifelse(is.na(occ_val5), "M", occ_val5),  # assign "M" if occ_val5 is NA
    occ_val7 = ifelse(grepl("M", occ_val6), "M", occ_val6)  # assign "M" if occ_val6 contains "M"
  ) %>%
  mutate(
    occ_val7 = factor(occ_val7, levels = c("A", "O", "U", "M")),  # set factor levels for occupation values
    wt = v005 / 1000000  # calculate weights
  )

#pick up diarrheal disease values 
# ir_mr_rural <- ir_mr_rural %>%  dplyr::mutate(across(c(h11_1:h11_6), ~ dplyr::case_when(. %in% c("2") ~ "1",
#                                                                                         . %in% c("8", "9") ~ NA,
#                                                                                         TRUE ~ as.character(.)), .names = "rec_{col}")) %>%
#   mutate(across(starts_with("rec"), ~as.numeric(.))) %>%
#   mutate(sum_dia = rowSums(select(., contains("rec_")), na.rm =TRUE))

# plot to view rural agricultural data and save dataset
dhs_ir_mr_rural <- ir_mr_rural %>%
  group_by(code_year) %>%
  mutate(max_year = max(dhs_year)) %>% # calculate the maximum year for each code_year
  ungroup() %>%
  mutate( 
    year_combo = ifelse(max_year == min_year, # create a year_combo string based on min and max years
                        max_year, 
                        paste(min_year, "-", str_sub(max_year, -2))),
    country_year = paste0(CountryName, " ", year_combo) # combine country name with year information
  )

# select relevant columns for plotting
plot_u_df <- dhs_ir_mr_rural %>%
  dplyr::select(country_year, occ_val7, code_year)

# define labels and colors for the plot
xlabel = "Occupation (A - Agricultural work, O - other, U - unemployed, M - Missing)"
color = c("#a7cece", "#e8a6bd", "#afb2cb", "#aaa3a2")

# create the bar plot using the defined function (see functions_employment.R script)
p <- bar_fun(
  plot_u_df, 
  "occ_val7", 
  "occ_val7", 
  "DHS datasets with agricultural worker data based on responses from the women and men's survey (rural areas)", 
  xlabel
) + 
  scale_fill_manual(values = color) + 
  facet_wrap(vars(country_year), scales = "free")
p

# save the plot to a PDF file
ggsave(paste0(FigDir,"/", Sys.Date(),"_women_men_survey_rural_DHS_datasets_agric_data.pdf"), p, width = 6.8, height = 5.5)


## -----------------------------------------------------------------------------------------------------------------------------------------
### 6. Create Survey Weighted Plots (Rural)
## -----------------------------------------------------------------------------------------------------------------------------------------

# rural data processing and plotting
plot_u_df <- dhs_ir_mr_rural %>%
  # create a survey design object using specified ids, strata, and weights
  as_survey_design(ids = id, strata = strat, nest = TRUE, weights = wt) %>%
  # group by country year and occupation value, then summarize the survey means and totals
  group_by(country_year, occ_val7) %>%
  summarize(
    percent = survey_mean() * 100,  # calculate the weighted percentage
    total = survey_total()            # calculate the total number of observations
  )

# convert occ_val7 to a factor with specified levels
plot_u_df <- plot_u_df %>%
  mutate(occ_val7 = factor(occ_val7, levels = c("M", "U", "O", "A")))

# define labels and colors for the plot
label = c("Missing","Unemployed","Other work", "Agricultural worker")
color = c("#aaa3a2", "#afb2cb", "#e8a6bd", "#a7cece")

# create the plot using the defined function (see functions_employment.R)
p <- col_fun(plot_u_df, "country_year", "percent", "occ_val7", "Weighted Percentage (rural areas)", color, label)
p

# save the plot to a PDF file
ggsave(paste0(FigDir,"/", Sys.Date(),"_women_men_survey_rural_DHS_datasets_agric_data_weighted_percent.pdf"), p, width = 5.5, height = 3)

# create household exposure data
hh_ir_rural = ir_mr_rural %>%
  group_by(code_year) %>% # group by code year to calculate maximum year
  mutate(max_year = max(dhs_year)) %>%
  ungroup() %>%
  mutate(
    year_combo = ifelse(max_year == min_year, # create year_combo to represent year range
                        max_year, 
                        paste(min_year, "-", str_sub(max_year, -2))),
    country_year = paste0(CountryName, " ", year_combo) # create a combined country-year identifier
  ) %>%
  group_by(country_year, code_year, v001, v002) %>% # group by country year and other identifiers to summarize data
  summarise(
    n = n(),  # count number of observations
    home_type = paste0(occ_val7, collapse = "")  # concatenate occupation values
  ) %>%
  mutate(home_type2 = ifelse(grepl("A", home_type), "A", "O")) %>% # classify households based on home_type
  drop_na(home_type2) %>%  # remove rows with missing home_type2
  ungroup()

# prepare data for plotting
plot_u_df <- hh_ir_rural %>%
  dplyr::select(country_year, home_type2, code_year)

# define labels and colors for the plot
xlabel = "Occupation (A - Agricultural work, O - other occupation, unemployed or occupation is missing)"
color = c("#a7cece", "#e8a6bd")

# create the bar plot using the defined function (see functions_employment.R)
p <- bar_fun(plot_u_df, 
             "home_type2", 
             "home_type2", 
             "Household exposure to agricultural occupation based on responses from the women and men's survey (rural areas), 
               households labeled as A have at least one adult woman or man in the household engaged in agricultural work", 
             xlabel) +
  scale_fill_manual(values = color) +  # apply color scale
  facet_wrap(vars(country_year), scales = "free")  # create facets by country year
p

# save the plot to a PDF file
ggsave(paste0(FigDir,"/", Sys.Date(),"agric_HH_exposure_women_men_survey_rural.pdf"), p, width = 6.8, height = 5.5)

# now join to the pr dataset 
# (the pr dataset is 64008 and the hh ir dataset is 114031. 
# joining resulted in 59489 obs after filtering missing test and home type data)
rural_df = left_join(pr_rural, hh_ir_rural, 
                     by = c("code_year", "hv001" = "v001", "hv002" = "v002")) %>%
  filter(!is.na(test_result)) %>% # filter out observations with missing test results
  filter(!is.na(home_type2)) # filter out observations with missing home type

# plot the joined data for agricultural exposure
plot_u_df <- rural_df %>%
  dplyr::select(country_year.x, home_type2, code_year, test_result)

# define labels and colors for the plot
xlabel = "Occupation (A - Agricultural work, O - other occupation, unemployed or occupation is missing)"
color = c("#a7cece", "#e8a6bd")

# create the bar plot using the specified function (see functions_employment.R)
p <- bar_fun(plot_u_df, 
             "home_type2", 
             "home_type", 
             "Occupation based on responses from the women and men's survey (rural areas), 
               after combining with the DHS PR dataset", 
             xlabel) +
  scale_fill_manual(values = color) +  # apply color scale
  facet_wrap(vars(country_year.x), scales = "free")  # create facets by country year
p

# save the plot as a PDF file
ggsave(paste0(FigDir,"/", Sys.Date(),"agric_HH_exposure_joined_pr_data_women_men_survey_rural.pdf"), p, width = 6.8, height = 5.5)


# plot the malaria variables for all data (figure 2) (will be combined with urban area data below)
# define colors for the plot
color = c("#f2a5a1", "#c55c80")

# summarize the overall data by home type and test result
plot_overall = plot_u_df %>%
  group_by(home_type2, test_result) %>%
  summarise(value = n()) %>%
  # calculate percentage of each test result
  mutate(percent = round(value / sum(value) * 100, 0))

# set the title for the plot
plot_overall$title = "Rural"

# create the bar plot
p_rural <- ggplot(plot_overall, aes(fill = test_result, x = home_type2)) + 
  geom_bar(aes(y = value), position = "stack", stat = "identity") +  # stack bars for each test result
  theme_manuscript() +  # apply manuscript theme (see functions_employment.R)
  scale_x_discrete(labels = c("Agricultural worker \n household (HH)", 
                              "Non-agricultural \n worker HH")) +  # label x-axis
  scale_fill_manual(name = "Malaria test result", 
                    labels = c("Negative", "Positive"), 
                    values = color) +  # customize fill colors
  geom_text(aes(label = paste0(value, " ", "(", percent, "%", ")"), y = value), 
            position = position_stack(vjust = 0.5),  # center labels within bars
            color = "white") +  # set label color to white
  labs(x = "", y = "") +  # remove axis labels
  facet_wrap(vars(title)) +  # create facets based on the title
  theme(strip.text.x = element_text(size = 12, color = "black")) +  # customize facet label appearance
  coord_cartesian(ylim = c(0, 44000)) +  # set y-axis limits
  theme(axis.text.y = element_blank(),  # remove y-axis text
        axis.ticks.y = element_blank(),  # remove y-axis ticks
        axis.title.y = element_blank())  # remove y-axis title

# combine urban and rural plots and adjust legend position
p_malaria = p_urban + p_rural + plot_layout(guides = "collect") & 
  theme(legend.position = 'bottom')
p_rural

# save the combined plot as a PDF
ggsave(paste0(FigDir,"/", Sys.Date(),"malaria_prevalence_HH_occupation_exposure_urban_rural.pdf"), p_malaria, width = 6.8, height = 5)

# plot by country (figure for supplement)
plot_country = plot_u_df %>% 
  group_by(country_year.x, home_type2, test_result) %>% 
  summarise(value = n()) %>% 
  mutate(percent = round(value / sum(value) * 100, 0)) # calculate percentage of each test result

# create the bar plot by country
p <- ggplot(plot_country, aes(fill = test_result, x = home_type2)) + 
  geom_bar(aes(y = value), position = "stack", stat = "identity") +  # stack bars for each test result
  theme_manuscript() +  # apply manuscript theme
  scale_x_discrete(labels = c("Agric worker \n HH", "Non-agric \n worker HH")) +  # label x-axis
  scale_fill_manual(name = "Malaria test result", 
                    labels = c("Negative", "Positive"), 
                    values = color) +  # customize fill colors
  geom_text(aes(label = paste0(value, " ", "(", percent, "%", ")"), y = value), 
            position = position_stack(vjust = 0.5),  # center labels within bars
            color = "white") +  # set label color to white
  labs(x = "", 
       y = "Number of children, 6 - 59 months, tested positive for malaria in 22 DHS datasets") +  # y-axis label
  facet_wrap(vars(country_year.x), scales = "free") +  # create facets based on country year
  theme(legend.position = 'bottom')  # set legend position
p

# save the country-specific plot as a PDF
ggsave(paste0(FigDir,"/", Sys.Date(),"malaria_prevalence_by agric_exposure_rural_by_country.pdf"), p, width = 7, height = 8) 

# write the rural dataframe to a CSV file for analysis
write_csv(rural_df, file.path(PopDir, "analysis_dat/rural_df_for_analysis_trend.csv"))














