#The purpose of this script is to recode variables in the ir, mr and pr datasets, summarize the ir and mr script at the household level and merge them for urban and rural areas 

#remove.packages("rdhs")
#learn about RDHS, used for downloading the DHS data here https://github.com/ropensci/rdhs

rm(list = ls())

#devtools::install_github("ropensci/rdhs")

library(rdhs)

## -----------------------------------------
### Directories
## -----------------------------------------
user <- Sys.getenv("USERNAME")
if ("ido0493" %in% user) {
  user_path <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("OneDrive"))))
  DriveDir <- file.path(user_path, "urban_malaria")
  PopDir <- file.path(DriveDir, "data", "Urban_malaria_net_ownership_data")
  ManDir <- file.path(DriveDir, "projects", "Manuscripts", "agriculture_malaria_manuscript")
  FigDir <- file.path(ManDir, "figures", "220623_new_figures")
} else if  ("Chilo Chiziba" %in% user) {
  Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
  DriveDir <- file.path(Drive, 'OneDrive - Northwestern University', 'urban_malaria')
  ManDir <- file.path(DriveDir, "projects", "Manuscripts", "agriculture_malaria_manuscript")
} else {
  Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
  DriveDir <- file.path(Drive, 'Library', 'CloudStorage', 'OneDrive-NorthwesternUniversity', "urban_malaria")
  #DriveDir <- file.path(Drive,  "OneDrive - Northwestern University", "urban_malaria")
  PopDir <- file.path(DriveDir, "data", 'Urban_malaria_net_ownership_data')
  ManDir <- file.path(DriveDir, "projects", "Manuscripts", "agriculture_malaria_manuscript")
  FigDir <- file.path(ManDir, "figures", "220623_new_figures")
}

## -----------------------------------------
### Required functions and settings
## -----------------------------------------
#note before sourcing functions and packages, run the code below to download rdhs if you don't already have it
#devtools::install_github("ropensci/rdhs", force = T)
source("functions/functions_employment.R")
options(survey.lonely.psu="adjust")  # this option allows admin units with only one cluster to be analyzed

## --------------------------------------------------------------------------------------------------------------------------------------------------
### read in the survey ids for surveys to be analyzed and logging into the DHS account (no need to login to dhs or run commented out script unless files need to be updated)
## ---------------------------------------------------------------------------------------------------------------------------------------------------

#login into dhs (email and password needs to be removed from this script before it becomes public)
my_config <- set_rdhs_config(email = "ozodiegwui@gmail.com",
                             project = "Association of household member engagement in agricultural work and malaria",
                             config_path = "rdhs.json",
                             cache_path = "data",
                             password_prompt = TRUE,
                             global = FALSE, timeout = 600)



survs <- read.csv(file.path(ManDir, "csv", "surveyids_for_analysis.csv"))
# 
# obtaining country ids
ids <- dhs_countries(returnFields=c("CountryName", "DHS_CountryCode", "SubregionName"))
# 
# 
# ## --------------------------------------------------------------------------------------------------------------------
# ### Data loading  and transforming IR and MR datasets (no need to run commented out script unless dhs files need an update)
# ## ---------------------------------------------------------------------------------------------------------------------
# #download IR datasets 
ir_datasets <- dhs_datasets(surveyIds = survs$SurveyId, fileFormat = "DT", fileType = "IR") %>%
  group_by(CountryName) %>%
  slice_max(SurveyYear)

ir_downloads <- list()

for (i in 1:nrow(ir_datasets)){
  # if (ir_datasets$FileName[i] == "MDIR81DT.ZIP"){ # for some reason, I am not able to download this file with my credentials so downloading manually
  #   df <- list("MDIR81FL"="project_one/datasets/MDIR81FL.ZIP") #manually create link for madagascar 2021
  #   ir_downloads<- append(ir_downloads, df)
  # }
  # sending ir data link to the folder "project_one
  tryCatch({
  df <- get_datasets(ir_datasets$FileName[i], download_option = "zip", verbose_argument= T)
  ir_downloads<- append(ir_downloads, df)

  }, error = function(e){cat("ERROR:", conditionMessage(e), "\n")})

}
# 
# 
# manually have to download the rest 
file_df <- list.files(path = "data/datasets", pattern = ".ZIP|.zip", full.names = TRUE)
length(file_df)

## ---------------------------------------------------------------------------------------------------------------------
#save ir_downloads and mr_downloads as RDS so no need to sign in to read dhs data until files are updated 
#saveRDS(ir_downloads, "ir_downloads.rds")
#ir_downloads<- readRDS("analysis_dat/ir_downloads.rds")



# creates lists of dataframes and cleans it up, adding NA columns where mutated variables are absent 
dhs_ir_urban <- list()
dhs_ir_rural <- list()

for (i in 1:length(file_df)){

unzip(file_df[[i]],  exdir = "data/opened")

link_ir<- str_replace(file_df[[i]], "data/datasets/", "data/opened/")

link_ir<- str_replace(link_ir, ".ZIP|.zip", ".DTA")

link_ir<- str_replace(link_ir, "DT", "FL")

dhs_ir <- read_dta(link_ir)

df <- dhs_ir %>%
  dplyr::select(matches("v000|v001|v002|v003|v005|v006|v007|v012|v021|v022|v025|v106|v157|v158|v159|v168|v190|v501|v505|v704|v704a|v705|v716|v717|v731|v732|v743a|s1108ai|s1108ba|s1108bc|s1108bd|s1108bf|b19|h11_1|h11_2|h11_3|h11_4|h11_5|h11_6")) %>%
  mutate(agri_partner=tryCatch(ifelse(v705 %in% c(4, 5), "A",ifelse(v705  %in% c(1, 2, 3, 6, 7,8, 9,96), "O", ifelse(v705 == 0, "U", NA))), error =function(e) return(NA)),
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
  mutate(across(contains("s1108"), ~ifelse(.x >=8, NA, .x), .names = "new_{.col}")) %>%
  mutate(kap_index = if("new_s1108ai" %in% colnames(.)) (new_s1108ai+ new_s1108ba + new_s1108bc + new_s1108bd + kap_weak)/5 else NA) %>%
  dplyr::rename(strat=v022, id=v021, dhs_year = v007, edu_woman  = v106, age_woman = v012,wealth = v190) %>%
  dplyr::select(-c(starts_with("s1108"))) %>%
  unite(col = "HH_occ", agri_partner, agri_woman, na.rm=TRUE, sep="", remove = FALSE) %>%
  mutate(HH_occ = ifelse(HH_occ == "", "M", HH_occ)) %>%
  mutate(occ_val = ifelse(grepl("A", HH_occ), "A", HH_occ)) %>%
  mutate(occ_val = ifelse(grepl("O", occ_val), "O", occ_val)) %>%
  mutate(occ_val = ifelse(grepl("U", occ_val), "U", occ_val)) %>%
  mutate(occ_val = factor(occ_val, levels = c("A", "O", "U", "M")), wt=v005/1000000)

df <- df %>%  mutate(DHS_CountryCode = str_sub(v000, 1, 2)) %>%  left_join(ids) %>%
mutate( min_year =min(dhs_year), code_year = paste0(DHS_CountryCode, min_year))

df_urban <- df %>%  filter(v025 == 1) #filter to urban

print(paste("appending urban data from", unique(df_urban[["CountryName"]]), "to list of data frames"))
dhs_ir_urban <- append(dhs_ir_urban, list(df_urban))

df_rural <- df %>%  filter(v025 == 2)

print(paste("appending rural data from", unique(df_urban[["CountryName"]]), "to list of data frames"))
dhs_ir_rural <- append(dhs_ir_rural, list(df_rural))
}

lapply(dhs_ir_urban, function(x) table(x$h11_6))
lapply(dhs_ir_rural, function(x) table(x$h11_1))

#saveRDS(dhs_ir_urban, "analysis_dat/dhs_ir_urban.rds")
#dhs_ir_urban<- readRDS("dhs_ir_urban.rds")

#saveRDS(dhs_ir_rural, "analysis_dat/dhs_ir_rural.rds")
#dhs_ir_rural<- readRDS("dhs_ir_rural.rds")

# filter list to keep only datasets with agriculture workers that are partners as this eliminates countries with no agricultural data 
#dhs_ir_urban <- dhs_ir_urban %>%purrr::discard(~all(is.na(.x$agri_partner)))
#dhs_ir_rural <- dhs_ir_rural %>%purrr::discard(~all(is.na(.x$agri_partner)))



## ------------------------------------------------
### Exploring data to see if it is fit for purpose
## ------------------------------------------------

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
#   scale_fill_manual(values= color)+
#   facet_wrap(vars(country_year), scales="free")
# ggsave(paste0(FigDir,"/", Sys.Date(),"_urban_DHS_datasets_agric_data.png"), p, width = 13, height = 13)
# 
# p <- bar_fun(plot_r_df, "category" , "category","DHS datasets with agricultural worker data (rural areas)", xlabel)+
#   scale_fill_manual(values=color)+
#   facet_wrap(vars(country_year), scales="free")
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
# 
# # rural 
# plot_r_df <- dhs_ir_rural  %>% map(~as_survey_design(., ids= id,strata=strat,nest=T,weights= wt))%>% map(~drop_na(.,category)) %>%  
#   map(~group_by(., category)) %>%  map(~summarize(., across(country_year), percent = survey_mean() *100,
#                                                   total = survey_total())) %>%  map(~distinct(., category, .keep_all = TRUE))
# plot_r_df  <- plyr::ldply(plot_r_df) %>%  mutate(category = factor(category, levels = c("M", "U", "O", "A")))
# p <- col_fun(plot_r_df, "country_year", "percent", "category", "Weighted Percentage (rural areas)", color, label)
# ggsave(paste0(FigDir,"/", Sys.Date(),"_rural_DHS_datasets_agric_data_weighted_percent.png"), p, width = 13, height = 13)
# 


## --------------------------------------------
### Data loading  and transforming MR datasets
## --------------------------------------------
# download MR datasets 
# mr_datasets <- dhs_datasets(surveyIds = survs$SurveyId, fileFormat = "DT", fileType = "MR") %>%
#   group_by(CountryName) %>%
#   slice_max(SurveyYear)
# 
# 
# mr_downloads <- list()
# 
# for (i in 1:nrow(mr_datasets)){
#   tryCatch({
#     df <- get_datasets(mr_datasets$FileName[i], download_option = "zip", verbose_argument= T)
#     mr_downloads<- append(mr_downloads, df)
# 
#   }, error = function(e){cat("ERROR:", conditionMessage(e), "\n")})
# 
# }
# 
# saveRDS(mr_downloads, "mr_downloads.rds")
#need download all 36 datasets for MR also 
mr_downloads<- readRDS("analysis_dat/mr_downloads.rds")
# 
dhs_mr_urban <- list()
dhs_mr_rural <- list()
# 
for (i in 1:length(mr_downloads)){

unzip(mr_downloads[[i]],  exdir = "data/datasets")

link_mr<- str_replace(mr_downloads[[i]], "DT", "FL")

link_mr<- str_replace(link_mr, ".ZIP", ".DTA")


dhs_mr <- read_dta(link_mr)


df <- dhs_mr %>%
    dplyr::select(matches("mv000|mv001|mv002|mv005|mv007|mv012|mv022|mv021|mv025|mv106|mv190|mv717")) %>%
    mutate(agric_work_man_response = tryCatch(ifelse(mv717 %in% c(4, 5), "A", ifelse(mv717 %in% c(1, 2, 3, 6, 7, 8, 9, 11), "O", ifelse(mv717 == 0, "U", NA))), error =function(e) return(NA))) %>%
    dplyr::rename(strat=mv022, id=mv021, dhs_year = mv007, edu_woman  = mv106, age_woman = mv012,wealth = mv190) %>%
    mutate(wt=mv005/1000000)


df <- df %>%  mutate(DHS_CountryCode = str_sub(mv000, 1, 2)) %>%  left_join(ids) %>%
    mutate( min_year =min(dhs_year), code_year = paste0(DHS_CountryCode, min_year)) %>%
  mutate(agric_work_man_response2= ifelse(is.na(agric_work_man_response), "M", agric_work_man_response),
         agric_work_man_response2 = factor(agric_work_man_response2, levels = c("A", "O", "U", "M")))


df_urban <- df %>%  filter(mv025 == 1) #filter to urban

print(paste("appending urban data from", unique(df_urban[["CountryName"]]), "to list of data frames"))
dhs_mr_urban <- append(dhs_mr_urban, list(df_urban))

df_rural <- df %>%  filter(mv025 == 2)

print(paste("appending rural data from", unique(df_urban[["CountryName"]]), "to list of data frames"))
dhs_mr_rural <- append(dhs_mr_rural, list(df_rural))
}

saveRDS(dhs_mr_urban, "analysis_dat/dhs_mr_urban.rds")
#dhs_mr_urban <- readRDS("dhs_mr_urban.rds")

saveRDS(dhs_mr_rural, "analysis_dat/dhs_mr_rural.rds")
#dhs_mr_rural <- readRDS("dhs_mr_rural.rds")




## ------------------------------------------------
### Exploring data to see if it is fit for purpose
## ------------------------------------------------

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




## --------------------------------------------
### Data loading  and transforming PR datasets
## --------------------------------------------

# download PR datasets 
# pr_datasets <- dhs_datasets(surveyIds = survs$SurveyId, fileFormat = "DT", fileType = "PR") %>%
#   group_by(CountryName) %>%
#   slice_max(SurveyYear)
# 
# pr_downloads <- list()
# 
# for (i in 1:nrow(pr_datasets)){
#   # if (ir_datasets$FileName[i] == "MDIR81DT.ZIP"){ # for some reason, I am not able to download this file with my credentials so downloading manually
#   #   df <- list("MDIR81FL"="project_one/datasets/MDIR81FL.ZIP") #manually create link for madagascar 2021
#   #   ir_downloads<- append(ir_downloads, df)
#   # }
#   # sending ir data link to the folder "project_one
#   tryCatch({
#   df <- get_datasets(pr_datasets$FileName[i], download_option = "zip", verbose_argument= T)
#   pr_downloads<- append(pr_downloads, df)
# 
#   }, error = function(e){cat("ERROR:", conditionMessage(e), "\n")})
# 
# }

#saveRDS(pr_downloads, "pr_downloads.rds")
pr_downloads<- readRDS("analysis_dat/pr_downloads.rds")

# creates lists of dataframes and cleans it up, adding NA columns where mutated variables are absent 
dhs_pr_urban <- list()
dhs_pr_rural <- list()


for (i in 1:length(pr_downloads)){

  unzip(pr_downloads[[i]],  exdir = "data/datasets")

  link_pr<- str_replace(pr_downloads[[i]], "DT", "FL")

  link_pr<- str_replace(link_pr, ".ZIP", ".DTA")

  dhs_pr <- read_dta(link_pr)


  df <- dhs_pr %>%
    dplyr::select(matches("hc1|hc60|hml12|hml16a|hml32|hml35|hv000|hv001|hv002|hv003|hv005|hv006|hv007|hv009|hv021|hv025|hv022|hv042|hv103|hv213|hv214|hv215|hv253|hv270|hvidx|sh418|sh511"))

  if(paste0(unique(df[["hv000"]]), unique(df[["hv007"]]))[1] == "CM62011"){

    df <- df %>%  mutate(test_result = ifelse(sh418 %in% c(1, 2, 3), "+ve", ifelse(sh418 == 4, "-ve", NA)))

  }else if(paste0(unique(df[["hv000"]]), unique(df[["hv007"]]))[1] == "TZ52007") {

    df <- df %>%  mutate(test_result = ifelse(sh511 ==1, "+ve", ifelse(sh511 ==0,  "-ve", NA)))

  }else {

    df <- df %>% mutate(hml32 = tryCatch(ifelse(hml32 > 1, NA, hml32), error = function(e) return(NA)),
                        hml35 = tryCatch(ifelse(hml35 > 1, NA, hml35), error = function(e) return(NA)),
                        test_result=tryCatch(ifelse(!is.na(hml32), hml32, hml35), error =function(e) return(NA)),
                        test_result = ifelse(test_result==1, "+ve",ifelse(test_result==0, "-ve", NA)), test_result = as.character(test_result))
  }

  #child_age =tryCatch(ifelse(!is.na(hc1), hc1, hml16a), error = function(e) return(NA)),

  df <- df %>%  mutate(u5_net_use = tryCatch(ifelse(hml12 %in% c(1,2), 1,0), error = function(e) return(NA)),
                       floor_type = ifelse(hv213 == 30| hv213 == 31|hv213 == 33| hv213 == 34|hv213 == 35,1, 0),
                       wall_type = ifelse(hv214 == 30| hv214 == 31| hv214 == 33| hv214 == 34,0, 1),
                       roof_type = ifelse(hv215 == 30| hv215 == 31|hv215 == 33| hv215 == 34|hv215 == 35|hv215 == 36,1, 0),
                       wealth = hv270,
                       interview_month = hv006,
                       wt=hv005/1000000) %>%
    dplyr::rename(strat = hv022, id = hv021, dhs_year = hv007, hh_size = hv009)

  df <- df %>%  mutate(DHS_CountryCode = str_sub(hv000, 1, 2)) %>%left_join(ids) %>%
    mutate( min_year =min(dhs_year), code_year = paste0(DHS_CountryCode, min_year))

  df_urban <- df %>%  filter(hv042 == 1 & hv103 == 1 & hc1 %in% c(6:59), hv025 == 1) #filter to urban

  print(paste("appending urban data from", unique(df_urban[["CountryName"]]), "to list of data frames"))
  dhs_pr_urban <- append(dhs_pr_urban, list(df_urban))

  df_rural <- df %>%  filter(hv042 == 1 & hv103 == 1 & hc1 %in% c(6:59), hv025 == 2) #filter to rural

  print(paste("appending rural data from", unique(df_urban[["CountryName"]]), "to list of data frames"))
  dhs_pr_rural <- append(dhs_pr_rural, list(df_rural))
}

lapply(dhs_pr_urban, function(x) table(x$hv253))
lapply(dhs_pr_rural, function(x) table(x$hv253))


# filter list to keep only datasets with malaria test data  
dhs_pr_urban <- dhs_pr_urban %>%purrr::discard(~all(is.na(.x$test_result)))
dhs_pr_rural <- dhs_pr_rural %>%purrr::discard(~all(is.na(.x$test_result)))

saveRDS(dhs_pr_urban, "analysis_dat/dhs_pr_urban.rds")
#dhs_pr_urban<-readRDS("dhs_pr_urban.rds")

saveRDS(dhs_pr_rural, "analysis_dat/dhs_pr_rural.rds")
#dhs_pr_rural<-readRDS("dhs_pr_rural.rds")

## ------------------------------------------------
### Exploring data to see if it is fit for purpose
## ------------------------------------------------

# plot to view malaria data 
dhs_pr_urban <- dhs_pr_urban %>% map(~mutate(., max_year = max(dhs_year), year_combo = ifelse(max_year == min_year, max_year, paste(min_year, "-",str_sub(max_year, -2))),
                                             country_year = paste0(CountryName, " ", year_combo)))
dhs_pr_rural <- dhs_pr_rural %>% map(~mutate(., max_year = max(dhs_year), min_year = min(as.numeric(dhs_year)), year_combo = ifelse(max_year == min_year, max_year, paste(min_year, "-", str_sub(max_year, -2))),
                                             country_year = paste0(CountryName, " ", year_combo)))

plot_u_df<- dhs_pr_urban %>% map(~dplyr::select(., country_year, test_result, code_year)) %>%  bind_rows(.id = "column_label")
plot_r_df <- dhs_pr_rural %>% map(~dplyr::select(., country_year, test_result, code_year)) %>%  bind_rows(.id = "column_label")



#do we have enough data for this analysis?
# label = "malaria_test results"
# color = c("darkslategray2", "deeppink3", "#aaa3a2") #"#967cb9"
# p <- bar_fun(plot_u_df, "test_result" , "test_result", "DHS datasets with malaria test results among 6 - 59 years (urban areas)", label)+
#   scale_fill_manual(values= color)+
#   xlab("Malaria test results by Microscopy or RDT")+
#   facet_wrap(vars(country_year), scales="free")
# ggsave(paste0(FigDir,"/", Sys.Date(),"_urban_DHS_datasets_malaria_test_data.png"), p, width = 13, height = 13)

# p <- bar_fun(plot_r_df, "test_result" , "test_result",  "DHS datasets with malaria test results among 6 - 59 years (rural areas)", label)+
#   scale_fill_manual(values= color)+
#   xlab("Malaria test results by Microscopy or RDT")+
#   facet_wrap(vars(country_year), scales="free")
# ggsave(paste0(FigDir,"/", Sys.Date(),"_rural_DHS_datasets_malaria_test_data.png"), p, width = 13, height = 13)


# Change to  weighted percentage to compare across countries 
plot_u_df <- dhs_pr_urban  %>% map(~as_survey_design(., ids= id,strata=strat,nest=T,weights= wt))%>% map(~mutate(., test_result= ifelse(is.na(test_result), "missing", test_result))) %>%
  map(~group_by(., test_result)) %>%  map(~summarize(., across(c(country_year, code_year)), percent = survey_mean() *100,
                                                     total = survey_total())) %>%  map(~distinct(., test_result, .keep_all = TRUE))
plot_u_df <- plyr::ldply(plot_u_df) %>%mutate(test_result = factor(test_result, levels = c("missing", "-ve", "+ve")))
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



## --------------------------------------------------------------------------------------------
### get survey ids for dataset with > 1% positive malaria tests    
## ---------------------------------------------------------------------------------------------
df <- plot_u_df %>%  filter(test_result == "+ve" & percent > 1) %>%  select(code_year) 
# 
# 
# 
# 
# ## --------------------------------------------------------------------------------------------
# ### create analysis datasets   
# ## ---------------------------------------------------------------------------------------------
ir_urban <- dhs_ir_urban %>%purrr::keep(~all(.x$code_year %in% df$code_year))
ir_rural <- dhs_ir_rural %>%purrr::keep(~all(.x$code_year %in% df$code_year))

mr_urban <- dhs_mr_urban %>%purrr::keep(~all(.x$code_year %in% df$code_year))
mr_rural <- dhs_mr_rural %>%purrr::keep(~all(.x$code_year %in% df$code_year))

pr_urban <- dhs_pr_urban %>%purrr::keep(~all(.x$code_year %in% df$code_year))
pr_rural <- dhs_pr_rural %>%purrr::keep(~all(.x$code_year %in% df$code_year))


#combine all ir datasets
ir_urban %>% map(~dim(.x)[[2]]) # list column dimensions to get smallest column length in list and position
ir_urban<- ir_urban %>%map(~dplyr::select(., colnames(ir_urban[[3]])))
ir_urban <- plyr::ldply(ir_urban)


ir_rural %>% map(~dim(.x)[[2]]) #get smallest column length in list and position
ir_rural<- ir_rural %>%map(~dplyr::select(., colnames(ir_rural[[3]])))
ir_rural <- plyr::ldply(ir_rural)


#combine all mr datasets
mr_urban %>% map(~dim(.x)[[2]]) #get smallest column length in list and position
mr_urban<- mr_urban %>%map(~dplyr::select(., colnames(mr_urban[[3]])))
mr_urban <- plyr::ldply(mr_urban)


mr_rural %>% map(~dim(.x)[[2]]) #get smallest column length in list and position
mr_rural<- mr_rural %>%map(~dplyr::select(., colnames(mr_rural[[3]])))
mr_rural<- plyr::ldply(mr_rural)


#combine all pr datasets
pr_urban %>% map(~dim(.x)[[2]]) #get smallest column length in list and position
pr_urban<- pr_urban %>%map_if(~all(c('sh511') %in% colnames(.x)), ~dplyr::select(., -sh511))
pr_urban<- pr_urban %>%map(~dplyr::select(., colnames(pr_urban[[3]])))
pr_urban <- plyr::ldply(pr_urban)


pr_rural %>% map(~dim(.x)[[2]]) #get smallest column length in list and position
pr_rural<- pr_rural %>%map_if(~all(c('sh511') %in% colnames(.x)), ~dplyr::select(., -sh511))
pr_rural<- pr_rural %>%map(~dplyr::select(., colnames(pr_rural[[3]])))
pr_rural<- plyr::ldply(pr_rural)


#saveRDS(ir_rural, "analysis_dat/ir_rural.rds")
ir_rural<-readRDS("analysis_dat/ir_rural.rds")

#saveRDS(mr_urban, "analysis_dat/mr_urban.rds")
mr_urban<-readRDS("analysis_dat/mr_urban.rds")

#saveRDS(mr_rural, "analysis_dat/mr_rural.rds")
mr_rural<-readRDS("analysis_dat/mr_rural.rds")


#saveRDS(pr_urban, "analysis_dat/pr_urban.rds")
pr_urban<-readRDS("analysis_dat/pr_urban.rds")

#saveRDS(pr_rural, "analysis_dat/pr_rural.rds")
pr_rural<-readRDS("analysis_dat/pr_rural.rds")




###################################################################combine data##################################################
#combine ir  and mr(at hh level) and pr datasets 
#some households in the ir dataset is not present in the pr dataset and I am not sure why 


###################################################################urban##################################################
#urban ir and mr 
hh_mr_urban = mr_urban %>%  group_by(code_year, mv001, mv002) %>%  summarise(agric_man = paste0(agric_work_man_response2, collapse = ""), n_men= n())

ir_mr_urban = ir_urban %>%    left_join(hh_mr_urban, by =c("code_year", "v001" = "mv001", "v002" = "mv002")) %>% 
  mutate(agric_partner_other = ifelse(is.na(agri_partner), agric_man, agri_partner)) %>% 
  unite(col = "HH_occ2", agric_partner_other, agri_woman, na.rm=TRUE, sep="", remove = FALSE) %>%
    mutate(HH_occ3 = ifelse(HH_occ2 == "", "M", HH_occ2)) %>%
    mutate(occ_val3 = ifelse(grepl("A", HH_occ3), "A", HH_occ3)) %>%
    mutate(occ_val4 = ifelse(grepl("O", occ_val3), "O", occ_val3)) %>%
    mutate(occ_val5 = ifelse(grepl("U", occ_val4), "U", occ_val4)) 

ir_mr_urban = ir_mr_urban %>%  mutate(occ_val6 = ifelse(is.na(occ_val5), "M", occ_val5),
                                      occ_val7= ifelse(grepl("M", occ_val6), "M", occ_val6)) %>%
    mutate(occ_val7 = factor(occ_val7, levels = c("A", "O", "U", "M")), wt=v005/1000000)


#pick up diarrheal disease values 
ir_mr_urban <- ir_mr_urban %>%  dplyr::mutate(across(c(h11_1:h11_6), ~ dplyr::case_when(. %in% c("2") ~ "1", 
                                                                                         . %in% c("8", "9") ~ NA,
                                                                                         TRUE ~ as.character(.)), .names = "rec_{col}")) %>%
  mutate(across(starts_with("rec"), ~as.numeric(.))) %>% 
  mutate(sum_dia = rowSums(select(., contains("rec_")), na.rm =TRUE))
  

# plot to view urban agric data and save dataset  
dhs_ir_mr_urban <- ir_mr_urban %>% group_by(code_year) %>% mutate(max_year = max(dhs_year)) %>% ungroup() %>% 
  mutate(year_combo = ifelse(max_year == min_year, max_year, paste(min_year, "-",str_sub(max_year, -2))),
                                             country_year = paste0(CountryName, " ", year_combo))

plot_u_df<- dhs_ir_mr_urban %>% dplyr::select(., country_year, occ_val7, code_year)


xlabel = "Occupation (A - Agricultural work, O - other, U - unemployed, M - Missing)"
color = c("#a7cece", "#e8a6bd", "#afb2cb", "#aaa3a2")
p <- bar_fun(plot_u_df, "occ_val7" ,"occ_val7", "DHS datasets with agricultural worker data based on responses from the women and men's survey (urban areas)", xlabel)+
  scale_fill_manual(values= color)+
  facet_wrap(vars(country_year), scales="free")
ggsave(paste0(FigDir,"/", Sys.Date(),"_women_men_survey_urban_DHS_datasets_agric_data.pdf"), p, width = 6.8, height = 5.5)


#weighted plots

# urban
plot_u_df <- dhs_ir_mr_urban  %>% as_survey_design(ids= id,strata=strat,nest=T,weights= wt)%>% 
 group_by(country_year, occ_val7) %>% summarize(percent =survey_mean() *100,
                                                  total = survey_total()) 
plot_u_df <- plot_u_df %>%  mutate(occ_val7 = factor(occ_val7, levels = c("M", "U", "O", "A")))

label = c("Missing","Unemployed","Other work", "Agricultural worker")
color = c("#aaa3a2", "#afb2cb", "#e8a6bd", "#a7cece")
p <- col_fun(plot_u_df, "country_year", "percent", "occ_val7", "Weighted Percentage (urban areas)", color, label)
ggsave(paste0(FigDir,"/", Sys.Date(),"_women_men_survey_urban_DHS_datasets_agric_data_weighted_percent.pdf"), p, width = 5.5, height = 3)



#create HH exposure data  
hh_ir_urban =  ir_mr_urban%>%  group_by(code_year) %>% 
  mutate(max_year = max(dhs_year)) %>% ungroup() %>% 
  mutate(year_combo = ifelse(max_year == min_year, max_year, paste(min_year, "-",str_sub(max_year, -2))),
         country_year = paste0(CountryName, " ", year_combo)) %>% 
  group_by(country_year, code_year,v001,v002) %>% 
  summarise(n = n(), home_type = paste0(occ_val7, collapse = ""), total_diarrhea = sum(sum_dia, na.rm =TRUE)) %>% 
  mutate(home_type2 = ifelse(grepl("A", home_type), "A", "O")) %>%  
  drop_na(home_type2) %>% ungroup()


plot_u_df<- hh_ir_urban %>% dplyr::select(country_year, home_type2, code_year)

xlabel = "Occupation (A - Agricultural work, O - other occupation, unemployed or occupation is missing)"
color = c("#a7cece", "#e8a6bd")
p <- bar_fun(plot_u_df, "home_type2" ,"home_type2", "Household exposure to agricultural occupation based on responses from the women and men's survey (urban areas), 
             households labeled as A have at least one adult woman or man in the household engaged in agricultural work", xlabel)+
  scale_fill_manual(values= color)+
  facet_wrap(vars(country_year), scales="free")  

ggsave(paste0(FigDir,"/", Sys.Date(),"agric_HH_exposure_women_men_survey_urban.pdf"), p, width = 6.8, height = 5.5)

#now join to pr dataset  (the pr dataset is 29394 and the hh ir dataset is 64727. joining resulted in 26843 obs after filtering missing test and home type data)
urban_df=left_join(pr_urban, hh_ir_urban, by =c("code_year", "hv001" ="v001", "hv002"="v002")) %>%
  filter(!is.na(test_result)) %>% 
  filter(!is.na(home_type2))

#plot the joined data for agric  
plot_u_df<- urban_df %>% dplyr::select(country_year.x, home_type2, code_year, test_result) 

xlabel = "Occupation (A - Agricultural work, O - other occupation, unemployed or occupation is missing)"
color = c("#a7cece", "#e8a6bd")
p <- bar_fun(plot_u_df, "home_type2" ,"home_type", "Occupation based on responses from the women and men's survey (urban areas),
             after combining with the DHS PR dataset", xlabel)+
  scale_fill_manual(values= color)+
  facet_wrap(vars(country_year.x), scales="free")

ggsave(paste0(FigDir,"/", Sys.Date(),"agric_HH_exposure_joined_pr_data_women_men_survey_urban.pdf"), p, width = 6.8, height = 5.5)


#plot the malaria variables for all data (figure 2) (will be combined with rural area data below)
color = c("#f2a5a1", "#c55c80")

plot_overall = plot_u_df %>%  group_by(home_type2, test_result) %>%  summarise(value= n()) %>% mutate(percent = round(value/sum(value) *100, 0))
plot_overall$title = "Urban"
p_urban<-ggplot(plot_overall, aes(fill=test_result, x= home_type2)) + 
  geom_bar(aes(y = value), position="stack", stat = "identity")+
  theme_manuscript()+
  scale_x_discrete(labels = c("Agricultural worker \n household (HH)", "Non-agricultural \n worker HH"))+
  scale_fill_manual(name = "Malaria test result", labels= c("Negative", "positive"), values= color)+
  geom_text(aes(label = paste0(value, " ", "(", percent, "%", ")"), y = value),
            position = position_stack(vjust = 0.5),
            color = "white") +
  labs(x = "", y  = "Number of children, 6 - 59 months,
  tested positive for malaria in 22 DHS datasets") +
  facet_wrap(vars(title))+
  theme(strip.text.x = element_text(
      size = 12, color = "black")) +
  coord_cartesian(ylim = c(0, 44000))


#plot by country (figure for supplement)
plot_country = plot_u_df %>%  group_by(country_year.x,home_type2, test_result) %>%  summarise(value= n()) %>% mutate(percent = round(value/sum(value) *100, 0)) %>% 
  mutate(country_year.x = ifelse(country_year.x == "Congo Democratic Republic 2013 - 14", "DRC 2013 - 14",country_year.x),
         home_type2 = ifelse(home_type2 == 'A', 'Agricultural worker Household (HH)', "Non-Agricultural worker HH"))



p<-ggplot(plot_country , aes(x = reorder(country_year.x, -percent), y = percent, fill = test_result)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  coord_flip() +
  scale_fill_manual(name = "Malaria test result", labels= c("Negative", "positive"), values= color)+
  facet_grid(. ~ home_type2, scales = "free", space = "free") +
  theme(legend.title = element_blank()) +
  labs(x = "", y = "Percentage of children, 6 - 59 months 
       tested for malaria in urban clusters per country")+
  theme_manuscript()+
  theme(legend.position = "bottom")






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
  
ggsave(paste0(FigDir,"/", Sys.Date(),"malaria_prevalence_by agric_exposure_urban_by_country.pdf"), p, width = 8.5, height = 6) 

write_csv(urban_df, file.path("analysis_dat/urban_df_for_analysis.csv"))

###################################################################rural##################################################
#rural ir and mr 
hh_mr_rural = mr_rural %>%  group_by(code_year, mv001, mv002) %>%  summarise(agric_man = paste0(agric_work_man_response2, collapse = ""), n_men= n())

ir_mr_rural = ir_rural %>%  left_join(hh_mr_rural, by =c("code_year", "v001" = "mv001", "v002" = "mv002")) %>% 
  mutate(agric_partner_other = ifelse(is.na(agri_partner), agric_man, agri_partner)) %>% 
  unite(col = "HH_occ2", agric_partner_other, agri_woman, na.rm=TRUE, sep="", remove = FALSE) %>%
  mutate(HH_occ3 = ifelse(HH_occ2 == "", "M", HH_occ2)) %>%
  mutate(occ_val3 = ifelse(grepl("A", HH_occ3), "A", HH_occ3)) %>%
  mutate(occ_val4 = ifelse(grepl("O", occ_val3), "O", occ_val3)) %>%
  mutate(occ_val5 = ifelse(grepl("U", occ_val4), "U", occ_val4)) 

ir_mr_rural = ir_mr_rural %>%  mutate(occ_val6 = ifelse(is.na(occ_val5), "M", occ_val5),
                                      occ_val7= ifelse(grepl("M", occ_val6), "M", occ_val6)) %>%
  mutate(occ_val7 = factor(occ_val7, levels = c("A", "O", "U", "M")), wt=v005/1000000)


#pick up diarrheal disease values 
ir_mr_rural <- ir_mr_rural %>%  dplyr::mutate(across(c(h11_1:h11_6), ~ dplyr::case_when(. %in% c("2") ~ "1", 
                                                                                        . %in% c("8", "9") ~ NA,
                                                                                        TRUE ~ as.character(.)), .names = "rec_{col}")) %>%
  mutate(across(starts_with("rec"), ~as.numeric(.))) %>% 
  mutate(sum_dia = rowSums(select(., contains("rec_")), na.rm =TRUE))


# plot to view rural agric data and save dataset  
dhs_ir_mr_rural <- ir_mr_rural %>% group_by(code_year) %>% mutate(max_year = max(dhs_year)) %>% ungroup() %>% 
  mutate(year_combo = ifelse(max_year == min_year, max_year, paste(min_year, "-",str_sub(max_year, -2))),
         country_year = paste0(CountryName, " ", year_combo))

plot_u_df<- dhs_ir_mr_rural %>% dplyr::select(., country_year, occ_val7, code_year)


xlabel = "Occupation (A - Agricultural work, O - other, U - unemployed, M - Missing)"
color = c("#a7cece", "#e8a6bd", "#afb2cb", "#aaa3a2")
p <- bar_fun(plot_u_df, "occ_val7" ,"occ_val7", "DHS datasets with agricultural worker data based on responses from the women and men's survey (rural areas)", xlabel)+
  scale_fill_manual(values= color)+
  facet_wrap(vars(country_year), scales="free")
ggsave(paste0(FigDir,"/", Sys.Date(),"_women_men_survey_rural_DHS_datasets_agric_data.pdf"), p, width = 6.8, height = 5.5)


#weighted plots

# rural
plot_u_df <- dhs_ir_mr_rural  %>% as_survey_design(ids= id,strata=strat,nest=T,weights= wt)%>% 
  group_by(country_year, occ_val7) %>% summarize(percent =survey_mean() *100,
                                                 total = survey_total()) 
plot_u_df <- plot_u_df %>%  mutate(occ_val7 = factor(occ_val7, levels = c("M", "U", "O", "A")))

label = c("Missing","Unemployed","Other work", "Agricultural worker")
color = c("#aaa3a2", "#afb2cb", "#e8a6bd", "#a7cece")
p <- col_fun(plot_u_df, "country_year", "percent", "occ_val7", "Weighted Percentage (rural areas)", color, label)
ggsave(paste0(FigDir,"/", Sys.Date(),"_women_men_survey_rural_DHS_datasets_agric_data_weighted_percent.pdf"), p, width = 5.5, height = 3)



#create HH exposure data  
hh_ir_rural =  ir_mr_rural%>%  group_by(code_year) %>% 
  mutate(max_year = max(dhs_year)) %>% ungroup() %>% 
  mutate(year_combo = ifelse(max_year == min_year, max_year, paste(min_year, "-",str_sub(max_year, -2))),
         country_year = paste0(CountryName, " ", year_combo)) %>% 
  group_by(country_year, code_year,v001,v002) %>% 
  summarise(n = n(), home_type = paste0(occ_val7, collapse = ""), total_diarrhea = sum(sum_dia, na.rm =TRUE)) %>% 
  mutate(home_type2 = ifelse(grepl("A", home_type), "A", "O")) %>%  
  drop_na(home_type2) %>% ungroup()


plot_u_df<- hh_ir_rural %>% dplyr::select(country_year, home_type2, code_year)

xlabel = "Occupation (A - Agricultural work, O - other occupation, unemployed or occupation is missing)"
color = c("#a7cece", "#e8a6bd")
p <- bar_fun(plot_u_df, "home_type2" ,"home_type2", "Household exposure to agricultural occupation based on responses from the women and men's survey (rural areas), 
             households labeled as A have at least one adult woman or man in the household engaged in agricultural work", xlabel)+
  scale_fill_manual(values= color)+
  facet_wrap(vars(country_year), scales="free")  

ggsave(paste0(FigDir,"/", Sys.Date(),"agric_HH_exposure_women_men_survey_rural.pdf"), p, width = 6.8, height = 5.5)

#now join to pr dataset  (the pr dataset is 64008 and the hh ir dataset is 114031. joining resulted in 59489 obs after filtering missing test and home type data)
rural_df=left_join(pr_rural, hh_ir_rural, by =c("code_year", "hv001" ="v001", "hv002"="v002")) %>%
  filter(!is.na(test_result)) %>% 
  filter(!is.na(home_type2))

#plot the joined data for agric  
plot_u_df<- rural_df %>% dplyr::select(country_year.x, home_type2, code_year, test_result) 

xlabel = "Occupation (A - Agricultural work, O - other occupation, unemployed or occupation is missing)"
color = c("#a7cece", "#e8a6bd")
p <- bar_fun(plot_u_df, "home_type2" ,"home_type", "Occupation based on responses from the women and men's survey (rural areas),
             after combining with the DHS PR dataset", xlabel)+
  scale_fill_manual(values= color)+
  facet_wrap(vars(country_year.x), scales="free")

ggsave(paste0(FigDir,"/", Sys.Date(),"agric_HH_exposure_joined_pr_data_women_men_survey_rural.pdf"), p, width = 6.8, height = 5.5)


#plot the malaria variables for all data (figure 2) (will be combined with urban area data below)
color = c("#f2a5a1", "#c55c80")

plot_overall = plot_u_df %>%  group_by(home_type2, test_result) %>%  summarise(value= n()) %>% mutate(percent = round(value/sum(value) *100, 0))
plot_overall$title = "Rural"
p_rural<-ggplot(plot_overall, aes(fill=test_result, x= home_type2)) + 
  geom_bar(aes(y = value), position="stack", stat = "identity")+
  theme_manuscript()+
  scale_x_discrete(labels = c("Agricultural worker \n household (HH)", "Non-agricultural \n worker HH"))+
  scale_fill_manual(name = "Malaria test result", labels= c("Negative", "positive"), values= color)+
  geom_text(aes(label = paste0(value, " ", "(", percent, "%", ")"), y = value),
            position = position_stack(vjust = 0.5),
            color = "white") +
  labs(x = "", y  = "") +
  facet_wrap(vars(title))+
  theme(strip.text.x = element_text(
    size = 12, color = "black")) +
  coord_cartesian(ylim = c(0, 44000)) + 
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.y = element_blank())


p_malaria = p_urban +p_rural+ plot_layout(guides = "collect") & theme(legend.position = 'bottom')
ggsave(paste0(FigDir,"/", Sys.Date(),"malaria_prevalence_HH_occupation_exposure_urban_rural.pdf"), p_malaria, width = 6.8, height = 5)

#plot by country (figure for supplement)
plot_country = plot_u_df %>%  group_by(country_year.x,home_type2, test_result) %>%  summarise(value= n()) %>% mutate(percent = round(value/sum(value) *100, 0))
p<-ggplot(plot_country, aes(fill=test_result, x= home_type2)) + 
  geom_bar(aes(y = value), position="stack", stat = "identity")+
  theme_manuscript()+
  scale_x_discrete(labels = c("Agric worker \n HH", "Non-agric \n worker HH"))+
  scale_fill_manual(name = "Malaria test result", labels= c("Negative", "positive"), values= color)+
  geom_text(aes(label = paste0(value, " ", "(", percent, "%", ")"), y = value),
            position = position_stack(vjust = 0.5),
            color = "white") +
  labs(x = "", y  = "Number of children, 6 - 59 months, tested positive for malaria 
       in 22 DHS datasets") +
  facet_wrap(vars(country_year.x), scales="free")+
  theme(legend.position = 'bottom')

ggsave(paste0(FigDir,"/", Sys.Date(),"malaria_prevalence_by agric_exposure_rural_by_country.pdf"), p, width = 7, height = 8) 

write_csv(rural_df, file.path("analysis_dat/rural_df_for_analysis.csv"))



























