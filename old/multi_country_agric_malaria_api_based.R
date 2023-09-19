
#libraries
#devtools::install_github("ropensci/rdhs", force = TRUE)#make sure to reinstall rdhs if it is not updated to the version from May 2022



#######using local data here. yet to be replaced once API package is fixed. 

### Directories

user <- Sys.getenv("USERNAME")

Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
DriveDir <- file.path(Drive, 'Box', 'NU-malaria-team')
DataDir <- file.path(DriveDir, "data", 'Urban_malaria_net_ownership_data','DHS')
ResultsDir<- file.path(DriveDir, "projects", 'urban_malaria', 'urban_malaria_net_ownership', 'Extracted_csv')


#DataDir <- file.path(Drive, "Downloads", "to_clean2")


#Loading functions and libraries

source("functions_employment.R")

# this option allows admin units with only one cluster to be analyzed
options(survey.lonely.psu="adjust") 


##############################
# Setting up  API
##############################


#obtaining country ids
ids <- dhs_countries(returnFields=c("CountryName", "DHS_CountryCode"))


## find all the surveys that match the search criteria. ie .2015

survs <- dhs_surveys(surveyYearStart = 2015)

#getting list of datasets from api

datasets <- dhs_datasets(surveyIds = survs$SurveyId, fileFormat = "FL")
str(datasets)

## login into dhs
my_config <- set_rdhs_config(email = "cchiziba@gmail.com",
                             project = "Net ownership by individual",
                             config_path = "rdhs.json",
                             cache_path = "project_one",
                             password_prompt = TRUE,
                             global = FALSE)

# caching the dhs list: the first time we call this function, rdhs will make the API request

microbenchmark::microbenchmark(dhs_surveys(surveyYear = 2015),times = 1)



#######################################
#Data loading  and tranforming
#######################################


#listing list of datasets to explore
datasets <- dhs_datasets(surveyIds = survs$SurveyId, fileFormat = "FL", fileType = "IR")# comment - can you filter by only surveys in Africa?

#ir_datasets <- datasets$FileName uncomment if you want all IR datasets past 2015

ir_datasets <- c("BUIR71FL.ZIP", "NGIR7BFL.ZIP") # you can replace with file names of interest only. 
#comment for above- you are not only working with Nigeria and Burundi so need to create code that reads data and performs manipulations for all countries? willl 

# Downloading and readingIR datasetes using API

downloads <- get_datasets(ir_datasets) 

dhs = lapply(downloads, readRDS)


# looping through datasets to extract woman related data

dhs_ir <- dhs %>%  purrr::map(~mutate(., wt=v005/1000000,strat=v022, id=v021,
                                      agri_worker_partner = ifelse(v705 %in% c(4, 5), 1, ifelse(v705 >=98, NA, 0)))) %>% 
  purrr::map(~filter(., v025 == 1)) %>%
  purrr::map(~dplyr::select(., v000, v001, v002, agri_worker_partner, wt, strat, id))

#binging all countries agric variable


ir_bind <- bind_rows(dhs_ir)

#listing list of PR datasets to explore

pr_datasets <- c("BUPR71FL.ZIP", "NGPR7BFL.ZIP")

# Downloading IR datasetes using API

downloads <- get_datasets(pr_datasets) # cooment - it says pr here? should it be pr above?

dhs_pr = lapply(downloads, readRDS)

look_for(dhs_pr[[1]], "v025")
  
# Estimating malaria status by microscopy 

dhs_pr <- dhs_pr %>% map(~filter(., hv042 == 1 & hv103 == 1 & hc1 %in% c(6:59) & hml32 %in% c(0, 1),hv025 == 1)) %>%
  map(~mutate(., test_result = ifelse(hml32==1, 1,0))) %>% purrr::map(~filter(., hv025 == 1)) %>%
  purrr::map(~dplyr::select(., test_result, hv000, hv001, hv002))#comment - make sure that purrr is spelt correctly. You have it as purr twice

#continuing comment - I get this error that v025 cannot be found. The value should be hv025 not v025

#binging all countries' malaria test variable

pr_bind <- bind_rows(dhs_pr)

#merging agric and malaria datasets

df <- ir_bind %>% left_join(pr_bind, by = c("v000"="hv000","v001"="hv001", "v002"="hv002"))


#split df

cntrs_df_list <- split(df, with(df, interaction(v000)), drop = TRUE)


#table

table.fun <- function(df_1){
  
  tbl_summary(df_1, by = "test_result", include = c( agri_worker_partner)) %>% add_overall()
  
}

table_list <- lapply(cntrs_df_list, table.fun)

#merge sexualityr tables
tbl <- tbl_merge(tbls = table_list, 
                          tab_spanner = c("**Burundi**", "**Nigeria**"))
tbl #looking at the result for burundi and what you have in Table XXA, it appears total number for agric_worker_partner should be 201?
#whereas for NIgeria, the values in the table output and what you have in your document is very different?


#survey design list

svyd_list <- lapply(cntrs_df_list, svydesign.fun)



#function fitting bivariate models
model.fun <- function(s_design){
  
svyglm(test_result ~ agri_worker_partner, design = s_design, family = binomial)
  
}

#applying model funtion to fit models
model_list <- lapply(svyd_list, model.fun)

#summarizing findings
summary_list <- lapply(model_list, summary)
summary_list

