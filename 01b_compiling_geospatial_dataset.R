
#This script extracts environmental data at cluster level lagged two months before the survey month
#Last updated: 2023-10-21
rm(list = ls())

## -----------------------------------------
### Directories
## -----------------------------------------
user <- Sys.getenv("USERNAME")
if ("ozodi"  %in% user) {
  Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", gsub("OneDrive", "", Sys.getenv("HOME")))))
  Drive <- file.path(gsub("[//]", "/", Drive))
  DriveDir <- file.path(Drive, "Urban Malaria Proj Dropbox", "urban_malaria")
  PopDir <- file.path(DriveDir, "data", "data_agric_analysis")
  ManDir <- file.path(DriveDir, "projects", "Manuscripts", "ongoing", "agriculture_malaria_manuscript")
  FigDir <- file.path(ManDir, "figures", "220623_new_figures")
  EviDir <- file.path(PopDir, "MAP_EVI_monthly")
  PrecDir <- file.path(PopDir, "chirps_monthly_precip")
  HumDir <- file.path(PopDir, "ERA5_rel_humidity_monthly")
  TempDir <- file.path(PopDir, "ERA5_temperature")
  OutDir <- file.path(PopDir, "analysis_dat")
} else if  ("CHZCHI003" %in% user) {
  Drive <- file.path("C:/Users/CHZCHI003/OneDrive")
  DriveDir <- file.path(Drive, "urban_malaria")
  PopDir <- file.path(DriveDir, "data", 'data_agric_analysis')
  ManDir <- file.path(DriveDir, "projects", "Manuscripts", "agriculture_malaria_manuscript")
  FigDir <- file.path(ManDir, "figures", "220623_new_figures")
  EviDir <- file.path(PopDir, "MAP_EVI_monthly")
  PrecDir <- file.path(PopDir, "chirps_monthly_precip")
  HumDir <- file.path(PopDir, "ERA5_rel_humidity_monthly")
  TempDir <- file.path(PopDir, "ERA5_temperature")
  OutDir <- file.path(PopDir, "analysis_dat")
} else {
  Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
  DriveDir <- file.path(Drive, 'Library', 'CloudStorage', 'OneDrive-NorthwesternUniversity', "urban_malaria")
  #DriveDir <- file.path(Drive,  "OneDrive - Northwestern University", "urban_malaria")
  PopDir <- file.path(DriveDir, "data", 'data_agric_analysis')
  ManDir <- file.path(DriveDir, "projects", "Manuscripts", "agriculture_malaria_manuscript")
  FigDir <- file.path(ManDir, "figures", "220623_new_figures")
  EviDir <- file.path(PopDir, "MAP_EVI_monthly")
  PrecDir <- file.path(PopDir, "chirps_monthly_precip")
  HumDir <- file.path(PopDir, "ERA5_rel_humidity_monthly")
  TempDir <- file.path(PopDir, "ERA5_temperature")
  OutDir <- file.path(PopDir, "analysis_dat")
}

## -----------------------------------------
### Required functions and settings
## -----------------------------------------
#note before sourcing functions and packages, run the code below to download rdhs if you don't already have it
#devtools::install_github("ropensci/rdhs", force = T)
source("functions/functions_employment.R")
options(survey.lonely.psu="adjust")  # this option allows admin units with only one cluster to be analyzed

## -----------------------------------------
### Reading and processing dhs gps coordinates
## -----------------------------------------

ge_files <-  list.files(path = file.path(PopDir, "DHS_coordinate"), 
                        pattern = "*FL.shp$", full.names = TRUE, recursive = T) 

all_GPS <- lapply(ge_files, st_read) %>% purrr::map(~mutate(., cntry_year = paste0(DHSCC, "_", DHSYEAR)))

#Assign names to SP dataframes in list 
cntry_years <- list(all_GPS %>% bind_rows() %>% as.data.frame() %>% select(cntry_year) %>%  unique() %>% 
                      mutate(cntry_year = ifelse(cntry_year == "SN_2012", "SN_2012a", cntry_year)), 
                    all_GPS %>% bind_rows() %>% as.data.frame() %>% select(cntry_year) %>%  unique() %>% 
                      mutate(cntry_year = ifelse(cntry_year == "SN_2012", "SN_2012b", cntry_year))) %>% 
  bind_rows()  %>%  unique() %>% arrange(cntry_year)

names(all_GPS) <- c(cntry_years$cntry_year)

## -----------------------------------------
### Reading and processing dhs datasets to obtain survey month
## -----------------------------------------
#read dhs pr files
pr_files <- list.files(path = file.path(PopDir, "data/opened/PR"), 
                       pattern = "*FL.DTA$", full.names = TRUE, recursive = F) 

pr_downloads <- lapply(pr_files, read_dta)
 

##processing by country _ attempt to shorten code 
# dhs_dat <- list()
# GPS_dat <- list()
# 
# for (i in 1:length(pr_downloads)){
#   dhs_all1 <- list(pr_downloads[[i]])
#   names(dhs_all1) <- paste0(str_sub(pr_downloads[[i]][1, "hv000"], 1, 2), "_", min(pr_downloads[[i]]$hv007))
#   print(names(dhs_all1))
#   names(dhs_all1) <- ifelse(names(dhs_all1) == "BJ_2011", "BJ_2012", names(dhs_all1))
#   names(dhs_all1) <- ifelse(names(dhs_all1) == "CI_2011", "CI_2012", names(dhs_all1))
#   names(dhs_all1) <- ifelse(names(dhs_all1) == "MR_2019", "MR_2020", names(dhs_all1))
#   dhs_all<- dhs_all1 %>%  map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
#     map(~distinct(.,)) #get cluster numbers by month and survey year
#   
#   survey_gps_dat <- survey_gps_comb(x= names(dhs_all1), y= names(dhs_all1))
#   for(i in seq_along(survey_gps_dat)) {names(survey_gps_dat)[[i]] <- paste0(unique(survey_gps_dat[[i]]$DHSCC), "_",
#     unique(survey_gps_dat[[i]]$hv007), '_', unique(survey_gps_dat[[i]]$hv006))}
#   
#   GPS_all <- sapply(c(survey_gps_dat), sf:::as_Spatial, simplify = F)
#   dhs_dat <- append(dhs_dat, dhs_all)
#   print(paste("appending", names(dhs_all), "to list of DHS dataframes"))
#   GPS_dat <- append(GPS_dat, GPS_all)
#   print(paste("appending", paste0(unique(GPS_all[[i]]$DHSCC),"_", unique(GPS_all[[i]]$DHSYEAR)), "to list of GPS dataframes"))
# }

# Angola - need to fix manual labeling
dhs_all1 <- list(pr_downloads[[1]])
names(dhs_all1) <- paste0(str_sub(pr_downloads[[1]][1, "hv000"], 1, 2), "_", min(pr_downloads[[1]]$hv007))
print(names(dhs_all1))

dhs_all <- dhs_all1 %>%  map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get cluster numbers by month and survey year

AO_2015 <- survey_gps_comb(x= names(dhs_all1), y= names(dhs_all1))
for(i in seq_along(AO_2015)) {names(AO_2015)[[i]] <- paste0(unique(AO_2015[[i]]$hv007), '_', unique(AO_2015[[i]]$hv006))}

GPS_all_OA <- sapply(c(AO_2015), sf:::as_Spatial, simplify = F)
names(GPS_all_OA) 
dhs_all_OA <- dhs_all


#Burkina Faso-
dhs_all1 <- list(pr_downloads[[2]], pr_downloads[[3]])
name1 <- paste0(str_sub(pr_downloads[[2]][1, "hv000"], 1, 2), "_", min(pr_downloads[[2]]$hv007))
name2 <- paste0(str_sub(pr_downloads[[3]][1, "hv000"], 1, 2), "_", min(pr_downloads[[3]]$hv007))
names(dhs_all1) <- c(name1, name2)
print(names(dhs_all1))

dhs_all <- dhs_all1 %>%  map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get cluster numbers by month and survey year

BF_2010 <- survey_gps_comb(x= name1, y= name1)
for(i in seq_along(BF_2010)) {names(BF_2010)[[i]] <- paste0(unique(BF_2010[[i]]$hv007), '_', unique(BF_2010[[i]]$hv006))}

BF_2021 <- survey_gps_comb(x= name2, y= name2)
for(i in seq_along(BF_2021)) {names(BF_2021)[[i]] <- paste0(unique(BF_2021[[i]]$hv007), '_', unique(BF_2021[[i]]$hv006))}

GPS_all_BF <- sapply(c(BF_2010, BF_2021), sf:::as_Spatial, simplify = F)
names(GPS_all_BF) 
dhs_all_BF <- dhs_all

# Benin
dhs_all1 <- list(pr_downloads[[4]], pr_downloads[[5]])
name1 <- paste0(str_sub(pr_downloads[[4]][1, "hv000"], 1, 2), "_", max(pr_downloads[[4]]$hv007))
name2 <- paste0(str_sub(pr_downloads[[5]][1, "hv000"], 1, 2), "_", min(pr_downloads[[5]]$hv007))
names(dhs_all1) <- c(name1, name2)
print(names(dhs_all1))


dhs_all <- dhs_all1 %>% map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get cluster numbers by month and survey year

BJ_2012 <- survey_gps_comb(x= name1, y= name1)
for(i in seq_along(BJ_2012)) {names(BJ_2012)[[i]] <- paste0(unique(BJ_2012[[i]]$hv007), '_', unique(BJ_2012[[i]]$hv006))}

BJ_2017 <- survey_gps_comb(x= name2, y= name2)
for(i in seq_along(BJ_2017)) {names(BJ_2017)[[i]] <- paste0(unique(BJ_2017[[i]]$hv007), '_', unique(BJ_2017[[i]]$hv006))}

GPS_all_BJ <- sapply(c(BJ_2012, BJ_2017), sf:::as_Spatial, simplify = F)
names(GPS_all_BJ) 
dhs_all_BJ <- dhs_all 


# Burundi
dhs_all1 <- list(pr_downloads[[6]])
names(dhs_all1) <- paste0(str_sub(pr_downloads[[6]][1, "hv000"], 1, 2), "_", min(pr_downloads[[6]]$hv007))
print(names(dhs_all1))

dhs_all <- dhs_all1 %>% map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get cluster numbers by month and survey year

BU_2016 <- survey_gps_comb(x= names(dhs_all1), y= names(dhs_all1))
for(i in seq_along(BU_2016)) {names(BU_2016)[[i]] <- paste0(unique(BU_2016[[i]]$hv007), '_', unique(BU_2016[[i]]$hv006))}

GPS_all_BU <- sapply(c(BU_2016), sf:::as_Spatial, simplify = F)
names(GPS_all_BU) 
dhs_all_BU <- dhs_all 

# DRC
dhs_all <- list(pr_downloads[[7]])
names(dhs_all) <- paste0(str_sub(pr_downloads[[7]][1, "hv000"], 1, 2), "_", min(pr_downloads[[7]]$hv007))
print(names(dhs_all))

dhs_all <- dhs_all %>% map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get cluster numbers by month and survey year

CD_2013 <- survey_gps_comb(x= "CD_2013", y= "CD_2013")
for(i in seq_along(CD_2013)) {names(CD_2013)[[i]] <- paste0(unique(CD_2013[[i]]$hv007), '_', unique(CD_2013[[i]]$hv006))}

GPS_all_CD <- sapply(c(CD_2013), sf:::as_Spatial, simplify = F)
names(GPS_all_CD) 
dhs_all_CD <- dhs_all 

#Cote d'Ivoire
dhs_all1 <- list(pr_downloads[[8]], pr_downloads[[9]])
name1 <- paste0(str_sub(pr_downloads[[8]][1, "hv000"], 1, 2), "_", max(pr_downloads[[8]]$hv007))
name2 <- paste0(str_sub(pr_downloads[[9]][1, "hv000"], 1, 2), "_", min(pr_downloads[[9]]$hv007))
names(dhs_all1) <- c(name1, name2)
print(names(dhs_all1))


dhs_all <- dhs_all1 %>% map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get cluster numbers by month and survey year

CI_2012 <- survey_gps_comb(x= name1, y= name1)
for(i in seq_along(CI_2012)) {names(CI_2012)[[i]] <- paste0(unique(CI_2012[[i]]$hv007), '_', unique(CI_2012[[i]]$hv006))}

CI_2021 <- survey_gps_comb(x= name2, y= name2)
for(i in seq_along(CI_2021)) {names(CI_2021)[[i]] <- paste0(unique(CI_2021[[i]]$hv007), '_', unique(CI_2021[[i]]$hv006))}

GPS_all_CI <- sapply(c(CI_2012, CI_2021), sf:::as_Spatial, simplify = F)
names(GPS_all_CI)
dhs_all_CI <- dhs_all


# Cameroon
dhs_all <- list(pr_downloads[[10]], pr_downloads[[11]])
name1 <- paste0(str_sub(pr_downloads[[10]][1, "hv000"], 1, 2), "_", max(pr_downloads[[10]]$hv007))
name2 <- paste0(str_sub(pr_downloads[[11]][1, "hv000"], 1, 2), "_", min(pr_downloads[[11]]$hv007))
names(dhs_all) <- c(name1, name2)
print(names(dhs_all))

dhs_all <- dhs_all %>%  map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get cluster numbers by month and survey year

CM_2011 <- survey_gps_comb(x= name1, y= name1)
for(i in seq_along(CM_2011)) {names(CM_2011)[[i]] <- paste0(unique(CM_2011[[i]]$hv007), '_', unique(CM_2011[[i]]$hv006))}

CM_2018 <- survey_gps_comb(x= name2, y= name2)
for(i in seq_along(CM_2018)) {names(CM_2018)[[i]] <- paste0(unique(CM_2018[[i]]$hv007), '_', unique(CM_2018[[i]]$hv006))}

GPS_all_CM <- sapply(c(CM_2011, CM_2018), sf:::as_Spatial, simplify = F)
names(GPS_all_CM) 
dhs_all_CM <- dhs_all 


# Ghana
dhs_all <- list(pr_downloads[[12]], pr_downloads[[13]])
name1 <- paste0(str_sub(pr_downloads[[12]][1, "hv000"], 1, 2), "_", min(pr_downloads[[12]]$hv007))
name2 <- paste0(str_sub(pr_downloads[[13]][1, "hv000"], 1, 2), "_", min(pr_downloads[[13]]$hv007))
names(dhs_all) <- c(name1, name2)
print(names(dhs_all))


dhs_all <- dhs_all %>%  map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get cluster numbers by month and survey year


GH_2014 <- survey_gps_comb(x= name1, y= name1)
for(i in seq_along(GH_2014)) {names(GH_2014)[[i]] <- paste0(unique(GH_2014[[i]]$hv007), '_', unique(GH_2014[[i]]$hv006))}

GH_2022 <- survey_gps_comb(x= name2, y= name2)
for(i in seq_along(GH_2022)) {names(GH_2022)[[i]] <- paste0(unique(GH_2022[[i]]$hv007), '_', unique(GH_2022[[i]]$hv006))}

GPS_all_GH <- sapply(c(GH_2014, GH_2022), sf:::as_Spatial, simplify = F)
names(GPS_all_GH) 
dhs_all_GH <- dhs_all 



# Gambia
#2013 GPS data not collected
dhs_all <- list(pr_downloads[[15]])
names(dhs_all) <- paste0(str_sub(pr_downloads[[15]][1, "hv000"], 1, 2), "_", min(pr_downloads[[15]]$hv007))
print(names(dhs_all))

dhs_all <- dhs_all %>%  map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get cluster numbers by month and survey year
dhs_all_GM <- dhs_all 

GM_2019 <- survey_gps_comb(x= names(dhs_all), y= names(dhs_all))
for(i in seq_along(GM_2019)) {names(GM_2019)[[i]] <- paste0(unique(GM_2019[[i]]$hv007), '_', unique(GM_2019[[i]]$hv006))}

GPS_all_GM <- sapply(c(GM_2019), sf:::as_Spatial, simplify = F)
names(GPS_all_GM) 

# Guinea
dhs_all <- list(pr_downloads[[16]])
names(dhs_all) <- paste0(str_sub(pr_downloads[[16]][1, "hv000"], 1, 2), "_", min(pr_downloads[[16]]$hv007))
print(names(dhs_all))

dhs_all <- dhs_all %>%  map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get cluster numbers by month and survey year
dhs_all_GN <- dhs_all 

GN_2012 <- survey_gps_comb(x= names(dhs_all), y= names(dhs_all))
for(i in seq_along(GN_2012)) {names(GN_2012)[[i]] <- paste0(unique(GN_2012[[i]]$hv007), '_', unique(GN_2012[[i]]$hv006))}

GPS_all_GN <- sapply(c(GN_2012), sf:::as_Spatial, simplify = F)
names(GPS_all_GN) 

# Madagascar 
dhs_all <- list(pr_downloads[[17]])
names(dhs_all) <- paste0(str_sub(pr_downloads[[17]][1, "hv000"], 1, 2), "_", min(pr_downloads[[17]]$hv007))
print(names(dhs_all))


dhs_all <- dhs_all %>%  map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get cluster numbers by month and survey year
dhs_all_MD <- dhs_all


MD_2021 <- survey_gps_comb(x= names(dhs_all), y= names(dhs_all))
for(i in seq_along(MD_2021)) {names(MD_2021)[[i]] <- paste0(unique(MD_2021[[i]]$hv007), '_', unique(MD_2021[[i]]$hv006))}


GPS_all_MD <- sapply(c(MD_2021), sf:::as_Spatial, simplify = F)
names(GPS_all_MD) 

# Mali
dhs_all <- list(pr_downloads[[18]], pr_downloads[[19]])
name1 <- paste0(str_sub(pr_downloads[[18]][1, "hv000"], 1, 2), "_", min(pr_downloads[[18]]$hv007))
name2 <- paste0(str_sub(pr_downloads[[19]][1, "hv000"], 1, 2), "_", min(pr_downloads[[19]]$hv007))
names(dhs_all) <- c(name1, name2)


dhs_all <- dhs_all %>%  map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get cluster numbers by month and survey year
dhs_all_ML <- dhs_all

ML_2012 <- survey_gps_comb(x= name1, y= name1)
for(i in seq_along(ML_2012)) {names(ML_2012)[[i]] <- paste0(unique(ML_2012[[i]]$hv007), '_', unique(ML_2012[[i]]$hv006))}


ML_2018 <- survey_gps_comb(x= name2, y= name2)
for(i in seq_along(ML_2018)) {names(ML_2018)[[i]] <- paste0(unique(ML_2018[[i]]$hv007), '_', unique(ML_2018[[i]]$hv006))}

GPS_all_ML <- sapply(c(ML_2012, ML_2018), sf:::as_Spatial, simplify = F)
names(GPS_all_ML) 


# Mauritania
dhs_all <- list(pr_downloads[[20]])
names(dhs_all) <- paste0(str_sub(pr_downloads[[20]][1, "hv000"], 1, 2), "_", median(pr_downloads[[20]]$hv007))


dhs_all <- dhs_all %>%  map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get cluster numbers by month and survey year
dhs_all_MR <- dhs_all


MR_2020 <- survey_gps_comb(x= names(dhs_all), y= names(dhs_all))
for(i in seq_along(MR_2020)) {names(MR_2020)[[i]] <- paste0(unique(MR_2020[[i]]$hv007), '_', unique(MR_2020[[i]]$hv006))}


GPS_all_MR <- sapply(c(MR_2020), sf:::as_Spatial, simplify = F)
names(GPS_all_MR) 

# Mozambique
dhs_all <- list(pr_downloads[[21]], pr_downloads[[22]], pr_downloads[[23]])
name1 <- paste0(str_sub(pr_downloads[[21]][1, "hv000"], 1, 2), "_", min(pr_downloads[[21]]$hv007))
name2 <- paste0(str_sub(pr_downloads[[22]][1, "hv000"], 1, 2), "_", min(pr_downloads[[22]]$hv007))
name3 <- paste0(str_sub(pr_downloads[[23]][1, "hv000"], 1, 2), "_", min(pr_downloads[[23]]$hv007))
names(dhs_all) <- c(name1, name2, name3)

dhs_all <- dhs_all %>%  map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get cluster numbers by month and survey year
dhs_all_MZ <- dhs_all

MZ_2011 <- survey_gps_comb(x= name1, y= name1)
for(i in seq_along(MZ_2011)) {names(MZ_2011)[[i]] <- paste0(unique(MZ_2011[[i]]$hv007), '_', unique(MZ_2011[[i]]$hv006))}

MZ_2015 <- survey_gps_comb(x= name2, y= name2)
MZ_2015 <- MZ_2015[1:5] #Removed one list which contained one cluster (93) that is'nt available in the PR dataset
for(i in seq_along(MZ_2015)) {names(MZ_2015)[[i]] <- paste0(unique(MZ_2015[[i]]$hv007), '_', unique(MZ_2015[[i]]$hv006))}

MZ_2022 <- survey_gps_comb(x= name3, y= name3)
for(i in seq_along(MZ_2022)) {names(MZ_2022)[[i]] <- paste0(unique(MZ_2022[[i]]$hv007), '_', unique(MZ_2022[[i]]$hv006))}


GPS_all_MZ <- sapply(c(MZ_2011, MZ_2015, MZ_2022), sf:::as_Spatial, simplify = F)
names(GPS_all_MZ) #stopped here. Not sure where the NA is coming from 

#Nigeria
dhs_all <- list(pr_downloads[[24]])
names(dhs_all) <- paste0(str_sub(pr_downloads[[24]][1, "hv000"], 1, 2), "_", min(pr_downloads[[24]]$hv007))
print(names(dhs_all))

#read GPS data in: read_GPS_cluster_shapefiles.R

dhs_all <- dhs_all %>%  map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get cluster numbers by month and survey year
dhs_all_NG <- dhs_all

NG_2018 <- survey_gps_comb(x= names(dhs_all), y= names(dhs_all))
for(i in seq_along(NG_2018)) {names(NG_2018)[[i]] <- paste0(unique(NG_2018[[i]]$hv007), '_', unique(NG_2018[[i]]$hv006))}

GPS_all_NG <- sapply(c(NG_2018), sf:::as_Spatial, simplify = F)
names(GPS_all_NG) # Clusters by survey month= GPS data points


# Rwanda -- No GPS coordinates for clusters were provided for the 2017 survey - stopped here
dhs_all <- list(pr_downloads[[25]], pr_downloads[[26]], pr_downloads[[27]])
name1 <- paste0(str_sub(pr_downloads[[25]][1, "hv000"], 1, 2), "_", min(pr_downloads[[25]]$hv007))
name2 <- paste0(str_sub(pr_downloads[[26]][1, "hv000"], 1, 2), "_", min(pr_downloads[[26]]$hv007))
name3 <- paste0(str_sub(pr_downloads[[27]][1, "hv000"], 1, 2), "_", min(pr_downloads[[27]]$hv007))
names(dhs_all) <- c(name1, name2, name3)
print(names(dhs_all))


dhs_all <- dhs_all %>%  map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get cluster numbers by month and survey year
dhs_all_RW <- dhs_all

RW_2010 <- survey_gps_comb(x= name1, y= name1)
for(i in seq_along(RW_2010)) {names(RW_2010)[[i]] <- paste0(unique(RW_2010[[i]]$hv007), '_', unique(RW_2010[[i]]$hv006))}

RW_2014 <- survey_gps_comb(x= name2, y= name2)
for(i in seq_along(RW_2014)) {names(RW_2014)[[i]] <- paste0(unique(RW_2014[[i]]$hv007), '_', unique(RW_2014[[i]]$hv006))}

RW_2019 <- survey_gps_comb(x= name3, y= name3)
for(i in seq_along(RW_2019)) {names(RW_2019)[[i]] <- paste0(unique(RW_2019[[i]]$hv007), '_', unique(RW_2019[[i]]$hv006))}

GPS_all_RW <- sapply(c(RW_2010, RW_2014, RW_2019), sf:::as_Spatial, simplify = F)
names(GPS_all_RW)

# Senegal
dhs_all <- list(pr_downloads[[28]], pr_downloads[[29]], pr_downloads[[30]], pr_downloads[[31]], pr_downloads[[32]], pr_downloads[[33]])
name1 <- paste0(str_sub(pr_downloads[[28]][1, "hv000"], 1, 2), "_", min(pr_downloads[[28]]$hv007))
#name2 <- paste0(str_sub(pr_downloads[[29]][1, "hv000"], 1, 2), "_", min(pr_downloads[[29]]$hv007))
#name3 <- paste0(str_sub(pr_downloads[[30]][1, "hv000"], 1, 2), "_", max(pr_downloads[[30]]$hv007))
name4 <- paste0(str_sub(pr_downloads[[31]][1, "hv000"], 1, 2), "_", min(pr_downloads[[31]]$hv007))
name5 <- paste0(str_sub(pr_downloads[[32]][1, "hv000"], 1, 2), "_", min(pr_downloads[[32]]$hv007))
name6 <- paste0(str_sub(pr_downloads[[33]][1, "hv000"], 1, 2), "_", min(pr_downloads[[33]]$hv007))


names(dhs_all) <- c(name1, name4, name5, name6)
print(names(dhs_all))

#names(dhs_all) <- c("SN_2010", "SN_2012a", "SN_2012b", "SN_2014", "SN_2015", "SN_2016", "SN_2017")

dhs_all <- dhs_all %>%  map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get cluster numbers by month and survey year
dhs_all_SN <- dhs_all

dhs_all_SN[[6]] <- NULL
dhs_all_SN[[5]] <- NULL

SN_2010 <- survey_gps_comb(x= name1, y= name1)
for(i in seq_along(SN_2010)) {names(SN_2010)[[i]] <- paste0(unique(SN_2010[[i]]$hv007), '_', unique(SN_2010[[i]]$hv006))}

#SN_2012a <- survey_gps_comb(x= "SN_2012a", y= "SN_2012a")
#for(i in seq_along(SN_2012a)) {names(SN_2012a)[[i]] <- paste0(unique(SN_2012a[[i]]$hv007), '_', unique(SN_2012a[[i]]$hv006))}

# SN_2012 <- survey_gps_comb(x= name2, y= name2)
# for(i in seq_along(SN_2012)) {names(SN_2012)[[i]] <- paste0(unique(SN_2012[[i]]$hv007), '_', unique(SN_2012[[i]]$hv006))}

# SN_2014 <- survey_gps_comb(x= name3, y= name3)
# for(i in seq_along(SN_2014)) {names(SN_2014)[[i]] <- paste0(unique(SN_2014[[i]]$hv007), '_', unique(SN_2014[[i]]$hv006))}

SN_2015 <- survey_gps_comb(x= name4, y= name4)
for(i in seq_along(SN_2015)) {names(SN_2015)[[i]] <- paste0(unique(SN_2015[[i]]$hv007), '_', unique(SN_2015[[i]]$hv006))}
SN_2015[[10]] <- NULL

SN_2016 <- survey_gps_comb(x=  name5, y=  name5)
for(i in seq_along(SN_2016)) {names(SN_2016)[[i]] <- paste0(unique(SN_2016[[i]]$hv007), '_', unique(SN_2016[[i]]$hv006))}
SN_2016[[11]] <- NULL


SN_2017 <- survey_gps_comb(x= name6, y= name6)
for(i in seq_along(SN_2017)) {names(SN_2017)[[i]] <- paste0(unique(SN_2017[[i]]$hv007), '_', unique(SN_2017[[i]]$hv006))}
SN_2017[[10]] <- NULL

GPS_all_SN <- sapply(c(SN_2010, SN_2015, SN_2016, SN_2017), sf:::as_Spatial, simplify = F)
names(GPS_all_SN)

# Togo 
dhs_all <- list(pr_downloads[[34]])
names(dhs_all) <- paste0(str_sub(pr_downloads[[34]][1, "hv000"], 1, 2), "_", min(pr_downloads[[34]]$hv007))
print(names(dhs_all))


dhs_all <- dhs_all %>% map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get cluster numbers by month and survey year
dhs_all_TG <- dhs_all

TG_2013 <- survey_gps_comb(x= names(dhs_all), y= names(dhs_all))
for(i in seq_along(TG_2013)) {names(TG_2013)[[i]] <- paste0(unique(TG_2013[[i]]$hv007), '_', unique(TG_2013[[i]]$hv006))}

GPS_all_TG <- sapply(c(TG_2013), sf:::as_Spatial, simplify = F)
names(GPS_all_TG)

# Tanzania 
dhs_all <- list(pr_downloads[[36]], pr_downloads[[37]])
names1 <- paste0(str_sub(pr_downloads[[36]][1, "hv000"], 1, 2), "_", max(pr_downloads[[36]]$hv007))
names2 <- paste0(str_sub(pr_downloads[[37]][1, "hv000"], 1, 2), "_", min(pr_downloads[[37]]$hv007))
names(dhs_all) <- c(names1, names2)
print(names(dhs_all))


dhs_all <- dhs_all %>%  map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get cluster numbers by month and survey year
dhs_all_TZ <- dhs_all

TZ_2012 <- survey_gps_comb(x= names1, y= names1)
for(i in seq_along(TZ_2012)) {names(TZ_2012)[[i]] <- paste0(unique(TZ_2012[[i]]$hv007), '_', unique(TZ_2012[[i]]$hv006))}

TZ_2015 <- survey_gps_comb(x= names2, y= names2)
for(i in seq_along(TZ_2015)) {names(TZ_2015)[[i]] <- paste0(unique(TZ_2015[[i]]$hv007), '_', unique(TZ_2015[[i]]$hv006))}

GPS_all_TZ <- sapply(c(TZ_2012, TZ_2015), sf:::as_Spatial, simplify = F)
names(GPS_all_TZ)

# Uganda
dhs_all <- list(pr_downloads[[39]], pr_downloads[[40]])
names1 <- paste0(str_sub(pr_downloads[[39]][1, "hv000"], 1, 2), "_", min(pr_downloads[[39]]$hv007))
names2 <- paste0(str_sub(pr_downloads[[40]][1, "hv000"], 1, 2), "_", min(pr_downloads[[40]]$hv007))
names(dhs_all) <- c(names1, names2)
print(names(dhs_all))


dhs_all <- dhs_all %>%  map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get cluster numbers by month and survey year
dhs_all_UG <- dhs_all
dhs_all_UG[[1]]<- NULL

UG_2009 <- survey_gps_comb(x= names1, y= names1)
for(i in seq_along(UG_2009)) {names(UG_2009)[[i]] <- paste0(unique(UG_2009[[i]]$hv007), '_', unique(UG_2009[[i]]$hv006))}

UG_2016 <- survey_gps_comb(x=names2, y=names2)
for(i in seq_along(UG_2016)) {names(UG_2016)[[i]] <- paste0(unique(UG_2016[[i]]$hv007), '_', unique(UG_2016[[i]]$hv006))}

GPS_all_UG <- sapply(c(UG_2016), sf:::as_Spatial, simplify = F)
names(GPS_all_UG)


#### Data extraction parameter lists 
GPS_all <- list(GPS_all_OA, GPS_all_BF, GPS_all_BJ, GPS_all_BU, GPS_all_CD, GPS_all_CI,  
                GPS_all_CM, GPS_all_GH, GPS_all_GM, GPS_all_GN, GPS_all_MD, GPS_all_ML, GPS_all_MR,
                GPS_all_MZ, GPS_all_NG, GPS_all_RW, GPS_all_SN, GPS_all_TG, GPS_all_TZ, GPS_all_UG)

dhs_all <- list(dhs_all_OA, dhs_all_BF, dhs_all_BJ, dhs_all_BU, dhs_all_CD, dhs_all_CI,  
                dhs_all_CM, dhs_all_GH, dhs_all_GM, dhs_all_GN, dhs_all_MD, dhs_all_ML, dhs_all_MR,
                dhs_all_MZ, dhs_all_NG, dhs_all_RW, dhs_all_SN, dhs_all_TG, dhs_all_TZ, dhs_all_UG)


# buffers of interest

vars <- c(2000) #meters

#Extracting EVI using list within a list of survey gps points: This may take some time to run
df_list_evi <- list()
for (i in 1:length(GPS_all)) {
  
  #EVI rasters- this is the Part where the lag comes in! 
  EVI_comb <- lapply(dhs_all[[i]], pick_month, filepath= EviDir)
  EVI_comb_vc <- unlist(EVI_comb) #vector
  EVI_raster_all <- sapply(EVI_comb_vc, raster, simplify = F) #read in EVI raster files, with 2 month lag
  
  #for (i in 1:length(vars)) {
    var_name <- paste0('EVI_', as.character(vars[1]), 'm')
    df <- map2(GPS_all[[i]], EVI_raster_all, get_crs) 
    df <- pmap(list(EVI_raster_all, df, vars[1]), extract_fun_month) 
    df <- df %>%  map(~rename_with(., .fn=~paste0(var_name), .cols = contains('EVI')))
    df <- plyr::ldply(df) %>% dplyr::select(-c(ID)) 
    df <- df %>% arrange(month) %>%  group_by(dhs_year, hv001) %>%  slice(1) #get data for the first month if more than one survey month in a cluster
    
  #}
  df_list_evi[[i]] <- df
  df_binded_EVI <- df_list_evi %>% bind_rows()
  write.csv(df_binded_EVI, file = file.path(OutDir, paste0("EVI_DHS.csv")),row.names = FALSE)
}



#precipitation CHIRPS

df_list_prec <- list()
for (i in 1:length(GPS_all)) {
  
  #precip rasters- this is the Part where the lag comes in! 
  precip_comb <- lapply(dhs_all[[i]], pick_month, filepath= PrecDir)
  precip_comb_vc <- unlist(precip_comb) #vector
  prec_raster_all <- sapply(precip_comb_vc, raster, simplify = F) #read in precip raster files, with 2 month lag
  
  #for (i in 1:length(vars)) {
    var_name <- paste0('preci_monthly_', as.character(vars[1]), 'm')
    df <- map2(GPS_all[[i]], prec_raster_all, get_crs) #list of 13 vs. list of 144 
    df <- pmap(list(prec_raster_all, df, vars[1]), extract_fun_month) 
    df <- df %>%  map(~rename_with(., .fn=~paste0(var_name), .cols = contains('chirps')))
    df <- plyr::ldply(df) %>% dplyr::select(-c(ID)) 
    df <- df %>% arrange(month) %>%  group_by(dhs_year, hv001) %>%  slice(1) #get data for the first month if more than one survey month in a cluster
    
  #}
  df_list_prec[[i]] <- df
  df_binded_precip <- df_list_prec %>% bind_rows()%>% mutate(preci_monthly_2000m = ifelse(preci_monthly_2000m < 0, NA, preci_monthly_2000m)) #Getting rid of negative values
  write.csv(df_binded_precip, file = file.path(OutDir, paste0("precip_DHS.csv")),row.names = FALSE)
}



#### READ IN RASTER DATA- Relative Humidity at 1 atm ####
## Using a 2-month lag for relative humidity data

# 2009- 2023
list_RH <- list(humidity_2009 <- brick(file.path(HumDir, 'rel_humidity_2009.grib')),
                humidity_2010 <- brick(file.path(HumDir, 'rel_humidity_2010.grib')),
                humidity_2011 <- brick(file.path(HumDir, 'rel_humidity_2011.grib')),
                humidity_2012 <- brick(file.path(HumDir, 'rel_humidity_2012.grib')),
                humidity_2013 <- brick(file.path(HumDir, 'rel_humidity_2013.grib')),
                humidity_2014 <- brick(file.path(HumDir, 'rel_humidity_2014.grib')),
                humidity_2015 <- brick(file.path(HumDir, 'rel_humidity_2015.grib')),
                humidity_2016 <- brick(file.path(HumDir, 'rel_humidity_2016.grib')),
                humidity_2017 <- brick(file.path(HumDir, 'rel_humidity_2017.grib')),
                humidity_2018 <- brick(file.path(HumDir, 'rel_humidity_2018.grib')),
                humidity_2019 <- brick(file.path(HumDir, 'rel_humidity_2019.grib')),
                humidity_2020 <- brick(file.path(HumDir, 'rel_humidity_2020.grib')),
                humidity_2021 <- brick(file.path(HumDir, 'rel_humidity_2021.grib')),
                humidity_2022 <- brick(file.path(HumDir, 'rel_humidity_2022.grib')),
                humidity_2023 <- brick(file.path(HumDir, 'rel_humidity_2023.grib'))
                )


for (i in 1:length(list_RH)){
  names(list_RH[[i]]) <- paste0("RH_", month.abb) #redo for each file
}

nlayers(list_RH[[14]])

plot(list_RH[[1]], 1) #Visually inspecting Relative humidity- Jan 2010

names(list_RH) <- c("2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", 
                    "2017", "2018", "2019","2020", "2021", "2022", "2023")


#Apply over dhs_all list--- Repeat for each country

df_list_RH <- list()

for (k in 1:length(dhs_all)){
  
  RH_files <- lapply(dhs_all[[k]], get_month_str_RH)
  
  for (i in 1:length(RH_files)){
    RH_files[[i]] <- RH_files[[i]] %>% 
      pmap(~pick_files_RH(.y, .x)) #pick up month_lag and year to select the correct humidity raster files
  }
  
  raster_all <- unlist(RH_files)
  
  #for (i in 1:length(vars)) {
    var_name <- paste0('RH_monthly_', as.character(vars[1]), 'm')
    df <- map2(GPS_all[[k]], raster_all, get_crs)  #transform GPS coords to match raster projection
    df <- pmap(list(raster_all, df, vars[1]), extract_fun_month)
    df <- df %>% map(~rename_with(., .fn=~paste0(var_name), .cols = contains('RH')))
    df <- plyr::ldply(df)%>% dplyr::select(-c(ID))
    df <- df %>% arrange(month) %>%  group_by(dhs_year, hv001) %>%  slice(1)
  #}
  
  df_list_RH[[k]] <- df
  df_binded_RH <- df_list_RH %>% bind_rows()
  write.csv(df_binded_RH, file = file.path(OutDir, paste0("RH_monthly_DHS.csv")),row.names = FALSE)
}


#### READ IN RASTER DATA- Temperature ####
## Using a 2-month lag for temperature data

# 2009- 2023
list_temp <- list(temp_2009 <- brick(file.path(TempDir, 'temp_2009.grib')),
                  temp_2010 <- brick(file.path(TempDir, 'temp_2010.grib')),
                  temp_2011 <- brick(file.path(TempDir, 'temp_2011.grib')),
                  temp_2012 <- brick(file.path(TempDir, 'temp_2012.grib')),
                  temp_2013 <- brick(file.path(TempDir, 'temp_2013.grib')),
                  temp_2014 <- brick(file.path(TempDir, 'temp_2014.grib')),
                  temp_2015 <- brick(file.path(TempDir, 'temp_2015.grib')),
                  temp_2016 <- brick(file.path(TempDir, 'temp_2016.grib')),
                  temp_2017 <- brick(file.path(TempDir, 'temp_2017.grib')),
                  temp_2018 <- brick(file.path(TempDir, 'temp_2018.grib')),
                  temp_2019 <- brick(file.path(TempDir, 'temp_2019.grib')),
                  temp_2020 <- brick(file.path(TempDir, 'temp_2020.grib')),
                  temp_2021 <- brick(file.path(TempDir, 'temp_2021.grib')),
                  temp_2022 <- brick(file.path(TempDir, 'temp_2022.grib')),
                  temp_2023 <- brick(file.path(TempDir, 'temp_2023.grib'))
                  )


for (i in 1:length(list_temp)){
  names(list_temp[[i]]) <- paste0("temp_", month.abb) #redo for each file
}

nlayers(list_temp[[1]])

plot(list_temp[[1]], 1) #Visually inspecting Relative humidity- Jan 2010

names(list_temp) <- c("2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", 
                    "2017", "2018", "2019","2020", "2021", "2022", "2023")


#Apply over dhs_all list--- Repeat for each country

df_list_temp <- list()

for (k in 1:length(dhs_all)){
  
  temp_files <- lapply(dhs_all[[k]], get_month_str_RH)
  
  for (i in 1:length(temp_files)){
    temp_files[[i]] <- temp_files[[i]] %>% 
      pmap(~pick_files_temp(.y, .x)) #pick up month_lag and year to select the correct humidity raster files
  }
  
  raster_all <- unlist(temp_files)
  
  #for (i in 1:length(vars)) {
    var_name <- paste0('temp_monthly_', as.character(vars[1]), 'm')
    df <- map2(GPS_all[[k]], raster_all, get_crs)  #transform GPS coords to match raster projection
    df <- pmap(list(raster_all, df, vars[1]), extract_fun_month)
    df <- df %>% map(~rename_with(., .fn=~paste0(var_name), .cols = contains('temp')))
    df <- plyr::ldply(df)%>% dplyr::select(-c(ID))
    df <- df %>% arrange(month) %>%  group_by(dhs_year, hv001) %>%  slice(1)
  #}
  
  df_list_temp[[k]] <- df
  df_binded_temp <- df_list_temp %>% bind_rows()
  write.csv(df_binded_temp, file = file.path(OutDir, paste0("temp_monthly_DHS.csv")),row.names = FALSE)
}


### Merging all environemnt variables
df_binded_EVI <- read.csv(file.path(OutDir, "EVI_DHS.csv"))
df_binded_precip <- read.csv(file.path(OutDir, "precip_DHS.csv"))
df_binded_RH  <- read.csv(file.path(OutDir, "RH_monthly_DHS.csv"))
df_binded_temp <- read.csv(file.path(OutDir, "temp_monthly_DHS.csv"))

merged_df <- df_binded_EVI %>% left_join(df_binded_precip, by = c()) %>% 
  left_join(df_binded_RH, by = c()) %>% left_join(df_binded_temp, by = c()) %>% 
  mutate(temp_monthly_2000m = ifelse(grepl("GH_2022|MZ_2022", .id), temp_monthly_2000m, temp_monthly_2000m - 273.15)) # converting temp from kelvin to degrees Celsius 


write.csv(merged_df, file = file.path(OutDir, paste0("all_geospatial_monthly_DHS.csv")),row.names = FALSE)
#END