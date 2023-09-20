################################################

#### READ in ENVIRONMENTAL DATA PER CLUSTER ####
###  Author: Colleen Leonard

################################################

#Filepaths
EVI_dir <- "C:/Users/Colleen/NU-malaria-team Dropbox/data/africa_health_district_climate/climate/africa/MAP_EVI_monthly"
precip_dir <- "C:/Users/Colleen/NU-malaria-team Dropbox/data/africa_health_district_climate/climate/africa/chirps_monthly_precip"
rasterDir <- file.path(NuDir, "data", 'Urban_malaria_net_ownership_data', 'Rasters')
HumidityDataDir <- file.path(rasterDir, 'ERA5_rel_humidity_monthly')
GeoDir <- "C:/Users/Colleen/NU-malaria-team Dropbox/data/Urban_malaria_net_ownership_data/Extracted_csv/Objective_3/EVI"
GeoDir2 <- "C:/Users/Colleen/NU-malaria-team Dropbox/data/Urban_malaria_net_ownership_data/Extracted_csv/Objective_3/Precip"
GeoDir3 <- "C:/Users/Colleen/NU-malaria-team Dropbox/data/Urban_malaria_net_ownership_data/Extracted_csv/Objective_3/Relative humidity"

library(sf)
library(raster)
library(janitor)

## Functions
get_crs <- function(df, raster){
  dhs <- spTransform(x = df, CRSobj = crs(raster))
}


extract_fun <- function(raster, dhs, buffer){
  clu_val<-raster::extract(raster,dhs, buffer = buffer, fun = mean, df =TRUE) %>%
    mutate(dhs_year = dhs$DHSYEAR)%>%
    mutate(hv001 = dhs$DHSCLUST) 
}


extract_fun_month <- function(raster, dhs, buffer){
  clu_val<-raster::extract(raster,dhs, buffer = buffer, fun = mean, df =TRUE) %>%
    mutate(dhs_year = dhs$DHSYEAR, hv001 = dhs$DHSCLUST, month = dhs$hv006)
}

survey_gps_comb <- function(x, y){
  survey1 <- left_join(st_as_sf(all_GPS[[x]]), dhs_all[[y]], by = c("DHSCLUST"="hv001")) %>% 
    group_split(hv007, hv006) 
}


# Pick correct survey year and months from EVI and Precipitation data files- with 2 month lag
get_month_str <- function(file){
  file %>% 
    filter(hv025==1) %>% 
    dplyr::select(hv006, hv007) %>% 
    mutate(mo= hv006- 2,
           month_lag= if_else(mo< 10, str_c(".0", mo), str_c(".", mo))) %>% 
    mutate(month_lag= if_else(month_lag== ".0-1", ".11", #nov
                              if_else(month_lag== ".00", ".12", month_lag)), #dec 
           year= if_else(month_lag %in% c(".11", ".12"), hv007-1, hv007)) %>% #nov and dec (already lagged by 2mo) should be the year prior to interview year
    dplyr::select(month_lag, year) %>% 
    group_by(month_lag, year) %>% 
    slice(1)
} 


pick_month <- function(file, filepath){
  
  EVI_files <- list.files(path = filepath, pattern = ".tif$", full.names = TRUE, recursive = FALSE) # pull in files
  month_lag <- get_month_str(file) %>%  #get months and year with 2 month lag
    mutate(year_mo= paste0(year,month_lag))
  vect <- month_lag$year_mo
  EVI_files1 <- EVI_files[(grep(paste(vect, collapse="|"), EVI_files))]
  
}

get_month_str_RH <- function(file){
  file %>% 
    filter(hv025==1) %>% 
    dplyr::select(hv006, hv007) %>% 
    mutate(month_lag= hv006- 2) %>% 
    mutate(month_lag= if_else(month_lag== -1, 11, #nov
                              if_else(month_lag== 0, 12, month_lag)), #dec 
           year= if_else(month_lag %in% c(11, 12), hv007-1, hv007)) %>% #nov and dec (already lagged by 2mo) should be the year prior to interview year
    dplyr::select(month_lag, year) %>% 
    mutate(month_lag= as.numeric(month_lag), year= as.numeric(year)) %>% 
    group_by(year, month_lag) %>% 
    slice(1)
} 

#Select the rasterbrick files needed
pick_files_RH <- function(year, month_lag){
  x <- list_RH[[as.character(year)]]
  
  RH_file <- x[[month_lag]]
  return(RH_file)
}

#DHS clusters by year and month of sampling

#Run country by country 
#read in the PR files

dhs_all <- list(NG_2010_mis, NG_2015_mis, NG_2018_dhs, NG_2021_mis)
names(dhs_all) <- c("NG_2010", "NG_2015", "NG_2018", "NG_2021")

#read GPS data in: read_GPS_cluster_shapefiles.R

dhs_all <- dhs_all %>% map(~filter(., hv025 == 1)) %>%  map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get Urban cluster numbers by month and survey year


#Nigeria
NG_2010 <- survey_gps_comb(x= "NG_2010", y= "NG_2010")
for(i in seq_along(NG_2010)) {names(NG_2010)[[i]] <- paste0(unique(NG_2010[[i]]$hv007), '_', unique(NG_2010[[i]]$hv006))}

NG_2015 <- survey_gps_comb(x= "NG_2015", y= "NG_2015")
for(i in seq_along(NG_2015)) {names(NG_2015)[[i]] <- paste0(unique(NG_2015[[i]]$hv007), '_', unique(NG_2015[[i]]$hv006))}

NG_2018 <- survey_gps_comb(x= "NG_2018", y= "NG_2018")
for(i in seq_along(NG_2018)) {names(NG_2018)[[i]] <- paste0(unique(NG_2018[[i]]$hv007), '_', unique(NG_2018[[i]]$hv006))}

NG_2021 <- survey_gps_comb(x= "NG_2021", y= "NG_2021")
for(i in seq_along(NG_2021)) {names(NG_2021)[[i]] <- paste0(unique(NG_2021[[i]]$hv007), '_', unique(NG_2021[[i]]$hv006))}


GPS_all <- sapply(c(NG_2010, NG_2015, NG_2018, NG_2021), sf:::as_Spatial, simplify = F)
names(GPS_all) # Urban Clusters by survey month= GPS data points

# Angola
dhs_all <- list(AO_2011_mis, AO_2015_dhs)
names(dhs_all) <- c("AO_2011", "AO_2015")

dhs_all <- dhs_all %>% map(~filter(., hv025 == 1)) %>%  map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get Urban cluster numbers by month and survey year

AO_2011 <- survey_gps_comb(x= "AO_2011", y= "AO_2011")
for(i in seq_along(AO_2011)) {names(AO_2011)[[i]] <- paste0(unique(AO_2011[[i]]$hv007), '_', unique(AO_2011[[i]]$hv006))}

AO_2015 <- survey_gps_comb(x= "AO_2015", y= "AO_2015")
for(i in seq_along(AO_2015)) {names(AO_2015)[[i]] <- paste0(unique(AO_2015[[i]]$hv007), '_', unique(AO_2015[[i]]$hv006))}

GPS_all <- sapply(c(AO_2011, AO_2015), sf:::as_Spatial, simplify = F)
names(GPS_all) 

# Benin
dhs_all <- list(BJ_2012_dhs, BJ_2017_dhs)
names(dhs_all) <- c("BJ_2012", "BJ_2017")

dhs_all <- dhs_all %>% map(~filter(., hv025 == 1)) %>%  map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get Urban cluster numbers by month and survey year

BJ_2012 <- survey_gps_comb(x= "BJ_2012", y= "BJ_2012")
for(i in seq_along(BJ_2012)) {names(BJ_2012)[[i]] <- paste0(unique(BJ_2012[[i]]$hv007), '_', unique(BJ_2012[[i]]$hv006))}

BJ_2017 <- survey_gps_comb(x= "BJ_2017", y= "BJ_2017")
for(i in seq_along(BJ_2017)) {names(BJ_2017)[[i]] <- paste0(unique(BJ_2017[[i]]$hv007), '_', unique(BJ_2017[[i]]$hv006))}

GPS_all <- sapply(c(BJ_2012, BJ_2017), sf:::as_Spatial, simplify = F)
names(GPS_all) 


#Burkina Faso-
dhs_all <- list(BF_2010_dhs, BF_2014_dhs, BF_2017_dhs)
names(dhs_all) <- c("BF_2010", "BF_2014", "BF_2017")

dhs_all <- dhs_all %>% map(~filter(., hv025 == 1)) %>%  map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get Urban cluster numbers by month and survey year

BF_2010 <- survey_gps_comb(x= "BF_2010", y= "BF_2010")
for(i in seq_along(BF_2010)) {names(BF_2010)[[i]] <- paste0(unique(BF_2010[[i]]$hv007), '_', unique(BF_2010[[i]]$hv006))}

BF_2014 <- survey_gps_comb(x= "BF_2014", y= "BF_2014")
for(i in seq_along(BF_2014)) {names(BF_2014)[[i]] <- paste0(unique(BF_2014[[i]]$hv007), '_', unique(BF_2014[[i]]$hv006))}

BF_2017 <- survey_gps_comb(x= "BF_2017", y= "BF_2017")
for(i in seq_along(BF_2017)) {names(BF_2017)[[i]] <- paste0(unique(BF_2017[[i]]$hv007), '_', unique(BF_2017[[i]]$hv006))}

GPS_all <- sapply(c(BF_2010, BF_2014, BF_2017), sf:::as_Spatial, simplify = F)


# Burundi
dhs_all <- list(BU_2012_mis, BU_2016_dhs)
names(dhs_all) <- c("BU_2012", "BU_2016")

dhs_all <- dhs_all %>% map(~filter(., hv025 == 1)) %>%  map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get Urban cluster numbers by month and survey year

BU_2012 <- survey_gps_comb(x= "BU_2012", y= "BU_2012")
for(i in seq_along(BU_2012)) {names(BU_2012)[[i]] <- paste0(unique(BU_2012[[i]]$hv007), '_', unique(BU_2012[[i]]$hv006))}

BU_2016 <- survey_gps_comb(x= "BU_2016", y= "BU_2016")
for(i in seq_along(BU_2016)) {names(BU_2016)[[i]] <- paste0(unique(BU_2016[[i]]$hv007), '_', unique(BU_2016[[i]]$hv006))}

GPS_all <- sapply(c(BU_2012, BU_2016), sf:::as_Spatial, simplify = F)


# Cameroon
dhs_all <- list(CM_2011_dhs, CM_2018_dhs)
names(dhs_all) <- c("CM_2011", "CM_2018")

dhs_all <- dhs_all %>% map(~filter(., hv025 == 1)) %>%  map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get Urban cluster numbers by month and survey year

CM_2011 <- survey_gps_comb(x= "CM_2011", y= "CM_2011")
for(i in seq_along(CM_2011)) {names(CM_2011)[[i]] <- paste0(unique(CM_2011[[i]]$hv007), '_', unique(CM_2011[[i]]$hv006))}

CM_2018 <- survey_gps_comb(x= "CM_2018", y= "CM_2018")
for(i in seq_along(CM_2018)) {names(CM_2018)[[i]] <- paste0(unique(CM_2018[[i]]$hv007), '_', unique(CM_2018[[i]]$hv006))}

GPS_all <- sapply(c(CM_2011, CM_2018), sf:::as_Spatial, simplify = F)
names(GPS_all) 


# Gambia
dhs_all <- list(GM_2019_dhs)
names(dhs_all) <- "GM_2019"

dhs_all <- dhs_all %>% map(~filter(., hv025 == 1)) %>%  map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get Urban cluster numbers by month and survey year

GM_2019 <- survey_gps_comb(x= "GM_2019", y= "GM_2019")
for(i in seq_along(GM_2019)) {names(GM_2019)[[i]] <- paste0(unique(GM_2019[[i]]$hv007), '_', unique(GM_2019[[i]]$hv006))}

GPS_all <- sapply(c(GM_2019), sf:::as_Spatial, simplify = F)
names(GPS_all) 

# Ghana
dhs_all <- list(GH_2014_dhs, GH_2016_mis, GH_2019_mis)
names(dhs_all) <- c("GH_2014", "GH_2016", "GH_2019")

dhs_all <- dhs_all %>% map(~filter(., hv025 == 1)) %>%  map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get Urban cluster numbers by month and survey year

GH_2014 <- survey_gps_comb(x= "GH_2014", y= "GH_2014")
for(i in seq_along(GH_2014)) {names(GH_2014)[[i]] <- paste0(unique(GH_2014[[i]]$hv007), '_', unique(GH_2014[[i]]$hv006))}

GH_2016 <- survey_gps_comb(x= "GH_2016", y= "GH_2016")
for(i in seq_along(GH_2016)) {names(GH_2016)[[i]] <- paste0(unique(GH_2016[[i]]$hv007), '_', unique(GH_2016[[i]]$hv006))}

GH_2019 <- survey_gps_comb(x= "GH_2019", y= "GH_2019")
for(i in seq_along(GH_2019)) {names(GH_2019)[[i]] <- paste0(unique(GH_2019[[i]]$hv007), '_', unique(GH_2019[[i]]$hv006))}

GPS_all <- sapply(c(GH_2014, GH_2016, GH_2019), sf:::as_Spatial, simplify = F)
names(GPS_all) 


# Guinea
dhs_all <- list(GN_2012_dhs, GN_2021_mis)
names(dhs_all) <- c("GN_2012", "GN_2021")

dhs_all <- dhs_all %>% map(~filter(., hv025 == 1)) %>%  map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get Urban cluster numbers by month and survey year

GN_2012 <- survey_gps_comb(x= "GN_2012", y= "GN_2012")
for(i in seq_along(GN_2012)) {names(GN_2012)[[i]] <- paste0(unique(GN_2012[[i]]$hv007), '_', unique(GN_2012[[i]]$hv006))}

GN_2021 <- survey_gps_comb(x= "GN_2021", y= "GN_2021")
for(i in seq_along(GN_2021)) {names(GN_2021)[[i]] <- paste0(unique(GN_2021[[i]]$hv007), '_', unique(GN_2021[[i]]$hv006))}

GPS_all <- sapply(c(GN_2012, GN_2021), sf:::as_Spatial, simplify = F)
names(GPS_all) 

# Kenya
dhs_all <- list(KE_2015_mis, KE_2020_mis)
names(dhs_all) <- c("KE_2015", "KE_2020")

dhs_all <- dhs_all %>% map(~filter(., hv025 == 1)) %>%  map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get Urban cluster numbers by month and survey year

KE_2015 <- survey_gps_comb(x= "KE_2015", y= "KE_2015")
for(i in seq_along(KE_2015)) {names(KE_2015)[[i]] <- paste0(unique(KE_2015[[i]]$hv007), '_', unique(KE_2015[[i]]$hv006))}

KE_2020 <- survey_gps_comb(x= "KE_2020", y= "KE_2020")
for(i in seq_along(KE_2020)) {names(KE_2020)[[i]] <- paste0(unique(KE_2020[[i]]$hv007), '_', unique(KE_2020[[i]]$hv006))}

GPS_all <- sapply(c(KE_2015, KE_2020), sf:::as_Spatial, simplify = F)
names(GPS_all) 

# Liberia
dhs_all <- list(LB_2011_mis, LB_2016_mis)
names(dhs_all) <- c("LB_2011", "LB_2016")

dhs_all <- dhs_all %>% map(~filter(., hv025 == 1)) %>%  map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get Urban cluster numbers by month and survey year

LB_2011 <- survey_gps_comb(x= "LB_2011", y= "LB_2011")
for(i in seq_along(LB_2011)) {names(LB_2011)[[i]] <- paste0(unique(LB_2011[[i]]$hv007), '_', unique(LB_2011[[i]]$hv006))}

LB_2016 <- survey_gps_comb(x= "LB_2016", y= "LB_2016")
for(i in seq_along(LB_2016)) {names(LB_2016)[[i]] <- paste0(unique(LB_2016[[i]]$hv007), '_', unique(LB_2016[[i]]$hv006))}

GPS_all <- sapply(c(LB_2011, LB_2016), sf:::as_Spatial, simplify = F)
names(GPS_all) 

# Madagascar 
dhs_all <- list(MD_2011_mis, MD_2013_mis, MD_2016_mis, MD_2021_dhs)
names(dhs_all) <- c("MD_2011", "MD_2013", "MD_2016", "MD_2021")

dhs_all <- dhs_all %>% map(~filter(., hv025 == 1)) %>%  map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get Urban cluster numbers by month and survey year

MD_2011 <- survey_gps_comb(x= "MD_2011", y= "MD_2011")
for(i in seq_along(MD_2011)) {names(MD_2011)[[i]] <- paste0(unique(MD_2011[[i]]$hv007), '_', unique(MD_2011[[i]]$hv006))}

MD_2013 <- survey_gps_comb(x= "MD_2013", y= "MD_2013")
for(i in seq_along(MD_2013)) {names(MD_2013)[[i]] <- paste0(unique(MD_2013[[i]]$hv007), '_', unique(MD_2013[[i]]$hv006))}

MD_2016 <- survey_gps_comb(x= "MD_2016", y= "MD_2016")
for(i in seq_along(MD_2016)) {names(MD_2016)[[i]] <- paste0(unique(MD_2016[[i]]$hv007), '_', unique(MD_2016[[i]]$hv006))}

MD_2021 <- survey_gps_comb(x= "MD_2021", y= "MD_2021")
for(i in seq_along(MD_2021)) {names(MD_2021)[[i]] <- paste0(unique(MD_2021[[i]]$hv007), '_', unique(MD_2021[[i]]$hv006))}


GPS_all <- sapply(c(MD_2011, MD_2013, MD_2016, MD_2021), sf:::as_Spatial, simplify = F)
names(GPS_all) 

# Mali- DHS did not provide cluster GPS coordinates


# Malawi
dhs_all <- list(MW_2012_mis, MW_2014_mis, MW_2017_mis)
names(dhs_all) <- c("MW_2012", "MW_2014", "MW_2017")

dhs_all <- dhs_all %>% map(~filter(., hv025 == 1)) %>%  map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get Urban cluster numbers by month and survey year

MW_2012 <- survey_gps_comb(x= "MW_2012", y= "MW_2012")
for(i in seq_along(MW_2012)) {names(MW_2012)[[i]] <- paste0(unique(MW_2012[[i]]$hv007), '_', unique(MW_2012[[i]]$hv006))}

MW_2014 <- survey_gps_comb(x= "MW_2014", y= "MW_2014")
for(i in seq_along(MW_2014)) {names(MW_2014)[[i]] <- paste0(unique(MW_2014[[i]]$hv007), '_', unique(MW_2014[[i]]$hv006))}

MW_2017 <- survey_gps_comb(x= "MW_2017", y= "MW_2017")
for(i in seq_along(MW_2017)) {names(MW_2017)[[i]] <- paste0(unique(MW_2017[[i]]$hv007), '_', unique(MW_2017[[i]]$hv006))}

GPS_all <- sapply(c(MW_2012, MW_2014, MW_2017), sf:::as_Spatial, simplify = F)
names(GPS_all) 

# Mozambique
dhs_all <- list(MZ_2011_dhs, MZ_2015_dhs, MZ_2018_dhs)
names(dhs_all) <- c("MZ_2011", "MZ_2015", "MZ_2018")

dhs_all <- dhs_all %>% map(~filter(., hv025 == 1)) %>%  map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get Urban cluster numbers by month and survey year

MZ_2011 <- survey_gps_comb(x= "MZ_2011", y= "MZ_2011")
for(i in seq_along(MZ_2011)) {names(MZ_2011)[[i]] <- paste0(unique(MZ_2011[[i]]$hv007), '_', unique(MZ_2011[[i]]$hv006))}

MZ_2015 <- survey_gps_comb(x= "MZ_2015", y= "MZ_2015")
for(i in seq_along(MZ_2015)) {names(MZ_2015)[[i]] <- paste0(unique(MZ_2015[[i]]$hv007), '_', unique(MZ_2015[[i]]$hv006))}

MZ_2018 <- survey_gps_comb(x= "MZ_2018", y= "MZ_2018")
for(i in seq_along(MZ_2018)) {names(MZ_2018)[[i]] <- paste0(unique(MZ_2018[[i]]$hv007), '_', unique(MZ_2018[[i]]$hv006))}

GPS_all <- sapply(c(MZ_2011, MZ_2015, MZ_2018), sf:::as_Spatial, simplify = F)
names(GPS_all) 

# Rwanda -- No GPS coordinates for clusters were provided for the 2017 survey 
dhs_all <- list(RW_2010_mis, RW_2015_dhs, RW_2019_dhs)
names(dhs_all) <- c("RW_2010", "RW_2015", "RW_2019")
RW_2010_mis %>% filter(hv025==1) %>% tabyl(hv006, hv007)

dhs_all <- dhs_all %>% map(~filter(., hv025 == 1)) %>%  map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get Urban cluster numbers by month and survey year

RW_2010 <- survey_gps_comb(x= "RW_2010", y= "RW_2010")
for(i in seq_along(RW_2010)) {names(RW_2010)[[i]] <- paste0(unique(RW_2010[[i]]$hv007), '_', unique(RW_2010[[i]]$hv006))}

RW_2015 <- survey_gps_comb(x= "RW_2015", y= "RW_2015")
for(i in seq_along(RW_2015)) {names(RW_2015)[[i]] <- paste0(unique(RW_2015[[i]]$hv007), '_', unique(RW_2015[[i]]$hv006))}

RW_2019 <- survey_gps_comb(x= "RW_2019", y= "RW_2019")
for(i in seq_along(RW_2019)) {names(RW_2019)[[i]] <- paste0(unique(RW_2019[[i]]$hv007), '_', unique(RW_2019[[i]]$hv006))}

GPS_all <- sapply(c(RW_2010, RW_2015, RW_2019), sf:::as_Spatial, simplify = F)

# Senegal
dhs_all <- list(SN_2010_dhs, SN_2012_dhs, SN_2014_dhs, SN_2015_dhs, SN_2016_dhs, SN_2017_dhs, SN_2020_mis)
names(dhs_all) <- c("SN_2010", "SN_2012", "SN_2014", "SN_2015", "SN_2016", "SN_2017", "SN_2020")

dhs_all <- dhs_all %>% map(~filter(., hv025 == 1)) %>%  map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get Urban cluster numbers by month and survey year

SN_2010 <- survey_gps_comb(x= "SN_2010", y= "SN_2010")
for(i in seq_along(SN_2010)) {names(SN_2010)[[i]] <- paste0(unique(SN_2010[[i]]$hv007), '_', unique(SN_2010[[i]]$hv006))}

SN_2012 <- survey_gps_comb(x= "SN_2012", y= "SN_2012")
for(i in seq_along(SN_2012)) {names(SN_2012)[[i]] <- paste0(unique(SN_2012[[i]]$hv007), '_', unique(SN_2012[[i]]$hv006))}

SN_2014 <- survey_gps_comb(x= "SN_2014", y= "SN_2014")
for(i in seq_along(SN_2014)) {names(SN_2014)[[i]] <- paste0(unique(SN_2014[[i]]$hv007), '_', unique(SN_2014[[i]]$hv006))}

SN_2015 <- survey_gps_comb(x= "SN_2015", y= "SN_2015")
for(i in seq_along(SN_2015)) {names(SN_2015)[[i]] <- paste0(unique(SN_2015[[i]]$hv007), '_', unique(SN_2015[[i]]$hv006))}

SN_2016 <- survey_gps_comb(x= "SN_2016", y= "SN_2016")
for(i in seq_along(SN_2016)) {names(SN_2016)[[i]] <- paste0(unique(SN_2016[[i]]$hv007), '_', unique(SN_2016[[i]]$hv006))}

SN_2017 <- survey_gps_comb(x= "SN_2017", y= "SN_2017")
for(i in seq_along(SN_2017)) {names(SN_2017)[[i]] <- paste0(unique(SN_2017[[i]]$hv007), '_', unique(SN_2017[[i]]$hv006))}

SN_2020 <- survey_gps_comb(x= "SN_2020", y= "SN_2020")
for(i in seq_along(SN_2020)) {names(SN_2020)[[i]] <- paste0(unique(SN_2020[[i]]$hv007), '_', unique(SN_2020[[i]]$hv006))}

GPS_all <- sapply(c(SN_2010, SN_2012, SN_2014, SN_2015, SN_2016, SN_2017, SN_2020), sf:::as_Spatial, simplify = F)

# Togo 
dhs_all <- list(TG_2013_dhs, TG_2017_mis)
names(dhs_all) <- c("TG_2013", "TG_2017")

dhs_all <- dhs_all %>% map(~filter(., hv025 == 1)) %>%  map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get Urban cluster numbers by month and survey year

TG_2013 <- survey_gps_comb(x= "TG_2013", y= "TG_2013")
for(i in seq_along(TG_2013)) {names(TG_2013)[[i]] <- paste0(unique(TG_2013[[i]]$hv007), '_', unique(TG_2013[[i]]$hv006))}

TG_2017 <- survey_gps_comb(x= "TG_2017", y= "TG_2017")
for(i in seq_along(TG_2017)) {names(TG_2017)[[i]] <- paste0(unique(TG_2017[[i]]$hv007), '_', unique(TG_2017[[i]]$hv006))}

GPS_all <- sapply(c(TG_2013, TG_2017), sf:::as_Spatial, simplify = F)

# Tanzania 
dhs_all <- list(TZ_2012_dhs, TZ_2015_dhs, TZ_2017_mis)
names(dhs_all) <- c("TZ_2012", "TZ_2015", "TZ_2017")

dhs_all <- dhs_all %>% map(~filter(., hv025 == 1)) %>%  map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get Urban cluster numbers by month and survey year

TZ_2012 <- survey_gps_comb(x= "TZ_2012", y= "TZ_2012")
for(i in seq_along(TZ_2012)) {names(TZ_2012)[[i]] <- paste0(unique(TZ_2012[[i]]$hv007), '_', unique(TZ_2012[[i]]$hv006))}

TZ_2015 <- survey_gps_comb(x= "TZ_2015", y= "TZ_2015")
for(i in seq_along(TZ_2015)) {names(TZ_2015)[[i]] <- paste0(unique(TZ_2015[[i]]$hv007), '_', unique(TZ_2015[[i]]$hv006))}

TZ_2017 <- survey_gps_comb(x= "TZ_2017", y= "TZ_2017")
for(i in seq_along(TZ_2017)) {names(TZ_2017)[[i]] <- paste0(unique(TZ_2017[[i]]$hv007), '_', unique(TZ_2017[[i]]$hv006))}

GPS_all <- sapply(c(TZ_2012, TZ_2015, TZ_2017), sf:::as_Spatial, simplify = F)

# Uganda
dhs_all <- list(UG_2014_mis, UG_2016_dhs, UG_2018_mis)
names(dhs_all) <- c("UG_2014", "UG_2016", "UG_2018")

dhs_all <- dhs_all %>% map(~filter(., hv025 == 1)) %>%  map(~dplyr::select(., hv001, hv006, hv007, hv025)) %>%
  map(~distinct(.,)) #get Urban cluster numbers by month and survey year

UG_2014 <- survey_gps_comb(x= "UG_2014", y= "UG_2014")
for(i in seq_along(UG_2014)) {names(UG_2014)[[i]] <- paste0(unique(UG_2014[[i]]$hv007), '_', unique(UG_2014[[i]]$hv006))}

UG_2016 <- survey_gps_comb(x= "UG_2016", y= "UG_2016")
for(i in seq_along(UG_2016)) {names(UG_2016)[[i]] <- paste0(unique(UG_2016[[i]]$hv007), '_', unique(UG_2016[[i]]$hv006))}

UG_2018 <- survey_gps_comb(x= "UG_2018", y= "UG_2018")
for(i in seq_along(UG_2018)) {names(UG_2018)[[i]] <- paste0(unique(UG_2018[[i]]$hv007), '_', unique(UG_2018[[i]]$hv006))}

GPS_all <- sapply(c(UG_2014, UG_2016, UG_2018), sf:::as_Spatial, simplify = F)



#### Read in Raster Data ####

# buffers of interest

vars <- c(2000) #meters

#EVI rasters- this is the Part where the lag comes in! 
EVI_comb <- lapply(dhs_all, pick_month, filepath= EVI_dir) 
EVI_comb <- unlist(EVI_comb) #vector

EVI_raster_all <- sapply(EVI_comb, raster, simplify = F) #read in EVI raster files, with 2 month lag

#Run this code once per country
country_name <- "_UG"
for (i in 1:length(vars)) {
  var_name <- paste0('EVI_', as.character(vars[i]), 'm')
  df <- map2(GPS_all, EVI_raster_all, get_crs) #list of 13 vs. list of 144 
  df <- pmap(list(EVI_raster_all, df, vars[i]), extract_fun_month) 
  df <- df %>%  map(~rename_with(., .fn=~paste0(var_name), .cols = contains('EVI')))
  df <- plyr::ldply(df) %>% dplyr::select(-c(ID)) 
  df <- df %>% arrange(month) %>%  group_by(dhs_year, hv001) %>%  slice(1) #get data for the first month if more than one survey month in a cluster
  write.csv(df, file = file.path(GeoDir, paste0('EVI_', as.character(vars[i]), 
                                                'm_buffer', country_name, "_DHS.csv")),row.names = FALSE)
}


#precipitation CHIRPS

#loading CHIRPS rasters in months when DHS/MIS was conducted (lagged by 2 months)
precip_comb <- lapply(dhs_all, pick_month, filepath= precip_dir) 
precip_comb <- unlist(precip_comb) #vector

raster_all <- sapply(precip_comb, raster, simplify = F) #read in Precipitation raster files, with 2 month lag


#precipitation extraction
country_name <- "_UG"
for (i in 1:length(vars)) {
  var_name <- paste0('preci_monthly_', as.character(vars[i]), 'm')
  df <- map2(GPS_all, raster_all, get_crs)
  df <- pmap(list(raster_all, df, vars[i]), extract_fun_month)
  df <- df %>% map(~rename_with(., .fn=~paste0(var_name), .cols = contains('chirps')))
  df <- plyr::ldply(df)%>% dplyr::select(-c(ID))
  df <- df %>% arrange(month) %>%  group_by(dhs_year, hv001) %>%  slice(1)
  write.csv(df, file = file.path(GeoDir2, paste0('precip_monthly_', as.character(vars[i]), 
                                                'm_buffer', country_name, "_DHS.csv")),row.names = FALSE)
}

#Precip-- QA -- Manually updated in csv
#Gambia
print_labels(GM_2019_dhs$hv024)
GM_2019_dhs %>% filter(hv025==1 & hv024==1) %>% 
  tabyl(hv001, hv006) #clusters 1-40 are in Banjul. Some values are negative becasue so close to the ocean
GM_2019_precip <- read.csv(file.path(GeoDir2, "precip_monthly_2000m_buffer_GM_DHS.csv"))
options(scipen= 999)
GM_2019_precip %>% filter(preci_monthly_2000m <0)
GM_2019_precip %>%  filter(hv001 <41 & month==1) #cluster 9 should not be negative- assign to be the same as cluster 26 (both in Banjul)
GM_2019_precip %>%  filter(hv001 <41 & month==11) #all clusters in Banjul, surveyed Nov 2012 should be the same, none should be -9999
#Nigeria
NG_precip <- read.csv(file.path(GeoDir2, "precip_monthly_2000m_buffer_NG_DHS.csv"))
NG_precip %>% filter(preci_monthly_2000m <0) 
print_labels(NG_2018_dhs$hv022)
NG_2018_dhs %>% filter(hv001 %in% c(48, 122, 858, 867, 1298, 1339)) %>% 
  tabyl(hv001, hv022) #all in different regions, no need to recode just use NA 
#Ghana
GH_2019_mis %>% filter(hv025==1 & hv001 %in% c(44, 119)) %>% 
  tabyl(hv001, hv022) ##all in different regions, no need to recode just use NA 


#### READ IN RASTER DATA- Relative Humidity at 1 atm ####
## Using a 2-month lag for relative humidity data

# 2009- 2021
humidity_2009 <- brick(file.path(HumidityDataDir, 'rel_humidity_2009.grib'))
humidity_2010 <- brick(file.path(HumidityDataDir, 'rel_humidity_2010.grib'))
humidity_2011 <- brick(file.path(HumidityDataDir, 'rel_humidity_2011.grib'))
humidity_2012 <- brick(file.path(HumidityDataDir, 'rel_humidity_2012.grib'))
humidity_2013 <- brick(file.path(HumidityDataDir, 'rel_humidity_2013.grib'))
humidity_2014 <- brick(file.path(HumidityDataDir, 'rel_humidity_2014.grib'))
humidity_2015 <- brick(file.path(HumidityDataDir, 'rel_humidity_2015.grib'))
humidity_2016 <- brick(file.path(HumidityDataDir, 'rel_humidity_2016.grib'))
humidity_2017 <- brick(file.path(HumidityDataDir, 'rel_humidity_2017.grib'))
humidity_2018 <- brick(file.path(HumidityDataDir, 'rel_humidity_2018.grib'))
humidity_2019 <- brick(file.path(HumidityDataDir, 'rel_humidity_2019.grib'))
humidity_2020 <- brick(file.path(HumidityDataDir, 'rel_humidity_2020.grib'))
humidity_2021 <- brick(file.path(HumidityDataDir, 'rel_humidity_2021.grib'))

names(humidity_2021) <- paste0("RH_", month.abb) #redo for each file
nlayers(humidity_2010)

plot(humidity_2010, 1) #Visually inspecting Relative humidity- Jan 2010

# Make list of rasterbricks
list_RH <- list(humidity_2009, humidity_2010, humidity_2011, humidity_2012, humidity_2013, humidity_2014, humidity_2015,
                humidity_2016, humidity_2017, humidity_2018, humidity_2019, humidity_2020, humidity_2021)

names(list_RH) <- c("2009", "2010", "2011", "2012", "2013", "2014", "2015", "2016", "2017", "2018", "2019",
                    "2020", "2021")


#Apply over dhs_all list--- Repeat for each country

RH_files <- lapply(dhs_all, get_month_str_RH)
RH_files[[2]]

for (i in 1:length(RH_files)){
   RH_files[[i]] <- RH_files[[i]] %>% 
    pmap(~pick_files_RH(.y, .x)) #pick up month_lag and year to select the correct humidity raster files
}

raster_all <- unlist(RH_files)

# Relative humidity extraction
country_name <- "_UG"
for (i in 1:length(vars)) {
  var_name <- paste0('RH_monthly_', as.character(vars[i]), 'm')
  df <- map2(GPS_all, raster_all, get_crs)  #transform GPS coords to match raster projection
  df <- pmap(list(raster_all, df, vars[i]), extract_fun_month)
  df <- df %>% map(~rename_with(., .fn=~paste0(var_name), .cols = contains('RH')))
  df <- plyr::ldply(df)%>% dplyr::select(-c(ID))
  df <- df %>% arrange(month) %>%  group_by(dhs_year, hv001) %>%  slice(1)
  write.csv(df, file = file.path(GeoDir3, paste0('RH_monthly_', as.character(vars[i]), 
                                                 'm_buffer', country_name, "_DHS.csv")),row.names = FALSE)
}


#test- Same CRS
new <- get_crs(GPS_all[[1]], raster_all[[1]])
crs(new)
df <- map2(GPS_all, raster_all, get_crs)
compareCRS(df[[1]], raster_all[[1]]) #Same CRS

