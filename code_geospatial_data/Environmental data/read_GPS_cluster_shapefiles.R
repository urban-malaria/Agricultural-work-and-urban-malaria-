library(readxl)
library(haven)
library(sf)
library(listr)

#Read in file paths
user <- Sys.getenv("USERNAME")
Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
NuDir <- file.path(Drive, "NU-malaria-team Dropbox")
DataDir <- file.path(NuDir, "data", 'urban_malaria_net_ownership_data', 'DHS')
MISDataDir <- file.path(NuDir, "data", 'urban_malaria_net_ownership_data', 'MIS')

# FUNCTIONS

# Make function that reads inside a folder same way for all files
read.subfolders <- function(x) {
  subfolders <- list.dirs(path = x, full.names = TRUE, recursive = TRUE)
  files<- subfolders[(grep("GE", subfolders))]
}

#Function- read in cluster GPS data points from list of folders
read.files <- function(path) {
  files <- list.files(path = path , pattern = "*FL.shp$", full.names = TRUE, recursive = TRUE)
  shp <- st_read(files)
}

#Generate list of sub-folders in DHS and MIS folders with GPS cluster data 
directories <- list.dirs(path = DataDir, full.names = TRUE, recursive = TRUE)
MIS_directories <- list.dirs(path = MISDataDir, full.names = TRUE, recursive = TRUE)

subf <- lapply(directories, read.subfolders) 
subf_list <- subf[lengths(subf) != 0L] #length ne to 0
subf_list <- subf_list[[1]]

all_GPS1 <- lapply(subf_list, read.files)


#MIS data 
subf_mis <- lapply(MIS_directories, read.subfolders) 
subf_mis_list <- subf_mis[lengths(subf_mis) != 0L] 
subf_mis_list <- subf_mis_list[[1]]


all_GPS2 <- lapply(subf_mis_list, read.files)

all_GPS3 <- c(all_GPS1, all_GPS2)

# READ in the DATA- filter to Urban areas only 

#Convert all GPS to Urban
all_GPS <- lapply(all_GPS3, function(x){
    x %>% 
   filter(URBAN_RURA == "U") %>% 
    sf::as_Spatial()
})
  
#Assign names to SP dataframes in list 
names(all_GPS) <- c("AO_2015", "BF_2010", "BF_2014", "BF_2017", "BJ_2012", "BJ_2017", "BU_2012", "BU_2016",
  "CM_2011", "CM_2018", "GH_2014", "GH_2016", "GH_2019", "GM_2019", "GN_2012", "GN_2018", "GN_2021", "KE_2015",
  "KE_2020", "LB_2016", "MD_2013", "MD_2016", "MD_2021", "MW_2012", "MW_2014", "MW_2017", "MZ_2011", "MZ_2015",
  "MZ_2018", "NG_2015", "NG_2018", "NG_2021", "RW_2010", "RW_2015", "RW_2019", "SN_2010", "SN_2012", "SN_2014",
  "SN_2015", "SN_2016", "SN_2017", "SN_2020", "TG_2013", "TG_2017", "TZ_2012", "TZ_2015", "TZ_2017", "UG_2014",
  "UG_2016", "UG_2018",
  "AO_2011", "LB_2011", "MD_2011", "NG_2010")

#organize in alphabetic order
all_GPS= all_GPS[order(names(all_GPS))]


