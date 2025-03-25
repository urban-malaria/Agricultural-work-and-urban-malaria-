# ==========================================================================================================================================
# Script Name: Population Raster Extraction
# Author: Grace Legris, Research Data Analyst (gracebea@gmail.com)
# Purpose: Extract estimated population in each first-level administrative subdivision in each of the 15 countries analyzed using population
# rasters.
# ==========================================================================================================================================

# clear current workspace
rm(list = ls())

## =========================================================================================================================================
### Directory Management and File Paths
## =========================================================================================================================================

Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", gsub("OneDrive", "", Sys.getenv("HOME")))))
Drive <- file.path(gsub("[//]", "/", Drive))
DriveDir <- file.path(Drive, "Urban Malaria Proj Dropbox", "urban_malaria")
DataDir <- file.path(DriveDir, "data")


## =========================================================================================================================================
### Required Functions, Settings, and Processing
## =========================================================================================================================================

# note before sourcing functions and packages, run the code below to download rdhs if you don't already have it
# devtools::install_github("ropensci/rdhs")
source("functions/functions_employment.R")
options(survey.lonely.psu="adjust")  # this option allows admin units with only one cluster to be analyzed


## -----------------------------------------------------------------------------------------------------------------------------------------
### Read in Top-Down Constrained Estimates Rasters for each Country
## -----------------------------------------------------------------------------------------------------------------------------------------
countries <- c("angola", "burkina faso", "benin", "burundi", "drc", "cote d'ivoire", "cameroon",
               "ghana", "guinea", "madagascar", "mali", "mozambique", "nigeria", "togo", "uganda")
for (country in countries) {
  assign(paste0(gsub(" ", "_", country), "_raster"), 
         raster(file.path(DataDir, "data_agric_analysis", "population_rasters", paste0(country, ".tif"))))
}

## -----------------------------------------------------------------------------------------------------------------------------------------
### Read in Subdivision Shapefiles (e.g. State, Region, etc) - First-Level Administrative Subdivision in Each Country
## -----------------------------------------------------------------------------------------------------------------------------------------

# read in the shapefiles with first-level administrative division geographic boundaries (state, region, etc)
ao.subd <- st_read(file.path(PopDir, "data", "opened", "GPS", "subdivisions", "AO", "gadm41_AGO_1.shp"))
bf.subd <- st_read(file.path(PopDir, "data", "opened", "GPS", "subdivisions", "BF", "gadm41_BFA_1.shp"))
bj.subd <- st_read(file.path(PopDir, "data", "opened", "GPS", "subdivisions", "BJ", "gadm41_BEN_1.shp"))
bu.subd <- st_read(file.path(PopDir, "data", "opened", "GPS", "subdivisions", "BU", "gadm41_BDI_1.shp"))
cd.subd <- st_read(file.path(PopDir, "data", "opened", "GPS", "subdivisions", "CD", "gadm41_COD_1.shp"))
ci.subd <- st_read(file.path(PopDir, "data", "opened", "GPS", "subdivisions", "CI", "gadm41_CIV_1.shp"))
cm.subd <- st_read(file.path(PopDir, "data", "opened", "GPS", "subdivisions", "CM", "gadm41_CMR_1.shp"))
gh.subd <- st_read(file.path(PopDir, "data", "opened", "GPS", "subdivisions", "GH", "gadm41_GHA_1.shp"))
gn.subd <- st_read(file.path(PopDir, "data", "opened", "GPS", "subdivisions", "GN", "gadm41_GIN_1.shp"))
md.subd <- st_read(file.path(PopDir, "data", "opened", "GPS", "subdivisions", "MD", "gadm41_MDG_2.shp")) # shapefile 1 has the 6 provinces that were dissolved in 2009, use # 2 which has current 23 regions
ml.subd <- st_read(file.path(PopDir, "data", "opened", "GPS", "subdivisions", "ML", "gadm41_MLI_1.shp"))
mz.subd <- st_read(file.path(PopDir, "data", "opened", "GPS", "subdivisions", "MZ", "gadm41_MOZ_1.shp"))
ng.subd <- st_read(file.path(PopDir, "data", "opened", "GPS", "subdivisions", "NG", "gadm41_NGA_1.shp"))
tg.subd <- st_read(file.path(PopDir, "data", "opened", "GPS", "subdivisions", "TG", "gadm41_TGO_1.shp"))
ug.subd <- st_read(file.path(PopDir, "data", "opened", "GPS", "subdivisions", "UG", "uga_admbnda_adm1_ubos_20200824.shp")) # GADM doesn't have the 4 region boundaries, so downloaded them from https://data.humdata.org/dataset/cod-ab-uga?

subdivision_files <- list(
  "AO" = ao.subd, "BF" = bf.subd, "BJ" = bj.subd, "BU" = bu.subd, "CD" = cd.subd,
  "CI" = ci.subd, "CM" = cm.subd, "GH" = gh.subd, "GN" = gn.subd, "MD" = md.subd,
  "ML" = ml.subd, "MZ" = mz.subd, "NG" = ng.subd, "TG" = tg.subd, "UG" = ug.subd
)

country_names <- c("AO" = "angola", "BF" = "burkina Faso", "BJ" = "benin", "BU" = "burundi", "CD" = "drc", "CI" = "cote d'ivoire",
                   "CM" = "cameroon", "GH" = "ghana", "GN" = "guinea", "MD" = "madagascar", "ML" = "mali", "MZ" = "mozambique", "NG" = "nigeria", "TG" = "togo", "UG" = "uganda")

# rename region names column to NAME_1 for Madagascar to match the other countries, remove province name variable (provinces were dissolved in 2009)
subdivision_files[["MD"]] <- subdivision_files[["MD"]] %>%
  select(-NAME_1) %>%       # remove NAME_1 column (province names)
  rename(NAME_1 = NAME_2)   # rename NAME_2 to NAME_1 (region names)

# rename region name column for Uganda to match the other countries (different data source for Uganda so different var name)
subdivision_files[["UG"]] <- subdivision_files[["UG"]] %>%
  rename(NAME_1 = ADM1_EN)

## -----------------------------------------------------------------------------------------------------------------------------------------
### Extract Population Data
## -----------------------------------------------------------------------------------------------------------------------------------------

library(raster)
library(sf)
library(dplyr)
library(exactextractr)
library(officer)

# create an empty list to store population counts per subdivision
pop_counts <- list()

for (code in names(subdivision_files)) {
  
  # get country name and corresponding raster
  country <- country_names[[code]]
  subd <- subdivision_files[[code]]
  raster_var <- get(paste0(gsub(" ", "_", tolower(country)), "_raster"))
  
  # ensure CRS matches
  if (!st_crs(subd) == crs(raster_var)) {
    subd <- st_transform(subd, crs(raster_var))
  }
  
  # extract total population for each subdivision
  pop_data <- exact_extract(raster_var, subd, fun = "sum", progress = FALSE)
  
  # combine with subdivision names
  subd$pop_total <- pop_data
  
  # store in the list
  pop_counts[[code]] <- subd
}

# convert list to a single dataframe if needed
pop_counts_df <- bind_rows(pop_counts, .id = "Country_Code")

pop_counts_final <- pop_counts_df %>%
  select(Country_Code, COUNTRY, NAME_1, ENGTYPE_1, pop_total)

pop_top_3 <- pop_counts_final %>%
  group_by(COUNTRY) %>%
  top_n(3, pop_total) %>%
  ungroup()

# fill in uganda name, set madagascar type to region
pop_top_3 <- pop_top_3 %>%
  mutate(COUNTRY = case_when(
    Country_Code == "UG" ~ "Uganda",
    TRUE ~ COUNTRY
  )) %>%
  mutate(ENGTYPE_1 = case_when(
    Country_Code %in% c("UG", "MD") ~ "Region",
    TRUE ~ ENGTYPE_1
  )) %>%
  mutate(ENGTYPE_1 = case_when(
    Country_Code %in% c("MZ") ~ "Province",
    TRUE ~ ENGTYPE_1
  )) %>%
  mutate(ENGTYPE_1 = case_when(
    NAME_1 == "Abidjan" ~ "District",
    TRUE ~ ENGTYPE_1
  ))

# make word doc table with this data to put in supplement
pop_top_3_df <- pop_top_3 %>% 
  st_drop_geometry() %>%
  mutate(pop_total = round(pop_total)) %>% # round population to nearest whole number
  select(-Country_Code) %>%
  rename("Country" = COUNTRY,
         "Subdivision Name" = NAME_1,
         "Subdivision Type" = ENGTYPE_1,
         "Population Estimate" = pop_total)

doc <- read_docx()
doc <- doc %>%
  body_add_table(value = pop_top_3_df, style = "table_template")
print(doc, target = file.path(PopDir, "analysis_dat", "pop_top_3_df.docx"))
