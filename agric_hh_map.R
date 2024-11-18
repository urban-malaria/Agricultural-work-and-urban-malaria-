# ==========================================================================================================================================
# Script Name: Agric HHs by Cluster in Each Country
# Author: Grace Legris, Research Data Analyst (gracebea@gmail.com)
# Edited: [2024-11-13]
# Purpose: Generate map of proportion of agric HHs by cluster & first-level administrative division in each country
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
  FigDir <- file.path(ManDir, "figures", "main_figures")
  SupDir <- file.path(ManDir, "figures", "supplementary", "pdf_figures")
  ExpDir <- file.path(ManDir, "figures", "exploratory")
} else if  ("CHZCHI003" %in% user) {
  Drive <- file.path("C:/Users/CHZCHI003/Dropbox")
  DriveDir <- file.path("C:/Users/CHZCHI003/OneDrive/urban_malaria")
  PopDir <- file.path(Drive)
  ManDir <- file.path(Drive, "agriculture_malaria_manuscript")
  FigDir <- file.path(ManDir, "figures", "main_figures")
  SupDir <- file.path(ManDir, "figures", "supplementary", "pdf_figures")
  ExpDir <- file.path(ManDir, "figures", "exploratory")
} else if  ("cchiz" %in% user) {
  Drive <- file.path("C:/Users/cchiz/Dropbox")
  DriveDir <- file.path("C:/Users/cchiz/OneDrive/urban_malaria")
  PopDir <- file.path(Drive)
  ManDir <- file.path(Drive, "agriculture_malaria_manuscript")
  FigDir <- file.path(ManDir, "figures", "main_figures")
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
  FigDir <- file.path(ManDir, "figures", "main_figures")
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

## =========================================================================================================================================
### Data Prep
## =========================================================================================================================================

## -----------------------------------------------------------------------------------------------------------------------------------------
### Read in Country Shapefile
## -----------------------------------------------------------------------------------------------------------------------------------------

# read the shapefile for Africa country boundaries
afr.shp.base <- st_read(file.path(DriveDir, "data", "Urban_malaria_net_ownership_data", 
                                  "shapefiles", "africa_baundaries", "afr_g2014_2013_0.shp"))

# filter shapefile to only include countries of interest and rename Cote d'Ivoire to match other dfs (for merging)
# recode MG to MD (madagascar) and BI to BU (burundi) as these are the country codes we use
afr.shp.base <- afr.shp.base %>%
  mutate(ADM0_NAME = ifelse(ADM0_NAME == "Côte d'Ivoire", "Cote d'Ivoire", ADM0_NAME)) %>%
  mutate(ISO2 = ifelse(ISO2 == "MG", "MD", ISO2)) %>%
  mutate(ISO2 = ifelse(ISO2 == "BI", "BU", ISO2)) %>%
  filter(ADM0_NAME %in% c("Angola", "Burkina Faso", "Benin", "Burundi", "Democratic Republic of the Congo", "Cote d'Ivoire", 
                          "Cameroon", "Ghana", "Guinea", "Madagascar", "Mali", "Mozambique", "Nigeria", "Togo", "Uganda"))

## -----------------------------------------------------------------------------------------------------------------------------------------
### Read in Survey Data and Merge with GPS Data
## -----------------------------------------------------------------------------------------------------------------------------------------

# read in urban survey data
urban_df <- read_csv(file.path(PopDir, "analysis_dat/240729_urban_df_for_analysis.csv"))

urban_df <- urban_df %>%
  mutate(home_type2 = ifelse(home_type2 == "A", "Agricultural", "Non-Agricultural"))

# replace space-hyphen-space with en dash and no spaces (preferred in academic writing)
urban_df$year_combo <- gsub(" - ", "–", urban_df$year_combo)

# read in all shapefiles
shapefiles <- list()
country_codes <- c(
  "AOGE71FL" = "AO",  # Angola
  "BFGE81FL" = "BF",  # Burkina Faso
  "BJGE71FL" = "BJ",  # Benin
  "BUGE71FL" = "BU",  # Burundi
  "CDGE61FL" = "CD",  # Democratic Republic of Congo
  "CIGE81FL" = "CI",  # Côte d'Ivoire
  "CMGE71FL" = "CM",  # Cameroon
  "GHGE8AFL" = "GH",  # Ghana
  "GNGE71FL" = "GN",  # Guinea
  "MDGE81FL" = "MD",  # Madagascar
  "MLGE7AFL" = "ML",  # Mali
  "MZGE81FL" = "MZ",  # Mozambique
  "NGGE7BFL" = "NG",  # Nigeria
  "TGGE62FL" = "TG",  # Togo
  "UGGE7AFL" = "UG"   # Uganda
)

# path to the folder storing GPS data
gps_folder_path <- file.path(PopDir, "data", "opened", "GPS")

# loop through each country code to read in shapefiles
for (code in names(country_codes)) {
  shapefile_path <- file.path(gps_folder_path, code, paste0(code, ".shp"))
  shapefiles[[country_codes[code]]] <- st_read(shapefile_path)
}

# initialize a list to store merged datasets for each country
gps_survey_data <- list()

# loop through each shapefile and merge with the survey data
for (country_code in names(shapefiles)) {
  # extract the shapefile for the current country
  gps_data <- shapefiles[[country_code]]
  
  # filter the survey data for the corresponding country
  country_survey_data <- urban_df[urban_df$DHS_CountryCode == toupper(country_code), ]
  
  # ensure that the types of DHSCLUST and hv001 are the same
  gps_data$DHSCLUST <- as.character(gps_data$DHSCLUST)
  country_survey_data$hv001 <- as.character(country_survey_data$hv001)
  
  # merge the shapefile with the filtered survey data
  merged_data <- merge(country_survey_data, gps_data, by.x = "hv001", by.y = "DHSCLUST", all.x = TRUE)
  
  # store the merged dataset in the list
  gps_survey_data[[country_code]] <- merged_data
  
  # select only the needed columns for simplicity
  # rename hv001 to "cluster"
  # create a variable for proportion of agricultural households within each cluster
  gps_survey_data[[country_code]] <- gps_survey_data[[country_code]] %>%
    select(hv001, dhs_year, year_combo, strat, wt, test_result, DHS_CountryCode, CountryName, home_type2, LATNUM, LONGNUM, geometry) %>%
    rename(cluster = hv001) %>%
    group_by(cluster) %>%
      mutate(agric_proportion = sum(home_type2 == "Agricultural") / n()) %>%
      ungroup()
}

## -----------------------------------------------------------------------------------------------------------------------------------------
### Read in Subdivision Shapefiles (e.g. State, Region, etc) - First-Level Administrative Subdivision in Each Country
## -----------------------------------------------------------------------------------------------------------------------------------------

library(sf)

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

country_names <- c("AO" = "Angola", "BF" = "Burkina Faso", "BJ" = "Benin", "BU" = "Burundi", "CD" = "Congo Democratic Republic", "CI" = "Côte d'Ivoire",
                   "CM" = "Cameroon", "GH" = "Ghana", "GN" = "Guinea", "MD" = "Madagascar", "ML" = "Mali", "MZ" = "Mozambique", "NG" = "Nigeria", "TG" = "Togo", "UG" = "Uganda")

# rename region names column to NAME_1 for Madagascar to match the other countries, remove province name variable (provinces were dissolved in 2009)
subdivision_files[["MD"]] <- subdivision_files[["MD"]] %>%
  select(-NAME_1) %>%       # remove NAME_1 column (province names)
  rename(NAME_1 = NAME_2)   # rename NAME_2 to NAME_1 (region names)

# rename region name column for Uganda to match the other countries (different data source for Uganda so different var name)
subdivision_files[["UG"]] <- subdivision_files[["UG"]] %>%
  rename(NAME_1 = ADM1_EN)


## =========================================================================================================================================
### Create Maps Showing Agric HH Proportion per Subdivision (add Cluster points)
## =========================================================================================================================================

agric_palette <- c("#018571", "#80cdc1", "#f5f5f5", "#dfc27d", "#a6611a") # brown will be highest proportion of agric HHs

# define the top 3 populous subdivisions for each country (all data from Wikipedia, which displays most recent census data)
top_populous_subdivisions <- list(
  "Angola" = c("Luanda", "Huíla", "Benguela"),
  "Burkina Faso" = c("Centre", "Haut-Bassins", "Est"),
  "Benin" = c("Atlantique", "Borgou", "Ouémé"),
  "Burundi" = c("Gitega", "Ngozi", "Muyinga"),
  "Congo Democratic Republic" = c("Kinshasa", "Nord-Kivu", "Sud-Kivu"),
  "Côte d'Ivoire" = c("Abidjan", "Montagnes", "Sassandra-Marahoué"),
  "Cameroon" = c("Centre", "Adamaoua", "Est"),
  "Ghana" = c("Greater Accra", "Ashanti", "Eastern"),
  "Guinea" = c("Boké", "Conakry", "Faranah"),
  "Madagascar" = c("Analamanga", "Vakinankaratra", "Atsimo-Andrefana"),
  "Mali" = c("Sikasso", "Koulikoro", "Ségou"),
  "Mozambique" = c("Cabo Delgado", "Gaza", "Inhambane"),
  "Nigeria" = c("Kano", "Lagos", "Katsina"),
  "Togo" = c("Maritime", "Plateaux", "Savanes"),
  "Uganda" = c("Central", "Eastern", "Western")
)

interval_labels <- c("[0, 0.2]", "(0.2, 0.4]", "(0.4, 0.6]", "(0.6, 0.8]", "(0.8, 1]") # labels to go in legend

for (country_code in names(gps_survey_data)) { 
  # extract the specific country from the shapefile data
  country_shape <- afr.shp.base %>% filter(ISO2 == country_code)
  
  # extract the survey and gps cluster data for the current country
  country_data <- gps_survey_data[[country_code]]
  
  # extract subdivision data for the current country and make it a spatial object
  spat_country_subd <- st_as_sf(subdivision_files[[country_code]]) %>% st_make_valid()
  
  # extract survey data for the current country and make it a spatial object
  spat_gps_survey_data <- st_as_sf(gps_survey_data[[country_code]], coords = c("LONGNUM", "LATNUM"), crs = st_crs(spat_country_subd)) %>% st_make_valid()
  
  # perform spatial join to find which geographic subdivision each cluster point is in
  gps_survey_data_with_subdivision <- st_join(spat_gps_survey_data, spat_country_subd, join = st_within)
  
  # calculate agric hh proportion within each geographic subdivision
  subd_averages <- gps_survey_data_with_subdivision %>%
    group_by(NAME_1) %>% # NAME_1 is each subdivision name
    summarize(subd_proportion = mean(agric_proportion, na.rm = TRUE))
  
  # clean up any invalid geometries in the joined data - fixed error
  gps_survey_data_with_subdivision <- gps_survey_data_with_subdivision %>% st_make_valid()
  
  # join subd_averages to the gps_survey_data_with_subdivision
  gps_survey_data_with_subdivision <- gps_survey_data_with_subdivision %>% st_join(subd_averages, join = st_within)
  
  # get the bounding box of the country to help scale the plots uniformly
  bbox <- st_bbox(country_shape)
  
  # define a consistent zoom level for all countries
  buffer <- 0.1
  
  # adjust the bounding box to create uniform scaling (box around each country)
  xlim_range <- c(bbox["xmin"] - buffer, bbox["xmax"] + buffer)
  ylim_range <- c(bbox["ymin"] - buffer, bbox["ymax"] + buffer)
  
  # prepare the subd_proportion data
  subd_prop_data <- gps_survey_data_with_subdivision %>% 
    st_drop_geometry() %>%
    select(NAME_1.x, subd_proportion) %>% 
    distinct()
  
  # join this to the spatial subdivision data
  spat_country_subd_with_prop <- spat_country_subd %>%
    left_join(subd_prop_data, by = c("NAME_1" = "NAME_1.x"))
  
  # remove duplicate entries in spat_country_subd_with_prop
  spat_country_subd_with_prop <- spat_country_subd_with_prop %>%
    group_by(NAME_1) %>%
    filter(!(is.na(subd_proportion) & n() > 1)) %>%
    ungroup()
  
  # add a new column to flag the top 3 most populous subdivisions
  country_name <- country_names[country_code]
  top_subs <- top_populous_subdivisions[[country_name]]
  spat_country_subd_with_prop <- spat_country_subd_with_prop %>%
    mutate(is_top_populous = ifelse(NAME_1 %in% top_subs, TRUE, FALSE))
  
  # define breaks for 20% intervals (0, 0.2, 0.4, 0.6, 0.8, 1)
  even_cuts <- seq(0, 1, by = 0.2)
  
  # categorize subd_proportion by these even 20% intervals
  spat_country_subd_with_prop$prop_cat <- cut(
    spat_country_subd_with_prop$subd_proportion,
    breaks = even_cuts,
    labels = c(1, 2, 3, 4, 5),
    include.lowest = TRUE
  )
  
  # apply the same categorization for agricultural household proportions in country_data
  country_data$prop_cat <- cut(
    country_data$agric_proportion,
    breaks = even_cuts,
    labels = c(1, 2, 3, 4, 5),
    include.lowest = TRUE
  )
  
  # create the plot
  country_agric_map <- ggplot() +
    
    # color subdivisions by proportion using the agric_palette
    geom_sf(data = spat_country_subd_with_prop, aes(fill = prop_cat, geometry = geometry), color = "white") +
    
    # apply agric_palette to the fill scale
    scale_fill_manual(values = agric_palette, labels = interval_labels, na.value = "gray") +
    
    # # add cluster data points with jitter to prevent overlap
    # geom_jitter(data = country_data, aes(x = LONGNUM, y = LATNUM, fill = prop_cat),
    #             color = "black", size = 2.5, shape = 21, stroke = 0.2,
    #             width = 0.05, height = 0.05, show.legend = FALSE) +  # Adjust width/height for jitter effect

    # cluster data points without jitter
    geom_point(data = country_data, aes(x = LONGNUM, y = LATNUM, fill = prop_cat),
               size = 1.5, shape = 21, stroke = 0.2, show.legend = FALSE) +
    scale_color_gradientn(colors = agric_palette) +
    
    # add thick orange outlines for top 3 most populous subdivisions in each country
    geom_sf(data = filter(spat_country_subd_with_prop, is_top_populous == TRUE), 
            aes(geometry = geometry), 
            fill = NA, 
            color = "orange", 
            linewidth = 1,
            inherit.aes = FALSE) +
    
    geom_sf(data = country_shape, aes(geometry = geometry), fill = NA, color = "black") +  # country borders
    
    # set consistent zoom level so all countries appear the same size
    coord_sf(xlim = xlim_range, ylim = ylim_range, datum = NA) +
    theme_void() +
    
    # customize labels and title
    labs(title = paste(country_names[country_code], country_data$year_combo),
         fill = "Proportion of \nAgricultural \nHouseholds") +
    
    # no axes or labels
    theme(axis.text = element_blank(), axis.ticks = element_blank(), axis.title = element_blank())
  
  # assign the map plot to a variable
  assign(paste0(country_code, "_subd_plot"), country_agric_map)
}


## -----------------------------------------------------------------------------------------------------------------------------------------
### Combine Country Plots into Grid
## -----------------------------------------------------------------------------------------------------------------------------------------

# extract the combined malaria/net use legend
prop_legend <- get_only_legend(BU_subd_plot) 

# remove legends from all plots
AO_subd_plot <- AO_subd_plot + theme(legend.position = "none")
BF_subd_plot <- BF_subd_plot + theme(legend.position = "none")
BJ_subd_plot <- BJ_subd_plot + theme(legend.position = "none")
BU_subd_plot <- BU_subd_plot + theme(legend.position = "none")
CD_subd_plot <- CD_subd_plot + theme(legend.position = "none")
CI_subd_plot <- CI_subd_plot + theme(legend.position = "none")
CM_subd_plot <- CM_subd_plot + theme(legend.position = "none")
GH_subd_plot <- GH_subd_plot + theme(legend.position = "none")
GN_subd_plot <- GN_subd_plot + theme(legend.position = "none")
MD_subd_plot <- MD_subd_plot + theme(legend.position = "none")
ML_subd_plot <- ML_subd_plot + theme(legend.position = "none")
MZ_subd_plot <- MZ_subd_plot + theme(legend.position = "none")
NG_subd_plot <- NG_subd_plot + theme(legend.position = "none")
TG_subd_plot <- TG_subd_plot + theme(legend.position = "none")
UG_subd_plot <- UG_subd_plot + theme(legend.position = "none")

agric_subd_maps <- grid.arrange(
  AO_subd_plot, BF_subd_plot, BJ_subd_plot, BU_subd_plot, CD_subd_plot, 
  CI_subd_plot, CM_subd_plot, GH_subd_plot, GN_subd_plot, MD_subd_plot, 
  ML_subd_plot, MZ_subd_plot, NG_subd_plot, TG_subd_plot, UG_subd_plot, 
  nrow = 5, ncol = 3
)

# combine the plots and legend
agric_final_subd_maps <- grid.arrange(
  agric_subd_maps,
  prop_legend,
  ncol = 2,  # legend in the second column, plots in the first column
  widths = c(10, 2),  # adjust the width to give more space to the plots
  top = textGrob(
    "Proportion of Agricultural Households per Region (Only Urban Data)",
    gp = gpar(fontsize = 16, fontface = "bold", hjust = 0.5)  # center the title
  )
)

# save as .pdf
ggsave(paste0(FigDir, "/pdf_figures/", Sys.Date(),"_agric_maps.pdf"), agric_final_subd_maps, width = 10, height = 15)  

