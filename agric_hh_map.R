# ==========================================================================================================================================
# Script Name: Agric HHs by Cluster in Each Country
# Author: Grace Legris, Research Data Analyst (gracebea@gmail.com)
# Edited: [2024-11-1]
# Purpose: Generate map of proportion of agric HHs by cluster in each country
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

# Read the shapefile for Africa country boundaries
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

# read in urban and rural survey data
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
  "MDGE81FL" = "MD",  # Moldova
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

# Initialize a list to store merged datasets for each country
gps_survey_data <- list()

# Loop through each shapefile and merge with the survey data
for (country_code in names(shapefiles)) {
  # Extract the shapefile for the current country
  gps_data <- shapefiles[[country_code]]
  
  # Filter the survey data for the corresponding country
  country_survey_data <- urban_df[urban_df$DHS_CountryCode == toupper(country_code), ]
  
  # Ensure that the types of DHSCLUST and hv001 are the same
  gps_data$DHSCLUST <- as.character(gps_data$DHSCLUST)
  country_survey_data$hv001 <- as.character(country_survey_data$hv001)
  
  # Merge the shapefile with the filtered survey data
  merged_data <- merge(country_survey_data, gps_data, by.x = "hv001", by.y = "DHSCLUST", all.x = TRUE)
  
  # Store the merged dataset in the list
  gps_survey_data[[country_code]] <- merged_data
  
  # Select only the needed columns for simplicity
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
### Read in Subdivision Shapefiles (e.g. State, Region, etc), Collect 3 Largest Cities in Each Country (by Population)
## -----------------------------------------------------------------------------------------------------------------------------------------

library(sf)

# read in the shapefiles with subdivision geographic boundaries (state, region, etc)
ao.subd <- st_read(file.path(PopDir, "data", "opened", "GPS", "subdivisions", "AO", "gadm41_AGO_1.shp"))
bf.subd <- st_read(file.path(PopDir, "data", "opened", "GPS", "subdivisions", "BF", "gadm41_BFA_1.shp"))
bj.subd <- st_read(file.path(PopDir, "data", "opened", "GPS", "subdivisions", "BJ", "gadm41_BEN_1.shp"))
bu.subd <- st_read(file.path(PopDir, "data", "opened", "GPS", "subdivisions", "BU", "gadm41_BDI_1.shp"))
cd.subd <- st_read(file.path(PopDir, "data", "opened", "GPS", "subdivisions", "CD", "gadm41_COD_1.shp"))
ci.subd <- st_read(file.path(PopDir, "data", "opened", "GPS", "subdivisions", "CI", "gadm41_CIV_1.shp"))
cm.subd <- st_read(file.path(PopDir, "data", "opened", "GPS", "subdivisions", "CM", "gadm41_CMR_1.shp"))
gh.subd <- st_read(file.path(PopDir, "data", "opened", "GPS", "subdivisions", "GH", "gadm41_GHA_1.shp"))
gn.subd <- st_read(file.path(PopDir, "data", "opened", "GPS", "subdivisions", "GN", "gadm41_GIN_1.shp"))
md.subd <- st_read(file.path(PopDir, "data", "opened", "GPS", "subdivisions", "MD", "gadm41_MDG_1.shp"))
ml.subd <- st_read(file.path(PopDir, "data", "opened", "GPS", "subdivisions", "ML", "gadm41_MLI_1.shp"))
mz.subd <- st_read(file.path(PopDir, "data", "opened", "GPS", "subdivisions", "MZ", "gadm41_MOZ_1.shp"))
ng.subd <- st_read(file.path(PopDir, "data", "opened", "GPS", "subdivisions", "NG", "gadm41_NGA_1.shp"))
tg.subd <- st_read(file.path(PopDir, "data", "opened", "GPS", "subdivisions", "TG", "gadm41_TGO_1.shp"))
ug.subd <- st_read(file.path(PopDir, "data", "opened", "GPS", "subdivisions", "UG", "gadm41_UGA_1.shp"))

subdivision_files <- list(
  "AO" = ao.subd, "BF" = bf.subd, "BJ" = bj.subd, "BU" = bu.subd, "CD" = cd.subd,
  "CI" = ci.subd, "CM" = cm.subd, "GH" = gh.subd, "GN" = gn.subd, "MD" = md.subd,
  "ML" = ml.subd, "MZ" = mz.subd, "NG" = ng.subd, "TG" = tg.subd, "UG" = ug.subd
)

country_names <- c("AO" = "Angola", "BF" = "Burkina Faso", "BJ" = "Benin", "BU" = "Burundi", "CD" = "Congo Democratic Republic", "CI" = "Côte d'Ivoire",
                   "CM" = "Cameroon", "GH" = "Ghana", "GN" = "Guinea", "MD" = "Madagascar", "ML" = "Mali", "MZ" = "Mozambique", "NG" = "Nigeria", "TG" = "Togo", "UG" = "Uganda")

# create a data frame for the coordinates of the 3 largest cities for each country
city_coords <- data.frame(
  country_code = c("AO", "AO", "AO", "BF", "BF", "BF", "BJ", "BJ", "BJ", "BU", "BU", "BU", 
                   "CD", "CD", "CD", "CI", "CI", "CI", "CM", "CM", "CM", "GH", "GH", "GH", 
                   "GN", "GN", "GN", "MD", "MD", "MD", "ML", "ML", "ML", "MZ", "MZ", "MZ", 
                   "NG", "NG", "NG", "TG", "TG", "TG", "UG", "UG", "UG"),
  city_name = c("Luanda", "Huambo", "Lobito", "Ouagadougou", "Bobo-Dioulasso", "Ouahigouya",
                "Cotonou", "Porto-Novo", "Parakou", "Bujumbura", "Muyinga", "Ruyigi",
                "Kinshasa", "Lubumbashi", "Mbuji-Mayi", "Abidjan", "Bouaké", "San-Pedro",
                "Douala", "Yaoundé", "Bamenda", "Accra", "Kumasi", "Sekondi-Takoradi",
                "Conakry", "Nzérékoré", "Guéckédou", "Antananarivo", "Toamasina", "Antsirabe",
                "Bamako", "Sikasso", "Kalabancoro", "Maputo", "Matola", "Nampula",
                "Lagos", "Kano", "Ibadan", "Lomé", "Sokodé", "Kara", "Kampala", "Nansana", "Kira Town"),
  latitude = c(-8.8368, -12.7761, -12.3601, 12.6392, 11.1786, 12.2500, 6.3667, 6.4969, 9.6884,
               -3.3801, -3.4261, -2.9711, -4.4419, -11.6500, -6.1500, 5.3097, 7.6920, 6.8719,
               3.8480, 4.0511, 5.9631, 5.6037, 6.6892, 9.4000, 9.5091, 7.7464, 10.3861, -18.8792,
               -18.1475, -19.8656, 12.6392, 13.4286, 14.4711, -25.9653, -19.8411, -15.1167, 6.5244,
               12.0022, 7.3775, 6.1375, 10.3569, 9.5500, 0.3136, 0.0610, -0.6114),
  longitude = c(13.2343, 15.7392, 13.5463, -1.7668, -4.2965, -2.2833, 2.4270, 2.6280, 1.6607,
                29.3644, 29.9303, 29.9486, 15.2663, 27.4800, 23.6000, -4.0127, -5.0569, -6.4536,
                11.5021, 9.7075, 10.1596, -0.1870, -1.6244, -0.8500, -13.7127, -8.8080, -9.3044,
                47.5079, 49.4023, 47.0304, -8.0029, -6.2700, -5.8541, 32.5892, 34.8384, 39.2667,
                3.3792, 8.5916, 3.9470, 1.2122, 0.1750, 1.1833, 32.5818, 32.4467, 30.6544)
)
head(city_coords)

## -----------------------------------------------------------------------------------------------------------------------------------------
### Create Maps
## -----------------------------------------------------------------------------------------------------------------------------------------

# plot proportion of agric HHs in each cluster (include subdivisions)
for (country_code in names(gps_survey_data)) { 
  # Extract the specific country from the shapefile data
  country_shape <- afr.shp.base %>% filter(ISO2 == country_code)
  
  # Extract the survey and GPS cluster data for the current country
  country_data <- gps_survey_data[[country_code]]
  
  # Extract the subdivision data for the current country
  country_subd <- subdivision_files[[country_code]]
  
  # Get the bounding box of the country to help scale the plots uniformly
  bbox <- st_bbox(country_shape)
  
  # Define a consistent zoom level for all countries
  buffer <- 0.1  # Adjust this value to control how much larger the bounding box will be
  
  # Adjust the bounding box to create uniform scaling
  xlim_range <- c(bbox["xmin"] - buffer, bbox["xmax"] + buffer)
  ylim_range <- c(bbox["ymin"] - buffer, bbox["ymax"] + buffer)
  
  # Subset the city coordinates for the current country
  city_data <- city_coords[city_coords$country_code == country_code, ]
  
  # Create the map with subdivisions and cities
  country_agric_map <- ggplot() +
    geom_sf(data = country_shape, aes(geometry = geometry), fill = "white", color = "black") +
    geom_sf(data = country_subd, aes(geometry = geometry), fill = NA, color = "gray") +  # Plot subdivisions
    geom_point(data = country_data, aes(x = LONGNUM, y = LATNUM, color = agric_proportion), size = 1, alpha = 0.7) +
    scale_color_gradient(low = "lightblue", high = "navy") + 
    # Plot stars for the 3 largest cities
    geom_point(data = city_data, aes(x = longitude, y = latitude), shape = 17, color = "gold", fill = "yellow", size = 3) +
    # Add text labels for the 3 largest cities
    geom_text(data = city_data, aes(x = longitude, y = latitude, label = city_name), 
              color = "black", hjust = -0.1, vjust = -0.5, size = 3) + # Adjust the position of the text labels
    coord_sf(xlim = xlim_range, ylim = ylim_range, datum = NA) +  # Apply consistent zoom
    theme_void() +  # Remove axes and labels
    labs(x = NULL, y = NULL) +
    labs(title = paste(country_names[country_code], country_data$dhs_year[1]),
         color = "Proportion of \nAgricultural Households") +
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank()
    )
  
  # Assign a name to the plot
  assign(paste0(country_code, "_plot"), country_agric_map)
}

## -----------------------------------------------------------------------------------------------------------------------------------------
### Combine Country Plots into Grid
## -----------------------------------------------------------------------------------------------------------------------------------------

# function to extract the combined malaria/net use legend
get_only_legend <- function(AO_plot) { 
  plot_table <- ggplot_gtable(ggplot_build(AO_plot))  
  legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box")  
  legend <- plot_table$grobs[[legend_plot]] 
  return(legend)  
}

prop_legend <- get_only_legend(AO_plot) 

# remove legends from all plots
AO_plot <- AO_plot + theme(legend.position = "none")
BF_plot <- BF_plot + theme(legend.position = "none")
BJ_plot <- BJ_plot + theme(legend.position = "none")
BU_plot <- BU_plot + theme(legend.position = "none")
CD_plot <- CD_plot + theme(legend.position = "none")
CI_plot <- CI_plot + theme(legend.position = "none")
CM_plot <- CM_plot + theme(legend.position = "none")
GH_plot <- GH_plot + theme(legend.position = "none")
GN_plot <- GN_plot + theme(legend.position = "none")
MD_plot <- MD_plot + theme(legend.position = "none")
ML_plot <- ML_plot + theme(legend.position = "none")
MZ_plot <- MZ_plot + theme(legend.position = "none")
NG_plot <- NG_plot + theme(legend.position = "none")
TG_plot <- TG_plot + theme(legend.position = "none")
UG_plot <- UG_plot + theme(legend.position = "none")


agric_maps <- grid.arrange(
  AO_plot, BF_plot, BJ_plot, BU_plot, CD_plot, CI_plot, CM_plot, GH_plot, GN_plot, MD_plot, ML_plot, MZ_plot, NG_plot, TG_plot, UG_plot, 
  nrow = 5, ncol = 3
)

# combine the plots and legend
agric_final_maps <- grid.arrange(
  agric_maps,
  prop_legend,
  ncol = 2,  # legend in the second column, plots in the first column
  widths = c(10, 2),  # adjust the width to give more space to the plots
  top = textGrob(
    "Proportion of Agricultural Households per Cluster (Only Urban Data)",
    gp = gpar(fontsize = 16, fontface = "bold", hjust = 0.5)  # center the title
  )
)

# save as .pdf
ggsave(paste0(FigDir, "/pdf_figures/", Sys.Date(),"_agric_final_maps.pdf"), agric_final_maps, width = 10, height = 15)  


## =========================================================================================================================================
### Create Maps Showing Agric HH Proportion per Subdivision (not Cluster)
## =========================================================================================================================================

# plot proportion of agric HHs in each cluster (include subdivisions)
for (country_code in names(gps_survey_data)) { 
  # Extract the specific country from the shapefile data
  country_shape <- afr.shp.base %>% filter(ISO2 == country_code)
  
  # Extract the survey and GPS cluster data for the current country
  country_data <- gps_survey_data[[country_code]]
  
  # extract subdivision data for the current country and make it a spatial object
  spat_country_subd <- st_as_sf(subdivision_files[[country_code]])
  
  # extract survey data for the current country and make it a spatial object
  spat_gps_survey_data <- st_as_sf(gps_survey_data[[country_code]], coords = c("LONGNUM", "LATNUM"), crs = st_crs(country_subd))
  
  # perform spatial join to find which geographic subdivision each cluster point is in (use this to map agric proportion by subdivision)
  gps_survey_data_with_subdivision <- st_join(spat_gps_survey_data, spat_country_subd["NAME_1"], join = st_within)
  
  # calculate agric HH proportion within each geographic subdivision
  subd_averages <- gps_survey_data_with_subdivision %>%
    group_by(NAME_1) %>%
    summarize(subd_proportion = mean(agric_proportion, na.rm = TRUE))
  gps_survey_data_with_subdivision <- gps_survey_data_with_subdivision %>%
    st_join(subd_averages, by = "NAME_1")
  
  # Get the bounding box of the country to help scale the plots uniformly
  bbox <- st_bbox(country_shape)
  
  # Define a consistent zoom level for all countries
  buffer <- 0.1  # Adjust this value to control how much larger the bounding box will be
  
  # Adjust the bounding box to create uniform scaling
  xlim_range <- c(bbox["xmin"] - buffer, bbox["xmax"] + buffer)
  ylim_range <- c(bbox["ymin"] - buffer, bbox["ymax"] + buffer)
  
  # Subset the city coordinates for the current country
  city_data <- city_coords[city_coords$country_code == country_code, ]
  
  # First, join the subd_proportion data to the subdivision shapefile
  spat_country_subd_with_prop <- spat_country_subd %>%
    st_join(gps_survey_data_with_subdivision %>% 
              select(NAME_1.x, subd_proportion) %>% 
              distinct(),
            by = c("NAME_1" = "NAME_1.x"))
  
  # Now create the plot
  country_agric_map <- ggplot() +
    geom_sf(data = country_shape, aes(geometry = geometry), fill = "white", color = "black") +
    geom_sf(data = spat_country_subd_with_prop, aes(fill = subd_proportion, geometry = geometry), color = "black") +  # Color subdivisions by proportion
    scale_fill_gradient(low = "#CCCCFF", high = "#000080", na.value = "gray90") +
    
    # Plot yellow stars for the 3 largest cities
    geom_point(data = city_data, aes(x = longitude, y = latitude), shape = 24, color = "black", fill = "gold", size = 4, stroke = 1.5) +
    
    # Add text labels for the 3 largest cities
    geom_text(data = city_data, aes(x = longitude, y = latitude, label = city_name),
              color = "black", hjust = -0.1, vjust = -0.5, size = 4) +
    
    # Set consistent zoom level
    coord_sf(xlim = xlim_range, ylim = ylim_range, datum = NA) +
    theme_void() +  # Remove axes and labels
    
    # Customize labels and title
    labs(title = paste(country_names[country_code], country_data$year_combo),
         fill = "Proportion of \nAgricultural Households") +
    
    # Adjust theme for a clean look
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank()
    )
  
  # Assign the map plot to a variable
  assign(paste0(country_code, "_subd_plot"), country_agric_map)
}

## -----------------------------------------------------------------------------------------------------------------------------------------
### Combine Country Plots into Grid
## -----------------------------------------------------------------------------------------------------------------------------------------

# function to extract the combined malaria/net use legend
get_only_legend <- function(AO_subd_plot) { 
  plot_table <- ggplot_gtable(ggplot_build(AO_subd_plot))  
  legend_plot <- which(sapply(plot_table$grobs, function(x) x$name) == "guide-box")  
  legend <- plot_table$grobs[[legend_plot]] 
  return(legend)  
}

prop_legend <- get_only_legend(AO_subd_plot) 

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
ggsave(paste0(FigDir, "/pdf_figures/", Sys.Date(),"_agric_final_subd_maps.pdf"), agric_final_subd_maps, width = 10, height = 15)  

