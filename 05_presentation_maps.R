# ==========================================================================================================================================
# Script Name: Maps for Presentation
# Author: Grace Legris, Research Data Analyst (gracebea@gmail.com)
# Purpose: Create malaria prevalence map, population map, and calculate summary statistics.
# ==========================================================================================================================================

rm(list=ls())

user <- Sys.getenv("USER")
if ("grace" %in% user) {
  Drive <- "/Users/grace/Urban Malaria Proj Dropbox"
  DriveDir <- file.path(Drive, "urban_malaria")
  PopDir <- file.path(DriveDir, "data", 'data_agric_analysis')
  ManDir <- file.path(DriveDir, "projects", "Manuscripts", "ongoing", "agriculture_malaria_manuscript")
  FigDir <- file.path(ManDir, "figures", "main_figures")
  SupDir <- file.path(ManDir, "figures", "supplementary", "pdf_figures")
  ExpDir <- file.path(ManDir, "figures", "exploratory")
  DataDir <- file.path(DriveDir, "data")
  AbidjanDir <-  file.path(DataDir, "abidjan")
  DHS_data <- file.path(AbidjanDir, 'DHS')
  program_dat <- file.path(AbidjanDir, "program_data")
  Abi_shapefile <- readRDS(file.path(program_dat, "shapefilesCIV.rds"))
  ProjectDir <- file.path(DriveDir, "projects", "urban_microstratification", "Abidjan_CI")
  plots <- file.path(ProjectDir, "plots")
}

map_theme <- function(){
  theme(axis.text.x = ggplot2::element_blank(),
        axis.text.y = ggplot2::element_blank(),
        axis.ticks = ggplot2::element_blank(),
        rect = ggplot2::element_blank(),
        plot.background = ggplot2::element_rect(fill = "white", colour = NA), 
        plot.title = element_text(hjust = 0.5),
        legend.title.align=0.5,
        legend.title=element_text(size=8, colour = 'black'), 
        legend.text =element_text(size = 8, colour = 'black'),
        legend.key.height = unit(0.65, "cm"))
}

theme_manuscript <- function(){
  theme_bw() + 
    theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size = 12, color = "black"), 
          axis.text.y = element_text(size = 12, color = "black"),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size =12),
          legend.title=element_text(size=12, colour = 'black'),
          legend.text =element_text(size = 12, colour = 'black'),
          legend.key.height = unit(1, "cm"))
}

# #read files function 
# read.files <- function(path, general_pattern, specific_pattern, fun) {
#   files <- list.files(path = path , pattern = general_pattern, full.names = TRUE, recursive = TRUE)
#   files<- files[(grep(specific_pattern, files))]
#   sapply(files, fun, simplify = F)
# }


options(survey.lonely.psu="adjust") 

# Read in PR and KR data files and rename state variable
# pr_files <- read.files(DHS_data, "*CIPR.*\\.DTA", 'CIPR62FL|CIPR81FL', read_dta)
# hr_files <- read.files(DHS_data, "*CIHR.*\\.DTA", 'CIHR62FL|CIHR81FL', read_dta)

# grace read in datasets
pr_data <- read_dta(file.path(DHS_data, "2021_CI", "CIPR81DT", "CIPR81FL.DTA"))
hr_data <- read_dta(file.path(DHS_data, "2021_CI", "CIHR81DT", "CIHR81FL.DTA"))

# rename region column
pr_data$district <- as_label(pr_data$hv024)
hr_data$district <- as_label(hr_data$hv024)

# pr_files[[1]]$region <- as_label(pr_files[[1]]$hv024)
# pr_files[[2]]$region <- as_label(pr_files[[2]]$hv024)
# hr_files[[1]]$region <- as_label(hr_files[[1]]$hv024)
# hr_files[[2]]$region <- as_label(hr_files[[2]]$hv024)
# 
# pr_data <- bind_rows(pr_files)
# hr_data <- bind_rows(hr_files)


# lapply(pr_files, function(x) table(x$hml32))
# lapply(pr_files, function(x) table(x$hv006))


# kr_files[[5]]$state <- as_label(kr_files[[5]]$sstate)
# kr_files[[6]]$state <- as_label(kr_files[[6]]$sstate)
# kr_files[[7]]$state <- as_label(kr_files[[7]]$v024)
# kr_data <- bind_rows(kr_files)


#load spatial points
# sf12 = sf::st_read(file.path(DHS_data, "2012_CI", "CIGE61FL", "CIGE61FL.shp"),)

sf21 = sf::st_read(file.path(DHS_data, "2021_CI", "CIGE81FL", "CIGE81FL.shp"),) 
sf21 <- sf21 %>% rename(cluster = DHSCLUST)

# sf_all = rbind(sf12, sf21) %>%  
#   rename(cluster = DHSCLUST)

## -----------------------------------------------------------------------------------------------------------------------------------------
### Compute malaria prevalence in Cote d'Ivoire at the regional level
## -----------------------------------------------------------------------------------------------------------------------------------------

malaria_prev <- pr_data %>%
  filter(hv042 == 1 & hv103 == 1 & hc1 %in% c(6:59) ) %>%
  filter(hml32 <= 1) %>% 
  mutate(malaria = ifelse(hml32 == 1, 1, 0), wt = hv005/1000000, id  = hv021, strat=hv022) %>% 
  srvyr:: as_survey_design(., ids= id,strata=strat,nest=T,weights= wt) %>%
  group_by(district, year = hv007) %>% 
  summarize( prev =round(survey_mean(malaria),2) * 100,
             total_malaria = survey_total(), first_month_survey = as.character(first(hv006))) %>%
  mutate(class= cut(prev,  c(seq(0, 20, 5),30,50, 100), include.lowest = T)) 

## -----------------------------------------------------------------------------------------------------------------------------------------
### Compute malaria prevalence in Cote d'Ivoire at the cluster level
## -----------------------------------------------------------------------------------------------------------------------------------------

malaria_prev <- pr_data %>%
  filter(hv042 == 1 & hv103 == 1 & hc1 %in% c(6:59) ) %>%
  filter(hml32 <= 1) %>% 
  mutate(malaria = ifelse(hml32 == 1, 1, 0), wt = hv005/1000000, id  = hv021, strat=hv022) %>% 
  srvyr:: as_survey_design(., ids= id,strata=strat,nest=T,weights= wt) %>%
  group_by(cluster = hv001, year = hv007) %>% 
  summarize( prev =round(survey_mean(malaria),2) * 100,
             total_malaria = survey_total(), first_month_survey = as.character(first(hv006))) %>%
  mutate(class= cut(prev,  c(seq(0, 20, 5),30,50, 100), include.lowest = T)) %>% 
  inner_join(sf21, by = c("year" = "DHSYEAR","cluster" = "cluster" )) %>%
  drop_na(prev)

## -----------------------------------------------------------------------------------------------------------------------------------------
### Read in health districts shapefiles for Abidjan
## -----------------------------------------------------------------------------------------------------------------------------------------

Abidjan = Abi_shapefile[[3]] %>% filter(NAME_1 == "Abidjan")
df_abidjan1 = st_intersection(Abi_shapefile[[7]], Abidjan)
ggplot(data = Abidjan)+
  geom_sf(data = df_abidjan1, color = "black", fill = 	"#ece9f7")+
  geom_text_repel(
    data = df_abidjan1,
    aes(label = str_to_sentence(NOM), geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  labs(title="All 15 health districts of Abidjan", 
       fill = "", x = NULL, y = NULL)+
  map_theme()

## -----------------------------------------------------------------------------------------------------------------------------------------
### Malaria prevalence map for Abidjan district
## -----------------------------------------------------------------------------------------------------------------------------------------

new_plottingdata <- sf::st_as_sf(malaria_prev, coords = c("LONGNUM", "LATNUM"))

sf::st_crs(new_plottingdata) <- 4326
sf::st_crs(df_abidjan1) <- 4326

sf::st_crs(new_plottingdata) <-  sf::st_crs(df_abidjan1)

new_plotd <- st_intersection(df_abidjan1, new_plottingdata)  

discrete_palettes <- list(rev(RColorBrewer::brewer.pal(7, "RdYlBu")))

abidjan_plot <- ggplot(data = df_abidjan1) +
  geom_sf(fill = "white", color = "black") +
  geom_sf(data = new_plotd, aes(fill = class, geometry =geometry), size=3, shape=21)+
  geom_text_repel(
    data = df_abidjan1,
    aes(label = str_to_sentence(NOM), geometry = geometry),color ='black',
    stat = "sf_coordinates", min.segment.length = 0, size = 3.5, force = 1)+
  labs(title = "Positivity by Cluster in Abidjan District, Cote d'Ivoire (2021)", subtitle = '', fill = "TPR ", x = "", y = "", color = "") +
  scale_fill_discrete(drop=FALSE, name="TPR", type = discrete_palettes)+
  scale_color_discrete(drop=FALSE, name="TPR", type = discrete_palettes)+
  map_theme()

ggsave(
  filename = file.path(plots, "Abidjan_dhs_clusters_regular.pdf"),
  plot = abidjan_plot, 
  width = 12, height = 9, dpi = 300, device = "pdf"
)

## -----------------------------------------------------------------------------------------------------------------------------------------
### 1. Malaria prevalence map for Côte d’Ivoire using DHS 2021 data, showing both overall and cluster-level prevalence.
## -----------------------------------------------------------------------------------------------------------------------------------------

# read the shapefile for CI districts
district_shp <- st_read(file.path(DriveDir, "data", "data_agric_analysis", "gadm41_CIV_shp", "gadm41_CIV_1.shp"))

# compute malaria prevalence at district level
district_malaria_prev <- pr_data %>%
  filter(hv042 == 1, hv103 == 1, hc1 %in% 6:59, hml32 <= 1) %>%
  mutate(malaria = as.integer(hml32 == 1), wt = hv005 / 1e6, id = hv021, strat = hv022) %>%
  srvyr::as_survey_design(ids = id, strata = strat, nest = TRUE, weights = wt) %>%
  group_by(district, year = hv007) %>%
  summarize(prev = round(survey_mean(malaria), 2) * 100, total_malaria = survey_total()) %>%
  mutate(class = cut(prev, c(seq(0, 20, 5), 30, 50, 100), include.lowest = TRUE))

# rename districts to match the shapefile format
district_malaria_prev <- district_malaria_prev %>%
  mutate(district = case_when(
    district == "bas sassandra" ~ "Bas-Sassandra",
    district == "comoe" ~ "Comoé",
    district == "denguele" ~ "Denguélé",
    district == "goh-djiboua" ~ "Gôh-Djiboua",
    district == "sassandra-marahoue" ~ "Sassandra-Marahoué",
    district == "vallee du bandama" ~ "Vallée du Bandama",
    TRUE ~ str_to_title(district)  # capitalize other district names
  ))

# prepare spatial data
district_map_data <- district_shp %>%
  left_join(district_malaria_prev, by = c("NAME_1" = "district"))

cluster_plot_data <- sf::st_as_sf(malaria_prev, coords = c("LONGNUM", "LATNUM"), crs = 4326)

# create country-level malaria prevalence map
civ_malaria_plot <- ggplot(district_map_data) +
  geom_sf(aes(fill = class), color = "black", linewidth = 0.25) +
  geom_sf(data = cluster_plot_data, aes(fill = class), size = 4, shape = 21, alpha = 0.7, color = "black", stroke = 0.25) +
  geom_text_repel(aes(label = str_to_sentence(NAME_1), geometry = geometry), 
                  stat = "sf_coordinates", min.segment.length = 0, size = 6, force = 5) +
  labs(title = "Malaria Prevalence in Cote d'Ivoire (2021)", 
       subtitle = "District-level Prevalence and Cluster Positivity", 
       fill = "Malaria Prevalence (%)",
       x = NULL,
       y = NULL) + 
  scale_fill_manual(values = rev(RColorBrewer::brewer.pal(7, "RdYlBu")), 
                    name = "Malaria Prevalence (%)",
                    drop = FALSE) +
  map_theme() +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18),
    plot.title = element_text(size = 20, hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5)
  )
civ_malaria_plot

ggsave(file.path(plots, "CIV_malaria_districts_clusters.pdf"), civ_malaria_plot, width = 12, height = 12, dpi = 300)

## -----------------------------------------------------------------------------------------------------------------------------------------
### 2.	Population map for Côte d’Ivoire, using DHS-representative district boundaries
## -----------------------------------------------------------------------------------------------------------------------------------------

# read in population raster for CI 2021
ci_raster <- raster(file.path(DataDir, "data_agric_analysis", "population_rasters", "cote d'ivoire.tif"))

# extract population data for CI by district
library(raster)
library(sf)
library(dplyr)
library(exactextractr)
library(officer)

# ensure CRS matches
if (!st_crs(district_shp) == crs(ci_raster)) {
  district_shp <- st_transform(district_shp, crs(ci_raster))
}

# extract total population for each subdivision
pop_data <- exact_extract(ci_raster, district_shp, fun = "sum", progress = FALSE)

# combine with subdivision names
district_shp$pop_total <- pop_data

# create .csv file with the population estimates
district_populations <- district_shp %>%
  st_drop_geometry() %>%
  select(NAME_1, pop_total) %>%
  rename("District" = NAME_1, "Population Estimate" = pop_total)
write.csv(district_populations, 
          file = file.path(DataDir, "data_agric_analysis", "analysis_dat", "CI_pop_by_district.csv"), 
          row.names = FALSE)

# create country-level population map
civ_population_plot <- ggplot(district_shp) +
  geom_sf(aes(fill = pop_total), color = "black", linewidth = 0.25) +
  geom_text_repel(aes(label = str_to_sentence(NAME_1), geometry = geometry), 
                  stat = "sf_coordinates", min.segment.length = 0, size = 6, force = 5) +
  labs(title = "Population in Cote d'Ivoire (2020)", 
       subtitle = "District-level Population",
       fill = "Total Population",
       x = NULL,
       y = NULL) + 
  scale_fill_gradient(name = "Total Population", 
                      low = "#87CEFB", high = "#00008C",
                      breaks = c(1000000, 2000000, 3000000, 4000000, 5000000), 
                      labels = scales::comma) +
  map_theme() +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18),
    plot.title = element_text(size = 20, hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5)
  )
civ_population_plot

ggsave(file.path(plots, "CIV_population_districts.pdf"), civ_population_plot, width = 12, height = 12, dpi = 300)

## -----------------------------------------------------------------------------------------------------------------------------------------
### 3.	Summary Statistics: Overall malaria prevalence for Abidjan vs. the rest of the districts
## -----------------------------------------------------------------------------------------------------------------------------------------

# compute prevalence for districts other than Abidjan
malaria_prev <- pr_data %>%
  filter(hv042 == 1 & hv103 == 1 & hc1 %in% c(6:59) ) %>%
  filter(hml32 <= 1) %>% 
  filter(district != "Abidjan") %>%
  mutate(malaria = ifelse(hml32 == 1, 1, 0), wt = hv005/1000000, id  = hv021, strat=hv022) %>% 
  srvyr:: as_survey_design(., ids= id,strata=strat,nest=T,weights= wt) %>%
  summarize( prev =round(survey_mean(malaria),2) * 100,
             total_malaria = survey_total()) %>%
  drop_na(prev)

## -----------------------------------------------------------------------------------------------------------------------------------------
### 4.	Net Use for Côte d’Ivoire, using DHS-representative district boundaries
## -----------------------------------------------------------------------------------------------------------------------------------------

# compute net use at cluster level
nets_cluster <- pr_data %>%
  filter(hv103 == 1) %>% 
  mutate(net_use = ifelse(hml12 %in% c(1) ,1, 0), wt = hv005/1000000, id  = hv021, strat=hv022) %>% 
  srvyr::as_survey_design(ids = id, strata = strat, nest = TRUE, weights = wt) %>%
  group_by(cluster = hv001, year = hv007) %>% 
  summarize(net_use_new =round(survey_mean(net_use),2) * 100,
            total_pop_net_use = survey_total(), first_month_survey = as.character(first(hv006))) %>%
  mutate(class= cut(net_use_new,   c(seq(0, 20, 5),30,50, 60, 70, 80, 90, 100), include.lowest = T)) %>% 
  inner_join(sf21, by = c("year" = "DHSYEAR","cluster" = "cluster" )) %>%
  drop_na(net_use_new)

# compute net use at district level
nets_district <- pr_data %>%
  filter(hv103 == 1) %>% 
  mutate(net_use = ifelse(hml12 %in% c(1) ,1, 0), wt = hv005/1000000, id  = hv021, strat=hv022) %>% 
  srvyr::as_survey_design(ids = id, strata = strat, nest = TRUE, weights = wt) %>%
  group_by(district, year = hv007) %>%
  summarize(net_use_new =round(survey_mean(net_use),2) * 100,
            total_pop_net_use = survey_total(), first_month_survey = as.character(first(hv006))) %>%
  mutate(class= cut(net_use_new,   c(seq(0, 20, 5),30,50, 60, 70, 80, 90, 100), include.lowest = T))

# rename districts to match the shapefile format
nets_district <- nets_district %>%
  mutate(district = case_when(
    district == "bas sassandra" ~ "Bas-Sassandra",
    district == "comoe" ~ "Comoé",
    district == "denguele" ~ "Denguélé",
    district == "goh-djiboua" ~ "Gôh-Djiboua",
    district == "sassandra-marahoue" ~ "Sassandra-Marahoué",
    district == "vallee du bandama" ~ "Vallée du Bandama",
    TRUE ~ str_to_title(district)  # capitalize other district names
  ))

# prepare spatial data
district_net_map_data <- district_shp %>%
  left_join(nets_district, by = c("NAME_1" = "district"))

cluster_plot_data <- sf::st_as_sf(nets_cluster, coords = c("LONGNUM", "LATNUM"), crs = 4326)

# create country-level malaria prevalence map
civ_net_plot <- ggplot(district_net_map_data) +
  geom_sf(aes(fill = class), color = "black", linewidth = 0.25) +
  geom_sf(data = cluster_plot_data, aes(fill = class), size = 4, shape = 21, alpha = 0.7, color = "black", stroke = 0.25) +
  geom_text_repel(aes(label = str_to_sentence(NAME_1), geometry = geometry), 
                  stat = "sf_coordinates", min.segment.length = 0, size = 6, force = 5) +
  labs(title = "U5 Net Use by District and Cluster in Cote d'Ivoire (2021)", 
       fill = "U5 Net Use (%)",
       x = NULL,
       y = NULL) + 
  scale_fill_manual(values = rev(RColorBrewer::brewer.pal(11, "RdYlBu")), 
                    name = "U5 Net Use (%)",
                    drop = FALSE) +
  map_theme() +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18),
    plot.title = element_text(size = 20, hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5)
  )
civ_net_plot

ggsave(file.path(plots, "CIV_nets_districts_clusters.pdf"), civ_net_plot, width = 12, height = 12, dpi = 300)

## -----------------------------------------------------------------------------------------------------------------------------------------
### 5.	Net Use Given Access for Côte d’Ivoire, using DHS-representative district boundaries
## -----------------------------------------------------------------------------------------------------------------------------------------

# compute net use given access at cluster level
net_access_cluster <- hr_data %>%
  mutate(potuse = 2 * rowSums(dplyr::select(., contains('hml10')),na.rm=T),
         slept_night = rowSums(dplyr::select(., contains('hv103')), na.rm=T),
         potuse2 = ifelse(potuse/slept_night > 1, slept_night, potuse),
         access = potuse2/slept_night,
         wt = hv005/1000000, id  = hv021, strat=hv022) %>% 
  drop_na(access) %>% 
  srvyr:: as_survey_design(., ids= id,strata=strat,nest=T,weights= wt) %>%
  group_by(cluster = hv001, year = hv007) %>% 
  summarize(net_access =round(survey_mean(access),2) * 100,
            total_pop_net_access = survey_total(), first_month_survey = as.character(first(hv006))) %>%
  mutate(class= cut(net_access,  c(seq(0, 20, 5),30,50, 60, 70, 80, 90, 100), include.lowest = T)) %>% 
  inner_join(sf21, by = c("year" = "DHSYEAR","cluster" = "cluster" )) %>%
  drop_na(net_access)

# compute net use given access at district level
net_access_district <- hr_data %>%
  mutate(potuse = 2 * rowSums(dplyr::select(., contains('hml10')),na.rm=T),
         slept_night = rowSums(dplyr::select(., contains('hv103')), na.rm=T),
         potuse2 = ifelse(potuse/slept_night > 1, slept_night, potuse),
         access = potuse2/slept_night,
         wt = hv005/1000000, id  = hv021, strat=hv022) %>% 
  drop_na(access) %>% 
  srvyr::as_survey_design(ids = id, strata = strat, nest = TRUE, weights = wt) %>%
  group_by(district, year = hv007) %>%
  summarize(net_access =round(survey_mean(access),2) * 100,
            total_pop_net_access = survey_total(), first_month_survey = as.character(first(hv006))) %>%
  mutate(class= cut(net_access, c(seq(0, 20, 5),30,50, 60, 70, 80, 90, 100), include.lowest = T))

# rename districts to match the shapefile format
net_access_district <- net_access_district %>%
  mutate(district = case_when(
    district == "bas sassandra" ~ "Bas-Sassandra",
    district == "comoe" ~ "Comoé",
    district == "denguele" ~ "Denguélé",
    district == "goh-djiboua" ~ "Gôh-Djiboua",
    district == "sassandra-marahoue" ~ "Sassandra-Marahoué",
    district == "vallee du bandama" ~ "Vallée du Bandama",
    TRUE ~ str_to_title(district)  # capitalize other district names
  ))

# prepare spatial data
district_net_access_map_data <- district_shp %>%
  left_join(net_access_district, by = c("NAME_1" = "district"))

cluster_plot_data <- sf::st_as_sf(net_access_cluster, coords = c("LONGNUM", "LATNUM"), crs = 4326)

# create country-level malaria prevalence map
civ_net_access_plot <- ggplot(district_net_access_map_data) +
  geom_sf(aes(fill = class), color = "black", linewidth = 0.25) +
  geom_sf(data = cluster_plot_data, aes(fill = class), size = 4, shape = 21, alpha = 0.7, color = "black", stroke = 0.25) +
  geom_text_repel(aes(label = str_to_sentence(NAME_1), geometry = geometry), 
                  stat = "sf_coordinates", min.segment.length = 0, size = 6, force = 5) +
  labs(title = "U5 Net Use Given Access by District and Cluster in Cote d'Ivoire (2021)", 
       fill = "U5 Net Use (%)",
       x = NULL,
       y = NULL) + 
  scale_fill_manual(values = rev(RColorBrewer::brewer.pal(11, "RdYlBu")), 
                    name = "U5 Net Use (%)",
                    drop = FALSE) +
  map_theme() +
  theme(
    legend.position = "right",
    legend.text = element_text(size = 16),
    legend.title = element_text(size = 18),
    plot.title = element_text(size = 20, hjust = 0.5),
    plot.subtitle = element_text(size = 16, hjust = 0.5)
  )
civ_net_access_plot

ggsave(file.path(plots, "CIV_net_access_districts_clusters.pdf"), civ_net_access_plot, width = 12, height = 12, dpi = 300)
