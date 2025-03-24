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