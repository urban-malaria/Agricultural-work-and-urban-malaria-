# The purpose of this script is to identify surveys that have the variables that we need for the analysis 

rm(list = ls())

## -----------------------------------------
### Directories
## -----------------------------------------
user <- Sys.getenv("USERNAME")

if ("ido0493" %in% user) {
  user_path <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("OneDrive"))))
  DriveDir <- file.path(user_path, "urban_malaria")
  PopDir <- file.path(DriveDir, "data", "Urban_malaria_net_ownership_data")
  FigDir <- file.path(DriveDir, "projects", "Manuscripts", "agriculture_malaria_manuscript", "figures", "220623_new_figures")
  ManDir <- file.path(DriveDir, "projects", "Manuscripts", "agriculture_malaria_manuscript")
} else {
  Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
  DriveDir <- file.path(Drive, "OneDrive - Northwestern University", "urban_malaria")
  #DriveDir <- file.path(user_path, "OneDrive - Northwestern University", "urban_malaria")
  #DriveDir <- file.path(Drive, '', 'Northwestern University', 'Ifeoma Doreen Ozodiegwu - urban_malaria')
  #DriveDir <- file.path(Drive, '', 'Library', 'CloudStorage', "OneDrive-NorthwesternUniversity", "urban_malaria")
  PopDir <- file.path(DriveDir, "data", 'Urban_malaria_net_ownership_data')
  ManDir <- file.path(DriveDir, "projects", "Manuscripts", "agriculture_malaria_manuscript")
  FigDir <- file.path(ManDir, "figures", "220623_new_figures")
}


## -----------------------------------------
### Required functions and settings
## -----------------------------------------
source("functions/functions_employment.R")

#get indicator name here >>> https://api.dhsprogram.com/rest/dhs/indicators?returnFields=IndicatorId,Label,Definition&f=html
#get DHS surveys for only Africa 
rdt_df <- dhs_surveys(indicatorIds="ML_PMAL_C_RDT") #malaria RDT 
mal_df <- dhs_surveys(indicatorIds="ML_PMAL_C_MSY") #malaria microscopy 
occp_df <- dhs_surveys(indicatorIds="EM_OCCP_W_AGR") %>% filter(RegionName == "Sub-Saharan Africa") #agric woman
occp_m_df <- dhs_surveys(indicatorIds="EM_OCCP_M_AGR") %>% filter(RegionName == "Sub-Saharan Africa") # agric man

#set_rdhs_config(timeout = 600)
#get API based estimates values for all Africa
rdt_dt <-  dhs_data(surveyIds = rdt_df$SurveyId, indicatorIds="ML_PMAL_C_RDT") %>%rename(R = Value) %>%  #R means RDT testing estimate 
  dplyr::select(SurveyId, R, SurveyYear)

mal_dt <- dhs_data(surveyIds = mal_df$SurveyId, indicatorIds="ML_PMAL_C_MSY") %>%   #M meanas microscopy testing estimate 
  filter(SurveyId %in% rdt_dt$SurveyId) %>%rename(M = Value) %>% 
  dplyr::select(SurveyId, M, SurveyYear)

occp_w_dt <- dhs_data(surveyIds = occp_df$SurveyId, indicatorIds="EM_OCCP_W_AGR") %>%  #Wo means the proportion of women engaged in agriculture work 
  filter(SurveyId %in% rdt_dt$SurveyId) %>% rename(Wo = Value) %>% 
  dplyr::select(SurveyId, Wo, SurveyYear)

occp_m_dt <- dhs_data(surveyIds = occp_m_df$SurveyId, indicatorIds="EM_OCCP_M_AGR") %>% #Mo means the proportion of men engaged in agriculture work 
  filter(SurveyId %in% rdt_dt$SurveyId) %>% rename(Mo = Value) %>% 
  dplyr::select(SurveyId, Mo, SurveyYear)

#create left joined table with all estimates 
df <- rdt_dt %>% left_join(mal_dt) %>%left_join(occp_w_dt) %>% left_join(occp_m_dt) 


#make plots
label = c("Tested positive by microscopy",  "Tested positive by RDT","Agricultural work (Male)", "Agricultural work (Female)")
color = c("deeppink4", "#e8a6bd", "#afb2cb", "#a7cece")
plot_dat <- df %>%  pivot_longer(cols = c("M", "R", "Wo", "Mo"), names_to = "name", values_to = "value")


plot_dat$SurveyId=fct_reorder(plot_dat$SurveyId, plot_dat$SurveyYear)
plot_dat$name = fct_relevel(plot_dat$name, "M", "R")

#countries with agric data or other data points 
p<- ggplot(plot_dat, aes(x= name, y = as.numeric(value), fill=name)) +
  geom_bar(stat = "identity")+ 
  facet_wrap(~SurveyId) +
  scale_fill_manual(values = color, label =label) +
  theme_manuscript() +
  theme(legend.title=element_blank(), legend.position = "top") +
  labs(x = "", y = "Percentage")
ggsave(paste0(FigDir,"/", Sys.Date(),"_country_datasets_malaria_agric_all_data.png"), p, width = 13, height = 14)


#countries with all agric and parasitemia data 
df_com <- df %>%  drop_na()  %>% pivot_longer(cols = c("M", "R", "Wo", "Mo"), names_to = "name", values_to = "value")
p<- ggplot(df_com, aes(x= name, y = as.numeric(value), fill=name)) +
  geom_bar(stat = "identity")+ 
  facet_wrap(~SurveyId) +
  scale_fill_manual(values = color, label =label) +
  theme_manuscript() +
  theme(legend.title=element_blank(), legend.position = "top") +
  labs(x = "", y = "Percentage")
ggsave(paste0(FigDir,"/", Sys.Date(),"_country_datasets_malaria_agric_with_all_needed_analysis_data.png"), p, width = 13, height = 13)


#eliminate countries with no agriculture data 
df_com <- df %>% filter_at(vars(Wo, Mo), any_vars(!is.na(.)))%>% pivot_longer(cols = c("M", "R", "Wo", "Mo"), names_to = "name", values_to = "value")
p<- ggplot(df_com, aes(x= name, y = as.numeric(value), fill=name)) +
  geom_bar(stat = "identity")+ 
  facet_wrap(~SurveyId) +
  scale_fill_manual(values = color, label =label) +
  theme_manuscript() +
  theme(legend.title=element_blank(), legend.position = "top") +
  labs(x = "", y = "Percentage")
ggsave(paste0(FigDir,"/", Sys.Date(),"_country_datasets_for_analysis_data.png"), p, width = 13, height = 13)

#saving country id for use in analysis 
df_com <- df %>% filter_at(vars(Wo, Mo), any_vars(!is.na(.))) %>%  filter(SurveyId != "GA2019DHS") # remove gabon as estimate of those working in agriculture is very small 
write_csv(df_com, file.path(ManDir, "csv", "surveyids_for_analysis.csv"))


##################################################################
#### Countries with IRS data 
#####################################################################
irs_df <- dhs_surveys(indicatorIds="ML_IRSM_H_IRS") %>% filter(RegionName == "Sub-Saharan Africa")

irs_dt <-  dhs_data(surveyIds = irs_df$SurveyId, indicatorIds="ML_IRSM_H_IRS") %>%rename(IRS = Value) %>%  #R means RDT testing estimate 
  dplyr::select(SurveyId, IRS, SurveyYear)

plot_dat <- irs_dt %>%  pivot_longer(cols = c("IRS"), names_to = "name", values_to = "value")

p<- ggplot(plot_dat, aes(x= name, y = as.numeric(value), fill=name)) +
  geom_bar(stat = "identity")+ 
  facet_wrap(~SurveyId) +
  theme_manuscript() +
  theme(legend.title=element_blank(), legend.position = "top") +
  labs(x = "", y = "Percentage")
