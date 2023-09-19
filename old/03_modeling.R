

## -----------------------------------------
### Directories
## -----------------------------------------
user <- Sys.getenv("USERNAME")
if ("ido0493" %in% user) {
  user_path <- file.path("C:/Users", user)
  DriveDir <- file.path(user_path, "OneDrive - Northwestern University", "urban_malaria")
  PopDir <- file.path(DriveDir, "data", "Urban_malaria_net_ownership_data")
  ManDir <- file.path(DriveDir, "projects", "Manuscripts", "agriculture_malaria_manuscript")
  FigDir <- file.path(ManDir, "figures", "final_figures", "model_plots")
  UrbgDir <- file.path(FigDir, "urban")
  RurgDir<- file.path(FigDir, "rural")
} else if  ("Chilo Chiziba" %in% user) {
  Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
  DriveDir <- file.path(Drive, 'OneDrive - Northwestern University', 'urban_malaria')
  ManDir <- file.path(DriveDir, "projects", "Manuscripts", "agriculture_malaria_manuscript")
  FigDir <- file.path(ManDir, "figures", "final_figures", "model_plots")
  UrbgDir <- file.path(FigDir, "urban")
  RurgDir<- file.path(FigDir, "rural")
} else {
  Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
  #DriveDir <- file.path(Drive, '', 'Northwestern University', 'Ifeoma Doreen Ozodiegwu - urban_malaria')
  DriveDir <- file.path(Drive, '', 'Library', 'CloudStorage', "OneDrive-NorthwesternUniversity", "urban_malaria")
  PopDir <- file.path(DriveDir, "data", 'Urban_malaria_net_ownership_data')
  ManDir <- file.path(DriveDir, "projects", "Manuscripts", "agriculture_malaria_manuscript")
  FigDir <- file.path(ManDir, "figures", "final_figures", "model_plots")
  UrbgDir <- file.path(FigDir, "urban")
  RurgDir<- file.path(FigDir, "rural")
}


#Loading functions and libraries
source("functions/functions_employment.R")
#functions 
# # Reading in the necessary packages 
list.of.packages <- c('readr', 'tidyr', 'dplyr', 'plyr', 'purrr', 'forcats',"survey", 
                      "haven", "ggplot2", "purrr",  "stringr", "sp", "rgdal", "raster","sf",   
                      "labelled", "plotrix", "arules", "fuzzyjoin", 'cowplot', 'gridExtra', 
                      'lme4', "patchwork", 'ggsci', 'glue', 'ggrepel', 'jtools', "srvyr", 
                      "tmap",'gtsummary', 'rstatix', 'ggcorrplot', 'viridis', 'effects', 
                      "rdhs", "microbenchmark", "rdhs",  "ggfittext", "forcats")

lapply(list.of.packages, library, character.only = TRUE) #applying the library function to packages


# this option allows admin units with only one cluster to be analyzed
options(survey.lonely.psu="adjust") 

#datsets
rur_urb_list <- list("urban_df_for_analysis.csv", "rural_df_for_analysis.csv")
tag_list <- list("_urban", "_rural")
dir_list <- list(UrbgDir, RurgDir)
forest_m_list <- list()
pred_p_list <- list()

for (i in 1:2){
  
  #loading data
  df1 <- read.csv(file.path(ManDir, "csv", rur_urb_list[[1]]), header = T, sep = ',') %>% 
    mutate(malaria_result = ifelse(test_result =="+ve", 1, ifelse(test_result== "-ve", 0, NA)))%>%
    mutate(agric_home = grepl("A", home_type))%>% 
    mutate(agric_home = ifelse(agric_home =="TRUE", "1", ifelse(agric_home== "FALSE", "0", NA))) %>% 
    mutate(agric_home_num =  ifelse(agric_home =="1", 1, ifelse(agric_home== "0", 0, NA))) %>% 
    mutate(wealth = ifelse(wealth %in% c(1,2), 1, 0)) %>%  #mutate(edu_woman = ifelse(edu_woman > 1, 1, 0))#%>%
    left_join(ids %>% select("SubregionName", "DHS_CountryCode", "RegionName"), by = "DHS_CountryCode")# %>%
    #mutate(wealth = as.factor(wealth))
  

  #df split
  cntrs_df_list <- split(df1, with(df1, interaction(SubregionName)), drop = TRUE)
  
  #############
  #survey design list
  
  svyd_list <- lapply(cntrs_df_list, svydesign.fun)
  
  ############################################
  #Univariate analysis
  ###############################################
  
  #function fitting bivariate models
  model.fun <- function(s_design){
    
    svyglm(malaria_result ~ agric_home, design = s_design, family = binomial)
    
  }
  
  #applying model funtion to fit models
  model_list <- lapply(svyd_list, model.fun)
  
  #summarizing findings
  summary_list <- lapply(model_list, summary)
  summary_list
  
  bi_df_list <- lapply(model_list, tidy)
  mu_coefs_df <- bind_rows(bi_df_list) %>% filter(term != "(Intercept)")%>% rename_at(3, ~"SE")
  
  #odds ratios
  results_df_mu <- data.frame(mu_coefs_df)%>% mutate(odds = (exp(estimate))) %>% 
    mutate(lower_ci = (exp(-1.96*SE+estimate))) %>% 
    mutate(upper_ci = (exp(1.96*SE+estimate))) %>% tibble::rownames_to_column()
  
  region_names <- df1 %>% distinct(SubregionName) %>%  arrange(SubregionName)
  final_results_df <- region_names$SubregionName %>% as.data.frame() %>% tibble::rownames_to_column() %>%
    left_join(results_df_mu, by =  "rowname") %>% rename_at(2, ~"country")
  
  #forest plot
  
  forest_b <- ggplot(final_results_df, aes(x = odds, y = country)) + 
    geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
    geom_errorbarh(aes(xmax = lower_ci, xmin = upper_ci), size = .5, height = 
                     .2, color = "blue") + 
    geom_point(size = 2.5, color = "red")+
    theme_bw()+
    theme(panel.grid.minor = element_blank())+ 
    theme(panel.border = element_blank())+
    ylab("SSA Region")+
    xlab("Odds ratio")+
    labs (title = "Regional univariate", size=25)+
    theme_manuscript()+
    xlim(0.5,3.7)
  
  forest_b
  ggsave(paste0(dir_list[[i]],"/", Sys.Date(), paste0(tag_list[[i]]), "_forest_bivar.pdf"), forest_b, width = 4, height = 1.5652778)
  
  ######################
  #Overall
  #######################
  
  
  svyd_df <- svydesign.fun(df1)
  ov_model <- svyglm(malaria_result ~ agric_home, design = svyd_df, family = binomial)
  summary(ov_model)
  
  agric_effect <- predictorEffects(ov_model, ~agric_home)
  plot(agric_effect, axes=list(grid=TRUE), main="Effect of agricultural household")
  
  
  ############################################
  #multivariate analysis
  ###############################################
  model.fun <- function(s_design){
    
    svyglm(malaria_result ~ agric_home + floor_type + roof_type + 
             wall_type + hh_size + wealth, design = s_design, family = binomial)
    
  }
  
  #applying model funtion to fit multivariate models
  m_model_list <- lapply(svyd_list, model.fun)
  
  #summarizing findings
  m_summary_list <- lapply(m_model_list, summary)
  m_summary_list
  
  
  m_df_list <- lapply(m_model_list, tidy)
  mu_coefs_df <- bind_rows(m_df_list) %>% filter(term != "(Intercept)")%>% rename_at(3, ~"SE")
  
  
  #Overall model
  ov_model <- svyglm(malaria_result ~ agric_home + floor_type + roof_type + 
                       wall_type + hh_size + wealth, design = svyd_df, family = binomial)
  summary(ov_model)
  
  o_df <- tidy(ov_model)
  mu_ov_coefs_df <- o_df %>% filter(term != "(Intercept)")%>% rename_at(3, ~"SE")%>% 
    filter(term == "agric_home1") %>%
    mutate(odds = (exp(estimate))) %>% mutate(lower_ci = (exp(-1.96*SE+estimate))) %>% 
    mutate(upper_ci = (exp(1.96*SE+estimate))) %>% tibble::rownames_to_column() %>%
    mutate(rowname = ifelse(rowname == 1, "Overall", rowname))
  
  #odds ratios
  results_df_mu <- data.frame(mu_coefs_df)%>% filter(term == "agric_home1") %>%
    mutate(odds = (exp(estimate))) %>% mutate(lower_ci = (exp(-1.96*SE+estimate))) %>% 
    mutate(upper_ci = (exp(1.96*SE+estimate))) %>% tibble::rownames_to_column()
  
  
  final_results_df <- region_names$SubregionName %>% as.data.frame() %>% tibble::rownames_to_column() %>%
    left_join(results_df_mu, by =  "rowname") %>% rename_at(2, ~"country") %>%
    left_join(ids, by = c("country"="CountryName")) %>% 
    #binging overall
    bind_rows(mu_ov_coefs_df) %>% mutate(country = ifelse(is.na(country), "Overall", country))
  
  #df split
  region_df_list <- split(results_df_mu, with(results_df_mu , interaction(rowname)), drop = TRUE)
  
  #forest plot
  #forest plot
  
  forest_m <- ggplot(final_results_df, aes(x = odds, y = country)) + 
    geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
    geom_errorbarh(aes(xmax = lower_ci, xmin = upper_ci), size = .5, height = 
                     .2, color = "blue") + 
    geom_point(size = 2.5, color = "red")+
    theme_bw()+
    theme(panel.grid.minor = element_blank())+ 
    theme(panel.border = element_blank())+
    ylab("")+
    xlab("")+
    labs (title = "", size=25)+
    theme(panel.grid.minor = element_blank())+ 
    theme(panel.border = element_blank(), axis.title.x=element_blank(),
          main.title=element_blank())+
    theme_manuscript()+
    xlim(0.5,3)
  forest_m_list[[i]] = forest_m
  ggsave(paste0(dir_list[[i]],"/", Sys.Date(), paste0(tag_list[[i]]), "_forest_multiv.pdf"), forest_m, width = 4, height = 4)
  
  
  #predicted probabilities
  
  effct_df_east <- effect_df_fun(m_model_list[[1]])
  effct_df_middle <- effect_df_fun(m_model_list[[2]])
  effct_df_west <- effect_df_fun(m_model_list[[3]])
  effct_df_overall <- effect_df_fun(ov_model)
  
  
  pred_p <- ggplot() + 
    geom_errorbar(data = effct_df_east, aes(y = effect, x = rowname, ymax = lower, ymin = upper), size = .5, width = .2, color = "salmon") + 
    geom_errorbar(data = effct_df_middle, aes(y = effect, x = rowname, ymax = lower, ymin = upper), size = .5, width = .2, color = "green") + 
    geom_errorbar(data = effct_df_west, aes(y = effect, x = rowname, ymax = lower, ymin = upper), size = .5, width = .2, color = "mediumorchid4") + 
    geom_errorbar(data = effct_df_overall, aes(y = effect, x = rowname, ymax = lower, ymin = upper), size = .5, width = .2, color = "cyan3") + 
    geom_point(data = effct_df_east, aes(y = effect, x = rowname), size = 2.5, color = "salmon")+
    geom_point(data = effct_df_middle, aes(y = effect, x = rowname), size = 2.5, color = "green")+
    geom_point(data = effct_df_west, aes(y = effect, x = rowname), size = 2.5, color = "mediumorchid4")+
    geom_point(data = effct_df_overall, aes(y = effect, x = rowname), size = 2.5, color = "cyan3")+
    theme(panel.grid.minor = element_blank())+ 
    theme(panel.border = element_blank(), axis.title.x=element_blank(),
          main.title.x=element_blank())+
    theme_manuscript()+
    ylim(0,0.5)
  pred_p_list[[i]]  <- pred_p
  ggsave(paste0(dir_list[[i]],"/", Sys.Date(), paste0(tag_list[[i]]), "_pred_multiv.pdf"), pred_p, width = 4, height = 4)
  
}


forest_pred <- forest_m_list[[1]] + forest_m_list[[2]] + 
  theme(axis.title.y=element_blank(), axis.text.y=element_blank()) + 
  pred_p_list[[1]] + pred_p_list[[2]] + theme(axis.title.y=element_blank()) +
  plot_annotation(tag_levels = 'A')
forest_pred
ggsave(paste0(FigDir,"/", Sys.Date(), "_forest_pred.pdf"), forest_pred, width = 8, height = 7)
