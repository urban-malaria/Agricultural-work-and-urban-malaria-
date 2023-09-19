rm(list=ls())
memory.limit(size = 50000)

## -----------------------------------------
### Paths
## -----------------------------------------

Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
NuDir <- file.path(Drive, "Box", "NU-malaria-team")
NGDir <-file.path(NuDir, "data", 'nigeria', "nigeria_dhs",  "data_analysis")
DataDir <-file.path(NGDir, "data")
ResultDir <-file.path(NGDir, "results")
DHSData <- file.path(DataDir, 'DHS')

DataDir <-file.path(Drive, "Downloads")
DataDir2 <-file.path(Drive, "Downloads")
DHSData <-file.path(Drive)
DataIn<-file.path(Drive, "Downloads", 'Education')

# -----------------------------------------
### Required functions and settings
## -----------------------------------------

# -----------------------------------------
### Required functions and settings
## -----------------------------------------

# # Reading in the necessary packages 
list.of.packages <- c("tidyverse", "survey", "haven", "ggplot2", "purrr",  "stringr", "sp", "rgdal", "raster",
                      "lubridate", "RColorBrewer","sf",   "labelled", "plotrix", "arules", "foreign",
                      "fuzzyjoin", "splitstackshape", "magrittr", "caTools", "sjlabelled", "raster", "rlist", 'rgeos',  'ggpubr',
                      'cowplot', 'gridExtra', 'lme4', "patchwork", 'ggsci', 'glue', 'ggrepel', 'jtools', 'huxtable', 'broom.mixed')


new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)


lapply(list.of.packages, library, character.only = TRUE) #applying the library function to packages


#read files function 
read.files <- function(path, general_pattern, specific_pattern, fun) {
  files <- list.files(path = path , pattern = general_pattern, full.names = TRUE, recursive = TRUE)
  files<- files[(grep(specific_pattern, files))]
  sapply(files, fun, simplify = F)
}

## ----------------------------------------------------
### estimation employment using the 2018 MR file 
## ----------------------------------------------------

dhs <- read.files(DataDir, "*NGMR.*\\.DTA", 'NGMR7AFL', read_dta) #reads in the IR files
dhs <- dhs %>%  map(~mutate(., wt=mv005/1000000,strat=mv022, id=mv021)) %>%  map(~filter(., mv025 == 1)) 

look_for(dhs[[1]], 'mv168')

table(dhs[[1]]$mv716)

#create a variable for movement proxy and occupation  
dhs <- dhs %>% map(~mutate(., trips_man = ifelse(mv167 >=97, NA, mv167), #Number of times away from home in the last 12 months 
                           duration_travel_man = ifelse(mv168 >=9, NA, mv168), #Away for more than one month in the last 12 months 
                           agri_worker_man = ifelse(mv717 %in% c(4, 5), 1, ifelse(mv717 >=98, NA, 0)), # if male is agricultural worker or not
                           last_work_man = ifelse(mv731 >=9, NA, ifelse(mv731 %in% c(1, 2), 1, 0)), # if male has worked in the last 12 months
                           seasonal_work_man = ifelse(mv732 == 2, 1, ifelse(mv732 ==9, NA, 0)), #if man is a seasonal worker or not 
                   last_work_woman = ifelse(mv731 %in% c(1, 2, 3), 1, ifelse(mv731 >=9, NA, 0)),#if respondent has worked in the last past year, is currently working, has a job or is on leave in the last 7 days 
                   seasonal_work_woman = ifelse(mv732 == 2, 1, ifelse(mv732 ==9, NA, 0)))) #if respondent or woman is a seasonal worker or not 




#logistic reg data

df_mr <- dhs[[1]] %>% dplyr::select(mv001, mv002, trips_man, duration_travel_man, agri_worker_man, last_work_man, 
                            seasonal_work_man, last_work_woman, seasonal_work_woman, wt, strat, id)

dhs_pr <- read.files(DataDir, "*NGPR.*\\.DTA", 'NGPR7AFL', read_dta) #reads in the PR files


pfpr_df <- dhs_pr[[1]] %>% filter(hv042 == 1 & hv103 == 1 & hc1 %in% c(6:59) & hml32 %in% c(0, 1),hv025 == 1) %>% mutate(test_result = ifelse(hml32==1, 1,0)) %>% 
  dplyr::select(hv001, hv002, test_result)


df <- left_join(df_mr, pfpr_df, by = c("mv001"="hv001", "mv002"="hv002")) %>% filter(test_result >=0)



#survey design function 
svydesign.fun <- function(filename){
  svydesign(id= ~id,
            strata=~strat,nest=T, 
            weights= ~wt, data=filename)
}


svyd_df <- svydesign.fun(df)


#bi variate model fit

bi_models <- list(model_1 <- svyglm(test_result ~ trips_man, design = svyd_df, family = binomial),
               model2 <- svyglm(test_result ~ duration_travel_man, design = svyd_df, family = binomial),
               model4 <- svyglm(test_result ~ agri_worker_man, design = svyd_df, family = binomial),
               model5 <- svyglm(test_result ~ last_work_man, design = svyd_df, family = binomial),
               model6 <- svyglm(test_result ~ seasonal_work_man, design = svyd_df, family = binomial),
               model7 <- svyglm(test_result ~ last_work_woman, design = svyd_df, family = binomial),
               model8 <- svyglm(test_result ~ seasonal_work_woman, design = svyd_df, family = binomial))

bi_df <- lapply(bi_models , tidy)

#multi varaite fit

multi_mod <- svyglm(formula = test_result ~ trips_man + duration_travel_man + last_work_man + seasonal_work_man + 
                      last_work_woman + seasonal_work_woman, design = svyd_df, family = binomial)


#computing odds ratios
coefs_bi <- list()

for (i in 1:7) { 
  results = coefficients(summary(bi_models[[i]]))
  sub_df <- subset(results,!rownames(results) %in% c("(Intercept)"))
  coefs_bi[[i]] = sub_df 

}


bi_coefs_df <- rbind(coefs_bi[[1]],coefs_bi[[2]], coefs_bi[[3]], coefs_bi[[4]], coefs_bi[[5]], coefs_bi[[6]], coefs_bi[[7]]) 
write.csv(bi_coefs_df, paste0(DataIn, "/bi_coefs_df.csv"))


colnames(bi_coefs_df)[2] = "SE"
results_df <- data.frame(bi_coefs_df)%>% mutate(odds = (exp(Estimate))) %>% 
  mutate(lower_ci = (exp(-1.96*SE+Estimate))) %>% 
  mutate(upper_ci = (exp(1.96*SE+Estimate))) #%>%  remove_rownames()

results_df["vars"] = c('trips_man_bi', 'duration_travel_man_bi', 'last_work_man_bi', 'seasonal_work_man_bi','last_work_woman_bi', 'seasonal_work_woman_bi')


mu_coefs_df <- coefficients(summary(multi_mod))
write.csv(mu_coefs_df, paste0(DataIn, "/mu_coefs_df.csv"))

mu_coefs_df <- subset(mu_coefs_df,!rownames(mu_coefs_df) %in% c("(Intercept)"))

colnames(mu_coefs_df)[2] = "SE"
results_df_mu <- data.frame(mu_coefs_df)%>% mutate(odds = (exp(Estimate))) %>% 
  mutate(lower_ci = (exp(-1.96*SE+Estimate))) %>% 
  mutate(upper_ci = (exp(1.96*SE+Estimate)))
results_df_mu["vars"] = c('trips_man_mu', 'duration_travel_man_mu', 'seasonal_work_man_mu')


#Forest plot

results_df_c <- rbind(results_df, results_df_mu)


data <- list(results_df, results_df_mu, results_df_c)
forst_titles <- list('Bivariate analysis', 'Muiltivariate analysis', 'Bivariate and Muiltivariate Analysis')


forest_plots <- list()
for (i in 1:3) { 
  c <- ggplot(data[[i]], aes(x = odds, y = vars)) + 
    geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
    geom_errorbarh(aes(xmax = lower_ci, xmin = upper_ci), size = .5, height = 
                     .2, color = "blue") + 
    geom_point(size = 2.5, color = "red")+
    theme_bw()+
    theme(panel.grid.minor = element_blank())+ 
    theme(panel.border = element_blank())+
    ylab("Covariates")+
    xlab("Odds Ratio")+
    labs (title = forst_titles[[i]], x = "values", size=25) 
  
  forest_plots[[i]] <- c 
}

forest_plots[[1]]
forest_plots[[2]]
forest_plots[[3]]

ggsave(paste0(DataIn, '/Figures/', Sys.Date(), 'bivariate_forest.pdf'), forest_plots[[1]], width = 14, height =9)
ggsave(paste0(DataIn, '/Figures/', Sys.Date(), 'multiivariate_forest.pdf'), forest_plots[[2]], width = 14, height =9)
ggsave(paste0(DataIn, '/Figures/', Sys.Date(), 'both_forest.pdf'), forest_plots[[2]], width = 14, height =9)

