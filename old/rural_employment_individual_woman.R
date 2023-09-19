rm(list=ls())
memory.limit(size = 50000)

## 
### Paths
## 

Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
NuDir <- file.path(Drive, "Box", "NU-malaria-team")
NGDir <-file.path(NuDir, "data", 'nigeria', "nigeria_dhs",  "data_analysis")
DataDir <-file.path(NGDir, "data")
ResultDir <-file.path(NGDir, "results")
DHSData <- file.path(DataDir, 'DHS')
DataIn <- file.path(NuDir, 'projects', 'urban_malaria', 'Occupational_malaria')  

DataDir <-file.path(Drive, "Downloads")
### Required functions and settings
## 

source("functions_employment.R")
options(survey.lonely.psu="adjust") # this option allows admin units with only one cluster to be analyzed


#################################################################################
### Loading data and Variable Transformation  
###################################################################################

## 
### Loading the 2018 IR file 
## 

dhs <- read.files(DataDir, "*NGIR.*\\.DTA", 'NGIR7AFL', read_dta) #reads in the IR files
dhs <- dhs %>%  map(~mutate(., wt=v005/1000000,strat=v022, id=v021)) %>% map(~filter(., v025 == 2)) 


dhs <- dhs %>% map(~mutate(., trips_woman = ifelse(v167 >=99, NA, v167), #Number of trips in last 12 months 
                           edu_woman  = v106, #ifelse(v106 %in% c(0, 1, 2), 0,ifelse(v106 >= 8, NA, ifelse(v106 == 3, 1, NA))),
                           age_woman = v012,
                           duration_travel_woman = ifelse(v168 >=9, NA, v168), #Away for more than one month in the last 12 months 
                           agri_worker_partner = ifelse(v705 %in% c(4, 5), 1, ifelse(v705 >=98, NA, 0)), # if husband/partner is agricultural worker or not
                           last_work_partner = ifelse(v704a >=8, NA, ifelse(v704a==1, 1, 0)), # if husband/partner has worked in the last 7 days or in the last 12 months
                           agri_worker_woman = ifelse(v717 %in% c(4, 5), 1, ifelse(v717 >=98, NA, 0)), # if woman is agricultural worker or no
                           agri_worker_both = ifelse(agri_worker_partner ==1 & agri_worker_woman ==1, 1, 0), # if both husband and wife are agricultural workers
                           last_work_woman = ifelse(v731 ==2, 1, ifelse(v731 >=9, NA, 0)),#if respondent has worked in the last past year, is currently working, has a job or is on leave in the last 7 days 
                           trans_work_partner = ifelse(v704 ==9, 1,ifelse(v704 ==99998, NA, 0)), #if husband works in transportation and material moving 
                           trans_work_woman = ifelse(v716 ==9, 1, 0), #if woman works in transportation and material moving
                           wealth = ifelse(v190 <4, 0, 1),
                           kap_cure_med = ifelse(s1108ai == 1, 1, 0),
                           kap_death = ifelse(s1108ba == 1, 1, 0),
                           kap_treat = ifelse(s1108bc == 1, 1, 0),
                           kap_know = ifelse(s1108bd == 1, 1, 0),
                           kap_weak  = ifelse(s1108bf == 0, 1, 0),
                           kap_index = (kap_cure_med + kap_death + kap_treat + kap_know + kap_weak)/5,
                           media_exposure = ifelse(v157 %in% c(2, 3) | (v159 %in% c(2, 3)) | (v158 %in% c(2, 3)), 0, 1), #access to atleast one for a week
                           co_wives = ifelse(v505 ==98,NA, v505),
                           health_dec = ifelse(v743a == 1, 1, 0),
                           seasonal_work_woman = ifelse(v732 == 2, 1, ifelse(v732 ==9, NA, 0)))) #if respondent or woman is a seasonal worker or not 



#logistic reg data

df_ir <- dhs[[1]] %>% dplyr::select(wt, strat, id, v001, v002, trips_woman, duration_travel_woman, agri_worker_woman, 
                                    agri_worker_partner, last_work_partner,last_work_woman, seasonal_work_woman, agri_worker_both, 
                                    age_woman, edu_woman, trans_work_partner, trans_work_woman, wealth, kap_index, media_exposure, 
                                    co_wives, health_dec) 


df_ir <- df_ir[!(duplicated(df_ir[c('v001','v002')])),]

## 
### Loading the 2018 PR file dataset to obtain malaraia estimates and houshold vars
## 

dhs_pr <- read.files(DataDir, "*NGPR.*\\.DTA", 'NGPR7AFL', read_dta) #reads in the PR files

# Estimating malaria status by microscopy 
dhs_pr_vars <- dhs_pr %>% map(~filter(., hv042 == 1 & hv103 == 1 & hc1 %in% c(6:59) & hml32 %in% c(0, 1),hv025 == 2)) %>%
  map(~mutate(., test_result = ifelse(hml32==1, 1,0),
              child_age = hc1,
              u5_net_use = ifelse(hml12 %in% c(1,2), 1,0),
              floor_type = ifelse(hv213 == 30| hv213 == 31|hv213 == 33| hv213 == 34|hv213 == 35,1, 0),
              wall_type = ifelse(hv214 == 30| hv214 == 31| hv214 == 33| hv214 == 34,0, 1),
              roof_type = ifelse(hv215 == 30| hv215 == 31|hv215 == 33| hv215 == 34|hv215 == 35|hv215 == 36,1, 0),
              hh_size = hv009)) %>%
  map(~dplyr::select(., hv001, hv002, hv003, test_result, child_age, u5_net_use, floor_type, wall_type, 
                     roof_type, hh_size))


labs <- c(paste(seq(0, 59, by = 12), seq(0 + 12 - 1, 60 - 1, by = 12),sep = "-"))
dhs_pr_vars <- dhs_pr_vars[[1]] %>% mutate(child_age_group = cut(child_age, breaks = c(seq(0, 59, by = 12), Inf), 
                                                                 labels = labs,  right = FALSE)) 

## 
### Loading the 2018 KR file dataset to obtain CHILD  estimates and houshold vars
## 

dhs_kr <- read.files(DataDir, "*NGKR.*\\.DTA", 'NGKR7AFL', read_dta) #reads in the PR files

dhs_kr_vars <- dhs_kr %>% map(~filter(., b5==1, b19 < 60, h22 == 1, v025 == 2)) %>%
  map(~mutate(., health_seek = ifelse(h32a == 1|h32a == 1|h32b == 1|h32j == 1|h32k == 1|
                                        h32l == 1| h32m == 1|h32s== 1|h32x== 1,0, 1))) %>%
  map(~dplyr::select(., v001, v002, v003, health_seek)) 

df_kr <- dhs_kr_vars[[1]] 

## 
### Loading the 2018 MR file dataset to obtain partner data
## 

dhs_mr <- read.files(DataDir, "*NGMR.*\\.DTA", 'NGMR7AFL', read_dta) #reads in the MR files

dhs_mr_vars <- dhs_mr %>% map(~mutate(., seasonal_work_man = ifelse(mv732 == 2, 1, ifelse(mv732 ==9, NA, 0)))) %>%#if man is a seasonal worker or not 
  map(~dplyr::select(., mv001, mv002, seasonal_work_man)) 

df_mr <- dhs_mr_vars[[1]]

df_mr <- df_mr[!(duplicated(df_mr[c('mv001','mv002')])),]

## 
### Loading the CSV on  EVI
## -
evi <- read_csv(file.path(DHSData,'Computed_cluster_information', 'urban_malaria_covariates', 'geospatial_covariates',
                          'EVI_2000m_buffer_DHS_10_15_18.CSV')) %>% dplyr::select(-.id) %>% filter(dhs_year == 2018)

dwb <- evi <- read_csv(file.path(DHSData,'Computed_cluster_information', 'urban_malaria_covariates', 'geospatial_covariates',
                                 'dist_water_bodies_2000m_buffer_DHS_10_15_18.CSV')) %>% filter(dhs_year == 2018)


#merging extracted dataset with malaria estimates

df <- left_join(df_ir, dhs_mr_vars[[1]], by = c("v001"='mv001', 'v002'="mv002")) %>% 
  left_join(dhs_kr_vars[[1]], by = c("v001"="v001", "v002"="v002")) %>% 
  left_join(dhs_pr_vars, by = c("v001"="hv001", "v002"="hv002")) #%>% 
left_join(evi, by = c("v001"="hv001")) %>% left_join(dwb, by = c("v001"="hv001")) %>% 
  filter(test_result ==0|test_result ==1)#%>% na.omit()


##############################
#spatial dataframe
##############################

#load spatial points
sf18 = st_read(file.path(DHSData, "Downloads", "NG_2018_DHS_11072019_1720_86355/NGGE7BFL/NGGE7BFL.shp"),)%>%
  dplyr::select("DHSCLUST","LATNUM", "LONGNUM", "ALT_GPS", "ALT_DEM", "DATUM", "geometry")

#joiing spacial geometry  to the data frame
df_spatial <- df %>% left_join(sf18, by = c('v001' = 'DHSCLUST'))


########################
#Survey design
##########################


#factoring categorica variables

cols <- c('edu_woman', 'child_age_group', 'agri_worker_partner', 'media_exposure',
          'health_seek')

df[cols] <- lapply(df[cols], as.factor)


#subjecting dataframe to survey design
svyd_df <- svydesign.fun(df)

###################################################################################
#Exploratory Data Analysis (EDA) 
###################################################################################

#Background charecteristics summary table
xtable <- tbl_summary(df, by = "test_result") %>% add_overall()
xtable




##
# Descritive plots
##

# Stacked Bar Plot with Colors and Legend

#agric work 

counts <- svytable(~agri_worker_partner+test_result, svyd_df)%>% as.data.frame()%>% 
  mutate(test_result=str_replace(test_result, '0','Negative')) %>%
  mutate(test_result=str_replace(test_result, '1','Positive'))%>% 
  mutate(agri_worker_partner=str_replace(agri_worker_partner, '1','Agric parents'))%>% 
  mutate(agri_worker_partner=str_replace(agri_worker_partner, '0','Non - agric parents'))%>% 
  rename_at(1,~"Var1") %>% rename_at(2,~"Var2")

agric_stac_plot <- barplot_stack.fun('') + coord_flip()

agric_prop_plot <- barplot_prop.fun('') + coord_flip()
agric_prop_plot

agric_stac_plot + agric_prop_plot

#women education 
counts <- svytable(~edu_woman+test_result, svyd_df)%>% as.data.frame()%>% 
  rename_at(1,~"Var1") %>% rename_at(2,~"Var2") %>% 
  mutate(Var2=str_replace(Var2, '0','Negative')) %>%
  mutate(Var1=str_replace(Var1, '0','no education')) %>%  mutate(Var1=str_replace(Var1, '1','primary')) %>%  
  mutate(Var1=str_replace(Var1, '2','secondary')) %>% mutate(Var1=str_replace(Var1, '3','higher')) %>%
  transform(Var1 = reorder(Var1, -Freq, decreasing = T)) 


ed_stac_plot <- barplot_stack.fun('Malaria tests by womens highest education attainement')

ed_prop_plot <- barplot_prop.fun('Malaria prop. by womens education attainement')

ed_stac_plot + ed_prop_plot

#children's age groups 
counts <- svytable(~child_age_group+test_result, svyd_df)%>% as.data.frame()%>% 
  rename_at(1,~"Var1") %>% rename_at(2,~"Var2") %>% 
  mutate(Var2=str_replace(Var2, '0','Negative')) %>%
  mutate(Var2=str_replace(Var2, '1','Positive'))

age_stac_plot <- barplot_stack.fun('Malaria tests by childrens age group')

age_prop_plot <- barplot_prop.fun('Malaria prop. by childrens age group')

age_stac_plot + age_prop_plot


#----------------------------------------------------------
#model fitting
#----------------------------------------------------------


#aassociated test: seasonal work and agric work
svychisq(~agri_worker_partner+seasonal_work_man, svyd_df)
#signifcant, hence eliminate seasonal work from model

svychisq(~agri_worker_partner + seasonal_work_woman, svyd_df)
#signifcant, hence eliminate seasonal work from model

svychisq(~age_woman+edu_woman, svyd_df)
#signifcant, hence eliminate one of them age

svychisq(~media_exposure+kap_index, svyd_df)
#Not signifcant

svychisq(~duration_travel_woman+agri_worker_partner, svyd_df)
#Not signifcant

svychisq(~duration_travel_woman+agri_worker_partner, svyd_df)
#Not signifcant

svychisq(~roof_type+wall_type, svyd_df)
svychisq(~wall_type+test_result, svyd_df)
#signifcant hence will only keep wall type in model that bcouse it is highly associated with test results

svychisq(~health_seek+kap_index, svyd_df)
#Not signifcant

svychisq(~health_seek+u5_net_use, svyd_df)
#signifcant net use will be removed 



#bivariate modelS fitS

bi_model <- list(#model1 <- svyglm(test_result ~ duration_travel_woman, design = svyd_df, family = binomial),
  model2 <- svyglm(test_result ~ agri_worker_partner, design = svyd_df, family = binomial),
  #model3 <- svyglm(test_result ~ seasonal_work_woman, design = svyd_df, family = binomial),
  #model4 <- svyglm(test_result ~ age_woman, design = svyd_df, family = binomial),
  model5 <- svyglm(test_result ~ edu_woman, design = svyd_df, family = binomial),
  #model6 <- svyglm(test_result ~ trans_work_partner, design = svyd_df, family = binomial),
  #model7 <- svyglm(test_result ~ trans_work_woman, design = svyd_df, family = binomial),
  model8 <- svyglm(test_result ~ kap_index, design = svyd_df, family = binomial),
  model9 <- svyglm(test_result ~ media_exposure, design = svyd_df, family = binomial),
  #model10 <- svyglm(test_result ~ co_wives, design = svyd_df, family = binomial),
  model11 <- svyglm(test_result ~ child_age_group, design = svyd_df, family = binomial),
  #model12 <- svyglm(test_result ~ u5_net_use, design = svyd_df, family = binomial),
  #model13 <- svyglm(test_result ~ floor_type, design = svyd_df, family = binomial),
  model14 <- svyglm(test_result ~ wall_type, design = svyd_df, family = binomial),
  #model15 <- svyglm(test_result ~ roof_type, design = svyd_df, family = binomial),
  #model16 <- svyglm(test_result ~ seasonal_work_man, design = svyd_df, family = binomial),
  model17 <- svyglm(test_result ~ health_seek, design = svyd_df, family = binomial),
  #model23 <- svyglm(test_result ~ EVI_2000m, design = svyd_df, family = binomial),
  model18 <- svyglm(test_result ~ dist_water_bodies_2000m.x, design = svyd_df, family = binomial),
  model18 <- svyglm(test_result ~ hh_size, design = svyd_df, family = binomial)) 
#if not working replace with "dist_water_bodies_2000m.x"

bi_df <- lapply(bi_model , tidy)
bi_df 


#multi varaite fit

multi_mod <- svyglm(test_result ~ agri_worker_partner + child_age_group + edu_woman + 
                      media_exposure+kap_index + wall_type + health_seek + hh_size +
                      dist_water_bodies_2000m.x, design = svyd_df, family = binomial) 
#if not working replace with "dist_water_bodies_2000m.x"

summary(multi_mod)
summary(multi_mod)$aic


#Getting best model fit using stepwise method 

#step.model <- MASS::stepAIC(multi_mod)


###################################################################################
#model fit plots and tables
###################################################################################

#computing coeficients  for Bi models

coefs_bi <- list()

for (i in 1:9) { 
  results = coefficients(summary(bi_model[[i]]))
  sub_df <- subset(results,!rownames(results) %in% c("(Intercept)"))
  coefs_bi[[i]] = sub_df 
  
}


bi_coefs_df <- rbind(coefs_bi[[1]],coefs_bi[[2]], coefs_bi[[3]], coefs_bi[[4]], 
                     coefs_bi[[5]], coefs_bi[[6]], coefs_bi[[7]],coefs_bi[[8]], 
                     coefs_bi[[9]],coefs_bi[[8]],coefs_bi[[9]])#,coefs_bi[[24]])

#computing odds ratios for bivariate models and their confidence intervals 

colnames(bi_coefs_df)[2] = "SE"
results_df_bi <- data.frame(bi_coefs_df)%>% mutate(odds = (exp(Estimate))) %>% 
  mutate(lower_ci = (exp(-1.96*SE+Estimate))) %>% 
  mutate(upper_ci = (exp(1.96*SE+Estimate))) %>% tibble::rownames_to_column('vars')#%>%  remove_rownames()

results_df_bi
write.csv(results_df_bi, paste0(DataIn, '/tables',"/woman_bi_coefs_df.csv"))

#computing odds ratios for multivariate model and their confidence intervals 

mu_coefs_df <- coefficients(summary(multi_mod))

mu_coefs_df <- subset(mu_coefs_df,!rownames(mu_coefs_df) %in% c("(Intercept)"))

colnames(mu_coefs_df)[2] = "SE"
results_df_mu <- data.frame(mu_coefs_df)%>% mutate(odds = (exp(Estimate))) %>% 
  mutate(lower_ci = (exp(-1.96*SE+Estimate))) %>% 
  mutate(upper_ci = (exp(1.96*SE+Estimate)))%>% tibble::rownames_to_column('vars')

results_df_mu

write.csv(results_df_mu, paste0(DataIn, '/tables',"/mu_coefs_df.csv"))
#computing odds ratios for final multivariate model and its confidence intervals

######################
#Forest plots
######################

data <- list(results_df_bi, results_df_mu)

forst_titles <- list('Bivariate analysis', 'muiltivariate model')

forst_titles <- list('Bivariate analysis', 'Model with all covariates')


forest_plots <- list()
for (i in 1:2) { 
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
    labs (title = forst_titles[[i]], x = "values", size=25)+
    theme_manuscript()
  
  forest_plots[[i]] <- c 
}


forest_plots[[1]]
forest_plots[[2]]


ggsave(paste0(DataIn, '/figures/', Sys.Date(), 'bivariate_forest.pdf'), forest_plots[[1]], width = 14, height =9)
ggsave(paste0(DataIn, '/figures/', Sys.Date(), 'multiivariate_forest.pdf'), forest_plots[[2]], width = 14, height =9)



####################################################
#Predicted probabilities 
###################################################

#i will populate this once we settle on finale model 

agric_effect <- predictorEffects(multi_mod, ~ agri_worker_partner + health_seek+  
                                   child_age_group + edu_woman )

plot(agric_effect, axes=list(grid=TRUE))

ggsave(paste0(DataIn, '/figures/', Sys.Date(), 'urb_agric_effect.pdf'), agric_effect_p, width = 14, height =9)


