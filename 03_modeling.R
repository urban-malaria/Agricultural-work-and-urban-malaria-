#The purpose of this script is to code up models that explore the relationship between being from an agricultural worker HH and malaria test positivity

rm(list = ls())

## -----------------------------------------
### Directories
## -----------------------------------------
user <- Sys.getenv("USERNAME")
if ("ozodi" %in% user) {
  Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", gsub("OneDrive", "", Sys.getenv("HOME")))))
  Drive <- file.path(gsub("[//]", "/", Drive))
  DriveDir <- file.path(Drive, "Urban Malaria Proj Dropbox", "urban_malaria")
  PopDir <- file.path(DriveDir, "data", "data_agric_analysis")
  ManDir <- file.path(DriveDir, "projects", "Manuscripts", "ongoing", "agriculture_malaria_manuscript")
  FigDir <- file.path(ManDir, "figures", "main_figures","pdf_figures")
  SupDir <- file.path(ManDir, "figures", "supplementary", "pdf_figures")
  ExpDir <- file.path(ManDir, "figures", "exploratory")
} else if  ("cchiz" %in% user) {
  Drive <- file.path("C:/Users/cchiz/OneDrive")
  DriveDir <- file.path(Drive, "urban_malaria")
  PopDir <- file.path(DriveDir, "data", 'data_agric_analysis')
  ManDir <- file.path(DriveDir, "projects", "Manuscripts", "agriculture_malaria_manuscript")
  FigDir <- file.path(ManDir, "figures", "main_figures","pdf_figures")
  SupDir <- file.path(ManDir, "figures", "supplementary", "pdf_figures")
  ExpDir <- file.path(ManDir, "figures", "exploratory")
} else if  ("CHZCHI003" %in% user) {
  Drive <- file.path("C:/Users/CHZCHI003/OneDrive")
  DriveDir <- file.path(Drive, "urban_malaria")
  PopDir <- file.path(DriveDir, "data", 'data_agric_analysis')
  ManDir <- file.path(DriveDir, "projects", "Manuscripts", "agriculture_malaria_manuscript")
  FigDir <- file.path(ManDir, "figures", "main_figures","pdf_figures")
  SupDir <- file.path(ManDir, "figures", "supplementary", "pdf_figures")
  ExpDir <- file.path(ManDir, "figures", "exploratory")
} else {
  Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
  DriveDir <- file.path(Drive, 'Library', 'CloudStorage', 'OneDrive-NorthwesternUniversity', "urban_malaria")
  #DriveDir <- file.path(Drive,  "OneDrive - Northwestern University", "urban_malaria")
  PopDir <- file.path(DriveDir, "data", 'data_agric_analysis')
  ManDir <- file.path(DriveDir, "projects", "Manuscripts", "agriculture_malaria_manuscript")
  FigDir <- file.path(ManDir, "figures", "main_figures","pdf_figures")
  SupDir <- file.path(ManDir, "figures", "supplementary", "pdf_figures")
  ExpDir <- file.path(ManDir, "figures", "exploratory")
}

## -----------------------------------------
### Required functions and settings
## -----------------------------------------
#note before sourcing functions and packages, run the code below to download rdhs if you don't already have it
#devtools::install_github("ropensci/rdhs")
source("functions/functions_employment.R")
options(survey.lonely.psu="adjust")  # this option allows admin units with only one cluster to be analyzed
library(svylme) #use glmer package instead 
## -------------------------------
### read in analysis datasets 
## -------------------------------
all_df <- read_csv(file.path(PopDir, "analysis_dat/urban_rural_analysis_data_for_modeling.csv")) %>% 
  mutate(malaria_result = ifelse(test_result =="+ve", 1,0), 
         EVI_2000m_new = case_when(is.na(EVI_2000m_new) ~ NA,
                                   TRUE ~ EVI_2000m_new * 10),
         home_type_factor = ifelse(home_type2 =="A", 1, 0),
         wealth_index = as.factor(wealth),
         dhs_year_factor = as.factor(dhs_year),
         sex = ifelse(hc27 == 2, 0, 1),
         stunting = ifelse(hc70 < -300, "Stunted", ifelse(hc70 > 8000, NA, "Not stunted")))

urban_df <-all_df %>%  filter(type == "Urban")  

glimpse(urban_df)


# Combine results for all location types
var <- list("home_type2", "hc1", "sex", "stunting", "u5_net_use", "hh_size", "roof_type", 
            "wealth_index", "dhs_year_factor", "EVI_2000m_new", "preci_monthly_2000m", "RH_monthly_2000m", "temp_monthly_2000m")

table_names <- c("Household occupation category: agricultural", "Age", "Gender: male", "Stunting: stunted", 
                 "Net use among children under the age of five years: use", "Household size", "Roof type: improved", 
                 "Wealth: poor", "Wealth: middle", "Wealth: rich", "Wealth: richest",
                 "DHS Year: 2013", "DHS Year: 2014", "DHS Year: 2015", "DHS Year: 2016", "DHS Year: 2017", 
                 "DHS Year: 2018", "DHS Year: 2019", "DHS Year: 2021", "DHS Year: 2022", "DHS Year: 2023", 
                 "Enhanced vegetation index", "Precipitation", "Relative humidity (%)", "Temperature")

all_df_renamed_vars <- all_df
#all_df_renamed_vars$

location_types <- c("Urban", "Rural")

all_results <- list() # To store results for each location type

for (location in location_types) {
  # Filter the data based on location type
  df <- all_df %>%
    filter(type == location)
  
  # Drop NA values
  df_new <- df %>% drop_na(EVI_2000m_new)
  unadj_df <- list()
  
  for (i in 1:length(var)) {
    svy_design <- svydesign.fun(df_new)
    
    # Construct the formula dynamically
    formula <- as.formula(paste("malaria_result ~", var[[i]], "+ (1|hv001)"))
    
    # Fit the model
    result <- svyglm(formula, design = svy_design, family = binomial(link = "logit"))
    
    # Summarize and tidy the result
    df_result <- tidy(result)
    
    # Filter and calculate additional metrics
    df_result <- df_result %>%
      filter(term != "(Intercept)") %>%
      rename_at(3, ~"SE") %>%
      mutate(odds = exp(estimate)) %>%
      mutate(lower_ci = exp(-1.96 * SE + estimate)) %>%
      mutate(upper_ci = exp(1.96 * SE + estimate)) %>%
      tibble::rownames_to_column() %>%
      mutate(type = "unadjusted", location = location) # Add location information
    
    unadj_df[[i]] <- df_result
  }
  
  # Combine results for this location type
  all_results[[location]] <- bind_rows(unadj_df)
}

# Combine results for all location types and format
all_results_combined <- bind_rows(all_results) %>%
  transmute(
    term,
    estimate = sprintf("%.3f (%.3f – %.3f)", 
                       round(odds, 3), round(lower_ci, 3), round(upper_ci, 3)), # Round and format odds and CIs
    location
  ) %>%
  pivot_wider(
    names_from = location,
    values_from = estimate
  )

all_results_combined


all_results_combined$term <- table_names 

ref_df <- data.frame(term = c("Gender: female", "Stunting: not stunted", 
                              "Household occupation category: none agricultural worker HH",
                              "Net use among children under the age of five years: did not use", 
                              "Roof type: poor","Wealth: poorest", "DHS Year: 2012"), 
                     Urban = c("1.000","1.000","1.000","1.000","1.000","1.000","1.000"), 
                     Rural = c("1.000","1.000","1.000","1.000","1.000","1.000","1.000"))

all_results_final <- all_results_combined %>% bind_rows(ref_df) %>% arrange(term)

write_xlsx(all_results_final, file.path(PopDir, "analysis_dat", "single_reg_results.xlsx"))



urban_df <- read_csv(file.path(PopDir, "analysis_dat/urban_df_for_analysis.csv")) %>%  mutate(type ="urban_data") 
rural_df <- read_csv(file.path(PopDir,"analysis_dat/rural_df_for_analysis.csv")) %>%  mutate(type ="rural_data")

df_env <- read.csv(file.path(PopDir, "analysis_dat/all_geospatial_monthly_DHS.csv")) %>% 
  mutate(code_year = paste(stringr::str_extract(.id, "^.{2}"), dhs_year, sep = "")) %>% select(-dhs_year)

# obtaining country ids
ids <- dhs_countries(returnFields=c("CountryName", "DHS_CountryCode", "SubregionName"))

#add subregion 
urban_df <- urban_df %>%  left_join(ids, by = ("DHS_CountryCode")) %>% left_join(df_env, by = c("code_year", "hv001")) 
rural_df <- rural_df %>%  left_join(ids, by = ("DHS_CountryCode")) %>% left_join(df_env, by = c("code_year", "hv001")) 

table(rural_df$u5_net_use)
table(urban_df$home_type2)
table(urban_df$hv253)

glimpse(urban_df)

## -------------------------------
### model  
## -------------------------------
#unadjusted model fit 

dat <- list(urban_df, rural_df)

unadj_df <- list()
location <- c("Urban", "Rural")

for (i in 1:length(dat)) {
mod_df <- dat[[i]] %>%  mutate(malaria_result = ifelse(test_result =="+ve", 1,0)) %>% 
  mutate(work_HH =  ifelse(home_type2 =="A", "1","0")) %>%  
  rename(agric_home = work_HH)

svy_design <- svydesign.fun(mod_df)
result <- svyglm(malaria_result ~ agric_home, design = svy_design, family = binomial(link ="logit"))#fits model with no additional covariates 
res_sum <- summary(result)


df <-  tidy(result)#tidies the result 
df <- df %>%  filter(term != "(Intercept)")%>% rename_at(3, ~"SE")
df <- data.frame(df)%>% mutate(odds = (exp(estimate))) %>% #odds ratio estimation 
  mutate(lower_ci = (exp(-1.96*SE+estimate))) %>% 
  mutate(upper_ci = (exp(1.96*SE+estimate))) %>% tibble::rownames_to_column() %>%  
  mutate(location = location[[i]], type = "unadjusted") 
unadj_df[[i]] <- df

}

unadj_df <- bind_rows(unadj_df)
unadj_df

#adjusted model fit 
#but first some exploration of other likely predictors to understand their relationship with agric
#categorical variable comparisons 
title<- c("Urban", "Rural")

plots <- list()
for (i in 1:length(dat)){

df <- dat[[i]] %>%  select(home_type2,roof_type) %>%  drop_na() %>% 
  mutate(roof = ifelse(roof_type == 1, "Low-risk", "High-risk")) %>%group_by(roof,home_type2) %>%   summarise(value= n()) %>% 
  mutate(percent = round(value/sum(value) *100, 0)) %>%  ungroup() 

p1<- ggplot(df, aes(fill=home_type2, x= roof)) + 
  geom_bar(aes(y = percent), position="stack", stat = "identity")+
  scale_fill_manual(name = "", label = c("Agricultural worker Household (HH)", "Non-Agricultural worker HH"), values = c("bisque", "forestgreen"))+
  theme_manuscript() +
  labs(x = "Roof", title = title[[i]] )

df2 <- dat[[i]] %>%  select(home_type2,wealth) %>%  drop_na()%>% group_by(wealth,home_type2) %>%   summarise(value= n()) %>% 
  mutate(percent = round(value/sum(value) *100, 0)) %>%  ungroup() 

p2<- ggplot(df2, aes(fill=home_type2, x= wealth)) + 
  geom_bar(aes(y = percent), position="stack", stat = "identity")+
  scale_fill_manual(name = "", label = c("Agricultural worker Household (HH)", "Non-Agricultural worker HH"), values = c("bisque", "forestgreen"))+
  theme_manuscript() +
  labs(x = "Wealth Quintile", title = title[[i]])+
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.y = element_blank())

df3 <- dat[[i]] %>%  select(home_type2,u5_net_use) %>%  drop_na() %>% 
mutate(net_use = ifelse(u5_net_use == 1, "Slept under a net", "Did not sleep under a net"))%>% 
  group_by(net_use,home_type2) %>%   summarise(value= n()) %>% 
  mutate(percent = round(value/sum(value) *100, 0)) %>%  ungroup() 

p3<- ggplot(df3, aes(fill=home_type2, x= net_use)) + 
  geom_bar(aes(y = percent), position="stack", stat = "identity")+
  scale_fill_manual(name = "", label = c("Agricultural worker Household (HH)", "Non-Agricultural worker HH"), values = c("bisque", "forestgreen"))+
  theme_manuscript() +
  labs(x = "Net use", title = title[[i]])+
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.y = element_blank())

p<- p1 + p2 + p3 + plot_layout(guides = "collect") & theme(legend.position = 'bottom')

plots[[i]] <- p
}

p_cov <- plots[[1]]/ plots[[2]]+ plot_layout(guides = "collect")
p_cov


ggsave(paste0(SupDir,"/", Sys.Date(),"likely_confounder_distribution.pdf"), p_cov, width = 8.5, height = 5) 


#IRS - no difference between the two groups  
df3 <- urban_df %>%  select(home_type2,hv253) %>%  drop_na() %>% 
  mutate(IRS = ifelse(hv253 == 1, "House was sprayed", ifelse(hv253 == 0, "Not sprayed", NA)))%>% 
  group_by(home_type2, IRS) %>%   summarise(value= n()) %>% 
  mutate(percent = round(value/sum(value) *100, 0)) %>%  ungroup() %>%  drop_na(IRS)

ggplot(df3, aes(fill=IRS, x= home_type2)) + 
  geom_bar(aes(y = value), position="stack", stat = "identity")+
  scale_fill_manual(name = "", label = c(" House was sprayed", "House was not sprayed"), values = c("#d391fa", "#3e00b3"))+
  theme_manuscript() +
  geom_text(aes(label = paste0(value, " ", "(", percent, "%", ")"), y = value),
            position = position_stack(vjust = 0.5),
            color = "white")+ 
  labs(x = "")+
  scale_x_discrete(labels = c("Agricultural worker \n household (HH)", "Non-agricultural \n worker HH"))
ggsave(paste0(ExpDir,"/", Sys.Date(),"IRS_distribution_urban.pdf"), width = 6, height = 5) 

df3 <- rural_df %>%  select(home_type2,hv253) %>%  drop_na() %>% 
  mutate(IRS = ifelse(hv253 == 1, "House was sprayed", ifelse(hv253 == 0, "Not sprayed", NA)))%>% 
  group_by(home_type2, IRS) %>%   summarise(value= n()) %>% 
  mutate(percent = round(value/sum(value) *100, 0)) %>%  ungroup() %>%  drop_na(IRS)

ggplot(df3, aes(fill=IRS, x= home_type2)) + 
  geom_bar(aes(y = value), position="stack", stat = "identity")+
  scale_fill_manual(name = "", label = c(" House was sprayed", "House was not sprayed"), values = c("#d391fa", "#3e00b3"))+
  theme_manuscript() +
  geom_text(aes(label = paste0(value, " ", "(", percent, "%", ")"), y = value),
            position = position_stack(vjust = 0.5),
            color = "white")+ 
  labs(x = "")+
  scale_x_discrete(labels = c("Agricultural worker \n household (HH)", "Non-agricultural \n worker HH"))
ggsave(paste0(ExpDir,"/", Sys.Date(),"IRS_distribution_rural.pdf"), width = 6, height = 5) 



#discrete and categorical variable comparisons
plot_df <- urban_df %>%  mutate(home_type3 = ifelse(home_type2 == "A", "Agricultural worker \n Household (HH)", 
                                                    "Non-Agricultural \n worker HH"),
                                type = "Urban")
p1 <- ggplot(plot_df, aes(x = home_type3, y = hh_size, fill = home_type3)) +
  scale_fill_manual(name = '', values =c('#5560AB','#FAAF43'))+
  geom_boxplot(outlier.size = -1, color="black", alpha = 0.7) +
  geom_point(aes(fill = home_type3), shape = 21,size=2, alpha=0.6, stroke=0, color = '#979797')+
  labs(x = "", y = "hh_size", fill = "home type") +
  theme_manuscript()+
  theme(legend.position = 'none')+
  labs(y = "Household size")+
  facet_wrap(vars(type)) +
  theme(strip.text.x = element_text(size = 12))


plot_df <- rural_df %>%  mutate(home_type3 = ifelse(home_type2 == "A", "Agricultural worker \n Household (HH)", 
                                                    "Non-Agricultural \n worker HH"),
                                type = "Rural")
p2 <- ggplot(plot_df, aes(x = home_type3, y = hh_size, fill = home_type3)) +
  scale_fill_manual(name = '', values =c('#5560AB','#FAAF43'))+
  geom_boxplot(outlier.size = -1, color="black", alpha = 0.7) +
  geom_point(aes(fill = home_type3), shape = 21,size=2, alpha=0.6, stroke=0, color = '#979797')+
  labs(x = "", y = "hh_size", fill = "home type") +
  theme_manuscript()+
  theme(legend.position = 'none')+
  labs(y = "Household size")+
  facet_wrap(vars(type))+ 
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.y = element_blank()) +
  theme(strip.text.x = element_text(size = 12))


p_hh_size = p1 +p2
p_hh_size
ggsave(paste0(SupDir,"/", Sys.Date(),"HH_size_distribution.pdf"), p_hh_size, width = 8.5, height = 4) 


#adjusted model fit 
#urban and rural

#assessing env covariates for colaration 
ev_cor <- urban_df %>% select(EVI_2000m, preci_monthly_2000m, RH_monthly_2000m, temp_monthly_2000m) %>% cor()
corr= ggcorrplot(ev_cor, lab = TRUE, legend.title = "Correlation coefficient")+ theme_corr()
corr

ev_cor_rural <- rural_df %>% select(EVI_2000m, preci_monthly_2000m, RH_monthly_2000m, temp_monthly_2000m) %>% cor()
corr_rural= ggcorrplot(ev_cor_rural, lab = TRUE, legend.title = "Correlation coefficient")+ theme_corr()
corr_rural

#correlation is very low with highest of -0.19

dat <- list(urban_df, rural_df)

adj_df <- list()
mod_list <- list()
location <- c("Urban", "Rural")

for (i in 1:length(dat)) {
  mod_df <- dat[[i]] %>%  mutate(malaria_result = ifelse(test_result =="+ve", 1,0)) %>% 
    mutate(work_HH =  ifelse(home_type2 =="A", "1","0")) %>% 
  rename(agric_home = work_HH)
  
  svy_design <- svydesign.fun(mod_df)
  result <- svyglm(malaria_result ~ agric_home + u5_net_use + roof_type + wealth + hh_size + dhs_year + DHS_CountryCode + 
                     EVI_2000m + preci_monthly_2000m + RH_monthly_2000m + temp_monthly_2000m, 
                   design = svy_design, family = binomial(link ="logit"))#fits model with no additional covariates 
  mod_list[[i]] <- result
  res_sum <- summary(result)
  
  
  df <-  tidy(result)#tidies the result 
  df <- df %>%  filter(term != "(Intercept)")%>% rename_at(3, ~"SE")
  df <- data.frame(df)%>% mutate(odds = (exp(estimate))) %>% #odds ratio estimation 
    mutate(lower_ci = (exp(-1.96*SE+estimate))) %>% 
    mutate(upper_ci = (exp(1.96*SE+estimate))) %>% tibble::rownames_to_column() %>%  
    mutate(location = location[[i]], type = "adjusted") 
  adj_df[[i]] <- df
  
}

adj_df <- bind_rows(adj_df)
adj_df

#bind aadjusted and unadjusted estimates 
all_est <- rbind(unadj_df, adj_df)
all_est

#predicted probability 
effct_df_urban <- effect_df_fun(mod_list[[1]]) %>%  mutate(rowname_new = ifelse(rowname==0, "Non-agricultural \n worker HH", "Agricultural \n worker HH"))
effct_df_rural <- effect_df_fun(mod_list[[2]]) %>%  mutate(rowname_new = ifelse(rowname==0, "Non-agricultural \n worker HH", "Agricultural \n worker HH"))


#country adjusted odds 
mod_df <- urban_df %>%  mutate(malaria_result = ifelse(test_result =="+ve", 1,0)) %>% 
  mutate(work_HH =  ifelse(home_type2 =="A", "1","0")) %>% 
  rename(agric_home = work_HH)

svy_design <- svydesign.fun(mod_df)
result <- svyby(formula = malaria_result ~ agric_home + u5_net_use + roof_type + wealth + hh_size +
                  EVI_2000m + preci_monthly_2000m + RH_monthly_2000m + temp_monthly_2000m, by = ~ DHS_CountryCode, design = svy_design, FUN = svyglm)


df <- result%>% mutate(odds = (exp(agric_home1))) %>% #odds ratio estimation 
  mutate(lower_ci = (exp(-1.96*se.agric_home1+agric_home1))) %>% 
  mutate(upper_ci = (exp(1.96*se.agric_home1+agric_home1))) %>% 
  select(DHS_CountryCode, agric_home1, se.agric_home1, odds, lower_ci, upper_ci)
 

############################################################
# Plots 
############################################################
#Forest plots 
df <- all_est %>%  filter(term == "agric_home1") %>% mutate(loc_type = paste0(location, " ", "(", type, ")"))

df$location <- factor(df$location)

library(scales)
numColors <- length(levels(df$location)) # How many colors you need
getColors <- scales::brewer_pal('qual') # Create a function that takes a number and returns a qualitative palette of that length (from the scales package)
myPalette <- getColors(numColors)
names(myPalette) <- levels(df$location) # Give every color an appropriate name



forest_b <- ggplot(df, aes(x = odds, y = loc_type)) + 
  geom_vline(aes(xintercept = 1), size = .25, linetype = "dashed") + 
  geom_errorbarh(aes(xmax = lower_ci, xmin = upper_ci, color = location), size = .5, height = 
                   .2) + 
  geom_point(aes(color = location), size = 2.5)+
  scale_color_manual(name ="", values =c("forestgreen", "darkorchid4")) +
  theme_bw()+
  theme(panel.grid.minor = element_blank())+ 
  theme(panel.border = element_blank())+
  ylab("")+
  xlab("Odds ratio for testing positive for malaria with RDT
        or microscopy among children, 6 - 59 months,
        in an agricultural worker HH compared to a 
        compared to a non-agricultural worker HH")+
  theme_manuscript()+
  theme(legend.position = "none")+
  xlim(0.5,3.7) + 
    theme(axis.text.y = element_text(colour=c("forestgreen", "forestgreen",  "darkorchid4", "darkorchid4")))
forest_b


#predicted probability 
pred_p <- ggplot() + 
  geom_errorbar(data = effct_df_urban, aes(y = effect, x = rowname_new, ymax = lower, ymin = upper), size = .5, width = .2, color = "darkorchid4") + 
  geom_errorbar(data = effct_df_rural, aes(y = effect, x = rowname_new, ymax = lower, ymin = upper), size = .5, width = .2, color = "forestgreen") + 
  geom_point(data = effct_df_urban, aes(y = effect, x = rowname_new), size = 2.5, color = "darkorchid4")+
  geom_point(data = effct_df_rural, aes(y = effect, x =rowname_new), size = 2.5, color = "forestgreen")+
  theme(panel.grid.minor = element_blank())+ 
  theme(panel.border = element_blank(), axis.title.x=element_blank(),
        main.title.x=element_blank())+
  theme_manuscript()+
  labs(x = "", y= "Adjusted predicted probability of testing positive for 
       malaria with RDT or microscopy among children, 
       6 - 59 months") +
  ylim(0,0.5)

all_p <- forest_b + pred_p

ggsave(paste0(FigDir,"/", Sys.Date(),"_Figure_4_odds_pred_prob.pdf"), all_p, width = 8.5, height = 5) 

##Environmental Variable effects

eff <- Effect("EVI_2000m", m1)


effect_df_fun <- function(model_, co_var){
  effect_list_est <- summary(Effect(co_var, model_)) 
  effect_list_est$effect %>% as.data.frame() %>% 
    bind_cols(effect_list_est$lower %>% as.data.frame()) %>% 
    bind_cols(effect_list_est$upper %>% as.data.frame()) %>% 
    rename(effect = ....1, lower = ....2, upper = ....3) %>% 
    tibble::rownames_to_column()
}

#predicted probability 
effct_df_urban <- effect_df_fun(mod_list[[1]], "EVI_2000m") 
effct_df_rural <- effect_df_fun(mod_list[[2]], "EVI_2000m")




#predicted probability 


vars <- c("EVI_2000m" , "preci_monthly_2000m" , "RH_monthly_2000m" , "temp_monthly_2000m")
y_lim <- list(0.5, 0.5 ,0.5, 0.5)
lables <- list("enhanced vegetation index", "precipitation", "relative humidity", "temperature")

pred_fun <- function(model_number){
  p <- list()
  for (i in 1:length(vars)) { 
    eff <- Effect(vars[[i]], mod_list[[model_number]])
    eff_dt <- data.frame(eff)
    eff_dt$fit <- (eff_dt$fit)#/2 #we are scaling by deviding by mean number of children tested in a cluster
    eff_dt$lower <- (eff_dt$lower)#/2
    eff_dt$upper <- (eff_dt$upper)#/2
    pt = ggplot(eff_dt,aes_string(vars[[i]], 'fit'))+ 
      geom_ribbon(aes(ymin=lower, ymax=upper), alpha =0.2, fill = "#666699")+
      geom_line(color = "#666699", size = 1)+ theme_manuscript()+ 
      ylim(0, y_lim[[1]]) + 
      labs(x = paste0(as.character(lables[[i]]), ' ', as.character('')), y ='predicted probability to test positive')
    p[[i]]<- pt
    
  }
  
  y=p[[1]]+ p[[2]] + p[[3]] + p[[4]]
  return(y) 
}


#urban
pred_fun(1)

#rural
pred_fun(2)

########################################################################
# model tests
#############################################################################
dat <- list(urban_df, rural_df) %>% 
  map(~select(.,(c(home_type2, u5_net_use, roof_type, wealth, hh_size, EVI_2000m, preci_monthly_2000m, dhs_year,
                   RH_monthly_2000m, temp_monthly_2000m, strat, wt, id, test_result, DHS_CountryCode)))) %>% map(~drop_na(.,))

mod_list2 <- list()
  
for (i in 1:length(dat)) {
  mod_df <- dat[[i]] %>%  mutate(malaria_result = ifelse(test_result =="+ve", 1,0)) %>% 
    mutate(work_HH =  ifelse(home_type2 =="A", "1","0")) %>% 
    rename(agric_home = work_HH)
  
  svy_design <- svydesign.fun(mod_df)
  result <- svyglm(malaria_result ~ agric_home + u5_net_use + roof_type + wealth + hh_size + dhs_year + DHS_CountryCode + 
                     EVI_2000m + preci_monthly_2000m + RH_monthly_2000m + temp_monthly_2000m, 
                   design = svy_design, family = binomial(link ="logit"))
  
  
  result_evi <- svyglm(malaria_result ~ agric_home + u5_net_use + roof_type + wealth + hh_size + dhs_year + DHS_CountryCode + 
                     EVI_2000m, design = svy_design, family = binomial(link ="logit"))
  
  
  result_preci <- svyglm(malaria_result ~ agric_home + u5_net_use + roof_type + wealth + hh_size + dhs_year + DHS_CountryCode + 
                         preci_monthly_2000m, design = svy_design, family = binomial(link ="logit"))
  
  result_RH <- svyglm(malaria_result ~ agric_home + u5_net_use + roof_type + wealth + hh_size + dhs_year + DHS_CountryCode + 
                         RH_monthly_2000m, design = svy_design, family = binomial(link ="logit"))
  
  result_temp <- svyglm(malaria_result ~ agric_home + u5_net_use + roof_type + wealth + hh_size + dhs_year + DHS_CountryCode + 
                         temp_monthly_2000m, design = svy_design, family = binomial(link ="logit"))
  
  #result <- list(result_evi, result_preci, result_RH, result_temp)
  mod_list2[[i]] <- result
}

#rural wald test
regTermTest(result_evi, test.terms = ~ EVI_2000m,
            df = degf(result_evi$survey.design), method = "LRT")

regTermTest(result_temp, test.terms = ~ temp_monthly_2000m,
            df = degf(result_temp$survey.design), method = "LRT")

regTermTest(result_RH, test.terms = ~ RH_monthly_2000m,
            df = degf(result_RH$survey.design), method = "LRT")

regTermTest(result_preci, test.terms = ~ preci_monthly_2000m,
            df = degf(result_preci$survey.design), method = "LRT")

#test entire model urban
urban_model <- mod_list2[[1]]
regTermTest(urban_model, test.terms = ~ EVI_2000m,
            df = degf(urban_model$survey.design), method = "LRT")

regTermTest(urban_model, test.terms = ~  temp_monthly_2000m,
            df = degf(urban_model$survey.design), method = "LRT")

regTermTest(urban_model, test.terms = ~ RH_monthly_2000m,
            df = degf(urban_model$survey.design), method = "LRT")

regTermTest(urban_model, test.terms = ~ preci_monthly_2000m,
            df = degf(urban_model$survey.design), method = "LRT")
 



#### testing 2LogLR percentage difference

desin_list <- list()

for (i in 1:length(dat)) {
  mod_df <- dat[[i]] %>%  mutate(malaria_result = ifelse(test_result =="+ve", 1,0)) %>% 
    mutate(work_HH =  ifelse(home_type2 =="A", "1","0")) %>% 
    rename(agric_home = work_HH)
  
  svy_design <- svydesign.fun(mod_df)
  
  desin_list[[i]] <- svy_design
  
}


#baseline model
result <- svyglm(malaria_result ~ agric_home,
                 design = desin_list[[1]], family = binomial(link ="logit"))

regTermTest(result_evi, test.terms = ~ agric_home,
            df = degf(result_evi$survey.design), method = "LRT")

#urban wald test


#evi
result_evi <- svyglm(malaria_result ~ EVI_2000m,
                 design = svy_design, family = binomial(link ="logit"))


regTermTest(result_evi, test.terms = ~ EVI_2000m,
            df = degf(result_evi$survey.design), method = "LRT")

#temp
result_temp <- svyglm(malaria_result ~ temp_monthly_2000m,
                     design = svy_design, family = binomial(link ="logit"))

regTermTest(result_temp, test.terms = ~ temp_monthly_2000m,
            df = degf(result_temp$survey.design), method = "LRT")

#humidity
result_RH <- svyglm(malaria_result ~ RH_monthly_2000m,
                      design = svy_design, family = binomial(link ="logit"))

regTermTest(result_RH, test.terms = ~ RH_monthly_2000m,
            df = degf(result_RH$survey.design), method = "LRT")

#humidity
result_preci <- svyglm(malaria_result ~ preci_monthly_2000m,
                    design = svy_design, family = binomial(link ="logit"))

regTermTest(result_preci, test.terms = ~ preci_monthly_2000m,
            df = degf(result_preci$survey.design), method = "LRT")
