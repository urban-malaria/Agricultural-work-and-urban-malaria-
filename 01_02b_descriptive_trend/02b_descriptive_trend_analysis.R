#The purpose of this script is generate figure 1, 4 and 5 

rm(list = ls())

## -----------------------------------------
### Directories
## -----------------------------------------
user <- Sys.getenv("USERNAME")
if ("ido0493" %in% user) {
  user_path <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("OneDrive"))))
  DriveDir <- file.path(user_path, "urban_malaria")
  PopDir <- file.path(DriveDir, "data", "data_agric_analysis")
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


## -------------------------------
### read in analysis datasets 
## -------------------------------

urban_df <- read_csv(file.path(PopDir, "analysis_dat/urban_df_for_analysis_trend.csv")) %>%  mutate(type ="urban_data") 
rural_df <- read_csv(file.path(PopDir,"analysis_dat/rural_df_for_analysis_trend.csv")) %>%  mutate(type ="rural_data")


## -------------------------------
### Plots  
## -------------------------------
#totals by country 
# all_df <- rbind(urban_df, rural_df) %>% mutate(country_year.x = ifelse(country_year.x == "Congo Democratic Republic 2013 - 14", "DRC 2013 - 14",
#                                                                        ifelse(country_year.x == "Cameroon 2018", "Cameroon 2018 - 19",country_year.x))) %>% 
#   as_survey_design(ids= id,strata=strat,nest=T,weights= wt)%>% 
#   group_by(country_year.x) %>%  summarize(total = round(survey_total(),0)) %>%  ungroup() 


#figure 1 sample description 
all_df <- rbind(urban_df, rural_df) %>% mutate(country_year.x = ifelse(country_year.x == "Congo Democratic Republic 2013 - 14", "DRC 2013 - 14",
                                                                       ifelse(country_year.x == "Uganda 2009", "Uganda 2009 - 10",
                                                                          ifelse(country_year.x == "Cameroon 2018", "Cameroon 2018 - 19",country_year.x)))) %>% 
  as_survey_design(ids= id,strata=strat,nest=T,weights= wt)%>% 
 group_by(country_year.x, type) %>%  summarize(total = round(survey_total(),0)) %>%  ungroup() 


df <- all_df%>% 
  group_by(country_year.x) %>%  summarise(percent =  round(total/sum(total) * 100, 0))
all <- cbind(all_df, df) %>% dplyr::select(-c("country_year.x")) %>% mutate(plot_label = ifelse(type == "rural_data", percent, NA))

#figure 1b
df <- rbind(urban_df, rural_df)



#urban
plot_u_df2<- urban_df %>% dplyr::select(country_year.x, home_type2, code_year, test_result) 



plot_overall = plot_u_df2 %>%  group_by(home_type2, test_result) %>%  summarise(value= n()) %>% mutate(percent = round(value/sum(value) *100, 0))
plot_overall$title = "Urban"


#plot by country 
plot_country = plot_u_df2 %>%  group_by(country_year.x, code_year, home_type2, test_result) %>%  summarise(value= n()) %>% mutate(percent = round(value/sum(value) *100, 0)) %>% 
  mutate(country_year.x = ifelse(country_year.x == "Congo Democratic Republic 2013 - 14", "DRC 2013 - 14",country_year.x),
         home_type2 = ifelse(home_type2 == 'A', 'Agricultural worker Household (HH)', "Non-Agricultural worker HH"))

df_u <- all_df %>% group_by(country_year.x) %>%  summarise(total2 = sum(total))

plot_country <- plot_country  %>%  left_join(df_u, by = "country_year.x")  %>% 
  mutate(plot_label = ifelse(test_result == "-ve", percent, NA), cntryId = stringr::str_extract(code_year, "^.{2}"))


#diff figure 2 urban
plot_country = plot_country %>% filter(test_result == "+ve")
diff_d_u <- plot_country %>% group_by(country_year.x, ) %>%  mutate(diff_val = percent[home_type2 == "Agricultural worker Household (HH)"] - percent) %>% 
  filter(home_type2 == "Non-Agricultural worker HH") 

diff_d_u$title = "Urban"



#rural

plot_u_df<- rural_df %>% dplyr::select(country_year.x, home_type2, code_year, test_result) 

plot_overall = plot_u_df %>%  group_by(home_type2, test_result) %>%  summarise(value= n()) %>% mutate(percent = round(value/sum(value) *100, 0))
plot_overall$title = "Rural"

#rural by country figure for supplement 
plot_country = plot_u_df %>%  group_by(country_year.x,home_type2, test_result) %>%  summarise(value= n()) %>% mutate(percent = round(value/sum(value) *100, 0)) %>% 
  mutate(country_year.x = ifelse(country_year.x == "Congo Democratic Republic 2013 - 14", "DRC 2013 - 14",country_year.x),
         home_type2 = ifelse(home_type2 == 'A', 'Agricultural worker Household (HH)', "Non-Agricultural worker HH")) %>% 
  mutate(country_year.x = ifelse(country_year.x == "Cameroon 2018", "Cameroon 2018 - 19",country_year.x)) %>% mutate(plot_label = ifelse(test_result == "-ve", percent, NA))

df_u <- all_df %>% group_by(country_year.x) %>%  summarise(total2 = sum(total))

plot_country <- plot_country  %>%  left_join(df_u, by = "country_year.x") 


#diff figure 2 rural
plot_country = plot_country %>% filter(test_result == "+ve")
diff_d_r <- plot_country %>% group_by(country_year.x) %>%  mutate(diff_val = percent[home_type2 == "Agricultural worker Household (HH)"] - percent) %>% 
  filter(home_type2 == "Non-Agricultural worker HH")
diff_d_r$title = "Rural"

#new test positivity difference plot 

diff_u <- diff_d_u %>% dplyr::select(country_year.x, diff_val, title)
diff_r <- diff_d_r %>%  dplyr::select(country_year.x, diff_val, title)

all_diff <- rbind(diff_u, diff_r) %>%  group_by(country_year.x) 

p_dat <- all_diff%>% 
  summarise(start = range(diff_val)[1], end = range(diff_val)[2]) %>% 
  ungroup()

p_dat$country_year.x <- factor(p_dat$country_year.x,levels=rev(unique(p_dat$country_year.x)))

p_2b<-ggplot(p_dat, aes(x = start, y = country_year.x, end))+
  geom_segment(aes(xend = end, yend = country_year.x)) +
  
  geom_point(
    data = all_diff,
    aes(diff_val, country_year.x, color = title), 
    size = 4, alpha =0.7
  ) +
  
  scale_color_manual(name= "", values=c( "darkgoldenrod1", "darkviolet")) +
  scale_x_continuous(breaks = seq(-2, 28, by = 2))+
  theme_manuscript() +
  theme(legend.position = "bottom") +
  labs(y = "", x = "Percentage difference in malaria test positivity rates
      between agricultural worker HH and non-agricultural worker HH")
p_2b



######################################################################
#Country level - difference between household type
#####################################################################

#urban
plot_u_df2<- urban_df %>% dplyr::select(country_year.x, home_type2, code_year, test_result) 



plot_overall = plot_u_df2 %>%  group_by(home_type2, test_result) %>%  summarise(value= n()) %>% mutate(percent = round(value/sum(value) *100, 0))
plot_overall$title = "Urban"


#plot by country 
plot_country = plot_u_df2 %>%  group_by(country_year.x, code_year, home_type2, test_result) %>%  summarise(value= n()) %>% mutate(percent = round(value/sum(value) *100, 0)) %>% 
  mutate(country_year.x = ifelse(country_year.x == "Congo Democratic Republic 2013 - 14", "DRC 2013 - 14",country_year.x),
         home_type2 = ifelse(home_type2 == 'A', 'Agricultural worker Household (HH)', "Non-Agricultural worker HH"))

df_u <- all_df %>% group_by(country_year.x) %>%  summarise(total2 = sum(total))

plot_country <- plot_country  %>%  left_join(df_u, by = "country_year.x")  %>% 
  mutate(plot_label = ifelse(test_result == "-ve", percent, NA), cntryId = stringr::str_extract(code_year, "^.{2}")) 
  


#diff figure 2 urban
plot_country = plot_country %>% filter(test_result == "+ve") %>% group_by(home_type2, cntryId) %>% mutate(id = as.character(row_number())) %>% 
  mutate(cntryId_id = paste(cntryId, id))

diff_d_u_a <- plot_country %>% filter(home_type2 != "Non-Agricultural worker HH") %>%  group_by(cntryId) %>%  
  mutate(diff_val = percent[id == "2"] - percent) %>% filter(id != "2")

diff_d_u <- plot_country %>% filter(home_type2 != "Agricultural worker Household (HH)") %>%  group_by(cntryId) %>%  
  mutate(diff_val = percent[id == "2"] - percent) %>% filter(id != "2") %>%  rbind(diff_d_u_a) 


diff_d_u $title = "Urban"


#rural
plot_r_df2<- rural_df %>% dplyr::select(country_year.x, home_type2, code_year, test_result) 



plot_overall = plot_r_df2 %>%  group_by(home_type2, test_result) %>%  summarise(value= n()) %>% mutate(percent = round(value/sum(value) *100, 0))
plot_overall$title = "Urban"


#plot by country 
plot_country = plot_r_df2 %>%  group_by(country_year.x, code_year, home_type2, test_result) %>%  summarise(value= n()) %>% mutate(percent = round(value/sum(value) *100, 0)) %>% 
  mutate(country_year.x = ifelse(country_year.x == "Congo Democratic Republic 2013 - 14", "DRC 2013 - 14",country_year.x),
         home_type2 = ifelse(home_type2 == 'A', 'Agricultural worker Household (HH)', "Non-Agricultural worker HH"))

df_r <- all_df %>% group_by(country_year.x) %>%  summarise(total2 = sum(total))

plot_country2 <- plot_country  %>%  left_join(df_r, by = "country_year.x")  %>% 
  mutate(plot_label = ifelse(test_result == "-ve", percent, NA), cntryId = stringr::str_extract(code_year, "^.{2}")) 



#diff figure 2 urban
plot_country = plot_country2 %>% filter(test_result == "+ve") %>% group_by(home_type2, cntryId) %>% mutate(id = as.character(row_number())) %>% 
  mutate(cntryId_id = paste(cntryId, id))

diff_d_r_a <- plot_country %>% filter(home_type2 != "Non-Agricultural worker HH") %>%  group_by(cntryId) %>%  
  mutate(diff_val = percent[id == "2"] - percent) %>% filter(id != "2")

diff_d_r <- plot_country %>% filter(home_type2 != "Agricultural worker Household (HH)") %>%  group_by(cntryId) %>%  
  mutate(diff_val = percent[id == "2"] - percent) %>% filter(id != "2") %>%  rbind(diff_d_r_a) 


diff_d_r $title = "Rural"

#new test positivity difference plot 

diff_u <- diff_d_u %>%  dplyr::select(cntryId, diff_val, home_type2, title)
diff_r <- diff_d_r %>%  dplyr::select(cntryId, diff_val, home_type2, title)

all_diff <- rbind(diff_u, diff_r) %>% 
  mutate(cntryId =  str_replace_all(cntryId, c("BF" = "Burkina Faso 2010 Vs. 2021", 
                                               "BJ" = "Benin 2011 - 12 Vs. 2017 - 18",
                                               "CM" = "Cameroon 2011 Vs. 2018",
                                               "CI" = "Cote d'Ivoire 2011 - 12 Vs. 2021",
                                               "ML" = "Mali 2012 - 13 Vs. 2018",
                                               "MZ" = "Mozambique 2011 Vs. 2015",
                                               "TZ" = " Tanzania 2011 - 12 Vs. 2015 - 16"))) %>% 
  group_by(cntryId, title) 

p_dat <- all_diff%>% 
  summarise(start = range(diff_val)[1], end = range(diff_val)[2]) %>% 
  ungroup() 

p_dat$cntryId <- factor(p_dat$cntryId,levels=rev(unique(p_dat$cntryId)))

#diff plot

p_2b<-ggplot(p_dat, aes(x = start, y = cntryId, end))+
  geom_segment(aes(xend = end, yend = cntryId)) +
  
  geom_point(
    data = all_diff,
    aes(diff_val, cntryId, color = home_type2), 
    size = 4, alpha =0.7
  ) +
  
  scale_color_manual(name= "", values=c( "darkgoldenrod1", "darkviolet")) +
  scale_x_continuous(breaks = seq(-65, 20, by = 10))+
  theme_manuscript() +
  theme(legend.position = "bottom") +
  labs(y = "", x = "Percentage difference in malaria test positivity rates by home type") +
  facet_wrap(~title)

p_2b


#################################################################################
#difference among household type
###################################################################################

#urban
plot_u_df2<- urban_df %>% dplyr::select(country_year.x, home_type2, code_year, test_result) 



plot_overall = plot_u_df2 %>%  group_by(home_type2, test_result) %>%  summarise(value= n()) %>% mutate(percent = round(value/sum(value) *100, 0))
plot_overall$title = "Urban"


#plot by country 
plot_country = plot_u_df2 %>%  group_by(country_year.x, code_year, home_type2, test_result) %>%  summarise(value= n()) %>% mutate(percent = round(value/sum(value) *100, 0)) %>% 
  mutate(country_year.x = ifelse(country_year.x == "Congo Democratic Republic 2013 - 14", "DRC 2013 - 14",country_year.x),
         home_type2 = ifelse(home_type2 == 'A', 'Agricultural worker Household (HH)', "Non-Agricultural worker HH"))

df_u <- all_df %>% group_by(country_year.x) %>%  summarise(total2 = sum(total))

plot_country <- plot_country  %>%  left_join(df_u, by = "country_year.x")  %>% 
  mutate(plot_label = ifelse(test_result == "-ve", percent, NA), cntryId = stringr::str_extract(code_year, "^.{2}")) 



#diff figure 2 urban
plot_country = plot_country %>% filter(test_result == "+ve") %>% group_by(home_type2, cntryId) %>% mutate(id = as.character(row_number())) %>% 
  mutate(cntryId_id = paste(cntryId, id))

df1 <- plot_country %>% filter(home_type2 == 'Agricultural worker Household (HH)') %>% dplyr::select(country_year.x, home_type2, percent, cntryId)
df2 <- plot_country %>% filter(home_type2 != 'Agricultural worker Household (HH)') %>% dplyr::select(country_year.x, home_type2, percent, cntryId)

diff_d_u <- bind_cols(df1, df2) %>% mutate(diff_val = percent...3 - percent...7)

diff_d_u $title = "Urban"

#rural

plot_r_df2<- rural_df %>% dplyr::select(country_year.x, home_type2, code_year, test_result) 



plot_overall = plot_r_df2 %>%  group_by(home_type2, test_result) %>%  summarise(value= n()) %>% mutate(percent = round(value/sum(value) *100, 0))
plot_overall$title = "Rural"


#plot by country 
plot_country = plot_r_df2 %>%  group_by(country_year.x, code_year, home_type2, test_result) %>%  summarise(value= n()) %>% mutate(percent = round(value/sum(value) *100, 0)) %>% 
  mutate(country_year.x = ifelse(country_year.x == "Congo Democratic Republic 2013 - 14", "DRC 2013 - 14",country_year.x),
         home_type2 = ifelse(home_type2 == 'A', 'Agricultural worker Household (HH)', "Non-Agricultural worker HH"))

df_r <- all_df %>% group_by(country_year.x) %>%  summarise(total2 = sum(total))

plot_country <- plot_country  %>%  left_join(df_r, by = "country_year.x")  %>% 
  mutate(plot_label = ifelse(test_result == "-ve", percent, NA), cntryId = stringr::str_extract(code_year, "^.{2}")) 



#diff figure 2 urban
plot_country = plot_country %>% filter(test_result == "+ve") %>% group_by(home_type2, cntryId) %>% mutate(id = as.character(row_number())) %>% 
  mutate(cntryId_id = paste(cntryId, id))

df1 <- plot_country %>% filter(home_type2 == 'Agricultural worker Household (HH)') %>% dplyr::select(country_year.x, home_type2, percent, cntryId)
df2 <- plot_country %>% filter(home_type2 != 'Agricultural worker Household (HH)') %>% dplyr::select(country_year.x, home_type2, percent, cntryId)

diff_d_r <- bind_cols(df1, df2) %>% mutate(diff_val = percent...3 - percent...7)

diff_d_r$title = "Rural"


all_diff <- rbind(diff_d_u, diff_d_r) %>% 
  mutate(cntryId =  str_replace_all(cntryId...4, c("BF" = "Burkina Faso 2010 Vs. 2021", 
                                               "BJ" = "Benin 2011 - 12 Vs. 2017 - 18",
                                               "CM" = "Cameroon 2011 Vs. 2018",
                                               "CI" = "Cote d'Ivoire 2011 - 12 Vs. 2021",
                                               "ML" = "Mali 2012 - 13 Vs. 2018",
                                               "MZ" = "Mozambique 2011 Vs. 2015",
                                               "TZ" = "Tanzania 2011 - 12 Vs. 2015 - 16"))) %>%
  group_by(cntryId...4, title) %>% mutate(id = as.character(row_number())) %>% 
  mutate(id = ifelse(id == "1", "Preceding survey", "Most recent survey")) %>% group_by(cntryId, title) 


p_dat <- all_diff%>% 
  summarise(start = range(diff_val)[1], end = range(diff_val)[2]) %>% 
  ungroup() 

p_dat$cntryId <- factor(p_dat$cntryId,levels=rev(unique(p_dat$cntryId)))

#diff plot

p_figure_2_2 <-ggplot(p_dat, aes(x = start, y = cntryId, end))+
  geom_segment(aes(xend = end, yend = cntryId)) +
  
  geom_point(
    data = all_diff,
    aes(diff_val, cntryId, color = id), 
    size = 4, alpha =0.7
  ) +
  
  scale_color_manual(name= "", values=c( "darkgoldenrod1", "darkviolet")) +
  scale_x_continuous(breaks = seq(-65, 20, by = 10))+
  theme_manuscript() +
  theme(legend.position = "bottom") +
  labs(y = "", x = "Percentage difference in malaria test positivity rates
      between agricultural worker HH and non-agricultural worker HH over time") +
  facet_wrap(~factor(title, levels = c("Urban", "Rural")))

p_figure_2_2 

ggsave(paste0(FigDir,"/", Sys.Date(),"_Figure_2_2.png"),p_figure_2_2, width = 8.5, height= 3.5) 



##############################################################################
##combined difference
#################################################################################

#urban
plot_u_df2<- urban_df %>% dplyr::select(country_year.x, home_type2, code_year, test_result) 



plot_overall = plot_u_df2 %>%  group_by(code_year, test_result) %>%  summarise(value= n()) %>% 
  mutate(percent = round(value/sum(value) *100, 0)) %>%  filter(test_result == "+ve")

diff_u <- plot_overall %>% 
  mutate(cntryId = stringr::str_extract(code_year, "^.{2}")) %>% group_by(cntryId)  %>%
  mutate(id = as.character(row_number())) %>% 
  mutate(diff_val = percent[id == "2"] - percent) %>% filter(id != "2")

diff_u$title = "Urban"


#rural

plot_r_df2<- rural_df %>% dplyr::select(country_year.x, home_type2, code_year, test_result) 



plot_overall = plot_r_df2 %>%  group_by(code_year, test_result) %>%  summarise(value= n()) %>% 
  mutate(percent = round(value/sum(value) *100, 0)) %>%  filter(test_result == "+ve")

diff_r <- plot_overall %>% 
  mutate(cntryId = stringr::str_extract(code_year, "^.{2}")) %>% group_by(cntryId)  %>%
  mutate(id = as.character(row_number())) %>% 
  mutate(diff_val = percent[id == "2"] - percent) %>% filter(id != "2")

diff_r$title = "Rural"


all_diff <- rbind(diff_u, diff_r) %>% 
  mutate(cntryId =  str_replace_all(cntryId, c("BF" = "Burkina Faso 2010 Vs. 2021", 
                                                   "BJ" = "Benin 2011 - 12 Vs. 2017 - 18",
                                                   "CM" = "Cameroon 2011 Vs. 2018",
                                                   "CI" = "Cote d'Ivoire 2011 - 12 Vs. 2021",
                                                   "ML" = "Mali 2012 - 13 Vs. 2018",
                                                   "MZ" = "Mozambique 2011 Vs. 2015",
                                                   "TZ" = "Tanzania 2011 - 12 Vs. 2015 - 16"))) %>%
  group_by(cntryId) 


p_dat <- all_diff%>% 
  summarise(start = range(diff_val)[1], end = range(diff_val)[2]) %>% 
  ungroup() 

p_dat$cntryId <- factor(p_dat$cntryId,levels=rev(unique(p_dat$cntryId)))

#diff plot

p_2b<-ggplot(p_dat, aes(x = start, y = cntryId, end))+
  geom_segment(aes(xend = end, yend = cntryId)) +
  
  geom_point(
    data = all_diff,
    aes(diff_val, cntryId, color = title), 
    size = 4, alpha =0.7
  ) +
  
  scale_color_manual(name= "", values=c( "darkgoldenrod1", "darkviolet")) +
  scale_x_continuous(breaks = seq(-65, 20, by = 10))+
  theme_manuscript() +
  theme(legend.position = "bottom") +
  labs(y = "", x = "Percentage difference in malaria test positivity rates over time") 

p_2b + labs(title = "Overall difference in malaria passivity between the last and most recent survey with Agric data
")


###############################################
#column plots
####################################################

#urban
plot_u_df2<- urban_df %>% dplyr::select(country_year.x, home_type2, code_year, test_result) 



plot_overall = plot_u_df2 %>%  group_by(code_year, home_type2,test_result) %>%  summarise(value= n()) %>% 
  mutate(percent = round(value/sum(value) *100, 0)) %>%  filter(test_result == "+ve"  ) %>%
  mutate(cntryId = stringr::str_extract(code_year, "^.{2}")) %>% mutate(year = parse_number(code_year))


ggplot() + 
  geom_col(data = plot_overall, aes(x = year, y = percent, fill = home_type2)) + 
  facet_wrap(~cntryId + home_type2) + 
  labs(title = "Malaria positivity trends for countries with multiple surveys with agric data")  + 
  theme(legend.position = "none")
  
##############################################################################
#                              NET USE 
##############################################################################


#urban
plot_u_df2<- urban_df %>% dplyr::select(country_year.x, home_type2, code_year, u5_net_use) 



plot_overall = plot_u_df2 %>%  group_by(home_type2, u5_net_use) %>%  summarise(value= n()) %>% mutate(percent = round(value/sum(value) *100, 0))
plot_overall$title = "Urban"


#plot by country 
plot_country = plot_u_df2 %>%  group_by(country_year.x, code_year, home_type2, u5_net_use) %>%  summarise(value= n()) %>% mutate(percent = round(value/sum(value) *100, 0)) %>% 
  mutate(country_year.x = ifelse(country_year.x == "Congo Democratic Republic 2013 - 14", "DRC 2013 - 14",country_year.x),
         home_type2 = ifelse(home_type2 == 'A', 'Agricultural worker Household (HH)', "Non-Agricultural worker HH"))

df_u <- all_df %>% group_by(country_year.x) %>%  summarise(total2 = sum(total))

plot_country <- plot_country  %>%  left_join(df_u, by = "country_year.x")  %>% 
  mutate(plot_label = ifelse(u5_net_use == 0, percent, NA), cntryId = stringr::str_extract(code_year, "^.{2}")) 



#diff figure 2 urban
plot_country = plot_country %>% filter(u5_net_use == 1) %>% group_by(home_type2, cntryId) %>% mutate(id = as.character(row_number())) %>% 
  mutate(cntryId_id = paste(cntryId, id))

df1 <- plot_country %>% filter(home_type2 == 'Agricultural worker Household (HH)') %>% dplyr::select(country_year.x, home_type2, percent, cntryId)
df2 <- plot_country %>% filter(home_type2 != 'Agricultural worker Household (HH)') %>% dplyr::select(country_year.x, home_type2, percent, cntryId)

diff_d_u <- bind_cols(df1, df2) %>% mutate(diff_val = percent...3 - percent...7)

diff_d_u $title = "Urban"

#rural

plot_r_df2<- rural_df %>% dplyr::select(country_year.x, home_type2, code_year, u5_net_use) 



plot_overall = plot_r_df2 %>%  group_by(home_type2, u5_net_use) %>%  summarise(value= n()) %>% mutate(percent = round(value/sum(value) *100, 0))
plot_overall$title = "Rural"


#plot by country 
plot_country = plot_r_df2 %>%  group_by(country_year.x, code_year, home_type2, u5_net_use) %>%  summarise(value= n()) %>% mutate(percent = round(value/sum(value) *100, 0)) %>% 
  mutate(country_year.x = ifelse(country_year.x == "Congo Democratic Republic 2013 - 14", "DRC 2013 - 14",country_year.x),
         home_type2 = ifelse(home_type2 == 'A', 'Agricultural worker Household (HH)', "Non-Agricultural worker HH"))

df_r <- all_df %>% group_by(country_year.x) %>%  summarise(total2 = sum(total))

plot_country <- plot_country  %>%  left_join(df_r, by = "country_year.x")  %>% 
  mutate(plot_label = ifelse(u5_net_use == 0, percent, NA), cntryId = stringr::str_extract(code_year, "^.{2}")) 



#diff figure 2 urban
plot_country = plot_country %>% filter(u5_net_use == 1) %>% group_by(home_type2, cntryId) %>% mutate(id = as.character(row_number())) %>% 
  mutate(cntryId_id = paste(cntryId, id))

df1 <- plot_country %>% filter(home_type2 == 'Agricultural worker Household (HH)') %>% dplyr::select(country_year.x, home_type2, percent, cntryId)
df2 <- plot_country %>% filter(home_type2 != 'Agricultural worker Household (HH)') %>% dplyr::select(country_year.x, home_type2, percent, cntryId)

diff_d_r <- bind_cols(df1, df2) %>% mutate(diff_val = percent...3 - percent...7)

diff_d_r$title = "Rural"


all_diff <- rbind(diff_d_u, diff_d_r) %>% 
  mutate(cntryId =  str_replace_all(cntryId...4, c("BF" = "Burkina Faso 2010 Vs. 2021", 
                                                   "BJ" = "Benin 2011 - 12 Vs. 2017 - 18",
                                                   "CM" = "Cameroon 2011 Vs. 2018",
                                                   "CI" = "Cote d'Ivoire 2011 - 12 Vs. 2021",
                                                   "ML" = "Mali 2012 - 13 Vs. 2018",
                                                   "MZ" = "Mozambique 2011 Vs. 2015",
                                                   "TZ" = "Tanzania 2011 - 12 Vs. 2015 - 16"))) %>%
  group_by(cntryId...4, title) %>% mutate(id = as.character(row_number())) %>% 
  mutate(id = ifelse(id == "1", "Preceding survey", "Most recent survey")) %>% group_by(cntryId, title) 


p_dat <- all_diff%>% 
  summarise(start = range(diff_val)[1], end = range(diff_val)[2]) %>% 
  ungroup()

p_dat$cntryId <- factor(p_dat$cntryId,levels=rev(unique(p_dat$cntryId)))

#diff plot

p_figure_3_2 <-ggplot(p_dat, aes(x = start, y = cntryId, end))+
  geom_segment(aes(xend = end, yend = cntryId)) +
  
  geom_point(
    data = all_diff,
    aes(diff_val, cntryId, color = id), 
    size = 4, alpha =0.7
  ) +
  
  scale_color_manual(name= "", values=c( "darkorange", "darkolivegreen")) +
  scale_x_continuous(breaks = seq(-65, 20, by = 10))+
  theme_manuscript() +
  theme(legend.position = "bottom") +
  labs(y = "", x = "Percentage difference in net use
      between agricultural worker HH and non-agricultural worker HH over time") +
  facet_wrap(~factor(title, levels = c("Urban", "Rural")))

p_figure_3_2 

ggsave(paste0(FigDir,"/", Sys.Date(),"_Figure_3_2.png"),p_figure_3_2, width = 8.5, height= 3.5) 


#combined, positivity and net use. 

diffs <- p_figure_2_2 / p_figure_3_2 + plot_annotation(tag_levels = 'A')
diffs
ggsave(paste0(FigDir,"/", Sys.Date(),"_Figure_4.pdf"),diffs, width = 8.5, height= 7) 

