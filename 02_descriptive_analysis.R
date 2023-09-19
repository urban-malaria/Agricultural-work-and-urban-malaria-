#The purpose of this script is generate figure 1, 4 and 5 

rm(list = ls())

## -----------------------------------------
### Directories
## -----------------------------------------
user <- Sys.getenv("USERNAME")
if ("ido0493" %in% user) {
  user_path <- file.path("C:/Users", "ido0493.FSM")
  DriveDir <- file.path(user_path, "OneDrive - Northwestern University", "urban_malaria")
  PopDir <- file.path(DriveDir, "data", "Urban_malaria_net_ownership_data")
  ManDir <- file.path(DriveDir, "projects", "Manuscripts", "agriculture_malaria_manuscript")
  FigDir <- file.path(ManDir, "figures", "220623_new_figures")
} else if  ("Chilo Chiziba" %in% user) {
  Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
  DriveDir <- file.path(Drive, 'OneDrive - Northwestern University', 'urban_malaria')
  ManDir <- file.path(DriveDir, "projects", "Manuscripts", "agriculture_malaria_manuscript")
} else {
  Drive <- file.path(gsub("[\\]", "/", gsub("Documents", "", Sys.getenv("HOME"))))
  #DriveDir <- file.path(Drive, '', 'Northwestern University', 'Ifeoma Doreen Ozodiegwu - urban_malaria')
  DriveDir <- file.path(Drive,  "OneDrive - Northwestern University", "urban_malaria")
  PopDir <- file.path(DriveDir, "data", 'Urban_malaria_net_ownership_data')
  ManDir <- file.path(DriveDir, "projects", "Manuscripts", "agriculture_malaria_manuscript")
  FigDir <- file.path(ManDir, "figures", "220623_new_figures")
  SupFigDir <- file.path(ManDir, "figures", "sup_figures")
  
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

urban_df <- read_csv(file.path("analysis_dat/urban_df_for_analysis.csv")) %>%  mutate(type ="urban_data") 
rural_df <- read_csv(file.path("analysis_dat/rural_df_for_analysis.csv")) %>%  mutate(type ="rural_data")


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
                                                                          ifelse(country_year.x == "Cameroon 2018", "Cameroon 2018 - 19",country_year.x))) %>% 
  as_survey_design(ids= id,strata=strat,nest=T,weights= wt)%>% 
 group_by(country_year.x, type) %>%  summarize(total = round(survey_total(),0)) %>%  ungroup() 


df <- all_df%>% 
  group_by(country_year.x) %>%  summarise(percent =  round(total/sum(total) * 100, 0))
all <- cbind(all_df, df) %>%select(-c("country_year.x")) 



p1 <- ggplot(all, aes(x = reorder(country_year.x, -total), y = total, fill =type, label = total)) +
  geom_bar( stat = "identity", alpha = 0.8) +
  scale_fill_manual(name = "", label = c("Rural", "Urban"), values = c("bisque", "darkorchid"))+
  #geom_text(aes(label = paste0(percent, "%"), y = percent),
           # position = position_stack(vjust = 0.5),
            #color = "white") +
  coord_flip()+
  theme_manuscript()+
  labs(x = "", y = "Number of children, 6 - 59 months tested for malaria 
       by RDT or microscopy in urban and rural clusters, combined")

#figure 1b
df <- rbind(urban_df, rural_df)

#quick chi-squared test 
table(df$home_type2, df$interview_month)
chisq.test(df$home_type2, df$interview_month)

svyd_df <- svydesign.fun(df)
table_df <- svytable(~home_type2 + interview_month, svyd_df)%>% as.data.frame() %>% 
  mutate(interview_month = as.numeric(interview_month))

table_df <- table_df %>%  mutate(home_type2 = ifelse(home_type2 == 'A', 'Agricultural worker Household (HH)', "Non-Agricultural worker HH")) %>% 
  group_by(home_type2) %>%  mutate(percent = round(Freq/sum(Freq)*100), 0)%>%  ungroup()

p2 <- table_df %>% ggplot(aes(x = interview_month, y = percent, label =percent)) +
  geom_col(fill = "forestgreen", alpha=0.8) +
  geom_smooth(se = FALSE, color = "goldenrod2")+
  # geom_text(position = position_stack(vjust = 0.5),
  #           color = "black") +
  facet_wrap(vars(home_type2)) + 
  theme_manuscript()+
  labs(y = "Percentage of children, 6 - 59 months 
       tested for malaria in urban and rural clusters", x = "Interview month") +
  #theme(strip.background = element_blank(), strip.text.x = element_blank())+
  scale_x_continuous(breaks = scales::pretty_breaks(n=12), expand = expansion(mult = c(0.02, 0.02)))

p <- p1/p2

#ggsave(paste0(FigDir,"/", Sys.Date(),"available_data_distribution_urban_rural_no_numbers.pdf"), p, width = 8, height = 7) 


#figure 2
#overall
color = c("#f2a5a1", "#c55c80")
plot_over <- df %>%  dplyr::select(country_year.x, home_type2, code_year, test_result)
plot_over2 = plot_over %>%  group_by(home_type2, test_result) %>%  summarise(value= n()) %>% mutate(percent = round(value/sum(value) *100, 0))

p<-ggplot(plot_over2, aes(fill=test_result, x= home_type2)) + 
  geom_bar(aes(y = value), position="stack", stat = "identity")+
  theme_manuscript()+
  scale_x_discrete(labels = c("Agricultural worker \n household (HH)", "Non-agricultural \n worker HH"))+
  scale_fill_manual(name = "Malaria test result", labels= c("Negative", "positive"), values= color)+
  geom_text(aes(label = paste0(value, " ", "(", percent, "%", ")"), y = value),
            position = position_stack(vjust = 0.5),
            color = "white") +
  labs(x = "", y  = "Number of children, 6 - 59 months,
  tested positive for malaria in 22 DHS datasets") +
  theme(strip.text.x = element_text(
    size = 12, color = "black")) 

#ggsave(paste0(FigDir,"/", Sys.Date(),"malaria_prevalence_by agric_exposure_all_countries.pdf"), p, width = 8.5, height = 6) 



#urban
plot_u_df2<- urban_df %>% dplyr::select(country_year.x, home_type2, code_year, test_result) 



plot_overall = plot_u_df2 %>%  group_by(home_type2, test_result) %>%  summarise(value= n()) %>% mutate(percent = round(value/sum(value) *100, 0))
plot_overall$title = "Urban"

p_urban<-ggplot(plot_overall, aes(fill=test_result, x= home_type2)) + 
  geom_bar(aes(y = value), position="stack", stat = "identity")+
  theme_manuscript()+
  scale_x_discrete(labels = c("Agricultural worker \n household (HH)", "Non-agricultural \n worker HH"))+
  scale_fill_manual(name = "Malaria test result", labels= c("Negative", "positive"), values= color)+
  geom_text(aes(label = paste0(value, " ", "(", percent, "%", ")"), y = value),
            position = position_stack(vjust = 0.5),
            color = "white") +
  labs(x = "", y  = "Number of children, 6 - 59 months,
  tested positive for malaria in 22 DHS datasets") +
  facet_wrap(vars(title))+
  theme(strip.text.x = element_text(
    size = 12, color = "black")) +
  coord_cartesian(ylim = c(0, 44000))


#plot by country 
plot_country = plot_u_df2 %>%  group_by(country_year.x,home_type2, test_result) %>%  summarise(value= n()) %>% mutate(percent = round(value/sum(value) *100, 0)) %>% 
  mutate(country_year.x = ifelse(country_year.x == "Congo Democratic Republic 2013 - 14", "DRC 2013 - 14",country_year.x),
         home_type2 = ifelse(home_type2 == 'A', 'Agricultural worker Household (HH)', "Non-Agricultural worker HH"))

df_u <- all_df %>% group_by(country_year.x) %>%  summarise(total2 = sum(total))

plot_country <- plot_country  %>%  left_join(df_u, by = "country_year.x") 

#supplement figure
p<-ggplot(plot_country , aes(x = reorder(country_year.x, -total2), y = percent, fill = test_result)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  coord_flip() +
  scale_fill_manual(name = "Malaria test result", labels= c("Negative", "positive"), values= color)+
  facet_grid(. ~ home_type2, scales = "free", space = "free") +
  theme(legend.title = element_blank()) +
  labs(x = "", y = "Percentage of children, 6 - 59 months 
       tested for malaria in urban clusters per country")+
  theme_manuscript()+
  theme(legend.position = "bottom")
#ggsave(paste0(FigDir,"/", Sys.Date(),"malaria_prevalence_by agric_exposure_urban_by_country.pdf"), p, width = 8.5, height = 6) 



#diff figure 2 urban
plot_country = plot_country %>% filter(test_result == "+ve")
diff_d_u <- plot_country %>% group_by(country_year.x) %>%  mutate(diff_val = percent[home_type2 == "Agricultural worker Household (HH)"] - percent) %>% 
  filter(home_type2 == "Non-Agricultural worker HH") 

diff_d_u$title = "Urban"
p_diff_u<-ggplot(diff_d_u , aes(x = reorder(country_year.x, -diff_val), y = diff_val, fill)) +
  geom_bar(stat = "identity",  width = 0.7, fill = "#c55c80") +
  geom_text(aes(label= diff_val), position = position_stack(vjust = 0.5),
            color = "black") +
  coord_flip() +
  theme(legend.title = element_blank()) +
  labs(x = "", y = "Percentage difference in malaria test positivity rates
      between agricultural worker HH and non-agricultural worker HH")+
  facet_wrap(vars(title))+
  theme_manuscript()+
  theme(legend.position = "bottom") + 
  ylim(0, 28)



#rural

plot_u_df<- rural_df %>% dplyr::select(country_year.x, home_type2, code_year, test_result) 

plot_overall = plot_u_df %>%  group_by(home_type2, test_result) %>%  summarise(value= n()) %>% mutate(percent = round(value/sum(value) *100, 0))
plot_overall$title = "Rural"
p_rural<-ggplot(plot_overall, aes(fill=test_result, x= home_type2)) + 
  geom_bar(aes(y = value), position="stack", stat = "identity")+
  theme_manuscript()+
  scale_x_discrete(labels = c("Agricultural worker \n household (HH)", "Non-agricultural \n worker HH"))+
  scale_fill_manual(name = "Malaria test result", labels= c("Negative", "positive"), values= color)+
  geom_text(aes(label = paste0(value, " ", "(", percent, "%", ")"), y = value),
            position = position_stack(vjust = 0.5),
            color = "white") +
  labs(x = "", y  = "") +
  facet_wrap(vars(title))+
  theme(strip.text.x = element_text(
    size = 12, color = "black")) +
  coord_cartesian(ylim = c(0, 44000)) + 
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.y = element_blank())


p_malaria = p_urban +p_rural+ plot_layout(guides = "collect") & theme(legend.position = 'bottom')
p_malaria
#ggsave(paste0(FigDir,"/", Sys.Date(),"malaria_prevalence_HH_occupation_exposure_urban_rural.pdf"), p_malaria, width = 6.8, height = 5)


#rural by country figure for supplement 
plot_country = plot_u_df %>%  group_by(country_year.x,home_type2, test_result) %>%  summarise(value= n()) %>% mutate(percent = round(value/sum(value) *100, 0)) %>% 
  mutate(country_year.x = ifelse(country_year.x == "Congo Democratic Republic 2013 - 14", "DRC 2013 - 14",country_year.x),
         home_type2 = ifelse(home_type2 == 'A', 'Agricultural worker Household (HH)', "Non-Agricultural worker HH")) %>% 
  mutate(country_year.x = ifelse(country_year.x == "Cameroon 2018", "Cameroon 2018 - 19",country_year.x))

df_u <- all_df %>% group_by(country_year.x) %>%  summarise(total2 = sum(total))

plot_country <- plot_country  %>%  left_join(df_u, by = "country_year.x") 

p<-ggplot(plot_country , aes(x = reorder(country_year.x, -total2), y = percent, fill = test_result)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  coord_flip() +
  scale_fill_manual(name = "Malaria test result", labels= c("Negative", "positive"), values= color)+
  facet_grid(. ~ home_type2, scales = "free", space = "free") +
  theme(legend.title = element_blank()) +
  labs(x = "", y = "Percentage of children, 6 - 59 months 
       tested for malaria in rural clusters per country")+
  theme_manuscript()+
  theme(legend.position = "bottom")

#ggsave(paste0(FigDir,"/", Sys.Date(),"malaria_prevalence_by agric_exposure_rural_by_country.pdf"), p, width = 8.5, height= 6) 



#diff figure 2 rural
plot_country = plot_country %>% filter(test_result == "+ve")
diff_d_r <- plot_country %>% group_by(country_year.x) %>%  mutate(diff_val = percent[home_type2 == "Agricultural worker Household (HH)"] - percent) %>% 
  filter(home_type2 == "Non-Agricultural worker HH")
diff_d_r$title = "Rural"
p_diff_r<-ggplot(diff_d_r , aes(x = reorder(country_year.x, -diff_val), y = diff_val, fill)) +
  geom_bar(stat = "identity",  width = 0.7, fill = "#c55c80") +
  geom_text(aes(label= diff_val), position = position_stack(vjust = 0.5),
                      color = "black") +
  coord_flip() +
  theme(legend.title = element_blank()) +
  labs(x = "", y = "Percentage difference in malaria test positivity rates
      between agricultural worker HH and non-agricultural worker HH")+
  facet_wrap(vars(title))+
  theme_manuscript()+
  theme(legend.position = "bottom") +
  ylim(-2, 28)


all_diff_p <- p_diff_u + p_diff_r

ggsave(paste0(FigDir,"/", Sys.Date(),"malaria_prevalence_diff_rural_urban_by_country.png"),all_diff_p, width = 8.5, height= 5) 

#new test positivity difference plot 

diff_u <- diff_d_u %>%  select(country_year.x, diff_val, title)
diff_r <- diff_d_r %>%  select(country_year.x, diff_val, title)

all_diff <- rbind(diff_u, diff_r) %>%  group_by(country_year.x) 

p_dat <- all_diff%>% 
  summarise(start = range(diff_val)[1], end = range(diff_val)[2]) %>% 
  ungroup()


p<-ggplot(p_dat, aes(x = start, y = reorder(country_year.x, end)))+
  geom_segment(aes(xend = end, yend = country_year.x)) +
  
  geom_point(
    data = all_diff,
    aes(diff_val, country_year.x, color = title), 
    size = 4, alpha =0.7
  ) +
  
  scale_color_manual(name= "", values=c( "darkgoldenrod1", "darkviolet")) +
  scale_x_continuous(breaks = seq(-2, 28, by = 2))+
  theme_manuscript() +
  labs(y = "", x = "Percentage difference in malaria test positivity rates
      between agricultural worker HH and non-agricultural worker HH")

ggsave(paste0(FigDir,"/", Sys.Date(),"_new_malaria_prevalence_diff_rural__by_country.pdf"),p, width = 8.5, height= 5) 
#figure 3

#net use vs occupation category 
color = c( "#621244", "#efeddb")
plot_urban = urban_df %>%  
  mutate(net_use = ifelse(u5_net_use == 1, "Slept under \n a net", "Did not sleep \n under a net")) %>% 
  group_by(home_type2, net_use) %>%  
  summarise(value= n()) %>% mutate(percent = round(value/sum(value) *100, 0))
plot_urban$title = "Urban"

p<-ggplot(plot_urban, aes(fill=net_use, x= home_type2)) + 
  geom_bar(aes(y = value), position="stack", stat = "identity")+
  theme_manuscript()+
  scale_x_discrete(labels = c("Agricultural worker \n household (HH)", "Non-agricultural \n worker HH"))+
  scale_fill_manual(name = "", values= color)+
  geom_text(aes(label = paste0(value, " ", "(", percent, "%", ")"), y = value),
            position = position_stack(vjust = 0.5),
            color = "white") +
  labs(x = "", y  = "Number of children, 6 - 59 months,
  tested positive for malaria in 22 DHS datasets") +
  facet_wrap(vars(title))+
  theme(strip.text.x = element_text(
    size = 12, color = "black")) +
  coord_cartesian(ylim = c(0, 44000))



plot_rural = rural_df %>% 
  mutate(net_use = ifelse(u5_net_use == 1, "Slept under \n a net", "Did not sleep \n under a net")) %>% 
  group_by(home_type2, net_use) %>%  
  summarise(value= n()) %>% mutate(percent = round(value/sum(value) *100, 0))
plot_rural$title = "Rural"

p1<-ggplot(plot_rural, aes(fill=net_use, x= home_type2)) + 
  geom_bar(aes(y = value), position="stack", stat = "identity")+
  theme_manuscript()+
  scale_x_discrete(labels = c("Agricultural worker \n household (HH)", "Non-agricultural \n worker HH"))+
  scale_fill_manual(name = "", values= color)+
  geom_text(aes(label = paste0(value, " ", "(", percent, "%", ")"), y = value),
            position = position_stack(vjust = 0.5),
            color = "white") +
  labs(x = "", y  = "Number of children, 6 - 59 months,
  tested positive for malaria in 22 DHS datasets") +
  facet_wrap(vars(title))+
  theme(strip.text.x = element_text(
    size = 12, color = "black")) +
  coord_cartesian(ylim = c(0, 44000)) + 
  theme(axis.text.y = element_blank(), 
        axis.ticks.y = element_blank(), 
        axis.title.y = element_blank())

p + p1
p_net = p + p1 + plot_layout(guides = "collect") & theme(legend.position = 'bottom')
p_net

ggsave(paste0(FigDir,"/", Sys.Date(),"netuse_agric_rural_urban.pdf"),p_net, width = 7.5, height= 5) 


#urban 

plot_country = urban_df %>%  group_by(country_year.x,home_type2, u5_net_use) %>%  summarise(value= n()) %>% mutate(percent = round(value/sum(value) *100, 0)) %>% 
  mutate(country_year.x = ifelse(country_year.x == "Congo Democratic Republic 2013 - 14", "DRC 2013 - 14",country_year.x),
         home_type2 = ifelse(home_type2 == 'A', 'Agricultural worker Household (HH)', "Non-Agricultural worker HH"))

df_u <- all_df %>% group_by(country_year.x) %>%  summarise(total2 = sum(total))

plot_country <- plot_country  %>%  left_join(df_u, by = "country_year.x") 

#plot by country urban 

#supplement figure
plot_country = plot_country %>%  
  mutate(net_use = ifelse(u5_net_use == 1, "Slept under \n a net", "Did not sleep \n under a net"))
p<-ggplot(plot_country , aes(x = reorder(country_year.x, -total2), y = percent, fill = net_use)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  coord_flip() +
  scale_fill_manual(name = "",  values= color)+
  facet_grid(. ~ home_type2, scales = "free", space = "free") +
  theme(legend.title = element_blank()) +
  labs(x = "", y = "Percent of children, 6 - 59 months \n tested for malaria in urban clusters per country")+
  theme_manuscript()+
  theme(legend.position = "bottom")
ggsave(paste0(FigDir,"/", Sys.Date(),"net_use_by agric_exposure_urban_by_country.pdf"), p, width = 8.5, height = 6) 



#diff 
plot_country = plot_country %>% filter(u5_net_use == 1)
diff_d_u <- plot_country %>% group_by(country_year.x) %>%  mutate(diff_val = percent[home_type2== "Agricultural worker Household (HH)"] - percent) %>% 
  filter(home_type2 == "Non-Agricultural worker HH") 

diff_d_u$title = "Urban"
p_diff_u<-ggplot(diff_d_u, aes(x = reorder(country_year.x, -diff_val), y = diff_val, fill)) +
  geom_bar(stat = "identity",  width = 0.7, fill = "#c55c80") +
  geom_text(aes(label= diff_val), position = position_stack(vjust = 0.5),
            color = "black") +
  coord_flip() +
  theme(legend.title = element_blank()) +
  labs(x = "", y = "Percentage difference in net use
      between agricultural worker HH and non-agricultural worker HH")+
  facet_wrap(vars(title))+
  theme_manuscript()+
  theme(legend.position = "bottom") 



#rural 

plot_country = rural_df %>%  group_by(country_year.x,home_type2, u5_net_use) %>%  summarise(value= n()) %>% mutate(percent = round(value/sum(value) *100, 0)) %>% 
  mutate(country_year.x = ifelse(country_year.x == "Congo Democratic Republic 2013 - 14", "DRC 2013 - 14",country_year.x),
         home_type2 = ifelse(home_type2 == 'A', 'Agricultural worker Household (HH)', "Non-Agricultural worker HH")) %>% 
  mutate(country_year.x = ifelse(country_year.x == "Cameroon 2018", "Cameroon 2018 - 19",country_year.x))


df_u <- all_df %>% group_by(country_year.x) %>%  summarise(total2 = sum(total))

plot_country <- plot_country  %>%  left_join(df_u, by = "country_year.x") 


#plot by country rural

#supplement figure
plot_country = plot_country %>%  
  mutate(net_use = ifelse(u5_net_use == 1, "Slept under \n a net", "Did not sleep \n under a net"))
p<-ggplot(plot_country , aes(x = reorder(country_year.x, -total2), y = percent, fill = net_use)) +
  geom_bar(stat = "identity", position = "stack", width = 0.7) +
  coord_flip() +
  scale_fill_manual(name = "",  values= color)+
  facet_grid(. ~ home_type2, scales = "free", space = "free") +
  theme(legend.title = element_blank()) +
  labs(x = "", y = "Percent of children, 6 - 59 months \n tested for malaria in rural clusters per country")+
  theme_manuscript()+
  theme(legend.position = "bottom")
ggsave(paste0(FigDir,"/", Sys.Date(),"net_use_by agric_exposure_rural_by_country.pdf"), p, width = 8.5, height = 6) 

#diff 

plot_country = plot_country %>% filter(u5_net_use == 1)
diff_d_r <- plot_country %>% group_by(country_year.x) %>%  mutate(diff_val = percent[home_type2== "Agricultural worker Household (HH)"] - percent) %>% 
  filter(home_type2 == "Non-Agricultural worker HH") 

diff_d_r$title = "Rural"
p_diff_r<-ggplot(diff_d_r , aes(x = reorder(country_year.x, -diff_val), y = diff_val, fill)) +
  geom_bar(stat = "identity",  width = 0.7, fill = "#c55c80") +
  geom_text(aes(label= diff_val), position = position_stack(vjust = 0.5),
            color = "black") +
  coord_flip() +
  theme(legend.title = element_blank()) +
  facet_wrap(vars(title))+
  theme_manuscript()+
  labs(x = "", y = "Percentage difference in net use
      between agricultural worker HH and non-agricultural worker HH")+
  theme(legend.position = "bottom") 

all_diff_p <- p_diff_u + p_diff_r

ggsave(paste0(FigDir,"/", Sys.Date(),"net_use_diff_rural_urban_by_country.pdf"),all_diff_p, width = 8.5, height= 5) 


#new net difference plot 

diff_u <- diff_d_u %>%  select(country_year.x, diff_val, title)
diff_r <- diff_d_r %>%  select(country_year.x, diff_val, title)

all_diff <- rbind(diff_u, diff_r) %>%  group_by(country_year.x) 

p_dat <- all_diff%>% 
  summarise(start = range(diff_val)[1], end = range(diff_val)[2]) %>% 
  ungroup()


p<-ggplot(p_dat, aes(x = start, y = reorder(country_year.x, end)))+
  geom_segment(aes(xend = end, yend = country_year.x)) +
  
  geom_point(
    data = all_diff,
    aes(diff_val, country_year.x, color = title), 
    size = 4, alpha =0.7
  ) +
  
  scale_color_manual(name= "", values=c( "darkorange", "darkolivegreen")) +
  scale_x_continuous(breaks = seq(-15, 10, by = 3))+
  theme_manuscript() +
  labs(y = "", x = "Percentage difference in net use 
      between agricultural worker HH and non-agricultural worker HH")

ggsave(paste0(FigDir,"/", Sys.Date(),"_new_net_use_diff_rural__by_country.pdf"),p, width = 8.5, height= 5) 


############################################################################
#does agricultural worker HHs have greater diarrheal disease burden? 24 vs 20%, not much of a difference there
############################################################################

df <- urban_df %>% mutate(diarrhea_grp = ifelse(total_diarrhea >= 1, "Had diarrhea", "No diarrhea")) %>% 
  select(home_type2, diarrhea_grp) %>% drop_na() %>% 
  group_by(home_type2, diarrhea_grp) %>%   summarise(value= n()) %>% 
  mutate(percent = round(value/sum(value) *100, 0)) %>%  ungroup() %>%  drop_na(diarrhea_grp)


ggplot(df3, aes(fill=diarrhea_grp, x= home_type2)) + 
  geom_bar(aes(y = value), position="stack", stat = "identity")+
  #scale_fill_manual(name = "", label = c(" House was sprayed", "House was not sprayed"), values = c("#d391fa", "#3e00b3"))+
  theme_manuscript() +
  geom_text(aes(label = paste0(value, " ", "(", percent, "%", ")"), y = value),
            position = position_stack(vjust = 0.5),
            color = "white")+ 
  labs(x = "")+
  scale_x_discrete(labels = c("Agricultural worker \n household (HH)", "Non-agricultural \n worker HH"))
