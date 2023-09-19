#The purpose of this script is to generate figures for the 

rm(list = ls())

## -----------------------------------------
### Directories
## -----------------------------------------
user <- Sys.getenv("USERNAME")
if ("ido0493" %in% user) {
  user_path <- file.path("C:/Users", user)
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


rural_df <- read_csv(file.path("analysis_dat/urban_df_for_analysis.csv")) %>%  mutate(type ="rural_data")



# urban_df <- read_csv(file.path(ManDir, "csv", "urban_df_for_analysis1.csv")) %>%  mutate(type ="urban_data") %>% 
#   left_join(ids, by = "CountryName") %>% mutate(country_year = str_replace(country_year, 'Congo Democratic Republic','DRC'))
# rural_df <- read_csv(file.path(ManDir, "csv", "rural_df_for_analysis1.csv")) %>%  mutate(type ="rural_data") %>% 
#   left_join(ids, by = "CountryName") %>% mutate(country_year=str_replace(country_year, 'Congo Democratic Republic','DRC'))


combined_data = rbind(urban_df, rural_df)
## -------------------------------
### descriptive statistics 
## -------------------------------
# sum(is.na(combined_data$test_result))
# df <- combined_data %>%group_by(test_result, type) %>%summarise(Num_child = n()) %>% 
#   mutate(test_result = ifelse(is.na(test_result), "NA", test_result))
# sp0 <- ggplot(df %>% filter(test_result!= "NA"), aes(fill=type, x=reorder(test_result,Num_child), y =Num_child), label = Num_child) + 
#   geom_bar(position="stack", stat="identity")+
#   geom_text( aes(label = Num_child), color = "black", size = 4, position = position_stack(vjust=0.5)) +
#   theme_manuscript()+
#   theme(legend.title = element_blank()) +
#   scale_fill_manual(name="", values = c("deeppink3", "darkslategray2"), label = c("rural", "urban")) +
#   labs(title= "Rural and urban",
#        x="Malaria Test Results by Microscopy or RDT in 22 DHS datsets" , y = "Fraction of children that tested positive for malaria")
# sp0
#ggsave(paste0(SupFigDir,"/", Sys.Date(), "_all_22_data_malaria_total.pdf"), sp0, width = 7, height = 3.5)

# df <- combined_data %>%group_by(test_result, type, country_year.x) %>%summarise(Num_child = n()) %>% 
#   mutate(test_result = ifelse(is.na(test_result), "NA", test_result))
# sp1 <- ggplot(df, aes(fill=type, x=reorder(test_result,Num_child), y =Num_child), label = Num_child) + 
#   geom_col(aes(fill = type))+
#   coord_flip()+
#   scale_fill_manual(values = c("deeppink3", "darkslategray2")) +
#   geom_text( aes(label = Num_child),hjust =0, size = 4,inherit.aes = TRUE) +
#   theme_manuscript()+
#   theme(legend.title = element_blank()) +
#   facet_wrap(vars(country_year.x))+
#   theme(legend.position = "bottom")+
#   labs(title= "",
#        x="Malaria Test Results by Microscopy or RDT" , y = "Fraction of children that tested positive for malaria")
# sp1
#ggsave(paste0(SupFigDir,"/", Sys.Date(),"all_data_malaria_urban_by_country.png"), sp1, width = 13, height = 13)

####################
#Country level totals - updated

ur_ru_list <- split(df, with(df, interaction(type)), drop = TRUE)
neg_pos_list_1 <- split(ur_ru_list[[1]], with(ur_ru_list[[1]], interaction(test_result)), drop = TRUE) %>%
  purrr::map(~rename_all(., paste0, "_ru"))

neg_pos_list_2 <- split(ur_ru_list[[2]], with(ur_ru_list[[2]], interaction(test_result)), drop = TRUE) %>%
  purrr::map(~rename_all(., paste0, "_ur"))

df_rur_children <- bind_cols(neg_pos_list_1) %>%  dplyr::select(country_year.x_ru...3, contains("child"))%>%
  rename_at(1,~"country") %>% rename_at(2,~"ru_neg") %>% rename_at(3,~"ru_pos") %>% 
  dplyr::select(country, ru_pos, ru_neg) %>% reshape2::melt(id.vars = "country")  


df_urb_children <- bind_cols(neg_pos_list_2) %>%  dplyr::select(country_year.x_ur...3, contains("child"))%>%
  rename_at(1,~"country") %>% rename_at(2,~"ur_neg") %>% rename_at(3,~"ur_pos") %>%  
  dplyr::select(country,ur_pos, ur_neg) %>% reshape2::melt(id.vars = "country")  

p_fun <- function(dataframe, fill_stack, y_lab){
  ggplot(dataframe, aes(x=reorder(country, value),y=value))+
    geom_col(position= fill_stack, aes(fill = variable))+
    scale_fill_manual(name="", values=c("aquamarine3", "deepskyblue4"), label = c("+ve", "-ve")) +
    coord_flip()+
    scale_y_continuous(expand=c(0,0))+
    labs(x = "Country", y= y_lab) +
    theme(axis.text.y=element_blank(),axis.line.y = element_blank(),
          axis.ticks.y = element_blank())+theme_classic2() +
    theme_manuscript()
}

# urban malaria 
sp2 <- p_fun(df_urb_children %>% filter(variable != "ur_na"), "stack", "Number of children") / 
  p_fun(df_urb_children %>% filter(variable != "ur_na"), "fill", "Proportion") + 
  plot_annotation(tag_levels = 'A', title = "Urban - Malaria Test Results by Microscopy or RDT in 22 DHS datsets",
                  theme = theme(plot.title = element_text(hjust = 0.5,face = 'bold', size = 14)))
sp2
#ggsave(paste0(SupFigDir,"/", Sys.Date(), "_all_22_data_malaria_urban.pdf"), sp2, width = 8, height = 8)

# rural malaria 
sp3 <- p_fun(df_rur_children %>% filter(variable != "ru_na"), "stack", "Number of children") / 
  p_fun(df_rur_children %>% filter(variable != "ru_na"), "fill", "Proportion") + 
  plot_annotation(tag_levels = 'A', title = "Rural - Malaria Test Results by Microscopy or RDT in 22 DHS datsets",
                  theme = theme(plot.title = element_text(hjust = 0.5,face = 'bold', size = 14)))
sp3
#ggsave(paste0(SupFigDir,"/", Sys.Date(), "_all_22_data_malaria_rural.pdf"), sp3, width = 8, height = 8)

#both urban and rural
color = c("deeppink3", "darkslategray2")
p_urb <- bar_fun(urban_df %>% drop_na(test_result), "test_result", "test_result", "Urban", "") +
  ylab("Number of children") + scale_fill_manual(values = color) + ylim(0,55000)
p_rur <- bar_fun(rural_df %>% drop_na(test_result), "test_result", "test_result", "Rural", "") +
  ylab("") + scale_fill_manual(values = color) + ylim(0,55000)

sp4 <- p_urb + p_rur + plot_annotation(tag_levels = 'A', caption = "Malaria Test Results by Microscopy or RDT in 22 DHS datsets",
                                       theme = theme(plot.title = element_text(hjust = 0.5,face = 'bold', size = 14), plot.caption = element_text(hjust = 0.5, size = 12)))& 
  theme(plot.tag = element_text(face = 'bold', size = 11, color = "black"))
sp4
#ggsave(paste0(SupFigDir,"/", Sys.Date(), "_all_22_data_malaria_both.pdf"), sp4, width = 7, height = 3.5)

#plot function for occupation data
p_total_fun <- function(df, main_title){
  p_occ <- ggplot(df, aes(x = home_type2, fill=home_type2))+
    geom_bar()+ ylim(0, 60000)+
    theme_manuscript()+
    geom_bar_text(stat = 'count', aes(label =..count..), vjust=0.5, size =4 * ggplot2::.pt, 
                  min.size = 4 * ggplot2::.pt,
                  padding.x = grid::unit(0, "pt"),
                  padding.y = grid::unit(0, "pt"),
                  outside = TRUE )+
    theme(legend.position = "none")+
    labs(title = main_title, x= "")
}

sp5 <- p_total_fun(urban_df, "Urban occupation - test totals") + p_total_fun(rural_df, "Rural occupation - test totals") +
  plot_annotation(tag_levels = 'A', caption = "Occupation (A - at least one member of the household is engaged in agricultural work, 
       Other - No member of the household is engaged in agricultural work)",
                  theme = theme(plot.title = element_text(hjust = 0.5,face = 'bold', size = 14), plot.caption = element_text(hjust = 0.5, size = 12)))& 
  theme(plot.tag = element_text(face = 'bold', size = 11, color = "black"))
sp5
#ggsave(paste0(SupFigDir,"/", Sys.Date(), "__occupation_totals.pdf"), sp5, width = 7, height = 3.5)

p5_prop_fun <- function(df){
  svyd_df <- svydesign.fun(df)
  p_data_bar <- svytable(~home_type2+test_result, svyd_df)%>% as.data.frame()
  sum_a <- fsum(fsubset(p_data_bar, home_type2 == "A")$Freq)
  sum_other <- fsum(fsubset(p_data_bar, home_type2 == "Other")$Freq)
  p_data_bar <- p_data_bar %>% mutate(percent = ifelse(home_type2 == "A", Freq/sum_a, Freq/sum_other))
  
  p5 <- ggplot(p_data_bar, aes(fill=test_result, y=percent, x=home_type2, label=scales::percent(percent, vjust=3))) + 
    geom_col(width=0.5)+
    #scale_fill_viridis(discrete = T) +
    scale_y_continuous(labels = function(x) format(round(x*100, 0), digits=2, nsmall=2),
                       breaks = scales::pretty_breaks(n = 5))+
    theme_manuscript()+
    scale_fill_manual(name="", values=c("cyan3", "blue"))+
    theme(legend.position = "none") +
    geom_text(nudge_y= -.01,
              color="white",
              size = 4,
              fontface="bold")+
    labs(x= "", y = "")
}

p5_stacked_fun <- function(df,  y_lim){
  svyd_df <- svydesign.fun(df)
  p_data_bar <- svytable(~home_type2+test_result, svyd_df)%>% as.data.frame()
  
  p5 <- ggplot(p_data_bar, aes(fill=test_result, y=Freq, x=home_type2)) + 
    geom_bar(position="stack", stat="identity", alpha = 0.8, width=0.5) +
    scale_fill_viridis(discrete = T) +
    theme_manuscript() +
    theme(legend.position = "none") +
    geom_text(aes(label=round(Freq, 0), vjust=1, nudge_y = -.01, fontface="bold"), size =4, color="white")+
    labs(x= "", y = "")+
    ylim(0, y_lim)+
    scale_fill_manual(name="", values=c("cyan3", "blue"))
}

p_annotate_fun <- function(df_urb, df_rur,  y_lim, patchwork_title){
  p5_stacked_fun(df_urb,  y_lim) + ylab("Number of children") + 
    p5_stacked_fun(df_rur,  y_lim) +
    theme(legend.position = "right") + p5_prop_fun(df_urb) + 
    ylab("Percentage of children") + 
    p5_prop_fun(df_rur) + 
    plot_annotation(tag_levels = 'A', title = patchwork_title,
                    caption = "Occupation (A - at least one member of the household is engaged in agricultural work, 
       Other - No member of the household is engaged in agricultural work)",
                    theme = theme(plot.title = element_text(hjust = 0.5,face = 'bold', size = 14), 
                                  plot.caption = element_text(hjust = 0.5, size = 12)))& 
    theme(plot.tag = element_text(face = 'bold', size = 11, color = "black"))
  
}

p1 <- p_annotate_fun(urban_df, rural_df, 60000, "Occupation and Malaria Test Results by Microscopy or RDT in 22 DHS datsets")
p1
#ggsave_fun("_occupation_test_results.pdf", "_occupation_test_results.png", p1, 8, 7)

# ggplot(urban_df, aes(x = test_result, fill=test_result))+
#   geom_bar()+
#   theme_manuscript()+
#   geom_bar_text(stat = 'count', aes(label =..count..), vjust=0.5, size =4 * ggplot2::.pt, 
#                 min.size = 4 * ggplot2::.pt,
#                 padding.x = grid::unit(0, "pt"),
#                 padding.y = grid::unit(0, "pt"),
#                 outside = TRUE )+
#   theme(legend.position = "none")+
#   labs(title= "",
#        x= "Malaria Test Results by Microscopy or RDT in 22 DHS datsets", y = "Number of children")
# ggsave(paste0(FigDir,"/", Sys.Date(),"all_22_data_malaria_data.pdf"), p, width = 13, height = 13)

#Regional - 
urb_regional_list <- split(urban_df, with(urban_df, interaction(SubregionName)), drop = TRUE)
rural_regional_list <- split(rural_df, with(rural_df, interaction(SubregionName)), drop = TRUE)

p2_r1 <- p5_stacked_fun(urb_regional_list[[1]], 14000) + ggtitle("Eastern")+
  theme(plot.title=element_text(margin=margin(t=10,b=-20)))+ p5_prop_fun(urb_regional_list[[1]]) 

p2_r2 <- p5_stacked_fun(urb_regional_list[[2]], 14000) +  ggtitle("Middle")+
  theme(plot.title=element_text(margin=margin(t=10,b=-20))) + p5_prop_fun(urb_regional_list[[2]]) 

p2_r3 <- p5_stacked_fun(urb_regional_list[[3]], 14000) + 
  ggtitle("Western")+theme(plot.title=element_text(margin=margin(t=10,b=-20))) + p5_prop_fun(urb_regional_list[[3]])

p2 <- p2_r1 / p2_r2 /p2_r3 +  plot_annotation(tag_levels = "A")
p2
#ggsave_fun("_region_occupation_test_results.pdf", "region_occupation_test_results.png", p2, 8, 7.2)

ggsave(paste0(SupFigDir,"/", Sys.Date(), "__country_occupation_totals.pdf"), sp6, width = 8, height = 13)

#time analsysis
#year
time_df <- urban_df %>% mutate(home_type_new = ifelse(grepl("A", home_type),"A", ifelse(is.na(home_type), NA, "Other"))) %>%
  mutate(dhs_code_year = substr(code_year, start = 3, stop = 6))
svyd_df <- svydesign.fun(time_df)
table_df <- svytable(~home_type_new + test_result + dhs_code_year, svyd_df)%>% as.data.frame()

sp7 <- ggplot() +
  geom_col(data = table_df, aes(y = Freq, x = dhs_code_year), fill = "dodgerblue1") + 
  facet_wrap(~test_result + home_type_new) + 
  theme_manuscript()+
  labs(title = "Urban overall test results over time by houshold type", y = "Number of childrem", x = "Survey year")
sp7 
#ggsave(paste0(SupFigDir,"/", Sys.Date(), "_urban_overtime_occupation_test_results.pdf"), sp7, width = 6, height = 6)


#month
table_df <- svytable(~home_type_new + interview_month, svyd_df)%>% as.data.frame() %>% 
  mutate(interview_month = as.numeric(interview_month))

p3_urban_month <- table_df %>% ggplot() +
  geom_col(aes(y = Freq, x = interview_month), fill = "dodgerblue1") +
  geom_smooth(aes(y = Freq, x = interview_month), se = FALSE, color = "goldenrod2")+
  facet_wrap(~home_type_new) + 
  theme_manuscript()+
  labs(y = "Number of childrem", x = "Interview month") +
  theme(strip.background = element_blank(), strip.text.x = element_blank())+
  scale_x_continuous(breaks = scales::pretty_breaks(n=12), expand = expansion(mult = c(0.02, 0.02)))

p3_urban_month #left = agric HH, and right plot = Non- agric HH


#p7 pop and number of children
table_df <- svytable(~dhs_code_year + interview_month + CountryName, svyd_df)%>% as.data.frame() %>% 
  filter(Freq >0) %>%  left_join(ids, by = "CountryName")

point_year_fun <- function(df, x_var, y_var, size_var, color_var, size_lab, y_lab, x_text_var){
  ggplot() + 
    geom_point(data = df, aes_string(x = x_var, y = y_var, 
                                     size = size_var, color = color_var)) + 
    scale_size(range = c(1.4, 8), name = size_lab) +
    labs(x = "year", y = y_lab) +
    geom_text(data=df, aes_string(label="DHS_CountryCode", x = x_text_var, y = "Freq"), color = "azure4", vjust = -0.5, hjust = -.5)+
    theme_manuscript() +
    theme(legend.position = "bottom") #+ 
  #ylim(0, 5500)
  
}

#survey month point plot
table_df_grouped1 <- table_df %>% group_by(DHS_CountryCode, dhs_code_year) %>% 
  summarise(Freq = sum(Freq)) %>% 
  left_join(table_df %>% select(DHS_CountryCode, dhs_code_year, SubregionName), by = c("DHS_CountryCode", "dhs_code_year"))%>%
  distinct(DHS_CountryCode, dhs_code_year, .keep_all = T)%>% left_join(ids, by = "DHS_CountryCode")

#write_csv(table_df_grouped1, file.path(ManDir, "csv", "survey_year_by_country.csv"))

#year - country - subregion plot
p3_year <- table_df_grouped1 %>% ggplot() + 
  geom_point(aes_string(x = "dhs_code_year", y = "Freq", color = "SubregionName.x"), size = 4) + 
  labs(x = "year", y = "Number of children") +
  geom_text(aes_string(label="DHS_CountryCode", x = "dhs_code_year", y = "Freq"), color = "azure4", vjust = -0.5, hjust = -.5)+
  theme_manuscript() +
  theme(legend.position = "bottom") +
  ylim(0, 6000)
p3_year

#regional
table_df_grouped_intrvw <- table_df %>% group_by(interview_month, SubregionName) %>% 
  summarise(Freq = sum(Freq))%>% mutate(interview_month = as.numeric(interview_month))

#write_csv(table_df_grouped1, file.path(ManDir, "csv", "survey_month_by_country.csv"))

p3_month <- table_df_grouped_intrvw %>% ggplot(aes(fill="dodgerblue1", y=Freq, x=interview_month)) + 
  geom_col(position="stack", stat="identity",  width=0.5) +
  geom_smooth(aes(y = Freq, x = interview_month), se = FALSE, color = "goldenrod2")+
  theme_manuscript() +
  theme(legend.position = "none") +
  facet_wrap(~SubregionName)+
  labs(x= "Interview month", y = "Number of children") +
  scale_x_continuous(breaks = scales::pretty_breaks(n=12), expand = expansion(mult = c(0.02, 0.02)))+
  theme(strip.background = element_blank(), strip.text.x = element_blank()) #uncoment to see title 1. east 2. middle, 3. west
p3_month


p3<- p3_year / p3_month / p3_urban_month +  plot_annotation(tag_levels = 'A')
p3

ggsave_fun("_merge_urban_pop_2.pdf", "_merge_urban_pop_2.png", p3, 7, 8.5)


## -------------------------------
### Analysis of other variables. 
## -------------------------------
#Countinuous variables household size and child age

box_plot_fun <- function(df, Yvar, y_lab){
  ggplot(df, aes_string(x="test_result",  y = Yvar, fill="test_result")) + 
    geom_boxplot(aes(weight=wt))+
    #geom_jitter(color="black", size=0.4, alpha=0.05) +
    labs (x = "", y = y_lab, title = "") +
    scale_fill_manual(values=c("cyan3", "blue"), name="") +
    theme_manuscript() + 
    facet_wrap(~hv025)+
    theme(legend.position = "none") +
    ylim(0,60) +
    theme(strip.background = element_blank(), strip.text.x = element_blank())
}

#demographic
box_p_age <- box_plot_fun(combined_data %>% drop_na(test_result), "hc1", "Child's age (months)")
box_p_hhsize <- box_plot_fun(combined_data %>% drop_na(test_result), "hh_size", "Household size") +
  theme(strip.background = element_blank(), strip.text.x = element_blank())

p4_box <- box_p_age + box_p_hhsize + plot_annotation(tag_levels = 'A')
p4_box

# socio and behavoir
p5_prop_fun <- function(df, var_name){
  #renamed_df <- df %>% mutate(var_name_1 = var_name)
  p_data_bar <- svytable(~var_name_1 + test_result + hv025, svydesign.fun(df))%>% as.data.frame()
  sum_yes_urb <- fsum(fsubset(p_data_bar, var_name_1 == "Yes" & hv025 == 1)$Freq)
  sum_yes_rur <- fsum(fsubset(p_data_bar, var_name_1 == "Yes" & hv025 == 2)$Freq)
  sum_no_urb <- fsum(fsubset(p_data_bar, var_name_1 == "No" & hv025 == 1)$Freq)
  sum_no_rur <- fsum(fsubset(p_data_bar, var_name_1 == "No" & hv025 == 2)$Freq)
  
  p_data_bar <- p_data_bar %>% mutate(percent = ifelse(var_name_1 == "Yes" & hv025 == 1, Freq/sum_yes_urb, NA))
  p_data_bar <- p_data_bar %>% mutate(percent = ifelse(var_name_1 == "Yes" & hv025 == 2, Freq/sum_yes_rur, percent))
  p_data_bar <- p_data_bar %>% mutate(percent = ifelse(var_name_1 == "No" & hv025 == 1, Freq/sum_no_urb, percent))
  p_data_bar <- p_data_bar %>% mutate(percent = ifelse(var_name_1 == "No" & hv025 == 2, Freq/sum_no_rur, percent))
  
  p5 <- ggplot(p_data_bar, aes(fill=test_result, y=percent, x=, var_name_1, label=scales::percent(percent, accuracy = 1L, vjust=3))) + 
    geom_col(width=0.5)+
    scale_y_continuous(labels = function(x) format(round(x*100, 0), digits=2, nsmall=2),
                       breaks = scales::pretty_breaks(n = 5))+
    theme_manuscript()+
    scale_fill_manual(name="", values=c("cyan3", "blue"))+
    facet_wrap(~hv025)+
    theme(legend.position = "none") +
    geom_text(nudge_y= -.01,
              color="white",
              size = 4,
              fontface="bold")+
    labs(x= "", y = "") +
    theme(strip.background = element_blank(), strip.text.x = element_blank())
}

p5_stacked_fun <- function(df,  var_name, y_lim){
  #renamed_df <- df %>% mutate(var_name_1 = var_name)
  p_data_bar <- svytable(~var_name_1 +test_result + hv025, svydesign.fun(df))%>% as.data.frame()%>%
    drop_na(test_result)
  
  ggplot(p_data_bar, aes(fill=test_result, y=Freq, x= var_name_1)) + 
    geom_bar(position="stack", stat="identity", alpha = 0.8, width=0.5) +
    facet_wrap(~hv025)+
    scale_fill_viridis(discrete = T) +
    theme_manuscript() +
    geom_text(aes(label=round(Freq, 0), vjust=1, nudge_y = -.01, fontface="bold"), size =4, color="white")+
    labs(x= "", y = "")+
    ylim(0, y_lim)+
    scale_fill_manual(name="", values=c("cyan3", "blue"))+
    theme(legend.position = "none")+
    theme(strip.background = element_blank(), strip.text.x = element_blank())
  
}

p_annotate_fun <- function(df,  var_name, y_lim){
  p5_stacked_fun(df,  var_name, y_lim) + ylab("No. of children") + 
    p5_prop_fun(df,  var_name) + ylab("% of children")
}


combined_data <- combined_data %>% drop_na(test_result, u5_net_use) %>%
  mutate(var_name_1 = ifelse(u5_net_use == 1, "Yes", ifelse(u5_net_use == 0, "No", NA)))
p4_nets <- p_annotate_fun(combined_data, u5_net_use, 60000) + ggtitle("U5 net use") +
  theme(plot.title=element_text(margin=margin(t=10,b=-30)))
p4_nets

combined_data <- combined_data %>% drop_na(test_result, floor_type) %>%
  mutate(var_name_1 = ifelse(floor_type == 1, "Yes", ifelse(floor_type == 0, "No", NA)))
p4_floor <- p_annotate_fun(combined_data, floor_type, 60000)+ ggtitle("Floor type") +
  theme(plot.title=element_text(margin=margin(t=10,b=-30)))
p4_floor


combined_data <- combined_data %>% drop_na(test_result, wall_type) %>%
  mutate(var_name_1 = ifelse(wall_type == 1, "Yes", ifelse(wall_type == 0, "No", NA)))
p4_wall <- p_annotate_fun(combined_data, wall_type, 60000)+ ggtitle("Wall type") +
  theme(plot.title=element_text(margin=margin(t=10,b=-30)))
p4_wall


combined_data <- combined_data %>% drop_na(test_result, roof_type) %>%
  mutate(var_name_1 = ifelse(roof_type == 1, "Yes", ifelse(roof_type == 0, "No", NA)))%>%
  mutate(type = as.character(type))
p4_roof <- p_annotate_fun(combined_data, roof_type, 60000)+ ggtitle("Roof type") +
  theme(plot.title=element_text(margin=margin(t=10,b=-30)))
p4_roof


p4 <- p4_box / p4_nets / p4_floor / p4_wall / p4_roof / plot_annotation(tag_levels = 'A')
p4 

ggsave_fun("_other_variables.pdf", "_other_variables.png", p4, 8, 9)

#Association of household agriculture worker and result of microscopy


combined_data <- combined_data %>% drop_na(test_result, agri_woman) %>%
  mutate(var_name_1 = ifelse(agri_woman == "A", "Yes", ifelse(agri_woman %in% c("O", "U"), "No", NA)))%>%
  mutate(type = as.character(type))
p4_agri_woman <- p_annotate_fun(combined_data, agri_woman, 60000)+ ggtitle("agri woman") +
  theme(plot.title=element_text(margin=margin(t=10,b=-30)))
p4_agri_woman

combined_data <- combined_data %>% drop_na(test_result, agri_partner) %>%
  mutate(var_name_1 = ifelse(agri_partner == "A", "Yes", ifelse(agri_partner %in% c("O", "U"), "No", NA)))%>%
  mutate(type = as.character(type))
p4_agri_partner <- p_annotate_fun(combined_data, agri_partner, 60000)+ ggtitle("agri partner") +
  theme(plot.title=element_text(margin=margin(t=10,b=-30)))
p4_agri_partner


combined_data <- combined_data %>% drop_na(test_result, home_type) %>% dplyr::filter(mar_stat %in% c(2,3)) %>%
  mutate(home_type_new = ifelse(grepl("AA", home_type),"AA", ifelse(is.na(home_type), NA, "Other"))) %>%
  mutate(type = as.character(type))
p4_agri <- p_annotate_fun(combined_data, home_type, 60000)+ ggtitle("both agri partners") +
  theme(plot.title=element_text(margin=margin(t=10,b=-30)))
p4_agri

p5 <- p4_agri_woman / p4_agri_partner / p4_agri / plot_annotation(tag_levels = 'A')
p5 
ggsave_fun("_agri_association.pdf", "_agri_association.png", p5, 8, 9)




#Association of household agriculture worker and result of microscopy


combined_data <- combined_data %>% drop_na(test_result, agri_woman) %>%
  mutate(var_name_1 = ifelse(agri_woman == "A", "Yes", ifelse(agri_woman %in% c("O", "U"), "No", NA)))%>%
  mutate(type = as.character(type))
p4_agri_woman <- p_annotate_fun(combined_data, agri_woman, 60000)+ ggtitle("agri woman") +
  theme(plot.title=element_text(margin=margin(t=10,b=-30)))
p4_agri_woman

combined_data <- combined_data %>% drop_na(test_result, agri_partner) %>%
  mutate(var_name_1 = ifelse(agri_partner == "A", "Yes", ifelse(agri_partner %in% c("O", "U"), "No", NA)))%>%
  mutate(type = as.character(type))
p4_agri_partner <- p_annotate_fun(combined_data, agri_partner, 60000)+ ggtitle("agri partner") +
  theme(plot.title=element_text(margin=margin(t=10,b=-30)))
p4_agri_partner


combined_data <- combined_data %>% drop_na(test_result, home_type) %>% dplyr::filter(mar_stat %in% c(2,3)) %>%
  mutate(home_type_new = ifelse(grepl("AA", home_type),"AA", ifelse(is.na(home_type), NA, "Other"))) %>%
  mutate(type = as.character(type))
p4_agri <- p_annotate_fun(combined_data, home_type, 60000)+ ggtitle("both agri partners") +
  theme(plot.title=element_text(margin=margin(t=10,b=-30)))
p4_agri

p5 <- p4_agri_woman / p4_agri_partner / p4_agri / plot_annotation(tag_levels = 'A')
p5 
ggsave_fun("_agri_association.pdf", "_agri_association.png", p5, 8, 9)






