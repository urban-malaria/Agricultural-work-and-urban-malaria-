
p <- ggplot(plot_u_df, aes(fill = category, x=country_year)) +
  geom_bar(position = "stack")+ 
  coord_flip()+ 
  labs(title = "DHS datasets with agricultural worker data (urban areas)", x = "", y = "Number of women, 15 - 49, that responded to the survey")+
  theme_manuscript()

p<-ggplot(plot_u_df, aes(x=category,  group=country_year)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes(label = scales::percent(round(..prop.., 2)),
                y= ..prop.. ), stat= "count", vjust = -.3) +
  labs(title = "DHS datasets with agricultural worker data (urban_areas)", y = "Percent", x="Occupation (A - Agricultural work, M - Missing, O - other, U - unemployed)")+
  facet_wrap(vars(country_year))+
  scale_y_continuous(labels=scales::percent)+
  theme_manuscript()+
  theme(legend.position = "none")
ggsave(paste0(FigDir,"/", Sys.Date(),"_urban_DHS_datasets_agric_data_percent.png"), p, width = 13, height = 13)


p<-ggplot(plot_r_df, aes(x=category,  group=country_year)) +
  geom_bar(aes(y = ..prop.., fill = factor(..x..)), stat="count") +
  geom_text(aes(label = scales::percent(round(..prop.., 2)),
                y= ..prop.. ), stat= "count", vjust = -.3) +
  labs(title= "DHS datasets with agricultural worker data (rural areas)", y = "Percent", x="Occupation (A - Agricultural work, M - Missing, O - other, U - unemployed)")+
  facet_wrap(vars(country_year))+
  scale_y_continuous(labels=scales::percent)+
  theme_manuscript()+
  theme(legend.position = "none")
ggsave(paste0(FigDir,"/", Sys.Date(),"_rural_DHS_datasets_agric_data_percent.png"), p, width = 13, height = 13)


# login into dhs
my_config <- set_rdhs_config(email = "cchiziba@gmail.com",
                             project = "Net ownership by individual",
                             config_path = "rdhs.json",
                             cache_path = "project_one",
                             password_prompt = TRUE,
                             global = FALSE)

# obtaining country ids
ids <- dhs_countries(returnFields=c("CountryName", "DHS_CountryCode"))


# find all the surveys that match the search criteria. ie .2015
survs <- dhs_surveys(surveyYearStart = 2015) %>% as.data.frame() %>%
  filter(RegionName == "Sub-Saharan Africa", SurveyId != "SN2020MIS")




# caching the dhs list: the first time we call this function, rdhs will make the API request
microbenchmark::microbenchmark(dhs_surveys(surveyYear = 2015),times = 1)







################################chilo's script starts here 



#binding all countries into one dataframe



ir_bind <- bind_rows(dhs_pr) # %>% filter(b19_01<60 | b19_02<60 |b19_03<60 |b19_04<60 |b19_05<60 |b19_06<60 |b19_07<60 
#                                        |b19_08<60 |b19_09<60 |b19_01<60 |b19_10<60 |b19_11<60 |b19_12<60 |b19_13<60 |
#                                          b19_14<60 |b19_15<60 |b19_16<60 |b19_17<60 |b19_18<60 |b19_19<60 |b19_20<60)

table(ir_bind$agri_worker_partner, ir_bind$v000)#comment - can you check the accuracy of the output here 
#and what you have on the table in the appendix. They are not the same 




#######################################
#Data loading  and transforming IR datasets
#######################################

pr_datasets <- dhs_datasets(surveyIds = survs$SurveyId, fileFormat = "FL", fileType = "PR")


#reading ir data
pr_downloads <- get_datasets(pr_datasets, clear_cache = TRUE) 


dhs = lapply(pr_downloads, readRDS)

dhs_pr <- dhs %>% map(~mutate(., smear_va = if("hml32" %in% colnames(.)) "Yes" else NA,
                              smear_test = if("hml32" %in% colnames(.)) smear_va else NA,
                              hml35 = if("hml35" %in% colnames(.)) hml35 else NA,
                              hml32 = if("hml32" %in% colnames(.)) hml32 else hml35,
                              hml16a = if("hml16a" %in% colnames(.)) hml16a else NA,
                              hc1 = if("hc1" %in% colnames(.)) hc1 else hml16a,
                              hv042 = if("hv042" %in% colnames(.)) hv042 else NA,
                              hv103 = if("hv103" %in% colnames(.)) hv103 else NA,
                              hml12 = if("hml12" %in% colnames(.)) hml12 else NA,
                              hv213 = if("hv213" %in% colnames(.)) hv213 else NA,
                              hv214 = if("hv214" %in% colnames(.)) hv214 else NA,
                              hv215 = if("hv215" %in% colnames(.)) hv215 else NA,
                              hv009 = if("hv009" %in% colnames(.)) hv009 else NA,
                              wt=hv005/1000000,
                              strat=hv022, 
                              id=hv021, 
                              test_result = ifelse(hml32==1, 1,ifelse(hml32==0, 0, NA)), 
                              dhs_year = hv007, child_age = hc1,
                              u5_net_use = ifelse(hml12 %in% c(1,2), 1,0),hh_size = hv009, 
                              floor_type = ifelse(hv213 == 30| hv213 == 31|hv213 == 33| hv213 == 34|hv213 == 35,1, 0),
                              wall_type = ifelse(hv214 == 30| hv214 == 31| hv214 == 33| hv214 == 34,0, 1),
                              roof_type = ifelse(hv215 == 30| hv215 == 31|hv215 == 33| hv215 == 34|hv215 == 35|hv215 == 36,1, 0),
                              smear_test = base::as.character(smear_test), 
                              rdt_result = as.numeric(hml35))) %>%
  map(~filter(., hv042 == 1 & hv103 == 1 & hc1 %in% c(6:59), hv025 == 1)) %>% 
  map(~dplyr::select(., test_result, rdt_result, dhs_year, smear_test, hv000, hv001, hv002, hv003, hv006, child_age, u5_net_use, 
                     floor_type, wall_type, roof_type, hh_size, hvidx))



#merging PR datasets for all countires

labs <- c(paste(seq(0, 59, by = 12), seq(0 + 12 - 1, 60 - 1, by = 12),sep = "-"))

pr_bind <- bind_rows(dhs_pr) %>% mutate(test_result = coalesce(test_result, rdt_result)) %>% 
  mutate(test_result = ifelse(test_result==1, 1,ifelse(test_result==0, 0, NA)))%>% filter(test_result != 6) %>% 
  mutate(country = str_replace_all(hv000, c("5" = "", "6" = "", "7" = ""))) %>%
  left_join(ids, by = c(country = "DHS_CountryCode"))%>% 
  mutate(hous_q = ifelse(floor_type == 1 & wall_type == 1 & roof_type == 1, 1, 0)) %>%
  mutate(child_age_group = cut(child_age, breaks = c(seq(0, 59, by = 12), Inf), labels = labs,  right = FALSE))



table(pr_bind$test_result, pr_bind$CountryName)



#######################################
#Data loading  and transforming KR datasets
#######################################


#listing list of kr datasets to explore

kr_datasets <- dhs_datasets(surveyIds = survs$SurveyId, fileFormat = "FL", fileType = "KR")


#reading kr data
kr_downloads <- get_datasets(kr_datasets, clear_cache = TRUE) 


dhs = lapply(kr_downloads, readRDS)

dhs_kr <- dhs %>% map(~mutate(.,b5 = if("b5" %in% colnames(.)) b5 else NA,
                              b19 = if("b19" %in% colnames(.)) b19 else NA,  
                              h22 = if("h22" %in% colnames(.)) h22 else hml35,
                              v025 = if("v025" %in% colnames(.)) v025 else NA,
                              h32a = if("h32a" %in% colnames(.)) h32a else NA,
                              h32b = if("h32b" %in% colnames(.)) h32b else NA,
                              h32j = if("h32j" %in% colnames(.)) h32j else NA,
                              h32k = if("h32k" %in% colnames(.)) h32k else NA,
                              h32l = if("h32l" %in% colnames(.)) h32l else NA,
                              h32m = if("h32m" %in% colnames(.)) h32m else NA,
                              h32s = if("h32s" %in% colnames(.)) h32s else NA,
                              h32x = if("h32x" %in% colnames(.)) h32x else NA,
                              ml13e = if("ml13e" %in% colnames(.)) ml13e else NA,
                              wt=v005/1000000,
                              strat=v022, 
                              id=v021, 
                              dhs_year = v007,
                              health_seek = ifelse(h32a == 1|h32b == 1|h32j == 1|h32k == 1|h32l == 1| h32m == 1|h32x== 1|ml13e== 1, 1, 0),
                              #health_seek = ifelse(ml13e== 1, 1, 0),
                              health_seek = as.numeric(health_seek))) %>% 
  map(~filter(., b5==1, b19 < 60, h22 == 1, v025 == 1)) %>% 
  map(~dplyr::select(., health_seek, dhs_year, v000, v001, v002, v003, wt, strat, id))


#extracting cluster level health acess data

#dhs_kr2 <- dhs_kr[1:2]

#dhs_kr_list <- list()

#vars <- c('health_seek')
#for (i in 1:length(vars)) {
#  col <- list(vars[i])
#by <- list('v001')
#df <- dhs_kr %>% 
#   map(~drop_na(.,vars[i]))
# df <- pmap(list(df,col,by), estim_prop)
# df <- plyr::ldply(df)
# df[, vars[i]]<- df[, vars[i]]
#dhs_kr_list[[i]] <- df

#}


kr_bind <- bind_rows(dhs_kr_list) 

table(kr_bind$health_seek)



############################################################
#merging agric and malaria datasets
#############################################################)

pr_bind_ord <- pr_bind[with(pr_bind_2, order(hv000, hv001, hv002,  hv003, dhs_year)),]
kr_bind_ord <- kr_bind[with(kr_bind, order(v000, v001, v002,  v003, dhs_year)),]
ir_bind_ord <- ir_bind[with(ir_bind, order(v000, v001, v002,  v003, dhs_year)),]


df <- pr_bind_ord %>% left_join(kr_bind_ord, by = c("hv000"="v000", "dhs_year"="dhs_year","hv001"="v001","hv002"="v002")) %>% 
  distinct(hv000, dhs_year, hvidx, hv001, hv002, child_age, .keep_all = TRUE) %>% 
  left_join(ir_bind_ord,by = c("hv000"="v000", "dhs_year"="dhs_year","hv001"="v001","hv002"="v002"))%>% 
  distinct(hv000, dhs_year, hvidx, hv001, hv002, child_age, .keep_all = TRUE)



df_a <- df %>% mutate(country = str_replace_all(v000, c("5" = "", "6" = "", "7" = ""))) %>%
  left_join(ids, by = c(country = "DHS_CountryCode"))%>% 
  mutate(test_results_2 = str_replace_all(test_result, c("1" = "positive", "0" = "negative")))%>% 
  mutate(agric_2 = str_replace_all(agri_worker_partner, c("1" = "yes", "0" = "no")))


write.csv(df_a, "mlt_cntry_df.csv")


table(df_a$CountryName)


#########################################
#preliminary  descriptive tables results
##########################################

#initial exploration to check which countries have sufficient malaria data

test_table <- tbl_summary(df_a, by = "test_results_2", 
                          include = c(CountryName, dhs_year,duration_travel_woman, agri_worker_woman, 
                                      agri_worker_partner, last_work_partner,last_work_woman, seasonal_work_woman, 
                                      agri_worker_both, age_woman, edu_woman, trans_work_partner, trans_work_woman, 
                                      wealth, media_exposure, co_wives, health_dec,child_age, u5_net_use, floor_type, 
                                      wall_type, roof_type, hh_size, health_seek)) %>% add_overall()
test_table

agric_table <- tbl_summary(df_a, by = "agric_2", include = c(CountryName)) %>% add_overall()


tbl_test_agric <- tbl_merge(tbls = list(agric_table, test_table), 
                            tab_spanner = c("**Agricultural occupation - self employed/employee**", 
                                            "**Final result of malaria from blood smear/RDT test**"))
tbl_test_agric



#Add descriptive table for worker category
agric_only <- tbl_summary(df_a, by = "agric_2", include = c(test_results_2)) %>% add_overall()

tbl_agric <- tbl_merge(tbls = list(agric_only), 
                       tab_spanner = c("**Distribution of parents working in agriculture by malaria test results in children under five**"))
tbl_agric #comment - the results from unknown is different from what you have in the table. Did something change?



###############################################################################
#final results tables
###############################################################################


######filter out contries witth less than 30 malaria cases only

target <- c("Angola", "Benin", "Burundi", "Cameroon", "Mali", "Nigeria", "Tanzania", "Uganda")


df_filtered <- filter(df_a, CountryName %in% target) %>% filter(!is.na(test_result)) 


pos_filtered <- filter(pr_bind, CountryName %in% target)


svyd <- lapply(list(pos_filtered, df_filtered), svydesign.fun)



#table 1: malaria and agric


test_tbl <- svyd[[1]] %>% tbl_svysummary(by = "test_result", include = c(CountryName), percent= "row")  
test_tbl


agric_tbl <- svyd %>% tbl_svysummary(by = "agric_2", include = c(CountryName), percent= "row")
agric_tbl

#table 1a: age group
df_positive <- df_filtered %>% filter(test_result == 1)

agric_pos_tbl <- svydesign.fun(df_positive) %>% tbl_svysummary(by = "agric_2", include = c(CountryName), percent= "row") %>% add_overall()
agric_pos_tbl

#table 2: age group
df_positive_pr <- pos_filtered %>% filter(test_result == 1)

age_tbl <- svydesign.fun(df_positive_pr) %>% tbl_svysummary(by = "child_age_group", include = c(CountryName), percent= "row") %>% add_overall()
age_tbl


#table 3: educational attainment 


edu_tbl <- svydesign.fun(df_positive) %>% tbl_svysummary(by = "edu_woman", include = c(CountryName), percent= "row")%>% 
  add_overall()

edu_tbl


#table 4: Wealth

#cols <- c('edu_woman')

#df_filtered[cols] <- lapply(df_filtered[cols], as.factor)

cntrs_df_list <- split(df_filtered, with(df_filtered, interaction(CountryName)), drop = TRUE)


table.fun <- function(dat){
  
  svydesign.fun(dat) %>% tbl_svysummary(by = "agric_2", include = c(wealth), percent= "row")
  
}

table_list <- lapply(cntrs_df_list, table.fun)

c_names <- list(df_filtered$CountryName)

c_names <- sapply(c_names, unique)


wealth_tbl <- tbl_merge(tbls = table_list,tab_spanner = c_names)
wealth_tbl


###################################################
#Descriptive plots
###############################################
#Fig 1 - one plot that depicts higher burden of malaria in children from agricultural workers - 
# - homes vs those in other occupations across all countries. Within the same figure, country by country breakdowns.

pstv_cntry <- svytable(~CountryName + test_result, svydesign.fun(pr_bind)) %>% as.data.frame() 

mlr_prop <- aggregate(Freq ~ CountryName, pstv_cntry, sum) %>% rename_at(2,~"total") %>% 
  inner_join(pstv_cntry, by = "CountryName")%>%filter(test_result == 1) %>% mutate(mal_prop = Freq/total)


agric_cntry <- svytable(~v000 + agri_worker_partner, svydesign.fun(ir_bind)) %>% as.data.frame() %>% 
  mutate(country = str_replace_all(v000, c("5" = "", "6" = "", "7" = ""))) %>%
  left_join(ids, by = c(country = "DHS_CountryCode")) %>% aggregate(Freq ~ CountryName + agri_worker_partner, sum) 

malr_agric_prop <- aggregate(Freq ~ CountryName, agric_cntry, sum) %>% rename_at(2,~"agric_total") %>% 
  inner_join(agric_cntry, by = "CountryName")%>%filter(agri_worker_partner == 1) %>% 
  mutate(agr_prop = Freq/agric_total) %>% left_join(mlr_prop, by = "CountryName")%>%
  left_join(ids, by = "CountryName") 

#Loading polygons for all coutries
poly <- read_sf(file.path(PopDir, 'shapefiles', 'grump-v1-urban-ext-polygons-rev02-shp',"global_urban_extent_polygons_v1.01.shp"))%>%
  dplyr::filter(Continent == 'Africa')

poly_sum <- poly %>% group_by(Countryeng) %>% summarize(st_union(geometry), area_id = sum(area))

poly_as<- poly %>% dplyr::filter(Continent == 'Africa') %>% sf::as_Spatial()


####burble plot

p_malr_agric <- malr_agric_prop %>% arrange(desc(Freq.y)) %>%
  mutate(DHS_CountryCode = factor(DHS_CountryCode)) %>%
  ggplot(aes(x=agr_prop, y=mal_prop, size=Freq.y)) +
  geom_text(aes(label=DHS_CountryCode),vjust=-.8,nudge_y = 0.005,hjust=-0.1, size = 3)+
  geom_point(alpha=0.7, shape=21, color="black", fill="mediumturquoise") +
  scale_size(range = c(3, 12), name="Malaria positives") +
  scale_fill_viridis(discrete=TRUE, guide=FALSE, option="A") +
  theme_manuscript() +
  theme(legend.position="bottom") +
  ylab("Proportion of individuals with agricultural occupations") +
  xlab("Proportion of under five children that tested positive") 

p_malr_agric



#Figure 2: a figure for all countries aggregated together depicting the social (wealth index, housing quality), 
#age, mean number of siblings, evi, k-complexity, access to care (case management among those with fever), 
#ITN use and other relevant covarites showing hiw they may differ from those whose parents don't engage in 
#agriculture based occupations. A similar country by country plot can be placed in the appendix

counts_ed <- svytable(~agri_worker_partner + edu_woman, svyd[[2]])%>% as.data.frame()%>%
  mutate(agri_worker_partner=str_replace(agri_worker_partner, '0','none-agric workers')) %>%
  mutate(agri_worker_partner=str_replace(agri_worker_partner, '1','agric workers')) %>%
  mutate(edu_woman=str_replace(edu_woman, '0','none')) %>% 
  mutate(edu_woman=str_replace(edu_woman, '1','primary')) %>% 
  mutate(edu_woman=str_replace(edu_woman, '2','secondary')) %>% 
  mutate(edu_woman=str_replace(edu_woman, '3','higher'))  %>% mutate(Freq = round(Freq, 0))

counts_we <- svytable(~agri_worker_partner + wealth, svyd[[2]])%>% as.data.frame()%>%
  mutate(agri_worker_partner=str_replace(agri_worker_partner, '0','none-agric workers')) %>%
  mutate(agri_worker_partner=str_replace(agri_worker_partner, '1','agric workers')) %>%
  mutate(wealth=str_replace(wealth, '1','poorest')) %>% 
  mutate(wealth=str_replace(wealth, '2','poorer')) %>%
  mutate(wealth=str_replace(wealth, '3','middle')) %>%  
  mutate(wealth=str_replace(wealth, '4','richer'))%>%  
  mutate(wealth=str_replace(wealth, '5','richest')) %>% mutate(Freq = round(Freq, 0))

counts_hq <- svytable(~agri_worker_partner + hous_q, svyd[[2]])%>% as.data.frame()%>%
  mutate(agri_worker_partner=str_replace(agri_worker_partner, '0','none-agric workers')) %>%
  mutate(agri_worker_partner=str_replace(agri_worker_partner, '1','agric workers')) %>%
  mutate(hous_q=str_replace(hous_q, '1','good')) %>% 
  mutate(hous_q=str_replace(hous_q, '0','poor')) %>% mutate(Freq = round(Freq, 0))

counts_hs <- svytable(~agri_worker_partner + health_seek, svyd[[2]])%>% as.data.frame()%>%
  mutate(agri_worker_partner=str_replace(agri_worker_partner, '0','non-agric workers')) %>%
  mutate(agri_worker_partner=str_replace(agri_worker_partner, '1','agric workers')) %>%
  mutate(health_seek=str_replace(health_seek, '1','good')) %>% 
  mutate(health_seek=str_replace(health_seek, '0','bad')) %>% mutate(Freq = round(Freq, 0))

counts_netuse <- svytable(~agri_worker_partner + u5_net_use, svyd[[2]])%>% as.data.frame()%>%
  mutate(agri_worker_partner=str_replace(agri_worker_partner, '0','none-agric workers')) %>%
  mutate(agri_worker_partner=str_replace(agri_worker_partner, '1','agric workers')) %>%
  mutate(u5_net_use=str_replace(u5_net_use, '1','yes')) %>% 
  mutate(u5_net_use=str_replace(u5_net_use, '0','no')) %>% mutate(Freq = round(Freq, 0))



p_agr_var.fun <- function(df_name, Var2, Var1, xlab_title, legend_opt){
  
  ggplot(df_name, aes_string(fill=Var2, y="Freq", x=Var1, order="Freq")) + 
    geom_bar(position="fill", stat="identity", alpha = 0.8) +
    #coord_flip()+
    scale_fill_viridis(discrete = T) +
    ggtitle("") +
    theme_manuscript() +
    xlab(xlab_title)+
    ylab('')+
    scale_fill_manual(name="Legend", values=c("limegreen", "royalblue1"))+
    theme(legend.position=legend_opt)
}

p_edu_agric <- p_agr_var.fun(counts_ed, "edu_woman", "agri_worker_partner", "education", "none")
p_we_agric <- p_agr_var.fun(counts_we, "agri_worker_partner", "wealth", "wealth", "none")
p_hq_agric <- p_agr_var.fun(counts_hq, "agri_worker_partner", "hous_q", "housing quality", "none")
p_hs_agric <- p_agr_var.fun(counts_hs, "agri_worker_partner", "health_seek", "health seeking behavior", "bottom")
p_netuse_agric <- p_agr_var.fun(counts_netuse, "agri_worker_partner", "u5_net_use", "U5 net use", "none")


p_agrc_socl <- (p_edu_agric | p_we_agric)/ (p_hq_agric | p_hs_agric | p_netuse_agric)+ 
  plot_annotation(tag_levels = 'A')& theme(plot.tag = element_text(size = 12, face = 'bold'))
p_agrc_socl

#






















######################################################
#model fitting bivariate
#####################################################
svyd_list <- lapply(cntrs_df_list, svydesign.fun) 

#loop fitting bivariate models

model_list <- list()

for(i in 1:length(svyd_list)){
  
  model_fit <- svyglm(test_result ~ agri_worker_partner, design = svyd_list[[i]], family = "binomial") 
  model_list[[i]] <- model_fit
}

#summarizing findings
summary_list <- lapply(model_list, summary)
summary_list

### CONVERTING RESULTS TO DF
model_results_df <- lapply(model_list, tidy)
model_results_df



#computing coeficients AND odds 


coefs_bi <- list()

for (i in 1:length(model_list)) { 
  results = coefficients(summary(model_list[[i]]))
  sub_df <- subset(results,!rownames(results) %in% c("(Intercept)"))
  coefs_bi[[i]] = sub_df 
  
}


bi_coefs_df <- rbind(coefs_bi[[1]],coefs_bi[[2]], coefs_bi[[3]], coefs_bi[[4]], 
                     coefs_bi[[5]], coefs_bi[[6]], coefs_bi[[7]],coefs_bi[[8]])

#computing odds ratios for bivariate models and their confidence intervals 

colnames(bi_coefs_df)[2] = "SE"
results_df_bi <- data.frame(bi_coefs_df)%>% mutate(odds = (exp(Estimate))) %>% 
  mutate(lower_ci = (exp(-1.96*SE+Estimate))) %>% 
  mutate(upper_ci = (exp(1.96*SE+Estimate))) %>% tibble::rownames_to_column('vars')#%>%  remove_rownames()

results_df_bi
#write.csv(results_df_bi, paste0(DataIn, '/tables',"/woman_bi_coefs_df.csv"))
write.csv(results_df_bi, "muilt_bi_coefs_df.csv")



######################################################
#model fitting muiltivariate
#####################################################

#looping through countries - fitting multivariate models 

mmodel_list <- list()

for(i in 1:length(svyd_list)){
  
  mmodel_fit <- svyglm(test_result ~ agri_worker_partner + child_age_group + edu_woman + 
                         media_exposure +  hh_size + wall_type, design = svyd_list[[i]], family = "binomial") 
  mmodel_list[[i]] <- mmodel_fit
}




#summarizing findings
summary_list <- lapply(mmodel_list, summary)
summary_list

### CONVERTING RESULTS TO DF
mmodel_results_df <- lapply(mmodel_list, tidy)
mmodel_results_df


#computing coeficients AND odds 


coefs_mi <- list()

for (i in 1:length(mmodel_list)) { 
  results = coefficients(summary(mmodel_list[[i]]))
  sub_df <- subset(results,!rownames(results) %in% c("(Intercept)"))
  coefs_mi[[i]] = sub_df 
  
}


mi_coefs_df <- rbind(coefs_mi[[1]],coefs_mi[[2]], coefs_mi[[3]], coefs_mi[[4]], 
                     coefs_mi[[5]], coefs_mi[[6]], coefs_mi[[7]],coefs_mi[[8]])

#computing odds ratios for multivariable models and their confidence intervals 

colnames(mi_coefs_df)[2] = "SE"
results_df_mi <- data.frame(mi_coefs_df)%>% mutate(odds = (exp(Estimate))) %>% 
  mutate(lower_ci = (exp(-1.96*SE+Estimate))) %>% 
  mutate(upper_ci = (exp(1.96*SE+Estimate))) %>% tibble::rownames_to_column('vars')#%>%  remove_rownames()

#results_df_mi
#creating unique variable to be used for spliting the results by variable. 
results_df_mi <- results_df_mi %>% 
  mutate(splitter = str_replace_all(vars, c("p12" = "12", ".47" = "",".1" = "",".2" = "", ".3" = "", 
                                            ".4" = "",".5" = "", ".6" = "", ".7" = "", ".8" = ""))) 

results_df_list <- split(results_df_mi, with(results_df_mi, interaction(splitter)), drop = TRUE)

#write.csv(results_df_mi, paste0(DataIn, '/tables',"/woman_mi_coefs_df.csv"))

###########################################################
#forest plots
###########################################################

rows.fun <- function(df_data){
  cbind(df_data, countries= c("Angola","Benin", "Burundi", "Cameroon", "Mali", "Nigeria", "Tanzania", "Uganda"))
}

data_b <- lapply(results_df_bi, rows.fun)
data_m <- lapply(results_df_list, rows.fun)


b_results_bind <- bind_rows(data_b)
write.csv(b_results_bind, "muilt_bi_coefs_df.csv")

m_results_bind <- bind_rows(data_m)
write.csv(m_results_bind, "muilt_mi_coefs_df.csv")



forst_titles <- list('Parent working in agric.', 'Child age group 12-23',
                     'Child age group 24-35','Child age group 36-47','Child age group 48-59', 
                     'Womans educational level', 'Household size', 'Media exposure')

forest_plots <- list()
for (i in 1:length(data_m)) { 
  c <- ggplot(data[[i]], aes(x = odds, y = countries)) + 
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


forest_plots[[1]] + forest_plots[[2]] + forest_plots[[3]] + forest_plots[[4]]+ 
  forest_plots[[5]] + forest_plots[[6]] + forest_plots[[7]] + forest_plots[[8]]

