# -----------------------------------------
### Required functions and settings
## -----------------------------------------

# # Reading in the necessary packages 
list.of.packages <- c('readr', 'tidyr', 'plyr', 'dplyr', 'purrr', 'forcats',"survey", 
                      "haven", "ggplot2", "purrr",  "stringr", "sp", "rgdal", "raster","sf",   "labelled", "plotrix", "arules", 
                      "fuzzyjoin", 'cowplot', 'gridExtra', 'lme4', "patchwork", "readxl" ,
                      'ggsci', 'glue', 'ggrepel', 'jtools', "srvyr", "ggpubr", "collapse",
                      'gtsummary', 'rstatix', 'ggcorrplot', 'viridis', 'effects', "rdhs", "microbenchmark",  "ggfittext", "forcats", "broom")


new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages)

lapply(list.of.packages, library, character.only = TRUE) #applying the library function to packages

library(rdhs)


#plotting functions
bar_fun <- function(df, x, fill, title, xlab){ #facet_var, 
  ggplot(df, aes(x = .data[[x]], fill=.data[[x]]))+
    geom_bar()+
    #coord_flip()+
    #facet_wrap(vars(.data[[facet_var]]), scales="free")+
    theme_manuscript()+
    geom_bar_text(stat = 'count', aes(label =..count..), vjust=0.5, size =5 * ggplot2::.pt, 
                  min.size = 4 * ggplot2::.pt,
                  padding.x = grid::unit(0, "pt"),
                  padding.y = grid::unit(0, "pt"),
                  outside = TRUE )+
    theme(legend.position = "none")+
    labs(title= title,
         x=xlab)
}


col_fun <- function(df, x, y, z, ylabel, color, label){
  ggplot(df, aes(x = .data[[x]], y =.data[[y]], fill=.data[[z]]))+
    geom_col()+
    coord_flip()+ 
    scale_x_discrete(limits = rev)+
    theme_manuscript()+
    theme(legend.title=element_blank())+
    labs(x="", y= ylabel )+
    scale_fill_manual(values=color, labels= label, guide = guide_legend(reverse = TRUE))
}



#read files function 
read.files <- function(filepat1,path,fun, encoding = "latin1") {
  filenames <- list.files(path = path, pattern = filepat1, recursive = TRUE, full.names = FALSE)
  sapply(filenames, fun, simplify = F)
  
}




theme_manuscript <- function(){
  theme_bw() + 
    theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5),
          plot.title = element_text(hjust = 0.5),
          axis.text.x = element_text(size = 12, color = "black"), 
          axis.text.y = element_text(size = 12, color = "black"),
          axis.title.x = element_text(size = 12),
          axis.title.y = element_text(size = 12),
          legend.title=element_text(size=12, colour = 'black'),
          legend.text =element_text(size = 12, colour = 'black'),
          legend.key.height = unit(1, "cm"))
}

theme_corr <- function(){
  theme(panel.border = element_rect(colour = "black", fill=NA, size=0.5), 
        axis.text.x = element_text(size = 12, color = "black"), 
        axis.text.y = element_text(size = 12, color = "black"))
}

# Stacked bar plot function

barplot_stack.fun <- function(main_title){
  barplot_stack <- ggplot(counts, aes(fill=Var2, y=Freq, x=Var1)) + 
    geom_bar(position="stack", stat="identity", alpha = 0.8) +
    scale_fill_viridis(discrete = T) +
    ggtitle(main_title) +
    theme_manuscript() +
    xlab("")+
    ylab('frequency')+
    scale_fill_manual(name="Legend", values=c("darkgrey", "brown")) 
}

#percentage bar plot function

barplot_prop.fun <- function(main_title){
  barplot_perc <- ggplot(counts, aes(fill=Var2, y=Freq, x=Var1)) + 
    geom_bar(position="fill", stat="identity", alpha = 0.8) +
    scale_fill_viridis(discrete = T) +
    ggtitle(main_title) +
    theme_manuscript() +
    scale_fill_manual(name="Legend", values=c("darkgrey", "brown"))+
    xlab("")+
    ylab('percent')
}

#percentage bar plot function

barplot_prop.fun2 <- function(dataframe){
  barplot_perc <- ggplot(dataframe, aes(fill=Var2, y=Freq, x=Var1, label=scales::percent(percent, vjust=3))) + 
    geom_bar(position="fill", stat="identity", alpha = 0.8) +
    scale_fill_viridis(discrete = T) +
    ggtitle("") +
    theme_manuscript() +
    scale_fill_manual(name="Legend", values=c("cyan", "darkcyan"))+
    xlab("")+
    ylab('proportion')
}  

#survey estimates generating functions  
result.prop<- function(var, var1, design) {
  p_est<-svyby(formula=make.formula(var), by=make.formula(var1), FUN=svymean, design, svyciprop, method ='logit', levels=0.95, vartype= "se", na.rm=T, influence = TRUE)
}

#survey design function 
svydesign.fun <- function(filename){
  svydesign(id= ~id,
            strata=~strat,nest=T, 
            weights= ~wt, data=filename)
}


#survey estimates generating functions  
result.prop<- function(var, var1, design) {
  p_est<-svyby(formula=make.formula(var), by=make.formula(var1), FUN=svytotal, design, svyciprop, method ='logit', levels=0.95, vartype= "se", na.rm=T, influence = TRUE)
}

#estimation functions 
estim_prop <- function(df, col, by){
  svy_mal <- svydesign.fun(df)
  clu_est <- result.prop(col, by, design=svy_mal)
}

#dhs ids

ids <- dhs_countries(returnFields=c("CountryName", "DHS_CountryCode", "SubregionName", "RegionName")) %>% 
  filter(RegionName == "Sub-Saharan Africa")


#ggsave function
ggsave_fun <- function(save_as_pdf, save_as_png, plot_name, width_size, height_sze){
  ggsave(paste0(FigDir,"/", Sys.Date(),save_as_pdf), plot_name, width = width_size, height =height_sze)
  ggsave(paste0(FigDir,"/", Sys.Date(), save_as_png), plot_name, width = width_size, height =height_sze)
}


#regional probabilities df generator fun
effect_df_fun <- function(model_){
  effect_list_est <- summary(Effect("agric_home", model_)) 
  effect_list_est$effect %>% as.data.frame() %>% 
    bind_cols(effect_list_est$lower %>% as.data.frame()) %>% 
    bind_cols(effect_list_est$upper %>% as.data.frame()) %>% 
    rename(effect = ....1, lower = ....2, upper = ....3) %>% 
    tibble::rownames_to_column()
}

#funtion - housing quality and wealth by agric household worker status


#geom column plots function 


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

#rura - urban stacked hist combo

ur_rur_pfun <- function(df_r_u){
  
  p2_r1 <- p5_stacked_fun(df_r_u[[1]], 22500) + ggtitle("Eastern")+
    theme(plot.title=element_text(margin=margin(t=10,b=-20)))+ p5_prop_fun(df_r_u[[1]]) 
  
  p2_r2 <- p5_stacked_fun(df_r_u[[2]], 22500) +  ggtitle("Middle")+
    theme(plot.title=element_text(margin=margin(t=10,b=-20))) + p5_prop_fun(df_r_u[[2]]) 
  
  p2_r3 <- p5_stacked_fun(df_r_u[[3]], 22500) + 
    ggtitle("Western")+theme(plot.title=element_text(margin=margin(t=10,b=-20))) + p5_prop_fun(df_r_u[[3]])
  
  p2_r1 / p2_r2 /p2_r3 +  plot_annotation(tag_levels = "A")
  
}

#housing quality and wealth by agric household worker statu

bar_prop_fun<- function(df, var_string, x_lab){
  ggplot(p_data_bar, aes_string(y="Freq", x="home_type_new", fill= var_string)) + 
    geom_bar(position="fill", stat="identity", alpha = 0.8) +
    #scale_fill_viridis(discrete = T) +
    facet_wrap(~hv025)+
    ggtitle("") +
    theme_manuscript() +
    scale_fill_manual(name="", values=c("darkcyan", "brown"))+
    #geom_text(aes(label = Freq)) +
    xlab(x_lab)+
    ylab('Proportion') +
    theme(legend.position = "none", strip.background = element_blank(),
          strip.text.x = element_blank()) 
}
