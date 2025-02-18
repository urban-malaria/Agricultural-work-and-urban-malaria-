## -----------------------------------------------------------------------------------------------------------------------------------------
### Figure 3 - Urban Plot (# children tested positive by agric/non-agric households in urban areas)
## -----------------------------------------------------------------------------------------------------------------------------------------

color = c("#f2a5a1", "#c55c80")

# create a dataframe for urban data with selected columns
plot_df_um <- urban_df %>% dplyr::select(country_year.x, home_type2, code_year, test_result, id, strat, wt)

# prepare data for plotting with survey design
plot_overall <- plot_df_um %>%
  as_survey_design(ids = id, strata = strat, nest = TRUE, weights = wt) %>%
  group_by(home_type2, test_result) %>% # group data by home type and test result
  dplyr::summarise(
    value = survey_mean(vartype = "se") # calculate survey mean with standard error
  ) %>%
  mutate(lower_ci = value - (1.96 * value_se),  # calculate lower bound of 95% CI
         upper_ci = value + (1.96 * value_se),  # calculate lower bound of 95% CI
         percent = round(value * 100, 0))       # convert mean to percentage

# set title for the plot
plot_overall$title = "Urban"

# create bar plot for urban data
p_urban <- ggplot(plot_overall, aes(fill = test_result, x = home_type2)) +
  geom_bar(aes(y = percent), position = "stack", stat = "identity") +  # Change to stacked position
  theme_manuscript() +
  scale_x_discrete(labels = c("Agricultural worker \n household (HH)", "Non-agricultural \n worker HH")) +
  scale_fill_manual(name = "Malaria test result", labels = c("Negative", "Positive"), values = color) +
  geom_text(aes(label = paste0(percent, "% (se=", round(value_se * 100, 1), ")"), y = percent),  # Include "se" in brackets
            position = position_stack(vjust = 0.5),  # Adjust label position for stack
            color = "black", vjust = 0.5) +
  labs(x = "", y = "") +
  facet_wrap(vars(title)) + # create facets by title
  theme(strip.text.x = element_text(size = 12, color = "black")) +
  coord_cartesian(ylim = c(0, 100))  # Adjust y-limit for stacked bars

# display the urban plot
p_urban

## -----------------------------------------------------------------------------------------------------------------------------------------
### Figure 3 - Rural Plot (# children tested positive by agric/non-agric households in rural areas)
## -----------------------------------------------------------------------------------------------------------------------------------------

plot_df_rm <- rural_df %>% dplyr::select(country_year.x, home_type2, code_year, test_result, id, strat, wt)

plot_overall <- plot_df_rm %>%
  as_survey_design(ids = id, strata = strat, nest = TRUE, weights = wt) %>%
  group_by(home_type2, test_result) %>%
  dplyr::summarise(
    value = survey_mean(vartype = "se")
  ) %>%
  mutate(
    lower_ci = value - (1.96 * value_se),  # lower bound of 95% CI
    upper_ci = value + (1.96 * value_se),  # upper bound of 95% CI
    percent = round(value * 100, 0)        # convert to percentages
  )
plot_overall$title <- "Rural"

p_rural <- ggplot(plot_overall, aes(fill = test_result, x = home_type2)) +
  geom_bar(aes(y = percent), position = "stack", stat = "identity") +  # Change to stacked position
  theme_manuscript() +
  scale_x_discrete(labels = c("Agricultural worker \n household (HH)", "Non-agricultural \n worker HH")) +
  scale_fill_manual(name = "Malaria test result", labels = c("Negative", "Positive"), values = color) +
  geom_text(aes(label = paste0(percent, "% (se=", round(value_se * 100, 1), ")"), y = percent),  # Include "se" in brackets
            position = position_stack(vjust = 0.5),  # Adjust label position for stack
            color = "black", vjust = 0.5) +
  labs(x = "", y = "") +
  facet_wrap(vars(title)) +
  theme(strip.text.x = element_text(size = 12, color = "black")) +
  coord_cartesian(ylim = c(0, 100)) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank())

# display the rural plot
p_rural

# combine the urban and rural plots, display and save as png
p_3a = p_urban + p_rural + plot_layout(guides = "collect") & theme(legend.position = 'none')
p_3a

## -----------------------------------------------------------------------------------------------------------------------------------------
### Figure 6 - Plot: Net Use vs Occupation Category and Malaria Positivity
## -----------------------------------------------------------------------------------------------------------------------------------------

# define colors for the plot
color = c( "#621244", "#efeddb")

# create a dataframe for urban data with selected columns
plot_df_un<- urban_df %>% dplyr::select(country_year.x, home_type2, code_year, u5_net_use, id, strat, wt)

# prepare urban data for plotting with survey design
plot_urban = plot_df_un %>% as_survey_design(ids= id,strata=strat,nest=T,weights= wt) %>%
  mutate(net_use = ifelse(u5_net_use == 1, "Slept under \n a net", "Did not sleep \n under a net")) %>%
  group_by(home_type2, net_use) %>% # group by home type and net use
  dplyr::summarise(value = round(survey_total(),0)) %>% # calculate total net use
  mutate(percent = round(value/sum(value) *100, 0)) # calculate percentage of net use

# set title for urban plot
plot_urban$title = "Urban"

# create bar plot for urban data
p_net_u <- ggplot(plot_urban, aes(fill=net_use, x= home_type2)) +
  geom_bar(aes(y = value), position="stack", stat = "identity")+ # stacked bar chart
  theme_manuscript()+
  scale_x_discrete(labels = c("Agricultural\nWorker HH", "Non-Agricultural\nWorker HH")) +
  scale_fill_manual(name = "", values= color)+ # set custom fill colors
  geom_text(aes(label = paste0(value, " ", "(", percent, "%", ")"), y = value),
            position = position_stack(vjust = 0.5),
            color = "white") +
  labs(x = "", y  = "Positive malaria tests: Children \n6-59 months (15 DHS datasets)") +
  facet_wrap(vars(title))+
  theme(strip.text.x = element_text(
    size = 12, color = "black")) +
  coord_cartesian(ylim = c(0, 44000))

# create a dataframe for rural data with selected columns
plot_df_rn <- rural_df %>%
  dplyr::select(country_year.x, home_type2, code_year, u5_net_use, id, strat, wt)

# prepare rural data for plotting with survey design
plot_rural = plot_df_rn %>% as_survey_design(ids= id,strata=strat,nest=T,weights= wt) %>%
  mutate(net_use = ifelse(u5_net_use == 1, "Slept under \n a net", "Did not sleep \n under a net")) %>%
  group_by(home_type2, net_use) %>% # group by home type and net use
  dplyr::summarise(value = round(survey_total(),0)) %>% # calculate total net use
  mutate(percent = round(value/sum(value) *100, 0)) # calculate percentage of net use

# set title for rural plot
plot_rural$title = "Rural"

# create bar plot for rural data
p_net_r<-ggplot(plot_rural, aes(fill=net_use, x= home_type2)) +
  geom_bar(aes(y = value), position="stack", stat = "identity")+
  theme_manuscript()+
  scale_x_discrete(labels = c("Agricultural\nWorker HH", "Non-Agricultural\nWorker HH")) +
  scale_fill_manual(name = "", values= color)+
  geom_text(aes(label = paste0(value, " ", "(", percent, "%", ")"), y = value),
            position = position_stack(vjust = 0.5),
            color = "white") +
  labs(x = "", y  = "Positive malaria tests: Children \n6-59 months (15 DHS datasets)") +
  facet_wrap(vars(title))+
  theme(strip.text.x = element_text(
    size = 12, color = "black")) +
  coord_cartesian(ylim = c(0, 44000)) +
  theme(axis.text.y = element_blank(),
        axis.ticks.y = element_blank(),
        axis.title.y = element_blank())

# combine urban and rural plots
p_6 <- p_net_u + p_net_r

# create a combined plot with legend positioned at the bottom and save as png
p_6 <- p_net_u + p_net_r + plot_layout(guides = "collect") & theme(legend.position = 'bottom')
p_6

combined <- grid.arrange(p_3a, p_6, nrow = 2, ncol = 2)
combined
