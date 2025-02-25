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
  summarise(
    value = survey_mean(vartype = "se"),
    count = n()  # Count of individuals in each category
    # calculate survey mean with standard error
  ) %>%
  mutate(lower_ci = value - (1.96 * value_se),  # calculate lower bound of 95% CI
         upper_ci = value + (1.96 * value_se),  # calculate lower bound of 95% CI
         percent = round(value * 100, 0),        # convert mean to percentage
         lower_ci_perc = lower_ci * 100,
         upper_ci_perc = upper_ci * 100)       

# set title for the plot
plot_overall$title = "Urban"

# create unstacked bar plot for urban data (includes confidence intervals and counts)
p_urban <- ggplot(plot_overall, aes(fill = test_result, x = home_type2)) + 
  geom_bar(aes(y = percent), position = "dodge", stat = "identity") +  # Change to dodged position
  geom_errorbar(aes(ymin = lower_ci_perc, ymax = upper_ci_perc), 
                width = 0.2,  # Width of the error bars
                position = position_dodge(0.9),  # Align error bars with bars
                color = "black") +  # Color for error bars
  theme_manuscript() +
  scale_x_discrete(labels = c("Agricultural\n Worker HH", "Non-Agricultural\n Worker HH")) +
  scale_fill_manual(name = "Malaria Test Result", labels = c("Negative", "Positive"), values = color) + 
  geom_text(aes(label = paste0(percent, "%\n(", count, ")"), y = percent),  # Combined label for percent and count
            position = position_dodge(0.9),  # Adjust label position for dodged bars
            color = "black", vjust = -0.4, hjust = 0.5, size = 4) +  # Centered in the bar, with smaller font size
  labs(x = "", y = "") + 
  theme(strip.text.x = element_text(size = 12, color = "black"), 
        axis.text.x = element_text(size = 12)) +
  facet_wrap(vars(title)) +  # create facets by title
  coord_cartesian(ylim = c(0, 100))  # Adjust y-limit for stacked bars

# display the urban plot
p_urban

## -----------------------------------------------------------------------------------------------------------------------------------------
### Figure 3 - Rural Plot (# children tested positive by agric/non-agric households in rural areas)
## -----------------------------------------------------------------------------------------------------------------------------------------

plot_df_rm <- rural_df %>% dplyr::select(country_year.x, home_type2, code_year, test_result, id, strat, wt)

plot_overall <- plot_df_rm %>% 
  as_survey_design(ids = id, strata = strat, nest = TRUE, weights = wt) %>%
  group_by(home_type2, test_result) %>% # group data by home type and test result  
  summarise(
    value = survey_mean(vartype = "se"),
    count = n()  # Count of individuals in each category
    # calculate survey mean with standard error
  ) %>%
  mutate(lower_ci = value - (1.96 * value_se),  # calculate lower bound of 95% CI
         upper_ci = value + (1.96 * value_se),  # calculate lower bound of 95% CI
         percent = round(value * 100, 0),        # convert mean to percentage
         lower_ci_perc = lower_ci * 100,
         upper_ci_perc = upper_ci * 100)    
plot_overall$title <- "Rural"

p_rural <- ggplot(plot_overall, aes(fill = test_result, x = home_type2)) + 
  geom_bar(aes(y = percent), position = "dodge", stat = "identity") +  # Change to dodged position
  geom_errorbar(aes(ymin = lower_ci_perc, ymax = upper_ci_perc), 
                width = 0.2,  # Width of the error bars
                position = position_dodge(0.9),  # Align error bars with bars
                color = "black") +  # Color for error bars
  theme_manuscript() +
  scale_x_discrete(labels = c("Agricultural\n Worker HH", "Non-Agricultural\n Worker HH")) +
  scale_fill_manual(name = "Malaria Test Result", labels = c("Negative", "Positive"), values = color) + 
  geom_text(aes(label = paste0(percent, "%\n(", count, ")"), y = percent),  # Combined label for percent and count
            position = position_dodge(0.9),  # Adjust label position for dodged bars
            color = "black", vjust = -0.4, hjust = 0.5, size = 4) +  # Centered in the bar, with smaller font size
  labs(x = "", y = "") + 
  theme(strip.text.x = element_text(size = 12, color = "black"), 
        axis.text.x = element_text(size = 12)) +
  facet_wrap(vars(title)) +  # create facets by title
  coord_cartesian(ylim = c(0, 100))  # Adjust y-limit for stacked bars

# display the rural plot
p_rural

# combine the urban and rural plots, display and save as png
positivity_plots = p_urban + p_rural + plot_layout(guides = "collect") & theme(legend.position = 'none')

## -----------------------------------------------------------------------------------------------------------------------------------------
### Figure 6 - Plot: Net Use vs Occupation Category and Malaria Positivity
# y-axis is number of positive malaria tests in children under 5
## -----------------------------------------------------------------------------------------------------------------------------------------

# define colors for the plot
color = c( "#1750AC", "#73B9EE")

## -----------------------------------------------------------------------------------------------------------------------------------------
### Urban Plot (# children that used nets by agric/non-agric households in urban areas)
## -----------------------------------------------------------------------------------------------------------------------------------------

# create a dataframe for urban data with selected columns
plot_df_un <- urban_df %>% dplyr::select(country_year.x, home_type2, code_year, u5_net_use, id, strat, wt)

# make net use factor
plot_df_un$u5_net_use <- as.factor(plot_df_un$u5_net_use)

plot_overall <- plot_df_un %>% 
  as_survey_design(ids = id, strata = strat, nest = TRUE, weights = wt) %>%
  group_by(home_type2, u5_net_use) %>% # group data by home type and test result  
  summarise(
    value = survey_mean(vartype = "se"),
    count = n()  # Count of individuals in each category
    # calculate survey mean with standard error
  ) %>%
  mutate(lower_ci = value - (1.96 * value_se),  # calculate lower bound of 95% CI
         upper_ci = value + (1.96 * value_se),  # calculate lower bound of 95% CI
         percent = round(value * 100, 0),        # convert mean to percentage
         lower_ci_perc = lower_ci * 100,
         upper_ci_perc = upper_ci * 100)    

# set title for urban plot
plot_overall$title = "Urban"

# create bar plot for urban data
p_net_urban <- ggplot(plot_overall, aes(fill = u5_net_use, x = home_type2)) + 
  geom_bar(aes(y = percent), position = "dodge", stat = "identity") +  # Change to dodged position
  geom_errorbar(aes(ymin = lower_ci_perc, ymax = upper_ci_perc), 
                width = 0.2,  # Width of the error bars
                position = position_dodge(0.9),  # Align error bars with bars
                color = "black") +  # Color for error bars
  theme_manuscript() +
  scale_x_discrete(labels = c("Agricultural\n Worker HH", "Non-Agricultural\n Worker HH")) +
  scale_fill_manual(name = "Net Use", labels = c("Did not sleep under a net", "Slept under a net"), values = color) + 
  geom_text(aes(label = paste0(percent, "%\n(", count, ")"), y = percent),  # Combined label for percent and count
            position = position_dodge(0.9),  # Adjust label position for dodged bars
            color = "black", vjust = -0.4, hjust = 0.5, size = 4) +  # Centered in the bar, with smaller font size
  labs(x = "", y = "") + 
  theme(strip.text.x = element_text(size = 12, color = "black"), 
        axis.text.x = element_text(size = 12)) +
  facet_wrap(vars(title)) +  # create facets by title
  coord_cartesian(ylim = c(0, 100))  # Adjust y-limit for stacked bars

p_net_urban

## -----------------------------------------------------------------------------------------------------------------------------------------
### Rural Plot (# children that used nets by agric/non-agric households in rural areas)
## -----------------------------------------------------------------------------------------------------------------------------------------

# create a dataframe for rural data with selected columns
plot_df_rn <- rural_df %>% dplyr::select(country_year.x, home_type2, code_year, u5_net_use, id, strat, wt)

# make net use factor
plot_df_rn$u5_net_use <- as.factor(plot_df_rn$u5_net_use)

plot_overall <- plot_df_rn %>% 
  as_survey_design(ids = id, strata = strat, nest = TRUE, weights = wt) %>%
  group_by(home_type2, u5_net_use) %>% # group data by home type and test result  
  summarise(
    value = survey_mean(vartype = "se"),
    count = n()  # Count of individuals in each category
    # calculate survey mean with standard error
  ) %>%
  mutate(lower_ci = value - (1.96 * value_se),  # calculate lower bound of 95% CI
         upper_ci = value + (1.96 * value_se),  # calculate lower bound of 95% CI
         percent = round(value * 100, 0),        # convert mean to percentage
         lower_ci_perc = lower_ci * 100,
         upper_ci_perc = upper_ci * 100)    

# set title for rural plot
plot_overall$title = "Rural"

# create bar plot for urban data
p_net_rural <- ggplot(plot_overall, aes(fill = u5_net_use, x = home_type2)) + 
  geom_bar(aes(y = percent), position = "dodge", stat = "identity") +  # Change to dodged position
  geom_errorbar(aes(ymin = lower_ci_perc, ymax = upper_ci_perc), 
                width = 0.2,  # Width of the error bars
                position = position_dodge(0.9),  # Align error bars with bars
                color = "black") +  # Color for error bars
  theme_manuscript() +
  scale_x_discrete(labels = c("Agricultural\n Worker HH", "Non-Agricultural\n Worker HH")) +
  scale_fill_manual(name = "Net Use", labels = c("Did not sleep under a net", "Slept under a net"), values = color) + 
  geom_text(aes(label = paste0(percent, "%\n(", count, ")"), y = percent),  # Combined label for percent and count
            position = position_dodge(0.9),  # Adjust label position for dodged bars
            color = "black", vjust = -0.4, hjust = 0.5, size = 4) +  # Centered in the bar, with smaller font size
  labs(x = "", y = "") + 
  theme(strip.text.x = element_text(size = 12, color = "black"), 
        axis.text.x = element_text(size = 12)) +
  facet_wrap(vars(title)) +  # create facets by title
  coord_cartesian(ylim = c(0, 100))  # Adjust y-limit for stacked bars

p_net_rural


# combine plots and remove legents
net_plots = p_net_urban + p_net_rural + plot_layout(guides = "collect") & theme(legend.position = 'none')

# combine malaria and net plots
final_fig2 <- positivity_plots / net_plots
final_fig2

ggsave(paste0(FigDir, "/pdf_figures/", Sys.Date(),"_final_fig2.pdf"), final_fig2, width = 8, height = 10) 

