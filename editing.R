
agric_palette <- c("#018571", "#80cdc1", "#f5f5f5", "#dfc27d", "#a6611a")

# define the top 3 populous subdivisions for each country (all data from Wikipedia, which shows census data)
top_populous_subdivisions <- list(
  "Angola" = c("Luanda", "Huíla", "Benguela"),
  "Burkina Faso" = c("Centre", "Haut-Bassins", "Est"),
  "Benin" = c("Atlantique", "Borgou", "Ouémé"),
  "Burundi" = c("Gitega", "Ngozi", "Muyinga"),
  "Congo Democratic Republic" = c("Kinshasa", "Nord-Kivu", "Sud-Kivu"),
  "Côte d'Ivoire" = c("Abidjan", "Montagnes", "Sassandra-Marahoué"),
  "Cameroon" = c("Centre", "Adamaoua", "Est"),
  "Ghana" = c("Greater Accra", "Ashanti", "Eastern"),
  "Guinea" = c("Boké", "Conakry", "Faranah"),
  "Madagascar" = c("Analamanga", "Vakinankaratra", "Atsimo-Andrefana"),
  "Mali" = c("Sikasso", "Koulikoro", "Ségou"),
  "Mozambique" = c("Cabo Delgado", "Gaza", "Inhambane"),
  "Nigeria" = c("Kano", "Lagos", "Katsina"),
  "Togo" = c("Maritime", "Plateaux", "Savanes"),
  "Uganda" = c("Central", "Eastern", "Western")
)

interval_labels <- c("[0, 0.2]", "[0.2, 0.4]", "[0.4, 0.6]", "[0.6, 0.8]", "[0.8, 1]")

for (country_code in names(gps_survey_data)) { 
  # extract the specific country from the shapefile data
  country_shape <- afr.shp.base %>% filter(ISO2 == country_code)
  
  # extract the survey and gps cluster data for the current country
  country_data <- gps_survey_data[[country_code]]
  
  # extract subdivision data for the current country and make it a spatial object
  spat_country_subd <- st_as_sf(subdivision_files[[country_code]]) %>% st_make_valid()
  
  # extract survey data for the current country and make it a spatial object
  spat_gps_survey_data <- st_as_sf(gps_survey_data[[country_code]], coords = c("LONGNUM", "LATNUM"), crs = st_crs(spat_country_subd)) %>% st_make_valid()
  
  # perform spatial join to find which geographic subdivision each cluster point is in
  gps_survey_data_with_subdivision <- st_join(spat_gps_survey_data, spat_country_subd, join = st_within)
  
  # calculate agric hh proportion within each geographic subdivision
  subd_averages <- gps_survey_data_with_subdivision %>%
    group_by(NAME_1) %>%  # Use subdivision name
    summarize(subd_proportion = mean(agric_proportion, na.rm = TRUE))
  
  # clean up any invalid geometries in the joined data - fixed error
  gps_survey_data_with_subdivision <- gps_survey_data_with_subdivision %>% st_make_valid()
  
  # join subd_averages to the gps_survey_data_with_subdivision
  gps_survey_data_with_subdivision <- gps_survey_data_with_subdivision %>% st_join(subd_averages, join = st_within)
  
  # get the bounding box of the country to help scale the plots uniformly
  bbox <- st_bbox(country_shape)
  
  # define a consistent zoom level for all countries
  buffer <- 0.1  # Adjust this value to control how much larger the bounding box will be
  
  # adjust the bounding box to create uniform scaling
  xlim_range <- c(bbox["xmin"] - buffer, bbox["xmax"] + buffer)
  ylim_range <- c(bbox["ymin"] - buffer, bbox["ymax"] + buffer)
  
  # prepare the subd_proportion data
  subd_prop_data <- gps_survey_data_with_subdivision %>% 
    st_drop_geometry() %>%
    select(NAME_1.x, subd_proportion) %>% 
    distinct()
  
  # join this to the spatial subdivision data
  spat_country_subd_with_prop <- spat_country_subd %>%
    left_join(subd_prop_data, by = c("NAME_1" = "NAME_1.x"))
  
  # remove duplicate entries in spat_country_subd_with_prop
  spat_country_subd_with_prop <- spat_country_subd_with_prop %>%
    group_by(NAME_1) %>%
    filter(!(is.na(subd_proportion) & n() > 1)) %>%
    ungroup()
  
  # add a new column to flag the top 3 most populous subdivisions
  country_name <- country_names[country_code]
  top_subs <- top_populous_subdivisions[[country_name]]
  spat_country_subd_with_prop <- spat_country_subd_with_prop %>%
    mutate(is_top_populous = ifelse(NAME_1 %in% top_subs, TRUE, FALSE))
  
  # define breaks for 20% intervals (0, 0.2, 0.4, 0.6, 0.8, 1)
  even_cuts <- seq(0, 1, by = 0.2)
  
  # categorize subd_proportion by these even 20% intervals
  spat_country_subd_with_prop$prop_cat <- cut(
    spat_country_subd_with_prop$subd_proportion,
    breaks = even_cuts,
    labels = c(1, 2, 3, 4, 5),
    include.lowest = TRUE
  )
  
  # apply the same categorization for agricultural household proportions in country_data
  country_data$prop_cat <- cut(
    country_data$agric_proportion,
    breaks = even_cuts,
    labels = c(1, 2, 3, 4, 5),
    include.lowest = TRUE
  )
  
  # create the plot
  country_agric_map <- ggplot() +
    
    # color subdivisions by proportion using the agric_palette
    geom_sf(data = spat_country_subd_with_prop, aes(fill = prop_cat, geometry = geometry), color = "white") +
    
    # apply agric_palette to the fill scale
    scale_fill_manual(values = agric_palette, labels = interval_labels, na.value = "gray") +
    
    # add cluster data points with jitter to prevent overlap
    geom_jitter(data = country_data, aes(x = LONGNUM, y = LATNUM, fill = prop_cat), 
                color = "black", size = 2.5, shape = 21, stroke = 0.2, 
                width = 0.05, height = 0.05, show.legend = FALSE) +  # Adjust width/height for jitter effect
    scale_color_gradientn(colors = agric_palette) +
    
    # add thick yellow outlines for top populous subdivisions
    geom_sf(data = filter(spat_country_subd_with_prop, is_top_populous == TRUE), 
            aes(geometry = geometry), 
            fill = NA, 
            color = "yellow", 
            linewidth = 1,
            inherit.aes = FALSE) +
    
    geom_sf(data = country_shape, aes(geometry = geometry), fill = NA, color = "black") +  # Country borders
    
    # set consistent zoom level
    coord_sf(xlim = xlim_range, ylim = ylim_range, datum = NA) +
    theme_void() +  # Remove axes and labels
    
    # customize labels and title
    labs(title = paste(country_names[country_code], country_data$year_combo),
         fill = "Proportion of \nAgricultural \nHouseholds") +
    
    # adjust theme for a clean look
    theme(
      axis.text = element_blank(),
      axis.ticks = element_blank(),
      axis.title = element_blank()
    )
  
  # assign the map plot to a variable
  assign(paste0(country_code, "_subd_plot"), country_agric_map)
}

print(BU_subd_plot)
