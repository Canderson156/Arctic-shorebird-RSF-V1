#### collating the best GIS coordinates for each plot




#GROUP 3

good_g3_gis <- GIS_shapefiles %>%
  filter(Plot %in% g3_yes_gis$Plot) %>%
  distinct(Plot) %>%
  mutate(dup = allDuplicated(Plot)) %>%
  filter(dup == FALSE) %>%
  select(-dup)




good_g3a_alert_dups <- shapefile_proj_list[[34]] %>%
  st_as_sf() %>%
  select(Plot = FN, Historic_Plot = LABEL) %>%
  filter(Plot %in% g3_yes_gis$Plot) %>%
  distinct(Plot, Historic_Plot) %>%
  mutate(dup = allDuplicated(Plot)) %>%
  filter(dup == TRUE)  %>%
  select(-dup)

















