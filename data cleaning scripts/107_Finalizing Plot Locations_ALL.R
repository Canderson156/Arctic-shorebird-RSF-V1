#g0_no_coords: asked Tyler and Isabel to look for them (14)
#g1_field_modified: 76 are fine, 76 need confirmation from Isaberl (76)
#g2_no_gis: script is prepared, waiting for corrections from Isabel (18)
#g2a_needs_transformation: still needs transformation. 2 diferent types
#g2b_point: needs a different script to be a point instead of a polygon
#g3: needs visual verification
#good_g3a_alert_dups: needs visual verification
    #needs one more column than the others





##try merging the ones that are ready together



all_polygons <- rbind(good_g1, good_g2, good_g3_gis)

all_polygons$Historic_Plot <- NA


all_polygons <- rbind(all_polygons, good_g3a_alert_dups)




good_g0 <- st_sf(Plot = g0_no_coords$Plot,
                 geometry = st_sfc(st_point()),
                 crs = NAD83)



all_points <- rbind(good_g0, good_g2a, good_g2b) 



#visualize them all

st_write(all_polygons, "exported/all_polygons_NAD83.shp")
st_write(all_points, "exported/all_points_NAD83.shp")



#as a dataframe

all_points_df <- as.data.frame(all_points) %>%
  mutate(geometry = str_remove(geometry, "c\\("), geometry = str_remove(geometry, "\\)")) %>%
  separate(geometry, c("long", "lat"), ", ")






all_polygons$L2 <- 1:nrow(all_polygons)

all_polygons <- all_polygons %>%
  mutate(Plot = ifelse(is.na(Historic_Plot), Plot, paste(Plot, Historic_Plot, sep = "_")))

all_polygons_coords <- data.frame(st_coordinates(all_polygons)) %>%
  select(-L1)

all_polygons_df <- as.data.frame(all_polygons) %>%
  select(-geometry)

test2 <- merge(all_polygons_df, all_polygons_coords) %>%
  select(-L2) %>%
  distinct()


test3 <- test2 %>%
  unite(coords, X,Y) %>%
  group_by(Plot) %>%
  mutate(index = 1:n()) %>%
  ungroup()


coord_cols <- data.frame(col = paste("coords", 1:max(test3$index), sep = "_"),
                         long = paste("long", 1:max(test3$index), sep = "_"),
                         lat = paste("lat", 1:max(test3$index), sep = "_"),
                         stringsAsFactors = FALSE)


test4 <- test3 %>%
  mutate(index = paste("coords", 1:n(), sep = "_")) %>%
  spread(index, coords)



for(i in 1:ncol(test4)){
  x <- colnames(test4[,i])
  if(x %in% coord_cols$col){
    c <- coord_cols[coord_cols$col == x,]
    separate(test4, x, c(c[,2]), c[,3])
  }
}


### would it be worth it to jut do this the long way?
## what if I just export it as a shapefile and send it to Laurent? then he can import it as a shapefile however he wants
#try importing my version as a shapefile





test_poly <- readOGR("exported/all_polygons_NAD83.shp")





