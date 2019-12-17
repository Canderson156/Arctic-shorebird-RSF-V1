

#create a new column call corner
#all existing coordinates are SW
#duplicate the dataframe following the code below to crteate the other corners
#order them accordingly





#####################################


#corner becomes an ordered factor so that the points move around the plot in a circle. SW2 leaves a place for duplicating the SW corner becasue spatial polygons want a duplicate point to close the loop
allplots2$corner <- factor(allplots2$corner, levels = c("SW", "NW", "NE", "SE", "SW2"))


#Base the other coordinates off the southwest corner and duplicate the southwest corner

SW <- filter(allplots2, corner == "SW")
SW2 <- filter(allplots2, corner == "SW") %>%
  mutate(corner = "SW2")


SE <- SW %>%
  mutate(Easting = Easting + 300, corner = "SE") 

NW <- SW %>%
  mutate(Northing = Northing + 400, corner = "NW")

NE <- SE %>%
  mutate(Northing = Northing + 400, corner = "NE") 



allplots3 <- rbind(SW, NW, NE, SE, SW2)
allplots3 <- arrange(allplots3, plot, corner)
rm(SW, SW2, SE, NW, NE, allplots1, allplots2)



#



# list of all utm zones and their crs
utms <- sort(unique(allplots$zone))

utm_crs <- vector(mode = "list", length = length(utms))
epsg <- seq(from = 26907, length.out = length(utms))

for(i in 1:length(utms)){
  crstext <- paste("+init=EPSG:", epsg[i], sep = "")
  utm_crs[[i]] <- CRS(crstext)
}
names(utm_crs) <- utms





#




# split into a list where each element is the plots in one utm zone
allplots4 <- group_split(allplots3, zone)
names(allplots4) <- utms

# use above to create list of polygons by UTM zone
prism_polys <- lapply(allplots4, df_to_SpatialPolygons, keys = "plot", coords = c("Easting", "Northing"), CRS())

#give each element of the list it's appropriate CRS
for(i in 1:length(utms)){
  proj4string(prism_polys[[i]]) <- utm_crs[[i]]
}





#



#reproject each one


prism_polys1 <- lapply(prism_polys, spTransform, CRSobj = NPLAEA)


#combine them together


allprismpolys <-  bind(prism_polys1[[1]], prism_polys1[[2]])
for(i in 3:length(prism_polys1)){
  allprismpolys <- bind(allprismpolys, prism_polys1[[i]])
}

rm(prism_polys, prism_polys1)



#do they have any data associated?

#turn into a spatial polygon with plot ID and region


plots_id <- do.call(rbind.data.frame, allplots4) %>%
  select(plot, region, quality) %>%
  unique()

allprismpolysDF <- SpatialPolygonsDataFrame(allprismpolys, plots_id, match.ID = F)  
























# I don't think I need to do this step any more since I just have the one corner




##rename UTM_zone to avoid gathering

SB <- rename(SB, zone = UTM_Zone)



SB1 <- SB %>% 
  gather(key = "axis", value = "coordinate", starts_with("UTM")) %>%
  mutate(axis = str_sub(axis, 5))





#########################


#gather everything that starts with UTM into one columns, seperate the names to get the different pieces of information from the original column name, get rid of the URM column which no longer serves a purpose
allplots1 <- allplots %>% 
  gather(key = "corner", value = "coordinate", starts_with("UTM")) %>%
  separate(corner, c("UTM", "corner", "axis"), c("_")) %>%
  select(-UTM)

#spread out the eastings from the northings
allplots2 <- allplots1 %>%
  spread(axis, coordinate)





#


