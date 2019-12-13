




## all geogrpahic data should be extracted from the GIS files as if may be unreliable in the excel spreadsheet: 

# region
# subregion
# plot coordinates
# plot area
# quality? others? what else is in the GIS files?




# compare GIS data vs excel data

#Weird shapes in excel file - do we see the same weird shapes in GIS files? 
#do the GIS files represent what WAS done or what SHOUld have been done on paper?





#looking at the data from region 12


reg12gis <- st_read("data/Shapefiles/Reg12_Plots_Surveyed.shp")

#removed quality and area for now becasue some were different within the same plot
r12plots <- allplots %>%
  filter(Region_code == 12) %>%
  select(Plot, 
         Survey_Lead, 
         Sub_region_name, 
         Sub_region_code,
         Plot_type,
         Selection_method) %>%
  distinct()

missing <- r12plots %>%
  filter(Plot %notin% reg12$Plot_Name_)


#how would I do this without subsetting the whole thing
test <- reg12 %>%
  filter(Plot_Name_ %notin% r12plots$Plot)





