#### geospatial data quality control



#### importing tylers shapefiles



# file paths of all the PRISM geodatabases
#note that the regions are in the order 1,10,11,12,2 .... 

filegdblist <- paste("E:\\2019 PRISM GIS v2\\", list.files("E:\\2019 PRISM GIS v2"), sep = "")

#re-order so that regions are sequential
filegdblist <- filegdblist[c(1,5:12,2:4)] 


#create a list of all of the objects inseide those geodatabases


subset(ogrDrivers(), grepl("GDB", name))  # I have no idea what this does, got it from this internet

fc_list <- lapply(filegdblist, ogrListLayers)


#selecting the features that I want to keep
#most of the have _Plots and _Surveyed_Plots. I only kept _Surveyed_Plots, but might want to investigate plots if some are missing
# "Reg7_Rassmussen_Plots_Surveyed" and "Rassmussen_Surveyed_Plots" are identical, so I only used the first one

fc_list_plots <- fc_list
fc_list_plots[[1]] <- fc_list[[1]][3]
fc_list_plots[[2]] <- fc_list[[2]][4:7] 
fc_list_plots[[3]] <- fc_list[[3]][4:5] # I left out the 2019 data
fc_list_plots[[4]] <- fc_list[[4]][8:9] #I left out intensive plots
fc_list_plots[[5]] <- fc_list[[5]][7]
fc_list_plots[[6]] <- fc_list[[6]][4:10]
fc_list_plots[[7]] <- fc_list[[7]][c(2:3,5:7)]
fc_list_plots[[8]] <- fc_list[[8]][8]
fc_list_plots[[9]] <- fc_list[[9]][c(4:6,8)]
fc_list_plots[[10]] <- fc_list[[10]][8:13] 
fc_list_plots[[11]] <- fc_list[[11]][7] 
fc_list_plots[[12]] <- fc_list[[12]][7]


# Read the feature class


# for loading shapefiles from geodatabase. Keeps the dsn(filepath) constant, and loads multiple layers from that geodatabase
readOGR_multi <- function(DSN, LAYER_VEC){
  shapefiles <- lapply(LAYER_VEC, readOGR, dsn = DSN)
  return(shapefiles)
}


#create a list of all plot shapefiles
shapefile_list <- readOGR_multi(filegdblist[[1]], fc_list_plots[[1]])

for(i in 2:length(filegdblist)){
  add <- readOGR_multi(filegdblist[[i]], fc_list_plots[[i]])
  shapefile_list <- c(shapefile_list, add)
}




#get them all into the same crs

shapefile_proj_list <- lapply(shapefile_list, spTransform, CRSobj = NPLAEA)




#Adding the corrections to plot names that Laurent created
#See MatchingPoltID_Larent for deails about why each was performed
#I should probably add this imformation here

shapefile_proj_list[[1]]@data$FN <- as.character(shapefile_proj_list[[1]]@data$Plot_Name_New) #Region 1
shapefile_proj_list[[1]]@data$FN <- gsub("\r\n", "", shapefile_proj_list[[1]]@data$FN)
shapefile_proj_list[[1]]@data$FN[shapefile_proj_list[[1]]@data$Unique_Plot_ID == "69091B1"] <- "QC-0012D"
shapefile_proj_list[[1]]@data$FN[shapefile_proj_list[[1]]@data$Unique_Plot_ID == "39426B1"] <- "QC-0016D"
shapefile_proj_list[[1]]@data$FN[shapefile_proj_list[[1]]@data$Unique_Plot_ID == "33783D1"] <- "QC-0057C"
shapefile_proj_list[[1]]@data$FN[shapefile_proj_list[[1]]@data$Unique_Plot_ID == "281250E1"] <- "QC-0091C"
shapefile_proj_list[[1]]@data$FN[shapefile_proj_list[[1]]@data$Unique_Plot_ID == "230131D1"] <- "QC-0094D"
shapefile_proj_list[[1]]@data$FN[shapefile_proj_list[[1]]@data$Unique_Plot_ID == "162572C1"] <- "QC-0095C"
shapefile_proj_list[[1]]@data$FN[shapefile_proj_list[[1]]@data$Unique_Plot_ID == "136350C1"] <- "QC-0121B"
shapefile_proj_list[[1]]@data$FN[shapefile_proj_list[[1]]@data$Unique_Plot_ID == "82565B1"] <- "QC-0138C"

shapefile_proj_list[[2]]@data$FN <- as.character(shapefile_proj_list[[2]]@data$Plot_Name) #Region 2.1

shapefile_proj_list[[3]]@data$FN <- as.character(shapefile_proj_list[[3]]@data$Plot_Name) #Region 2.2

shapefile_proj_list[[4]]@data$FN <- as.character(shapefile_proj_list[[4]]@data$Plot_Name) #Region 2.3

shapefile_proj_list[[5]]@data$FN <- as.character(shapefile_proj_list[[5]]@data$Plot_Name_1) #Region 2.4

shapefile_proj_list[[6]]@data$FN <- as.character(shapefile_proj_list[[6]]@data$Plot_Name_New) #Region 3.1

shapefile_proj_list[[7]]@data$FN <- as.character(shapefile_proj_list[[7]]@data$Plot_Name_New) #Region 3.2

shapefile_proj_list[[8]]@data$FN <- as.character(shapefile_proj_list[[8]]@data$Plot_Name_New) #Region 4.2

shapefile_proj_list[[9]]@data$FN <- as.character(shapefile_proj_list[[9]]@data$Plot_Name_New) #Region 4.1
shapefile_proj_list[[9]]@data$FN[shapefile_proj_list[[9]]@data$FN == "SHI-0009B\r\nSHI-0009B\r\n"] <- "SHI-0009B"

shapefile_proj_list[[10]]@data$FN <- as.character(shapefile_proj_list[[10]]@data$Plot_Name) #Region 5

shapefile_proj_list[[11]]@data$FN <- as.character(shapefile_proj_list[[11]]@data$Plot) #Region 6.1

shapefile_proj_list[[12]]@data$FN <- as.character(shapefile_proj_list[[12]]@data$Plot_Name_New) #Region 6.2

shapefile_proj_list[[13]]@data$FN <- as.character(shapefile_proj_list[[13]]@data$Unique_Plot_ID) #Region 6.3
shapefile_proj_list[[13]]@data$FN[shapefile_proj_list[[13]]@data$FN == "AMA-005\r\n"] <- "AMA-005"

shapefile_proj_list[[14]]@data$FN <- as.character(shapefile_proj_list[[14]]@data$plot_name) #Region 6.4

shapefile_proj_list[[15]]@data$FN <- as.character(shapefile_proj_list[[15]]@data$Plot) #Region 6.5

shapefile_proj_list[[16]]@data$FN <- as.character(shapefile_proj_list[[16]]@data$NAME) #Region 6.6

shapefile_proj_list[[17]]@data$FN <- as.character(shapefile_proj_list[[17]]@data$Plot_Name_New) #Region 6.7
shapefile_proj_list[[17]]@data$FN[shapefile_proj_list[[17]]@data$Unique_Plot_ID == "1797377_R6"] <- "ARV-0105C"
shapefile_proj_list[[17]]@data$FN[shapefile_proj_list[[17]]@data$Unique_Plot_ID == "1804180_R6"] <- "ARV-0107C"
shapefile_proj_list[[17]]@data$FN[shapefile_proj_list[[17]]@data$Unique_Plot_ID == "887080_R6"] <- "BAK-0080C"

shapefile_proj_list[[18]]@data$FN <- as.character(shapefile_proj_list[[18]]@data$Plot_Name_) #Region 7.1

shapefile_proj_list[[19]]@data$FN <- as.character(shapefile_proj_list[[19]]@data$Plot_Name_New) #Region 7.2

shapefile_proj_list[[20]]@data$FN <- as.character(shapefile_proj_list[[20]]@data$plot) #Region 7.4

shapefile_proj_list[[21]]@data$FN <- as.character(shapefile_proj_list[[21]]@data$plot) #Region 7.5


shapefile_proj_list[[22]]@data$FN <- as.character(shapefile_proj_list[[22]]@data$Plot_Name_New) #Region 7.6
shapefile_proj_list[[22]] <- shapefile_proj_list[[22]][shapefile_proj_list[[22]]$plot!=94005,]
shapefile_proj_list[[22]] <- shapefile_proj_list[[22]][shapefile_proj_list[[22]]$plot!=94033,]


shapefile_proj_list[[23]]@data$FN <- as.character(shapefile_proj_list[[23]]@data$Unique_Plot_ID) #Region 8
shapefile_proj_list[[23]]@data$FN <- gsub("\r\n", "", shapefile_proj_list[[23]]@data$FN)
shapefile_proj_list[[23]]@data$FN[shapefile_proj_list[[23]]@data$FN == "VIC-0001VIC-0001VIC-0001"] <- "VIC-0001"

shapefile_proj_list[[24]]@data$FN <- as.character(shapefile_proj_list[[24]]@data$Plot_Name_New) #Region 9.1
shapefile_proj_list[[24]]@data$FN[shapefile_proj_list[[24]]@data$FN == "POW-1029D\r\nPOW-1029A\r\nPOW-1029D"] <- "POW-1029D"

shapefile_proj_list[[25]]@data$FN <- as.character(shapefile_proj_list[[25]]@data$Plot_Name_New) #Region 9.2
shapefile_proj_list[[25]]@data$FN[shapefile_proj_list[[25]]@data$FN == "POW-1029D\r\nPOW-1029A\r\nPOW-1029D"] <- "POW-1029D"

shapefile_proj_list[[26]]@data$FN <- as.character(shapefile_proj_list[[26]]@data$Plot_Name) #Region 9.3


shapefile_proj_list[[27]]@data$FN <- as.character(shapefile_proj_list[[27]]@data$Plot_No) #Region 9.5. Bsed on Lauren't notes, many of these are probably duplicates that are also in region 10
somerset_plotnames <- bigdata %>%
  select(Historic_Field_Plot_name_.1, Standardized_Plot_Name, Sub_region_name, Region_code) %>%
  filter(Historic_Field_Plot_name_.1 %in% shapefile_proj_list[[27]]@data$Plot_No) %>%
  filter(Sub_region_name == "Somerset Island") %>%
  unique() %>%
  select(-Sub_region_name)
colnames(somerset_plotnames) <- c("Plot_No", "Standardized_Plot_Name", "Region_code")
shapefile_proj_list[[27]]@data <- merge(shapefile_proj_list[[27]]@data, somerset_plotnames, all = TRUE)
shapefile_proj_list[[27]]@data$FN <- as.character(shapefile_proj_list[[27]]@data$Standardized_Plot_Name) 
shapefile_proj_list[[27]] <- shapefile_proj_list[[27]][!is.na(shapefile_proj_list[[27]]$FN),]



shapefile_proj_list[[28]]@data$FN <- as.character(shapefile_proj_list[[28]]@data$Plot_Name_New) #Region 10.1

shapefile_proj_list[[29]]@data$FN <- as.character(shapefile_proj_list[[29]]@data$Plot_Name) #Region 10.2

shapefile_proj_list[[30]]@data$FN <- as.character(shapefile_proj_list[[30]]@data$Plot_Name_New) #Region 10.3

shapefile_proj_list[[31]]@data$FN <- as.character(shapefile_proj_list[[31]]@data$Plot_Name) #Region 10.4
shapefile_proj_list[[31]] <- shapefile_proj_list[[31]][!is.na(shapefile_proj_list[[31]]$FN),] 
#originally surveyed as smaller plots. aggregated into larger plots. This is why there are multiple plots with same name in FN.
#I wonder if this is also the case in Isabel's data

shapefile_proj_list[[32]]@data$FN <- as.character(shapefile_proj_list[[32]]@data$Plot_Name_New) #Region 10.5

shapefile_proj_list[[33]]@data$FN <- as.character(shapefile_proj_list[[33]]@data$Plot_Name) #Region 10.6

shapefile_proj_list[[34]]@data$FN <- as.character(shapefile_proj_list[[34]]@data$Plot_Name) #Region 11

shapefile_proj_list[[35]]@data$FN <- as.character(shapefile_proj_list[[35]]@data$Plot_Name_New) #Region 12
shapefile_proj_list[[35]] <- shapefile_proj_list[[35]][!is.na(shapefile_proj_list[[35]]$FN),]
shapefile_proj_list[[35]] <- shapefile_proj_list[[35]][shapefile_proj_list[[35]]$FN != "",]


#lapply(shapefile_proj_list, function(x) table.na(x$data$FN))
#lapply(shapefile_proj_list, function(x) any(table.na(x@data$FN)>1))




#Combine the 36 seperate shapefiles into one huge shapefile with all of the GIS plots
GIS_shapefiles <-  bind(shapefile_proj_list[[1]], shapefile_proj_list[[2]])
for(i in 3:length(shapefile_proj_list)){
  GIS_shapefiles <- bind(GIS_shapefiles, shapefile_proj_list[[i]])
}

#getting rid of all the mismatched data aside from the column name
GIS_shapefiles@data <- GIS_shapefiles@data %>%
  select(FN)

#add corrdiantes to the dataframe. Is this one corder per plot? or a centroid?
GIS_shapefiles@data$GIS_Easting <- coordinates(GIS_shapefiles)[,1]
GIS_shapefiles@data$GIS_Northing <- coordinates(GIS_shapefiles)[,2]
GIS_coords <- GIS_shapefiles@data

#give a list with the coordinates for each polygon

result <- lapply(GIS_shapefiles@polygons,function(p) data.frame(p@Polygons[[1]]@coords[1:4,]))

##check how many corners each up the plots have (get rid of [1:4,]), lappy something about number of rows and columns?
## put all corners into it the datafarmae of coordiantes by changes the [1:4,] part, giving each coord a name
#melt them all, so that I have just 2 columns (eating and northing), and 4 entries for each
#do the coordiantes always go in the same order? (sw, etc)
#either create an equivalent version of Isabels database (with 2 columns, 4 entire per plot). or could do 4 seperate dataframes for isabel's, then check them in tylers


########################


#Which of Tyler's GIS plots aren't in Isabel's database plots?

MissingObservations <- GIS_shapefiles$FN[GIS_shapefiles$FN %notin% allplots$Plot] %>%
  unique() #there are 2 duplicates

#which of Isabel's database plots aren't in Tyler's GIS files

MissingGIS <- allplots$Plot[allplots$Plot %notin% GIS_shapefiles$FN] %>%
  unique

############### I might be able to make these numbers higher by using the "Selected" GIS files as well



