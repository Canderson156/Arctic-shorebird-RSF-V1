
### This version has all UTM columns






#source("data cleaning scripts/01_library.R")
#source("data cleaning scripts/04_GIS shapefiles.R")
#GIS_shapefiles<- readRDS("Robjects/GIS_shapefiles.RDS")



# all surveys has 3117 observations
# all plots has 2457 observations. this includes some duplicates
# there are 2433 unique plots

#there are 2855 unique plots in bigdata. which ones did I get rid of?
#304 plots that had no rapid data associaterd with them
#11 Plot_type = RECONNAISSANCE plots
#21 plot from PCI 1996
#86 plots from 2019



### start with a more full version of PRISM. might as well check all of the plots that I can


check <- prism[,1:15] %>%
  distinct()

### Editing the raw data ###########################################################################################################################


# Create a version for editing / use for resetting without having to re-load from csv

bigdata <- bigdata_raw



#changing variable classes 

#Nas come from:
#Day, Day, Date: 477 "not applicable" and 1 "not recorded"
#Propotion surveyed: not applicable - final decision, not applicable - nest visit, not recorded
#Sighting code: 31 "not applicable - habitat description"
#Counts: 1 "not recorded". this appears in many columns becasue it is an invensive plot that is missing a final decision form


suppressWarnings(bigdata <- bigdata %>%
                   mutate_at(c("Day", "Month", "Proportion_of_Plot_surveyed", 
                               "Sighting_code", "Sub_region_code", "Count_Nests_found", 
                               "Count_Probable_nest", "Count_Pairs", "Count_Male", 
                               "Count_Female", "Count_Unknown_sex"), as.numeric) %>%
                   mutate(Date = mdy(Date)))



#not done yet:
#start: character to posixct     # these contain two different formats: 00:00 and 00:00:00. 
#end: character to posixct       # would need to write code to get rid of the sconds from the one that have it, then combine the date with this time to make a date-time object 
#duration: character to posixct   #this is being importanted incorrectly. even in excel sheet, need to change format to h:mm for it to make sense



#adding day of year variable

bigdata$yday <- yday(bigdata$Date)



# fixing capitalizations to make all consistent within a variable

bigdata$Plot_type <- toupper(bigdata$Plot_type)
bigdata$Plot_Selection_Method <- tolower(bigdata$Plot_Selection_Method)
bigdata$Standardized_Species_Code <- toupper(bigdata$Standardized_Species_Code)




#making all no data values into NAs

bigdata <- bigdata %>%
  mutate(Group = ifelse(Group == "not applicable", NA, Group)) %>%
  mutate(Group = ifelse(Group == "not recorded", NA, Group)) %>%
  mutate(Start_time_1 = ifelse(Start_time_1 %in% c("not recorded", "not applicable", "no data"), NA, Start_time_1))

#getting rid of one blank cell in human development

bigdata$Human_development_details[bigdata$Human_development_details == " "] <- "not recorded"


#count the number of surveyors

#for(i in 1:nrow(bigdata)) {
#  bigdata$n_surveyors[i] <- count.not.na(c(
#    bigdata$Surveyor_1_Initials[i],
#    bigdata$Surveyor_2_Initials[i],
#    bigdata$Surveyor_3_Initials[i],
#    bigdata$Surveyor_4_Initials[i], 
#    bigdata$Surveyor_5_Initials[i],
#    bigdata$Surveyor_6_Initials[i]))
#}



#edit the time column so that formats are uniform 
#NAs don't parse well with lubridate. 
#I haven't bothered turning in times with hm() becasue of the NAs becasue only planning to use it to make ID vars right now

bigdata <- bigdata %>%
  mutate(Start_time_1 = ifelse(nchar(Start_time_1) == 4, paste("0", Start_time_1, sep = ""), Start_time_1),
         Start_time_1 = ifelse(nchar(Start_time_1) == 8, substr(Start_time_1, 1, 5), Start_time_1))





#######################################################################################################################


# select only the columns containing required data
prism <- select(bigdata,
                Plot = Standardized_Plot_Name, #2855 unique plots
                Survey_Lead,
                Year, #1994-2019
                Month, #mostly June, some July, 475 "not applicable"
                Day,
                Date,
                yday,
                Start_time_1,
                Region_name, #89 plots with NAs that are outside the regions (south of Mackenzie Delta)
                Region_code, #89 plots with NAs that are outside the regions (south of Mackenzie Delta)
                Sub_region_name, #170 plots with NAs. 89 as above. 78 in Foxe basin becasue of differences in how subregions were calculated over time (2019 had no subregions). 3 in North Archipelago are confusing, but may stem from weird subregions that overlap near Alert (see email from Laurent RE Tyler's map of subregions)
                Sub_region_code, #same as above
                Plot_type,
                Survey_method,
                Plot_Shape,
                Quality_1 = GIS_Habitat_Quality_2_Code,
                Quality_2 = GIS_Habitat_Quality_1_Code, # how were these defined, when were they changed, which one is the one we should use, are these being redone?
                Prop_surveyed = Proportion_of_Plot_surveyed, 
                Selection_method = Plot_Selection_Method, # why are there so many field selected plots?
                Plot_area = Plot_area_km2,
                Sighting_code, 
                Species = Standardized_Species_Code,  #XXXX means nothing was observed
                Group,
                Count_Nests_found,
                Count_Probable_nest,
                Count_Pairs,
                Count_Male,
                Count_Female,
                Count_Unknown_sex,
                #n_surveyors,
                Human_development_details, 
                Map_datum,
                Plot_center_long = Plot_Center_X_coordinate..long.easting.,
                Plot_center_lat = Plot_Centre_Y_coordinate..lat.northing.,
                contains("UTM")) %>%
  select(-contains("Wildlife"))





allplots <- prism %>%
  select(Plot,
         Plot_type,
         Survey_Lead,
         Selection_method,
         Region_code,
         Map_datum,
         Plot_center_long,
         Plot_center_lat,
         contains("UTM")) %>%
  distinct()





## add in the missing data from rasmussen 2019


plots_ras_2019_raw <- read.csv("data/PRISM/Rasmussen_PlotCoordinates_2019s.csv") 


plots_ras_2019 <- plots_ras_2019_raw %>%
  select(-Lat, -Long, -UTM_Zone) %>%
  rename(Plot = Plot_Name) %>%
  unite(coords, UTM_E, UTM_N) %>%
  spread(Corner, coords) %>%
  separate(SW, c("UTM_1_Easting", "UTM_1_Northing")) %>%
  separate(NW, c("UTM_2_Easting", "UTM_2_Northing")) %>%
  separate(NE, c("UTM_3_Easting", "UTM_3_Northing")) %>%
  separate(SE, c("UTM_4_Easting", "UTM_4_Northing")) %>%
  mutate(UTM_1_Type = "SW", UTM_2_Type = "NW", UTM_3_Type = "NE", UTM_4_Type = "SE", 
         UTM_1_Status = "field", UTM_5_Type = NA, UTM_5_Easting = NA, UTM_5_Northing = NA) 

ras_cols <- colnames(plots_ras_2019)[colnames(plots_ras_2019) %notin% c("Plot", "UTM_Zone")]

allplots_ras_2019 <- allplots %>%
  filter(Plot %in% plots_ras_2019$Plot) %>%
  select(-one_of(ras_cols))

allplots_sub <- allplots %>%
  filter(Plot %notin% plots_ras_2019$Plot)


allplots_ras_2019 <- merge(allplots_ras_2019, plots_ras_2019, by = "Plot")

allplots <- rbind(allplots_sub, allplots_ras_2019)

rm(allplots_ras_2019, allplots_sub, plots_ras_2019)






#make a versions that I will subtract entries from to prevent duplicates
allplots2 <- allplots %>%
  mutate(PlotX = paste(Plot, 1:length(Plot), sep = "_"), dup = allDuplicated(Plot))


### divide into groups that I want to check





##################### Which coordinates to use?



#group 0: plots that have no coordinates anywhere



g0_no_coords <- allplots2 %>%
  filter(Plot %notin% GIS_shapefiles$Plot) %>%
  filter(is.na(UTM_1_Easting)) %>%
  mutate(UTM_1_Easting = Plot_center_long, UTM_1_Northing = Plot_center_lat, 
         UTM_1_Status = ifelse(is.na(UTM_1_Easting), NA, "LAT_LONG_DM")) %>%
  filter(is.na(UTM_1_Easting))
  
  
  

allplots2 <- allplots2 %>%
  filter(PlotX %notin% g0_no_coords$PlotX)




#group 1: field modified plots - verify


g1_field_modified <- allplots2 %>%
  filter(Selection_method == "field modified gis selected") %>%
  filter(not.na(UTM_1_Easting)) %>%
  filter(not.na(UTM_4_Easting)) %>%
  filter(Plot %in% GIS_shapefiles$Plot) %>%
  arrange(Region_code)

allplots2 <- allplots2 %>%
  filter(PlotX %notin% g1_field_modified$PlotX)


table(g1_field_modified$UTM_1_Status)




############### Plots that we could use the GIS files but we don't appear to have them


#Group 2: no GIS plots that I've uploaded so far

g2_no_gis <- allplots2 %>%
  filter(Plot %notin% GIS_shapefiles$Plot) 


allplots2 <- allplots2 %>%
  filter(PlotX %notin% g2_no_gis$PlotX)








################# Separate out plots with LAT/LONG instead of UTM




g2_no_gis <- g2_no_gis %>%
  mutate(UTM_1_Easting = ifelse(is.na(UTM_1_Easting), Plot_center_lat, UTM_1_Easting),
         UTM_1_Northing = ifelse(is.na(UTM_1_Northing), Plot_center_long, UTM_1_Northing),
         UTM_1_Status = ifelse(is.na(UTM_1_Status), "LAT_LONG_DM", UTM_1_Status))
  

g2a_needs_transformation <- g2_no_gis %>%
  filter(UTM_1_Status %in% c("LAT_LONG_DD", "LAT_LONG_DM"))


g2_no_gis <- g2_no_gis %>%
  filter(UTM_1_Status %notin% c("LAT_LONG_DD", "LAT_LONG_DM"))


############# Seperate out plots with only one coordinate



g2b_point <- g2_no_gis %>%
  filter(is.na(UTM_4_Easting))

g2_no_gis <- g2_no_gis %>%
  filter(not.na(UTM_4_Easting))





###relating back to g2

table(g2_no_gis$Plot_type, g2_no_gis$Survey_Lead)
#there are 2 duplicates: SHI-3261 (CWS Rapid and) PAL-0079 (Industry Rapid)

table(g2_no_gis$Plot_type[is.na(g2_no_gis$UTM_1_Easting)], g2_no_gis$Survey_Lead[is.na(g2_no_gis$UTM_1_Easting)])


table(g2_no_gis$UTM_1_Status)
#143 plots were "GIS confirmed" but I don't have these in the GIS files. Look for them?




test <- g2_no_gis %>%
  filter(Plot_type == "RAPID") %>%
  filter(Survey_Lead == "Canadian Wildlife Service")

table(test$Selection_method, is.na(test$UTM_1_Easting))






############### Plots that we can use the GIS files
g3_yes_gis <- allplots2 %>%
  arrange(Region_code)




nrow(g0_no_coords)+ nrow(g1_field_modified) + nrow(g2_no_gis) + nrow(g2a_needs_transformation) + nrow(g2b_point) + nrow(g3_yes_gis) == nrow(allplots)










