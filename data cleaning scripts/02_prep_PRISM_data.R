

## Script for producing:

## bigdata_raw     = all data imported from csv file with no edits made to it
## bigdata         = all data, editing data classes of columns, capitalization, NAs
## prism           = filtered to include the columns that seem useful for analyzing,
##                   only on plot rapid surveys, exclude plots that are questionable
## allsurveys      = One record for every survey that was done. Some plots were surveyed multiple times
## allplots        = One record for every plot
## sb and SB       = Several different iterations summarizing shorebird counts 
##                   (per survey or per plot, seperate or aggregated by year, by species or grouped)




### Editing the raw data ###########################################################################################################################


## TO DO:
# Go through the documentation I created in December and Laurent's list of plots that he removed becasue they were sketchy.
#      -> remove any plots that seem sketchy i.e. the human development ones



# Create a version for editing / use for resetting without having to re-load from csv

bigdata <- bigdata_raw



#changing variable classes 

#Nas come from:
#Day, Day, Date: 477 "not applicable" and 1 "not recorded"
#Propotion surveyed: not applicable - final decision, not applicable - nest visit, not recorded
#Sighting code: 31 "not applicable - habitat description"
#Counts: 1 "not recorded". this appears in many columns becasue it is an invensive plot that is missing a final decision form


bigdata <- bigdata %>%
  mutate_at(c("Day", "Month", "Proportion_of_Plot_surveyed", 
              "Sighting_code", "Sub_region_code", "Count_Nests_found", 
              "Count_Probable_nest", "Count_Pairs", "Count_Male", 
              "Count_Female", "Count_Unknown_sex"), as.numeric) %>%
  mutate(Date = mdy(Date))


  
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




### Filtering to all valid surveys ###########################################################################################################################



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
                GIS_UTM_1_zone,
                UTM_1_Easting, 
                UTM_1_Northing,
                UTM_1_Type)


#start = Start_time_1,
#end = Finish_time_1,
#duration = DERIVED_Survey_Duration..hours.minutes.




#remove the plots that weren't rapid plots
## decided to keep the rapid surveys of intensive plots

prism <- prism %>%
  filter(Survey_method == "rapid") %>%
  filter(Plot_type !="RECONNAISSANCE") 


#keep only on plot observations
## 0 = final decision, 1 = on plot, 2 = incidental, 3 = mammal, 4 = wildlife sign 
prism <- filter(prism, Sighting_code == 1)



#replace missing values of counts with 0s

prism <- prism %>%
  mutate_at(c("Count_Nests_found", 
              "Count_Probable_nest", "Count_Pairs", "Count_Male", 
              "Count_Female", "Count_Unknown_sex"), if.na.0)



#create a total birds column and unique survey identifiers

prism <- prism %>%
  mutate(total_birds = (Count_Nests_found*2) + (Count_Probable_nest*2) + (Count_Pairs*2) + Count_Male + Count_Female + Count_Unknown_sex) %>%
  mutate(plot_date = paste(Plot, Date, Start_time_1)) %>%
  mutate(plot_year = paste(Plot, Year)) %>%
  mutate(plot_species = paste(Plot, Species))

#%>%
#  group_by(plot_date) %>%
#  add_tally(total_birds)




#60 plots missing regions name and code - should be fixed with GIS
#97 missing subregion name and code - should be fixed with GIS
#both quality columns are missing thousands - ask Isabel? GIS?
#19 are missing proportion surveyed
#361 are missing Group - they are the ones that have species = XXXX, should I replace this?
#38 have NA for plot area
#233 have plot areas bigger than 400 x 400 - what's the deal?



# add a column for comparing plot selection methods biases



prism <- prism %>%
  mutate(comparison = Selection_method, 
         comparison = ifelse(Selection_method == "field selected" & Survey_Lead == "Industry", "field selected - industry", comparison),
         comparison = ifelse(Selection_method == "field selected" & Plot_type == "INTENSIVE", "field selected - intensive", comparison),
         comparison = ifelse(comparison == "field selected", "field selected - other", comparison))




#add a column that subs in quality code 1 when quality code 2 is missing

prism <- prism %>%
  mutate(quality = ifelse(is.na(Quality_2), Quality_1, Quality_2),
         quality = ifelse(quality %in% c(1,2,3), quality, NA))





#### filtering out data that is problematic

#remove plots from Prince Charles Island 1996
#remove plots that were surveyed in 2019?

prism <- prism %>%
  filter(!(Region_code == 3 & Year == 1996))

prism <- prism %>%
  filter(!(Year == 2019))

#keep only the rows with a southwest corner
  ###why had I done this? which ones don't have a southwest corner? 

#prism <- prism %>%
#  filter(UTM_1_Type == "SW corner")

#why are the PCI ones from this year missing? 












### Filtering to all unique surveys (no counts) ###########################################################################################################################



#create a dataset of all surveyed plots

allsurveys <- dplyr::select(prism,
                          Plot,
                          Survey_Lead,
                          Year, 
                          Month, 
                          Day,
                          Date,
                          yday,
                          Start_time_1,
                          Region_name, 
                          Region_code, 
                          Sub_region_name, 
                          Sub_region_code, 
                          Plot_type,
                          Survey_method,
                          Plot_Shape,
                          #Quality_1,
                          #Quality_2, 
                          quality,
                          #Prop_surveyed, 
                          Selection_method, 
                          Plot_area,
                          #n_surveyors, 
                          comparison, 
                          plot_date,
                          plot_year,
                          Human_development_details,
                          UTM_1_Type,
                          UTM_Zone = GIS_UTM_1_zone,
                          UTM_1_Easting, 
                          UTM_1_Northing)

allsurveys <- distinct(allsurveys) 



### Filtering to all unique plots (no counts) ###########################################################################################################################

allplots <- allsurveys %>%
  group_by(Plot) %>%
  mutate(UTM_Easting = mean(UTM_1_Easting),   #taking the mean of the UTM coordinates
         UTM_Northing = mean(UTM_1_Northing)) %>%
  ungroup() %>%
  select(Plot,
         #Species,
         Survey_Lead,
         Region_name, 
         Region_code, 
         Sub_region_name, 
         Sub_region_code, 
         Plot_type,
         Survey_method,
         #Plot_Shape, # a few plots differ between years
         #quality, # a few plots differ between years
         Selection_method, 
         #Plot_area,
         comparison,
         #plot_species,
         UTM_1_Type,
         UTM_Zone, 
         UTM_Easting, 
         UTM_Northing) %>%
  distinct()
  
  









### filtering to only shorebird data ###########################################################################################################################



## only shorebird observations


sb_list <- prism %>%
  filter(Group == "Shorebirds") %>%
  select(Species) %>%
  unique() %>%
  na.omit() %>%
  pull(Species)
  
  
#all shorebirds, all years, all surveys
#sb <- prism %>%
#  filter(Group == "Shorebirds") %>%


#all shorebirds, all years, mean of within year surveys
sb <- prism %>%
  filter(Group == "Shorebirds") %>%
  group_by(plot_year, Species) %>%
  mutate(mean_birds_year = mean(total_birds)) %>%
  select(Plot,
         Species,
         Survey_Lead,
         Year, 
         Region_name, 
         Region_code, 
         Sub_region_name, 
         Sub_region_code, 
         Plot_type,
         Survey_method,
         Plot_Shape,
         quality,
         Selection_method, 
         Plot_area,
         comparison,
         Human_development_details,
         plot_year,
         plot_species,
         mean_birds_year) %>%
  distinct()




#all shorebirds, mean between years (from mean within years)


sb <- sb %>%
  ungroup() %>%
  group_by(Plot) %>%
  ungroup() %>%
  group_by(plot_species) %>%
  mutate(mean_birds_all = mean(mean_birds_year)) %>%
  select(Plot,
         Species,
         Survey_Lead,
         Region_name, 
         Region_code, 
         Sub_region_name, 
         Sub_region_code, 
         Plot_type,
         Survey_method,
         #Plot_Shape, # a few plots differ between years
         #quality, # a few plots differ between years
         Selection_method, 
         Plot_area,
         comparison,
         plot_species,
         mean_birds_all) %>%
  distinct()
  

# above, with all shorebird species added together

SB <- sb %>%
  ungroup() %>%
  group_by(Plot) %>%
  mutate(sum_shorebirds = sum(mean_birds_all)) %>%
  select(Plot,
         #Species,
         Survey_Lead,
         Region_name, 
         Region_code, 
         Sub_region_name, 
         Sub_region_code, 
         Plot_type,
         Survey_method,
         #Plot_Shape, # a few plots differ between years
         #quality, # a few plots differ between years
         Selection_method, 
         #Plot_area,
         comparison,
         #plot_species,
         sum_shorebirds) %>%
  distinct()



SB <- SB %>%
  merge(allplots, all = TRUE) %>%
  mutate(sum_shorebirds = ifelse(is.na(sum_shorebirds), 0, round(sum_shorebirds)))



## use my expand grid from previous data cleaning script when I want to model by species





### Adding goose data ###########################################################################################################################


geese_sp <- c("CAGO", "SNGO", "ROGO", "CACG", "GWFG")

geese_plots <- prism %>%
  filter(Species %in% geese_sp) %>%
  select(Plot,
         plot_date,
         plot_year,
         total_birds) %>%
  group_by(plot_date) %>% 
  mutate(sum_geese_survey = sum(total_birds)) %>%
  ungroup() %>%
  select(-plot_date, -total_birds) %>%
  distinct() %>%
  group_by(plot_year) %>%
  mutate(mean_geese_year = mean(sum_geese_survey)) %>%
  ungroup() %>%
  select(-plot_year, -sum_geese_survey) %>%
  distinct() %>%
  group_by(Plot) %>% 
  mutate(mean_geese = round(mean(mean_geese_year))) %>%
  ungroup() %>%
  select(-mean_geese_year) %>%
  distinct()
  
  
#would I rather add this to the sb object that has individual species rather than summaries for all shorebirds? 
SB <- SB %>%
  merge(geese_plots, all = TRUE) %>%
  mutate(mean_geese = ifelse(is.na(mean_geese), 0, mean_geese))

rm(geese_sp, geese_plots, sb)


