

## Script for producing:

## bigdata_raw     = all data imported from csv file with no edits made to it
## bigdata         = all data, editing classes, capitalization, NAs
## prism           = filtered to include 


## a dataset that contain all shorebird observations
## a dataset that contains all goose observations
## a dataset that contain all survey plots    --  would prism work for this or did I filter out anything that I would want?
## a dataset that combines all of these






# Read in full PRISM dataset
bigdata_raw <-read.csv("data/PRISM/PRISM 1994 to 2019.csv"
                , stringsAsFactors = FALSE, na.strings=c("", "NA"))


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




# fixing capitalizations to make all consistent within a variable

bigdata$Plot_type <- toupper(bigdata$Plot_type)
bigdata$Plot_Selection_Method <- tolower(bigdata$Plot_Selection_Method)
bigdata$Standardized_Species_Code <- toupper(bigdata$Standardized_Species_Code)



#fixing mistakes

bigdata <- bigdata %>%
  mutate(Standardized_Species_Code = ifelse(Standardized_Species_Code == "AGPL", "AMGP", Standardized_Species_Code)) %>%
  mutate(Standardized_Species_Code = ifelse(Standardized_Species_Code == "RAVE", "CORA", Standardized_Species_Code)) %>%
  mutate(Group = ifelse(Standardized_Species_Code == "KIEI", "Waterfowl", Group)) %>%
  mutate(Group = ifelse(Standardized_Species_Code == "LALO", "Passerines", Group)) %>%
  mutate(Group = ifelse(Standardized_Species_Code == "GWFG", "Waterfowl", Group)) 

#making all no data values into NAs

bigdata <- bigdata %>%
  mutate(Group = ifelse(Group == "not applicable", NA, Group)) %>%
  mutate(Group = ifelse(Group == "not recorded", NA, Group))
  




# select only the columns containing required data
prism <- dplyr::select(bigdata,
                       Plot = Standardized_Plot_Name, #2855 unique plots
                       Year, #1994-2019
                       Month, #mostly June, some July, 475 "not applicable"
                       Day,
                       Date,
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
                       Count_Unknown_sex)


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
              "Count_Female", "Count_Unknown_sex"), if.na)



#create a total birds column

prism <- mutate(prism, total_birds = (Count_Nests_found*2) + (Count_Probable_nest*2) + (Count_Pairs*2) + Count_Male + Count_Female + Count_Unknown_sex)






#60 plots missing regions name and code - should be fixed with GIS
#97 missing subregion name and code - should be fixed with GIS
#both quality columns are missing thousands - ask Isabel? GIS?
#19 are missing proportion surveyed
#361 are missing Group - they are the ones that have species = XXXX, should I replace this?
#38 have NA for plot area
#233 have plot areas bigger than 400 x 400 - what's the deal?


test <- filter(bigdata, Standardized_Plot_Name == "KWI-1314B")




#plan of what I need to do


# seperate these datasets:
## a dataset that contain all shorebird observations
## a dataset that contains all goose observations
## a dataset that contain all survey plots
## a dataset that combines all of these





## all geogrpahic data should be extracted from the GIS files as if may be unreliable in the excel spreadsheet: 

# region
# subregion
# plot coordinates
# plot area
# quality? others? what else is in the GIS files?




# compare GIS data vs excel data

#Weird shapes in excel file - do we see the same weird shapes in GIS files? 
#do the GIS files represent what WAS done or what SHOUld have been done on paper?






