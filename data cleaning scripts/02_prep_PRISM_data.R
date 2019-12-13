

## Script for producing:

## bigdata_raw     = all data imported from csv file with no edits made to it
## bigdata         = all data, editing classes, capitalization, NAs
## prism           = filtered to include 


## a dataset that contain all shorebird observations
## a dataset that contains all goose observations
## a dataset that contain all survey plots    --  would prism work for this or did I filter out anything that I would want?
## a dataset that combines all of these









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




# select only the columns containing required data
prism <- dplyr::select(bigdata,
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
                       Human_development_details)


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



#create a total birds column and unique survey identifier

prism <- prism %>%
  mutate(total_birds = (Count_Nests_found*2) + (Count_Probable_nest*2) + (Count_Pairs*2) + Count_Male + Count_Female + Count_Unknown_sex) %>%
  mutate(plot_date = paste(Plot, Date, Start_time_1)) 

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





#create a dataset of all surveyed plots

allplots <- dplyr::select(prism,
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
                          Human_development_details)

allplots <- distinct(allplots) #2540 unique plots, 747 were surveyed more than once





## only shorebird observations


sb_list <- prism %>%
  filter(Group == "Shorebirds") %>%
  select(Species) %>%
  unique() %>%
  na.omit() %>%
  pull(Species)
  
  

sb <- prism %>%
  filter(Group == "Shorebirds")
  
#### I should apply some of the rules like taking the means of plots, becasue then the plot_date column will be irrelevant. 
#### I will need to add a plot_year column instead though


#plan of what I need to do


# seperate these datasets:
## a dataset that contain all shorebird observations
## a dataset that contains all goose observations
## a dataset that contain all survey plots
## a dataset that combines all of these










