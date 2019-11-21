#looking at the structure of the whole bigdata dataset

str(bigdata, list.len=ncol(df))


# all of the column names so that I can ceanf or column names, subset by column number etc
cols_bigdata <- data.frame(colnames(bigdata))



### Looking at subsets of the full dataset


cols <- c(17, 36, 41:42, 45, 331:338, 344, 677:714, 77, 84, 90, 97, 77:302)
cols <- c(313:322)

test <- bigdata %>%
  #filter(!is.na(UTM_44_Easting)) %>%
  select(cols)




#check if all of the UTMS are the same
cols <- c(77, 84, 90, 97)

test <- bigdata %>%
  #filter(!is.na(UTM_44_Easting)) %>%
  select(cols)

#returns true if first value matches all other values, 
#false if atleast one doesn't match,
#NA if there is an NA in any of the values

all.identical <- function(vec){
  all(vec[1] == vec)
} 


test$same <- apply(test, 1, all.identical)


h <- head(prism)

#### Checking functions created by Laurent

#Making sure that each subregion has unique code (and vice versa)
subregionList <- levels(bigdata$Sub_region_name)
for(i in 2:length(subregionList)){
  print(subregionList[i])
  print(unique(bigdata[bigdata$Sub_region_name==subregionList[i],"Sub_region_code"]))
}
#Some subregions are associated to more than one code: Rasmussen Lowlands (7.1-9.2), Hope Bay Mine Project (7.3-8.3), Izok Mine Project (6.5-7.5), Needs correction (7.4.99-7.5.99), Somerset Island (9.1-10.3), Tahera Mine Project (6.2-7.4)

subregioncodeList <- levels(bigdata$Sub_region_code)
for(i in 2:length(subregioncodeList)){
  print(subregioncodeList[i])
  print(unique(bigdata[bigdata$Sub_region_code==subregioncodeList[i],"Sub_region_name"]))
}
#Codes are not associated to more than one subregion name




#Function to check if any plots were surveyed at more than one date

checkplots <- function(datafile, region, column, plot, Date, year){
  #datafile: dataframe in which all data is contained
  #region: all possible sublevels for the region variable
  #column: character ID of the region variable
  #plot: character ID of the plot variable
  #Date: character ID of the Date variable
  #year: character ID of the year variable
  
  #Subsetting to a single region
  a <- datafile[datafile[,column]==region,]
  #Removing unneeded plot and date levels
  a[,plot] <- as.factor(as.character(a[,plot]))
  a[,Date] <- as.factor(as.character(a[,Date]))
  #Making a table of the frequency of combinations of plot and date
  b <- table(a[,plot], a[,Date])
  #Obtaining the number of times that each plot was surveyed
  counts <- rowSums(b!=0)
  doubleplots <- names(which(counts>=2))
  years <- unique(datafile[datafile[,plot] %in% doubleplots, year])
  if(any(counts>=2)){
    return(list(as.character(region), doubleplots, years, counts, b))
  }
}


#Making a list of subregions that have plots which were surveyed more than once
mult_survey <- vector()

#Loop to go through all subregions
for(i in unique(bigdata$Sub_region_name)){
  temp <- checkplots(bigdata[bigdata$Plot_type=="bigdata",], i, "Sub_region_name", "Standardized_Plot_Name", "Date", "Year")
  mult_survey <- append(mult_survey, temp, after = length(mult_survey))
}

pbp <- bigdata[bigdata$Protected_Area_1_Name=="Polar Bear Pass-Nanuit Itillinga National Wildlife Area",]
pbp$Year <- as.factor(as.character(pbp$Year))
pbp$Standardized_Plot_Name <- as.factor(as.character(pbp$Standardized_Plot_Name))
table(pbp$Year, pbp$Standardized_Plot_Name)
which(bigdata$Standardized_Plot_Name %in% "PBP")