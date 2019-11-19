

## Script for producing:
## a dataset that contain all shorebird observations
## a dataset that contains all goose observations
## a dataset that contain all survey plots
## a dataset that combines the two


# Read in full PRISM dataset

rapid<-read.csv("data/PRISM/EDITED_19NOV2019_PRISM__Rapid _to_2018.csv"
                , stringsAsFactors = FALSE, na.strings=c("", "NA"))


# Change variable classes

rapid$Day <- as.integer(rapid$Day) #how could some days be not applicable? 
#date: character to posixct
#start: character to posixct
#end: character to posixct
#duration: character to posixct
rapid$Nests_found <- as.integer(rapid$Nests_found)
rapid$Unknown_sex <- as.integer(rapid$Unknown_sex)
rapid[,startsWith(colnames(rapid), "UTM")] <- apply((rapid[,startsWith(colnames(rapid), "UTM")]), 2, as.numeric)
rapid[,startsWith(colnames(rapid), "len")] <- apply((rapid[,startsWith(colnames(rapid), "len")]), 2, as.numeric)
rapid$Total_Plot_Area_ha <- as.numeric(rapid$Total_Plot_Area_ha) #ignore these nas for now
rapid$Total_Snow_cover_in_plot <- as.numeric(rapid$Total_Snow_cover_in_plot) #ignore these nas for now
rapid$Plot_Selection_Method <- tolower(rapid$Plot_Selection_Method)

# it would be easier to check all of these with the reduced dataset. 
#even though they will run in this order, filer first to do the checking of this


