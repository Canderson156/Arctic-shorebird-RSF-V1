##### LOAD PACKAGES



library(raster)
library(ggplot2)
library(lme4)
library(lubridate)
#library(sf)
#library(rgeos)
#library(maptools)
#library(rgdal)
library(reshape2)
#library(SDMTools)
#library(velox)
#library(gdalUtils)
#library(MASS)
#library(corrplot)
#library(scales)
#library(Hmisc)
#library(pastecs)
#library(psych)
#library(data.table)
library(FRK)
#library(beepr)
library(rgdal)
library(sp)
library(MASS)
library(tidyr) # Error: there is no package called 'pillar'
library(rgeos)
library(maptools)
library(rgdal)
library(reshape)
library(raster)
library(SDMTools)
#library(velox)
library(gdalUtils)
library(sf)
library(plyr)
library(dplyr)
library(ggplot2)
library(corrplot)
library(scales)
library(tidyverse)
library(units)
library(stringi)
library(lwgeom)
library(measurements)

#-- Conflicts ------------------------------------------ tidyverse_conflicts() --
# x lubridate::as.difftime() masks base::as.difftime()
# x lubridate::date()        masks base::date()
# x tidyr::expand()          masks Matrix::expand()
# x tidyr::extract()         masks raster::extract()
# x dplyr::filter()          masks stats::filter()
# x lubridate::intersect()   masks raster::intersect(), base::intersect()
# x dplyr::lag()             masks stats::lag()
# x dplyr::select()          masks raster::select()
# x lubridate::setdiff()     masks base::setdiff()
# x lubridate::union()       masks raster::union(), base::union()




##### GENERAL FUNCTIONS


## notin function
## works like %in%, but allows you to select the instances of the first vector that are not in the second vector

`%notin%` <- Negate(`%in%`)

#not.na returns true is the object is not an NA

not.na <- Negate(is.na)

## make hidden plyr nunique function work without having to type the beginning part

n_unique <- function (x) plyr:::nunique(x)




## conditional mutation
## works like mutate(), but only performs the operation on a subset of the dataset, returns the whole dataset
## example: prism2 <- prism2 %>% mutate_cond(region == 'Quebec', UTM_Zone = replace_na(UTM_Zone, 15))


mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition , ] <- .data[condition, ] %>% mutate(...,  na.rm = TRUE)
  .data
}


#change the table function to include NAs by default

table <- function (..., useNA = 'ifany') base::table(..., useNA = useNA)


#function that that replaces NAs with 0s and leaves all other values as is

if.na.0 <- function(x){
  ifelse(is.na(x), 0, x)
}



#function for counting the number of values that are not NAs

count.not.na <- function(vec) {
  n <- length(vec)
  nas <- which(is.na(vec))
  x <- n-length(nas)
  return(x)
}




###function that turns dataframe into sf object



df_to_stPolygons <- function(dataset, long, lat, grouping_var) {
  output <- dataset %>%
    st_as_sf(coords = c(long, lat)) %>%
    group_by_(grouping_var) %>%
    summarise(geometry = st_combine(geometry)) %>%
    st_cast("POLYGON")
  return(output)
}





###function that tells you if an object has any duplicates, including the first instance

allDuplicated <- function(vec){
  front <- duplicated(vec)
  back <- duplicated(vec, fromLast = TRUE)
  all_dup <- front + back > 0
  return(all_dup)
}





##### OBJECTS SPECIFIC TO THIS PROJECT


## Coordinate reference system that I will do all of the analyses in
## North Pole Lambert Azimuthal Equal Area
## -----> should I chnage this to whatever Tyler used?

NPLAEA <-  CRS("+init=EPSG:3573")
AEA <- CRS("+proj=aea +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
LCC <- CRS("+proj=lcc +lat_1=50 +lat_2=70 +lat_0=40 +lon_0=-96 +x_0=0 +y_0=0 +datum=NAD83 +units=m +no_defs +ellps=GRS80 +towgs84=0,0,0")
NAD83 <- CRS("+init=EPSG:4269")



#### Read in raw data

# Read in full PRISM dataset
bigdata_raw <-read.csv("data/PRISM/PRISM 1994 to 2019_20191210_CAedits_20200508.csv"
                       , stringsAsFactors = FALSE, na.strings=c("", "NA"))


