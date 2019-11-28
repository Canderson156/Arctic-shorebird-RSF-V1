##### LOAD PACKAGES


#library(MASS)
library(tidyr)
library(lubridate)
#library(rgeos)
#library(maptools)
#library(rgdal)
#library(reshape)
library(raster)
#library(SDMTools)
#library(velox)
#library(gdalUtils)
library(plyr)
library(dplyr)
library(ggplot2)
#library(corrplot)
#library(scales)
#library(Hmisc)
#library(pastecs)
#library(psych)
#library(data.table)
#library(FRK)
#library(beepr)



##### GENERAL FUNCTIONS


## notin function
## works like %in%, but allows you to select the instances of the first vector that are not in the second vector

`%notin%` <- Negate(`%in%`)



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

if.na <- function(x){
  ifelse(is.na(x), 0, x)
}



##### OBJECTS SPECIFIC TO THIS PROJECT


## Coordinate reference system that I will do all of the analyses in
## North Pole Lambert Azimuthal Equal Area
## -----> should I chnage this to whatever Tyler used?

NPLAEA <-  CRS("+init=EPSG:3573")

