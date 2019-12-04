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
library(lme4)



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







##### OBJECTS SPECIFIC TO THIS PROJECT


## Coordinate reference system that I will do all of the analyses in
## North Pole Lambert Azimuthal Equal Area
## -----> should I chnage this to whatever Tyler used?

NPLAEA <-  CRS("+init=EPSG:3573")




#### Read in raw data

# Read in full PRISM dataset
bigdata_raw <-read.csv("data/PRISM/PRISM 1994 to 2019.csv"
                       , stringsAsFactors = FALSE, na.strings=c("", "NA"))


