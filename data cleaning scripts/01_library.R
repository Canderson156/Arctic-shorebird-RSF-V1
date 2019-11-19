#load packages


#library(MASS)
library(tidyr)
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


## notin function
## works like %in%, but allows you to select the instances of the first vector that are not in the second vector

`%notin%` <- Negate(`%in%`)



## conditional mutation
## works like mutate(), but only performs the operation on a subset of the dataset, returns the whole dataset

mutate_cond <- function(.data, condition, ..., envir = parent.frame()) {
  condition <- eval(substitute(condition), .data, envir)
  .data[condition, ] <- .data[condition, ] %>% mutate(...)
  .data
}

## Coordinate reference system that I will do all of the analyses in
## North Pole Lambert Azimuthal Equal Area

NPLAEA <-  CRS("+init=EPSG:3573")





