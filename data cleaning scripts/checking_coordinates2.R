# choose one region and try to compare the coordinates to see if the match up

#which prism plots do have matching GIS files
# is there some way I can attach the plot data that Laurent  edited to the shapefiles that I uploaded?

dontmatch <- readRDS("E:/2019 PRISM GIS v2/Z/dontmatch.RDS")
match <- readRDS("E:/2019 PRISM GIS v2/Z/match.RDS")

## choose one Shapefile to startwith

###in script 2, I took the mean of the coordiantes of allplots. this was probably becasue some had different coordinates. 
#its ikely that one was wrong and one was right, so now they will definately be wrong

#at a first glance, it seems like there are more GIS polygons than there are prism polygons. I suppose due to filtering etc

test <- GIS_shapefiles@data
