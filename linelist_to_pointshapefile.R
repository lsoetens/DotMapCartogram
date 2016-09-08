####### From linelist with cases and location to point pattern of cases #######

# What do you need?
# linelist with cases, with at least one column with administrative region the case belongs to (such as postal code, municipality etc)
# a shapefile corresponding with the administrative region of the cases (ie. shapefile of all postal codes of a certain country)

# set your working directory
setwd("C:/linktoworkingdirectory/")

# load packages
library(xlsx)
library(sp)
library(maptools)
library(plyr)

# load your case data
cases<- read.xlsx("file_with_case_locations.xlsx", sheetIndex = 1, header = T)

# load your corresponding shapefile
pc4_nl_2015<- readShapeSpatial( "shapefile_with_administrative_regions.shp")

# aggregate your case data by counting the number of cases per administrative region
case_agg<- count(cases$postcodenr)

# check if there are no 'lost' cases
sum(case_agg$freq)

# merge your aggregated case data with the shapefile
case_pc4<- merge(pc4_nl_2015, case_agg, by.x= "pc4", by.y= "x")

# check if there are no 'lost' cases
sum(case_pc4@data$freq, na.rm= T)

# set frequency to 0 if freq=NA in administrative region
case_pc4@data$freq<- ifelse(is.na(case_pc4@data$freq), 0, case_pc4@data$freq)

# select only regions with cases
case_pc4<- subset(case_pc4,  case_pc4@data$freq > 0)

# pick random location within administrative region for every case and output as point-shapefile
coordx<- c()
coordy<- c()

for (i in 1: length(case_pc4)){
 coord<- c()
 coord<- spsample(x= case_pc4[i,], n= case_pc4@data$freq[i], type= "random", iter= 40)
 coordx<- c(append(coordx, t(t(coord@coords[,1])), after= length(coordx)))
 coordy<- c(append(coordy, t(t(coord@coords[,2])), after= length(coordy)))
}

data.xy<- data.frame(x= coordx, y= coordy)

cases_points<- SpatialPointsDataFrame(coords= data.frame(x= coordx, y= coordy), data= data.xy)
writePointsShape(cases_points, "cases_ppp")


#### This spatial-points shapefile with cases can be loaded into the ScapeToad program together with a administrative region shapefile 
# with the number of population in each geographic area. You can then simultaneously deform the point pattern and the administrative region shapefile
# by background population. The output can be saved again as shapefiles and the script cart_shapefile_to_map.r generates a map out of these shapefiles.

