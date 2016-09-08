####### From cartogram shapefiles to map #######

# this script transforms the output cartogram shapefiles from ScapeToad into a nice looking map.

# What do you need?
# transformed point shapefile with cases
# transformed background shapefile 
# optional: additional layers with information (also transformed) with for example the province boundaries

# first set working directory
setwd("N:/Losse_opdrachten/Gudrun_Salmonella/")

# load packages
library(sp)
library(maptools)
library(ggplot2)
library(RColorBrewer)


# load your transformed shapefiles
# first cases
cases_t<- readShapeSpatial("cases_ppp_cart.shp")
# municipality
gem_t<- readShapeSpatial("gem_2015_cart.shp")
# province boundaries
prov_t<- readShapeSpatial("Provincie_cart.shp")


# to be able to plot with ggplot, the spatial area files need to be fortified
gem_t_f<- fortify(gem_t, region = "gemnr")
prov_t_f<- fortify(prov_t, region= "geb_code")


# the point file with cases needs to be tranformed to a dataframe with x and y coordinate
cases_xy<- data.frame(x= cases_t@coords[,1], y= cases_t@coords[,2])


#now plot

cartogram<- ggplot(prov_t_f, aes(x= long, y= lat))+
  # create grey layers with different alpha levels to create shading of the map
  geom_polygon(data= prov_t_f, aes(x= long+3000, y= lat-3000, group= group), fill= "gray60", colour=NA, alpha=0.1)+
  geom_polygon(data= prov_t_f, aes(x= long+2500, y= lat-2500, group= group), fill= "gray60", colour=NA, alpha=0.2)+
  geom_polygon(data= prov_t_f, aes(x= long+2000, y= lat-2000, group= group), fill= "gray60", colour=NA, alpha=0.5)+
  geom_polygon(data= prov_t_f, aes(x= long+1500, y= lat-1500, group= group), fill= "gray60", colour=NA, alpha=0.7)+
  geom_polygon(data= prov_t_f, aes(x= long+1000, y= lat-1000, group= group), fill= "gray60", colour=NA, alpha=0.9)+
  # add a layer with white background
  geom_polygon(data= prov_t_f, aes(x= long, y= lat, group= group), colour= NA, fill= "white", size= 0.3)+
  # add municipality boundaries
  geom_polygon(data= gem_t_f, aes(x= long, y= lat, group= group), colour= "gray90", fill= NA, size= 0.3)+
  # add province boundaries
  geom_polygon(data= prov_t_f, aes(x= long, y= lat, group= group), colour= "gray30", fill= NA, size= 0.4)+
  # add the cases
  geom_point(data= cases_xy, aes(x= x, y= y), colour= brewer.pal(n= 9, "Reds")[8], alpha=0.5, size=2)+
  #equal distance lat and long
  coord_equal()+
  #layout (remove axes, grid lines and titles)
  theme_bw()+
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y= element_blank(),
        axis.ticks.x= element_blank(),
        panel.grid.major= element_blank(),
        panel.grid.minor= element_blank(),
        panel.border= element_blank())


# export to pdf

pdf("cartogram.pdf")
cartogram
dev.off()

