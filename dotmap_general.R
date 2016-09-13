##### create a dot map ##########

library(maptools)
library(RColorBrewer)
library(rgeos)
library(ggplot2)
library(sp)

# set your working directory
setwd("C:/set/your/working/directory/")

# load your cases as a spatial points file (shapefile)
cases.ppp<- readShapeSpatial("cases_ppp.shp")
# put in a dataframe with x and y coordinate
cases.xy<- cases.ppp@coords
cases.xy<- data.frame(x= cases.xy[,1], y= cases.xy[,2])

# load your background file with areas to plot the cases on
prov<- readShapeSpatial( "Provincie.shp")
prov_f<- fortify(prov, region="geb_naam") #fortify to be able to plot with ggplot

# create df for plotting the north arrow
northarrowdf<- data.frame(x= c(min(prov_f$long)), y=c(min(prov_f$lat)+10000), arrow.length=c(20000))

# pick a color scheme
cols<- c("white", brewer.pal(n=6, "Reds"))


# plot the dot map
dotmap<- ggplot(prov_f, aes(x = long, y = lat)) +
  # create layers for shading of the map
  geom_polygon(data=prov_f, aes(x=long+3000, y= lat-3000, group=group), fill="gray60", colour=NA, alpha=0.1)+
  geom_polygon(data=prov_f, aes(x=long+2500, y= lat-2500, group=group), fill="gray60", colour=NA, alpha=0.2)+
  geom_polygon(data=prov_f, aes(x=long+2000, y= lat-2000, group=group), fill="gray60", colour=NA, alpha=0.5)+
  geom_polygon(data=prov_f, aes(x=long+1500, y= lat-1500, group=group), fill="gray60", colour=NA, alpha=0.7)+
  geom_polygon(data=prov_f, aes(x=long+1000, y= lat-1000, group=group), fill="gray60", colour=NA, alpha=0.9)+
  #add the normal map with white background
  geom_polygon(data=prov_f, aes(x= long, y=lat, group = group), colour=NA, fill="white")+
  #add the province boundaries
  geom_polygon(data=prov_f, aes(x= long, y=lat, group = group), colour="gray30", fill=NA,  size=0.3)+
  #add the cases
  geom_point(data= cases.xy, aes(x=x, y=y), colour=cols[6], alpha=0.5, size=0.05)+
  #equal distance lat and long
  coord_equal()+
  #layout
  theme_bw()+
  theme(axis.title.x=element_blank(), 
        axis.title.y=element_blank(),
        axis.text.x=element_blank(),
        axis.text.y=element_blank(),
        axis.ticks.y= element_blank(),
        axis.ticks.x= element_blank(),
        panel.grid.major= element_blank(),
        panel.grid.minor= element_blank(),
        panel.border= element_blank())+
  # add the north arrow
  geom_segment(data = northarrowdf, aes(x = x, y = y, xend = x, yend = y+arrow.length),
               arrow = arrow(length = unit(0.1,"cm")), size=0.1)+
  annotate("text", label = "N", x = northarrowdf$x , y = (northarrowdf$y+ (0.5*northarrowdf$arrow.length)), size = 3, colour = "black")+
  # add the scale bar
  geom_rect(data= prov_f, aes(xmin= min(long), xmax=min(long)+50000, ymin=min(lat), ymax=min(lat)+5000), col="black", fill="white", size=0.1)+
  geom_rect(data= prov_f, aes(xmin= min(long)+50000, xmax=min(long)+100000, ymin=min(lat), ymax=min(lat)+5000), col="black", fill="black", size=0.1)+
  annotate("text", label = c("0km","50km", "100km"), x = c(min(prov_f$long), min(prov_f$long)+50000, min(prov_f$long)+100000) , 
           y = c(min(prov_f$lat)-5000, min(prov_f$lat)-5000, min(prov_f$lat)-5000), size = 1.5, colour = "black")


# write to pdf

pdf("dotmap.pdf")
dotmap
dev.off()
