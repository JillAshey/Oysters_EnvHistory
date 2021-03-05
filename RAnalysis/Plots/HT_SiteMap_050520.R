#Annie Chesapeake Bay site map
#Sent to me on 5/4/20

library(readxl)
CSIO_loc <- read_excel("Jill's Honor's Locations.xlsx")

#creates a buffer in decimal degrees around points to show geography
buffer <- 0.11

#How you read in coordinates by giving the same min and max values for each bound
geo_bounds <- c(left = min(CSIO_loc$Long)-buffer, 
                bottom = min(CSIO_loc$Lat)-buffer, 
                right = max(CSIO_loc$Long)+buffer, 
                top = max(CSIO_loc$Lat)+buffer)

#Now we are taking the four coordinates from the previous step and making a box via a dataframe by indexing into geo_bounds above
Sites.grid <- expand.grid(lon_bound = c(geo_bounds[1], geo_bounds[3]), 
                          lat_bound = c(geo_bounds[2], geo_bounds[4]))

#load the sp packages
library(sp)
library(rgdal)
library(raster)

#This converts the numbers in our sites.grid df into spatial components
coordinates(Sites.grid) <- ~ lon_bound + lat_bound

#now you need to load a shapefile into r using this package
CB <- readOGR(dsn = "Desktop/VIMS/CB_GIS_Shp/VA_shapefiles",layer = "fullcoast_dd") #annie code

#Trying to open shapefile on my own computer
CB <- readOGR(dsn = "data/Full_CBay_Shp",layer = "fullcoast_dd") #my shapefile

file.exists("data/Full_CBay_Shp")

#plot the shapefile 
plot(CB)
ggplot() + geom_polygon(data = CB, aes(x=long,y=lat, group=group), fill="gray", colour="black") +theme_classic()

CB_subset = subset(CB, xmin =)

#subset the data set for just the VA portion of the Bay
VA_CB <- subset(CB, STATE == "Virginia")
plot(VA_CB)



library(raster)
library(rgeos)

#Image will be cropped to the spatial extent of my sites. MUST MAKE SURE that the shapefile and your coordinates are the same unit.
VA_CB_crop <- crop(VA_CB, extent(Sites.grid))
plot(VA_CB_crop)

library(ggplot2)
library(ggrepel)

#plot data points in ggpolt -> geom_label_repel is a way to plot the data point labels in boxes that wont overlap each other. segment.alpha is what gets rid of the lines pointing to each point. 
#Annie code
quartz()
ggplot() + geom_polygon(data = VA_CB_crop, aes(x=long,y=lat, group=group), fill="gray", colour="black") + 
  coord_equal() +
  geom_point(data = CSIO_loc, aes(x=Long, y=Lat), color = "black", size = 4) +
  geom_point(data=CSIO_loc, aes(x=Long, y=Lat), size = 3) +
  scale_color_manual(values=c("red", "green", "blue")) +
  geom_label_repel(data = CSIO_loc, aes(x=Long, y=Lat, label = CSIO_loc$`Site Name`), direction = "both", nudge_y = 0.015, segment.alpha = 0) +
  labs(x="Longitude", y="Latitude") +
  theme_classic() +
  theme_classic() +
  theme (legend.box.background = element_rect(), legend.title.align = 0.5, legend.title = element_text(face = "bold"), legend.text=element_text(size= 12)) 


#plot data points in ggpolt -> geom_label_repel is a way to plot the data point labels in boxes that wont overlap each other. segment.alpha is what gets rid of the lines pointing to each point. 
#my code
quartz()
ggplot() + geom_polygon(data = VA_CB_crop, aes(x=long,y=lat, group=group), fill="gray", colour="black") +
  coord_equal() +
  geom_point(data = CSIO_loc, aes(x=Long, y=Lat), color = "black", size = 3) +
  geom_label_repel(data = CSIO_loc, aes(x=Long, y=Lat, label = CSIO_loc$`Site Name`), size = 4, direction = "both", nudge_y = 0.015, segment.alpha = 0) +
  labs(x="Longitude", y="Latitude") +
  theme_classic() +
  theme (legend.box.background = element_rect(), legend.title.align = 0.5, legend.title = element_text(face = "bold"), legend.text=element_text(size= 12)) 

ggsave("test", device = "jpeg", width = 6, height = 7, dpi = 600)



