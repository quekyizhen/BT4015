###################
# Import packages #
###################
library(tmap)
library(rgdal)
library(sf)
library(GISTools)
library(rgeos)
library(sp)
library(raster)
library(spatstat)
library(leaflet)
library(leaflet.extras)
library(geosphere)

##############
# Read files #
##############
npc <- st_as_sf(readOGR("C:/Users/Yi Zhen/Desktop/Y4S1/BT4015/Project/Datasets/NPC_all_attributes.shp"))
hawker_centres <- st_as_sf(readOGR("C:/Users/Yi Zhen/Desktop/Y4S1/BT4015/Project/Datasets/hawker-centres.shp"))
community_clubs <- st_as_sf(readOGR("C:/Users/Yi Zhen/Desktop/Y4S1/BT4015/Project/Datasets/community-clubs.shp"))
tourist_attraction <- st_as_sf(readOGR("C:/Users/Yi Zhen/Desktop/Y4S1/BT4015/Project/Datasets/tourism.shp"))

# Read in and replace column names for the NPC shp file 
col_names <- read.csv("C:/Users/Yi Zhen/Desktop/Y4S1/BT4015/Project/Datasets/NPC_all_attributes_col_names.csv")
colnames(npc) = col_names$column_names

# Switch between tmap modes
tmap_mode('view') 
tmap_mode('plot') 

####################
# Non-spatial Data #
####################

# Distribution of Elderly Ratio By NPC
tm_shape(npc) + 
  tm_fill("Scaled.Elderly.Ratio", title = "Ratio of Elderly Residents by NPC") + 
  tm_borders("black") +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.7,
            legend.bg.alpha = 1, legend.outside = TRUE) + 
  tm_scale_bar(position=c("right", "bottom")) + 
  tm_compass(type="8star", position=c("right", "top"), show.labels = 3, size=2, text.size=0.5)

# Distribution of Gender Ratio By NPC
tm_shape(npc) + 
  tm_fill("Scaled.Male.Ratio", title = "Ratio of Male Residents by NPC") + 
  tm_borders("black") +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.7,
            legend.bg.alpha = 1, legend.outside = TRUE) + 
  tm_scale_bar(position=c("right", "bottom")) + 
  tm_compass(type="8star", position=c("right", "top"), show.labels = 3, size=2, text.size=0.5)

tm_shape(npc) + 
  tm_fill("Scaled.Female.Ratio", title = "Ratio of Female Residents by NPC") + 
  tm_borders("black") +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.7,
            legend.bg.alpha = 1, legend.outside = TRUE) + 
  tm_scale_bar(position=c("right", "bottom")) + 
  tm_compass(type="8star", position=c("right", "top"), show.labels = 3, size=2, text.size=0.5)

# Distribution of Ethnicity Ratio By NPC
tm_shape(npc) + 
  tm_fill("Scaled.Chinese.Ratio", title = "Ratio of Chinese Residents by NPC") + 
  tm_borders("black") +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.7,
            legend.bg.alpha = 1, legend.outside = TRUE) + 
  tm_scale_bar(position=c("right", "bottom")) + 
  tm_compass(type="8star", position=c("right", "top"), show.labels = 3, size=2, text.size=0.5)

tm_shape(npc) + 
  tm_fill("Scaled.Malay.Ratio", title = "Ratio of Malay Residents by NPC") + 
  tm_borders("black") +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.7,
            legend.bg.alpha = 1, legend.outside = TRUE) + 
  tm_scale_bar(position=c("right", "bottom")) + 
  tm_compass(type="8star", position=c("right", "top"), show.labels = 3, size=2, text.size=0.5)

tm_shape(npc) + 
  tm_fill("Scaled.Indian.Ratio", title = "Ratio of Indian Residents by NPC") + 
  tm_borders("black") +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.7,
            legend.bg.alpha = 1, legend.outside = TRUE) + 
  tm_scale_bar(position=c("right", "bottom")) + 
  tm_compass(type="8star", position=c("right", "top"), show.labels = 3, size=2, text.size=0.5)

tm_shape(npc) + 
  tm_fill("Scaled.Others.Ratio", title = "Ratio of Other Ethnicity Residents by NPC") + 
  tm_borders("black") +
  tm_layout(legend.title.size = 0.7,
            legend.text.size = 0.7,
            legend.outside = TRUE) + 
  tm_scale_bar(position=c("right", "bottom")) + 
  tm_compass(type="8star", position=c("right", "top"), show.labels = 3, size=2, text.size=0.5)

# Distribution of Household Income By NPC

tm_shape(npc) + 
  tm_fill("Scaled.Households.Above.Median.Income.Density", title = "Density of Household Above Median Income by NPC") + 
  tm_borders("black") +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.7,
            legend.bg.alpha = 1, legend.outside = TRUE) + 
  tm_scale_bar(position=c("right", "bottom")) + 
  tm_compass(type="8star", position=c("right", "top"), show.labels = 3, size=2, text.size=0.5)

tm_shape(npc) + 
  tm_fill("Scaled.Households.Below.Median.Income.Density", title = "Density of Household Below Median Income by NPC") + 
  tm_borders("black") +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.7,
            legend.bg.alpha = 1, legend.outside = TRUE) + 
  tm_scale_bar(position=c("right", "bottom")) + 
  tm_compass(type="8star", position=c("right", "top"), show.labels = 3, size=2, text.size=0.5)

# Distribution of Type of Dwellings By NPC
tm_shape(npc) + 
  tm_fill("Scaled.HDB.Housing.Density", title = "Density of HDB Housing by NPC") + 
  tm_borders("black")  +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.7,
            legend.bg.alpha = 1, legend.show = TRUE, legend.outside = TRUE) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type="8star", position=c("right", "top"), show.labels = 3, size=2, text.size=0.5)

tm_shape(npc) + 
  tm_fill("Scaled.Private.Housing.Density", title = "Density of Private Housing by NPC") + 
  tm_borders("black")  +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.7,
            legend.bg.alpha = 1, legend.show = TRUE, legend.outside = TRUE) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type="8star", position=c("right", "top"), show.labels = 3, size=2, text.size=0.5)

# Distribution of Population Density by NPC
tm_shape(npc) + 
  tm_fill("Scaled.Population.Density", title = "Population Density by NPC") + 
  tm_borders("black")  +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.7,
            legend.bg.alpha = 1, legend.show = TRUE, legend.outside = TRUE) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type="8star", position=c("right", "top"), show.labels = 3, size=2, text.size=0.5)

################
# Spatial Data #
################

# Distribution of Crime Rate by NPC

## Choropleth map
tm_shape(npc) + 
  tm_fill("crime.rate.npc.area", title = "Crime Rate by NPC") + 
  tm_borders("black")  +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.7,
            legend.bg.alpha = 1, legend.outside = TRUE) + 
  tm_scale_bar(position=c("right", "bottom")) + 
  tm_compass(type="8star", position=c("right", "top"), show.labels = 3, size=2, text.size=0.5)

## Heat map
npc_polygon_centroid <- centroid(readOGR("C:/Users/Yi Zhen/Desktop/Y4S1/BT4015/Project/Datasets/NPC_all_attributes.shp"))
npc_centroid_sf <- SpatialPointsDataFrame(coords = npc_polygon_centroid, data = as.data.frame(npc))

pal <- colorNumeric(
  palette = "YlOrRd",
  domain = npc_centroid_sf$crime.rate.npc.area
)

leaflet(npc) %>% 
  addPolygons(color = "black", weight = 1, smoothFactor = 0.5,
              opacity = 1.0, fillOpacity = 0, fillColor= "white")%>%
     addHeatmap(data = npc_centroid_sf, radius=20, intensity = npc_centroid_sf$crime.rate.npc.area, blur = 50, gradient= "YlOrRd") %>%
  addLegend("topright", pal = pal, values = npc_centroid_sf$crime.rate.npc.area,
            title = "Crime Rate by NPC",
            opacity = 1)

# Distribution of Hawker Centres by NPC
tm_shape(npc) +
  tm_fill("crime.rate.npc.area", title = "Crime Rate by NPC") + 
  tm_borders("black") +
  tm_shape(hawker_centres) + 
  tm_dots(col = "green", size = 0.1) +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.7,
            legend.bg.alpha = 1, legend.outside = TRUE) + 
  tm_scale_bar(position=c("right", "bottom")) + 
  tm_compass(type="8star", position=c("right", "top"), show.labels = 3, size=2, text.size=0.5)

# Distribution of Community Clubs by NPC
tm_shape(npc)  +
  tm_fill("crime.rate.npc.area", title = "Crime Rate by NPC") + 
  tm_borders("black") +
  tm_shape(community_clubs) + 
  tm_dots(col = "blue", size = 0.1) +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.7,
            legend.bg.alpha = 1, legend.outside = TRUE) + 
  tm_scale_bar(position=c("right", "bottom")) + 
  tm_compass(type="8star", position=c("right", "top"), show.labels = 3, size=2, text.size=0.5)

# Distribution of Tourist Attractions by NPC
tm_shape(npc) +
  tm_fill("crime.rate.npc.area", title = "Crime Rate by NPC") + 
  tm_borders("black") +
  tm_shape(tourist_attraction) + 
  tm_dots(col = "purple", size = 0.1) +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.7,
            legend.bg.alpha = 1, legend.outside = TRUE) + 
  tm_scale_bar(position=c("right", "bottom")) + 
  tm_compass(type="8star", position=c("right", "top"), show.labels = 3, size=2, text.size=0.5)

# Distribution of Elderly Ratio By NPC
tm_shape(npc) + 
  tm_fill("crime.rate.npc.area", title = "Crime Rate by NPC") + 
  tm_bubbles(size="Scaled.Elderly.Ratio", col="grey", breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8)) +
  tm_borders("black") +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.7,
            legend.bg.alpha = 1, legend.outside = TRUE) + 
  tm_scale_bar(position=c("right", "bottom")) + 
  tm_compass(type="8star", position=c("right", "top"), show.labels = 3, size=2, text.size=0.5)


# Distribution of Gender Ratio By NPC
tm_shape(npc) + 
tm_fill("crime.rate.npc.area", title = "Gender Ratio by NPC") + 
  tm_bubbles(size="Scaled.Female.Ratio", col="pink", alpha=1) +
tm_shape(npc) + 
  tm_bubbles(size="Scaled.Male.Ratio", col = "blue", alpha =1) +
tm_borders("black") + 
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.7,
            legend.bg.alpha = 1, legend.outside = TRUE) + 
  tm_scale_bar(position=c("right", "bottom")) + 
  tm_compass(type="8star", position=c("right", "top"), show.labels = 3, size=2, text.size=0.5)


# Distribution of Ethnicity By NPC
tm_shape(npc) + 
  tm_fill("crime.rate.npc.area", title = "Ratio of Chinese Residents by NPC") + 
  tm_bubbles(size="Scaled.Chinese.Ratio", col = c("blue"), alpha = 1) +
  tm_borders("black")  +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.7,
            legend.bg.alpha = 1, legend.outside = TRUE) + 
  tm_scale_bar(position=c("right", "bottom")) + 
  tm_compass(type="8star", position=c("right", "top"), show.labels = 3, size=2, text.size=0.5)

tm_shape(npc) + 
  tm_fill("crime.rate.npc.area", title = "Ratio of Malay Residents by NPC") + 
    tm_bubbles(size="Scaled.Malay.Ratio", col = c("green"), alpha = 1) +
  tm_borders("black")  +
  tm_layout(legend.title.size = 0.7,
            legend.text.size = 0.7,
            legend.bg.alpha = 1, legend.outside = TRUE) + 
  tm_scale_bar(position=c("right", "bottom")) + 
  tm_compass(type="8star", position=c("right", "top"), show.labels = 3, size=2, text.size=0.5)

tm_shape(npc) + 
  tm_fill("crime.rate.npc.area", title = "Ratio of Indian Residents by NPC") + 
  tm_bubbles(size="Scaled.Indian.Ratio", col = c("yellow"), alpha = 1) +
  tm_borders("black")  +
  tm_layout(legend.title.size = 0.7,
            legend.text.size = 0.7,
            legend.bg.alpha = 1, legend.outside = TRUE) + 
  tm_scale_bar(position=c("right", "bottom")) + 
  tm_compass(type="8star", position=c("right", "top"), show.labels = 3, size=2, text.size=0.5)

tm_shape(npc) + 
  tm_fill("crime.rate.npc.area", title = "Ratio of Other Residents by NPC") + 
  tm_bubbles(size="Scaled.Others.Ratio", col = c("pink"), alpha = 1) +
  tm_borders("black")  +
  tm_layout(legend.title.size = 0.8,
            legend.text.size = 0.7,
            legend.bg.alpha = 1, legend.outside = TRUE) + 
  tm_scale_bar(position=c("right", "bottom")) + 
  tm_compass(type="8star", position=c("right", "top"), show.labels = 3, size=2, text.size=0.5)

# Distribution of Household Income By NPC
tm_shape(npc) + 
  tm_fill("crime.rate.npc.area", title = "Household Income below Median Density by NPC") + 
  tm_bubbles(size="Scaled.Households.Below.Median.Income.Density", col = "blue", alpha = 1) +
  tm_borders("black")  +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.7,
            legend.bg.alpha = 1, legend.outside = TRUE) + 
  tm_scale_bar(position=c("right", "bottom")) + 
  tm_compass(type="8star", position=c("right", "top"), show.labels = 3, size=2, text.size=0.5)


tm_shape(npc) + 
  tm_fill("crime.rate.npc.area", title = "Household Income above Median Density by NPC") + 
  tm_bubbles(size="Scaled.Households.Above.Median.Income.Density", col = "green", alpha = 1) +
  tm_borders("black")  +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.7,
            legend.bg.alpha = 1, legend.outside = TRUE) + 
  tm_scale_bar(position=c("right", "bottom")) + 
  tm_compass(type="8star", position=c("right", "top"), show.labels = 3, size=2, text.size=0.5)

tm_shape(npc) + 
  tm_fill("crime.rate.npc.area", title = "Crime Rate by NPC") + 
  tm_bubbles(size="Scaled.Households.Above.Median.Income.Density", col = "green", alpha = 1) +
  tm_shape(npc) + 
  tm_bubbles(size="Scaled.Households.Below.Median.Income.Density", col = "blue", alpha = 1) +
  tm_borders("black") +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.7,
            legend.bg.alpha = 1, legend.outside = TRUE) + 
  tm_scale_bar(position=c("right", "bottom")) + 
  tm_compass(type="8star", position=c("right", "top"), show.labels = 3, size=2, text.size=0.5)

# Distribution of Type of dwelling By NPC
tm_shape(npc) + 
  tm_fill("crime.rate.npc.area", title = "Density of HDB Housing by NPC") + 
  tm_bubbles(size="Scaled.HDB.Housing.Density", col = c("blue"), alpha = 0.5) +
  tm_borders("black")  +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.7,
            legend.bg.alpha = 1, legend.outside = TRUE) + 
  tm_scale_bar(position=c("right", "bottom")) + 
  tm_compass(type="8star", position=c("right", "top"), show.labels = 3, size=2, text.size=0.5)

tm_shape(npc) + 
  tm_fill("crime.rate.npc.area", title = "Density of Private Housing by NPC") + 
  tm_bubbles(size="Scaled.Private.Housing.Density", col = c("green"), alpha = 0.5) +
  tm_borders("black")  +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.7,
            legend.bg.alpha = 1, legend.outside = TRUE) + 
  tm_scale_bar(position=c("right", "bottom")) + 
  tm_compass(type="8star", position=c("right", "top"), show.labels = 3, size=2, text.size=0.5)

tm_shape(npc) + 
  tm_fill("crime.rate.npc.area", title = "Crime Rate by NPC") + 
  tm_bubbles(size="Scaled.Private.Housing.Density", col = c("green"), alpha = 1) +
  tm_shape(npc) + 
  tm_bubbles(size="Scaled.HDB.Housing.Density", col = c("blue"), alpha = 1) +
  tm_borders("black") +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.7,
            legend.bg.alpha = 1, legend.outside = TRUE) + 
  tm_scale_bar(position=c("right", "bottom")) + 
  tm_compass(type="8star", position=c("right", "top"), show.labels = 3, size=2, text.size=0.5)

# Distribution of Population Density by NPC
tm_shape(npc) + 
  tm_fill("crime.rate.npc.area", title = "Crime Rate by NPC") + 
  tm_bubbles(size="Scaled.Population.Density", col = "grey", alpha = 1) +
  tm_borders("black")  +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.7,
            legend.bg.alpha = 1, legend.show = TRUE, legend.outside = TRUE) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type="8star", position=c("right", "top"), show.labels = 3, size=2, text.size=0.5)
