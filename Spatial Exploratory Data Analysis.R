library(tmap)
library(rgdal)
library(sf)
library(GISTools)
library(rgeos)
library(ggsubplot)
library(ggplot2)
library(sp)
library(raster)
library(spatstat)

# Read shp files and convert to sf objects
npc_area <- st_as_sf(readOGR("C:/Users/Yi Zhen/Desktop/Y4S1/BT4015/Project/Datasets/npc.shp"))
npc <- st_as_sf(readOGR("C:/Users/Yi Zhen/Desktop/Y4S1/BT4015/Project/Datasets/NPC_all_attributes.shp"))
hawker_centres <- st_as_sf(readOGR("C:/Users/Yi Zhen/Desktop/Y4S1/BT4015/Project/Datasets/hawker-centres.shp"))
community_clubs <- st_as_sf(readOGR("C:/Users/Yi Zhen/Desktop/Y4S1/BT4015/Project/Datasets/community-clubs.shp"))
tourist_attraction <- st_as_sf(readOGR("C:/Users/Yi Zhen/Desktop/Y4S1/BT4015/Project/Datasets/tourism.shp"))
col_names <- read.csv("C:/Users/Yi Zhen/Desktop/Y4S1/BT4015/Project/Datasets/NPC_all_attributes_col_names.csv")
colnames(npc) = col_names$column_names
planning_area <- st_as_sf(readOGR("C:/Users/Yi Zhen/Desktop/Y4S1/BT4015/Project/Datasets/planning-area.shp"))

dev.off()
tmap_mode('view') 
tmap_mode('plot') 
# Crime Count by NPC
## Chloropleth map
tm_shape(npc) + 
  tm_fill("crime.rate.npc.area", title = "Crime Rate by NPC") + 
  tm_borders("black")  +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.5,
            legend.bg.alpha = 1, legend.outside = TRUE) + 
  tm_scale_bar(position=c("right", "bottom")) + 
  tm_compass(type="8star", position=c("right", "top"), show.labels = 3, size=2, text.size=0.5)

tm_shape(npc) + 
  tm_fill("crime_count", title = "Crime Count by NPC") + 
  tm_borders("black")  +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.5,
            legend.bg.alpha = 1)
## Heatmap
leaflet() %>% 
     addProviderTiles(provider = providers$OpenTopoMap) %>% 
     addHeatmap(data = npc, intensity = npc$crm_cnt, blur = 50)


# HC
tm_shape(npc) +
  tm_fill("crime.rate.npc.area", title = "Crime Rate by NPC") + 
  tm_borders("black") +
  tm_shape(hawker_centres) + 
  tm_dots(col = "green", size = 0.1) +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.5,
            legend.bg.alpha = 1, legend.outside = TRUE) + 
  tm_scale_bar(position=c("right", "bottom")) + 
  tm_compass(type="8star", position=c("right", "top"), show.labels = 3, size=2, text.size=0.5)


## KDE 
hc = kde.points(hawker_centres,lims=npc)
level.plot(hc)
masker = poly.outer(hc,npc,extend=0)
plot(npc,add=TRUE)

# CC
tm_shape(npc)  +
  tm_fill("crime.rate.npc.area", title = "Crime Rate by NPC") + 
  tm_borders("black") +
  tm_shape(community_clubs) + 
  tm_dots(col = "blue", size = 0.1) +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.5,
            legend.bg.alpha = 1, legend.outside = TRUE) + 
  tm_scale_bar(position=c("right", "bottom")) + 
  tm_compass(type="8star", position=c("right", "top"), show.labels = 3, size=2, text.size=0.5)


cc = kde.points(community_clubs,lims=npc)
level.plot(cc)
masker = poly.outer(hc,npc,extend=0)
plot(npc,add=TRUE)

# Tourist Attractions
tm_shape(npc) +
  tm_fill("crime.rate.npc.area", title = "Crime Rate by NPC") + 
  tm_borders("black") +
  tm_shape(tourist_attraction) + 
  tm_dots(col = "purple", size = 0.1) +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.5,
            legend.bg.alpha = 1, legend.outside = TRUE) + 
  tm_scale_bar(position=c("right", "bottom")) + 
  tm_compass(type="8star", position=c("right", "top"), show.labels = 3, size=2, text.size=0.5)


# Elderly

tm_shape(npc) + 
  tm_fill("crime.rate.npc.area", title = "Elderly Ratio by NPC") + 
  tm_bubbles(size="Scaled.Elderly.Ratio", col="grey", breaks=c(0, 0.1, 0.2, 0.3, 0.4, 0.5, 0.6, 0.7, 0.8)) +
  tm_borders("black") +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.5,
            legend.bg.alpha = 1, legend.outside = TRUE) + 
  tm_scale_bar(position=c("right", "bottom")) + 
  tm_compass(type="8star", position=c("right", "top"), show.labels = 3, size=2, text.size=0.5)




# Gender
tmap_mode('plot')
tm_shape(npc) + 
tm_fill("crime.rate.npc.area", title = "Gender Ratio by NPC") + 
tm_bubbles(size="Scaled.Male.Ratio", col = "blue", alpha =0.5) +
tm_shape(npc) + 
tm_bubbles(size="Scaled.Female.Ratio", col="pink", alpha=0.5) +
tm_borders("black") + 
tm_layout(legend.title.size = 1,
          legend.text.size = 0.5,
          legend.bg.alpha = 1, legend.show = FALSE)

# Ethnicity 
tm_shape(npc) + 
  tm_fill("crime.rate.npc.area", title = "Density of Private Housing by NPC") + 
#  tm_bubbles(size="Scaled.Malay.Ratio", col = c("green"), alpha = 0.5) +
#  tm_shape(npc) +
  tm_bubbles(size="Scaled.Chinese.Ratio", col = c("blue"), alpha = 0.5) +
  tm_borders("black")  +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.5,
            legend.bg.alpha = 1, legend.show = FALSE)

# Income 
tm_shape(npc) + 
  tm_fill("crime.rate.npc.area", title = "Household Income below Median Density by NPC") + 
  tm_bubbles(size="Scaled.Households.Below.Median.Income.Density", col = "blue", alpha = 1) +
  tm_borders("black")  +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.5,
            legend.bg.alpha = 1, legend.show = FALSE)

tm_shape(npc) + 
  tm_fill("crime.rate.npc.area", title = "Household Income above Median Density by NPC") + 
  tm_bubbles(size="Scaled.Households.Above.Median.Income.Density", col = "green", alpha = 1) +
  tm_borders("black")  +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.5,
            legend.bg.alpha = 1, legend.show = FALSE)

tm_shape(npc) + 
  tm_fill("crime.rate.npc.area", title = "Crime Rate by NPC") + 
  tm_bubbles(size="Scaled.Households.Above.Median.Income.Density", col = "green", alpha = 1) +
  tm_shape(npc) + 
  tm_bubbles(size="Scaled.Households.Below.Median.Income.Density", col = "blue", alpha = 1) +
  tm_borders("black") +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.5,
            legend.bg.alpha = 1, legend.outside = TRUE) + 
  tm_scale_bar(position=c("right", "bottom")) + 
  tm_compass(type="8star", position=c("right", "top"), show.labels = 3, size=2, text.size=0.5)



# Type of dwelling
tm_shape(npc) + 
  tm_fill("crime.rate.npc.area", title = "Crime Rate by NPC") + 
  tm_bubbles(size="Scaled.Private.Housing.Density", col = c("green"), alpha = 1) +
  tm_shape(npc) + 
  tm_bubbles(size="Scaled.HDB.Housing.Density", col = c("blue"), alpha = 1) +
  tm_borders("black") +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.5,
            legend.bg.alpha = 1, legend.outside = TRUE) + 
  tm_scale_bar(position=c("right", "bottom")) + 
  tm_compass(type="8star", position=c("right", "top"), show.labels = 3, size=2, text.size=0.5)



tm_shape(npc) + 
  tm_fill("crime.rate.npc.area", title = "Density of HDB Housing by NPC") + 
  tm_bubbles(size="Scaled.HDB.Housing.Density", col = c("green"), alpha = 0.5) +
  tm_borders("black")  +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.5,
            legend.bg.alpha = 1, legend.show = FALSE)

tm_shape(npc) + 
  tm_fill("crime.rate.npc.area", title = "Density of Private Housing by NPC") + 
  tm_bubbles(size="Scaled.Private.Housing.Density", col = c("red"), alpha = 0.5) +
  tm_borders("black")  +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.5,
            legend.bg.alpha = 1, legend.show = FALSE)

#population 
tm_shape(npc) + 
  tm_fill("crime.rate.npc.area", title = "Crime Rate by NPC") + 
  tm_bubbles(size="Scaled.Population.Density", col = "grey", alpha = 1) +
  tm_borders("black")  +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.5,
            legend.bg.alpha = 1, legend.show = TRUE, legend.outside = TRUE) +
  tm_scale_bar(position=c("right", "bottom")) + tm_compass(type="8star", position=c("right", "top"), show.labels = 3, size=2, text.size=0.5)
