library(tmap)
library(rgdal)
library(sf)
library(gmodels)
library(plyr)
library(dplyr)
library(geosphere)
library(sfheaders)
library(readxl)
library(GISTools)


# Read shp files and convert to sf objects
planning_area <- st_as_sf(readOGR("C:/Users/user/Desktop/Study/BT4015/Project/Data/planning-area.shp"))
planning_area_st <- readOGR("C:/Users/user/Desktop/Study/BT4015/Project/Data/planning-area.shp")
npc_area <- st_as_sf(readOGR("C:/Users/user/Desktop/Study/BT4015/Project/Data/npc-area.shp"))
community_club <- st_as_sf(readOGR("C:/Users/user/Desktop/Study/BT4015/Project/Data/community-clubs.shp"))
hawker_center <- st_as_sf(readOGR("C:/Users/user/Desktop/Study/BT4015/Project/Data/hawker-centres.shp"))
# Read non-spatial data files
income <- read_excel("C:/Users/user/Desktop/Study/BT4015/Project/Data/Household Income.xlsx")
crime_per_npc <- read.csv("C:/Users/user/Desktop/Study/BT4015/Project/Data/Crime Rate in Each NPC.csv")
combined_attributes_with_total <- read_excel("C:/Users/user/Desktop/Study/BT4015/Project/Data/Combined Attributes.xlsx")
combined_attributes <- combined_attributes_with_total[combined_attributes_with_total$`Planning Area of Residence` != 'Total', ] 
combined_attributes$`Planning Area of Residence` <- lapply(combined_attributes$`Planning Area of Residence`, toupper)

# Data cleaning for npc_area

## Check validity
tmap_options(check.and.fix = TRUE)
st_is_valid(npc_area)
npc_area <- st_make_valid(npc_area)
st_is_valid(npc_area)

## Group separate polygons with the same NPC_NAME into one row
## npc_area should have 36 rows
npc_area <- npc_area %>% group_by(NPC_NAME) %>% 
                    summarise(geometry = sf::st_union(geometry)) %>%
                    ungroup()


# Quick map for planning_area and npc_area
qtm(planning_area, fill = "lightblue")
qtm(npc_area, fill = "lightgreen")


# Intersection
#intersection_area <- st_intersection(planning_area, npc_area)
#qtm(intersection_area)
#tm_shape(intersection_area) + 
#  tm_fill("gray") + 
#  tm_borders("black")


# Frequency count: planning area | npc | number of times they intersect
#counts <- ddply(intersection_area, .(intersection_area$PLN_AREA_N, intersection_area$NPC_NAME), nrow)
#counts


# Centroid approach

## Get centroid of planning area polygons
planning_area_centroid <- centroid(readOGR("C:/Users/user/Desktop/Study/BT4015/Project/Data/planning-area.shp"))
planning_area_centroid_sf <- SpatialPointsDataFrame(coords = planning_area_centroid, data = as.data.frame(planning_area_centroid))
qtm(planning_area_centroid_sf)

## Get centroid of planning area polygons
npc_area_centroid <- centroid(readOGR("C:/Users/user/Desktop/Study/BT4015/Project/Data/npc-area.shp"))
npc_area_centroid_sf <- SpatialPointsDataFrame(coords = npc_area_centroid, data = as.data.frame(npc_area_centroid))

## Plot planning area and centroid of planning area
tm_shape(planning_area) + 
  tm_fill("gray") + 
  tm_borders("black") +
  tm_shape(planning_area_centroid_sf) +
  tm_dots(size = 0.1)

## Plot npc area and centroid of planning area
tm_shape(npc_area) + 
  tm_fill("gray") + 
  tm_borders("black") +
tm_shape(planning_area_centroid_sf) +
  tm_dots(size = 0.1)

## Plot 2 centroids against each other
tm_shape(planning_area_centroid_sf) +
  tm_dots(size = 0.1, col = 'red') + 
tm_shape(npc_area_centroid_sf) +
  tm_dots(size = 0.1)


# 27 planning areas in household income
name_27 = lapply(income$`Planning Area of Residence`, toupper)
# 30 planning areas of concern
name_30 <- append(name_27, c('QUEENSTOWN', 'RIVER VALLEY', 'SEMBAWANG'))
index_30 = planning_area$PLN_AREA_N %in% name_30
planning_area_30 <- planning_area[index_30,]

tm_shape(planning_area) + 
  tm_fill("gray") + 
  tm_borders("black") +
tm_shape(planning_area_30) + 
  tm_fill("blue") + 
  tm_borders("black") +
tm_shape(planning_area_27) + 
  tm_fill("red") + 
  tm_borders("black") 


# Plot crime count per NPC against distribution of CC and HC
npc_crime <- merge(npc_area, crime_per_npc, by = "NPC_NAME")

tmap_mode("view")
tm_shape(npc_crime) + 
  tm_fill("crime.rate.npc.area") + 
  tm_borders("black") +
tm_shape(community_club) + 
  tm_dots(col = "blue", size = 0.03) +
tm_shape(hawker_center) + 
  tm_dots(col = "red", size = 0.03)



# Count points in each polygon
## Count the number of CC per NPC
cc_npc_intersection <- st_intersection(x = npc_crime, y = community_club)
cc_npc_count <- as.data.frame(cc_npc_intersection %>% group_by(NPC_NAME) %>% count())[, 1:2]
names(cc_npc_count)[names(cc_npc_count) == 'n'] <- 'CC_count'
## Count the number of HC per NPC
hc_npc_intersection <- st_intersection(x = npc_crime, y = hawker_center)
hc_npc_count <- as.data.frame(hc_npc_intersection %>% group_by(NPC_NAME) %>% count())[, 1:2]
names(hc_npc_count)[names(hc_npc_count) == 'n'] <- 'HC_count'
## Join npc_crime with counts of CC and HC
npc_crime <- merge(npc_crime, cc_npc_count, by = 'NPC_NAME', all.x=TRUE)
npc_crime <- merge(npc_crime, hc_npc_count, by = 'NPC_NAME', all.x=TRUE)


# Join planning area with Combined Attributes
planning_area_30 <- merge(planning_area_30, combined_attributes, 
                          by.x = 'PLN_AREA_N', by.y = 'Planning Area of Residence')
