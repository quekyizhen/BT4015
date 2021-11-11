#################
# Load packages #
#################
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
library(raster)
library(rgeos)


#############
# Read data #
#############
# Read shp files and convert to sf objects
planning_area <- st_as_sf(readOGR("C:/Users/user/Desktop/Study/BT4015/Project/Data/planning-area.shp"))
planning_area_st <- readOGR("C:/Users/user/Desktop/Study/BT4015/Project/Data/planning-area.shp")
npc_area <- st_as_sf(readOGR("C:/Users/user/Desktop/Study/BT4015/Project/Data/npc-area.shp"))
community_club <- st_as_sf(readOGR("C:/Users/user/Desktop/Study/BT4015/Project/Data/community-clubs.shp"))
hawker_center <- st_as_sf(readOGR("C:/Users/user/Desktop/Study/BT4015/Project/Data/hawker-centres.shp"))
tourist_attraction <- st_as_sf(readOGR("C:/Users/user/Desktop/Study/BT4015/Project/Data/tourism.shp"))
# Read non-spatial data files
crime_per_npc <- read.csv("C:/Users/user/Desktop/Study/BT4015/Project/Data/Crime Rate in Each NPC.csv")
combined_attributes_with_total <- read_excel("C:/Users/user/Desktop/Study/BT4015/Project/Data/Combined Attributes.xlsx")
combined_attributes <- combined_attributes_with_total[combined_attributes_with_total$`Planning Area of Residence` != 'Total', ] 
combined_attributes$`Planning Area of Residence` <- lapply(combined_attributes$`Planning Area of Residence`, toupper)
coverage <- read.csv("C:/Users/user/Desktop/Study/BT4015/Project/Data/Percentage Cover of HC, CC, TA in each point buffer.csv")


##############################
# Data cleaning for npc_area #
##############################

## Correct validity for npc_area
tmap_options(check.and.fix = TRUE)
st_is_valid(npc_area)
npc_area <- st_make_valid(npc_area)
st_is_valid(npc_area)

## Remove kml_3 (Jurong Island) from npc_area
npc_area <- st_make_valid(npc_area[npc_area$Name != 'kml_3',])
## Group separate polygons with the same NPC_NAME into one row
## npc_area should have 36 rows
npc_area <- npc_area %>% group_by(NPC_NAME) %>% 
                    summarise(geometry = sf::st_union(geometry)) %>%
                    ungroup()

# Quick map for planning_area and npc_area
qtm(planning_area, fill = "lightblue")
qtm(npc_area, fill = "lightgreen")



###########################################
# Intersection of planning areas and NPCs #
###########################################
# 30 planning areas of concern
name_30 <- combined_attributes$`Planning Area of Residence`
index_30 = planning_area$PLN_AREA_N %in% name_30
planning_area_30 <- planning_area[index_30,]

# Join planning area with Combined Attributes
planning_area_30 <- merge(planning_area_30, combined_attributes, 
                          by.x = 'PLN_AREA_N', by.y = 'Planning Area of Residence')

## View the 30 planning areas of residence
tm_shape(planning_area) + 
  tm_fill("gray") + 
  tm_borders("black") +
tm_shape(planning_area_30) + 
  tm_fill("blue") + 
  tm_borders("black") 

## View the difference in boundary of planning areas and NPCs
tm_shape(npc_area) +
  tm_borders(col = "white", lwd = 3) +
tm_shape(planning_area_30) +
  tm_fill("lightgray") +
  tm_borders("black", lwd = 0.5) +
tm_shape(npc_area) +
  tm_borders(col = "red", lwd = 3)



# Plot crime count per NPC against distribution of CC and HC
npc_crime <- merge(npc_area, crime_per_npc, by = "NPC_NAME")
npc_crime$Area.of.NPC <- area(as(npc_crime, 'Spatial'))
npc_crime$NPC.Area.in.km2 <- npc_crime$Area.of.NPC / 1000000
npc_crime$crime.rate.npc.area <- npc_crime$crime_count / npc_crime$NPC.Area.in.km2

## View crime rate, CC, HC, TA
tmap_mode("view")
tm_shape(npc_crime) + 
  tm_fill("crime.rate.npc.area") + 
  tm_borders("black") +
tm_shape(community_club) + 
  tm_dots(col = "blue", size = 0.03) +
tm_shape(hawker_center) + 
  tm_dots(col = "red", size = 0.03) +
tm_shape(tourist_attraction) + 
  tm_dots(col = "green", size = 0.03)

## View crime rate, planning area, CC, HC, TA
tm_shape(npc_crime) + 
  tm_fill("crime.rate.npc.area") + 
tm_shape(planning_area_30) + 
  tm_borders("black") +
tm_shape(community_club) + 
  tm_dots(col = "blue", size = 0.03) +
tm_shape(hawker_center) + 
  tm_dots(col = "red", size = 0.03) +
tm_shape(tourist_attraction) + 
  tm_dots(col = "green", size = 0.03)



# Count number of points in each polygon
## Count the number of CC per NPC
cc_npc_intersection <- st_intersection(x = npc_crime, y = community_club)
cc_npc_count <- as.data.frame(cc_npc_intersection %>% group_by(NPC_NAME) %>% count())[, 1:2]
names(cc_npc_count)[names(cc_npc_count) == 'n'] <- 'CC_count'
## Count the number of HC per NPC
hc_npc_intersection <- st_intersection(x = npc_crime, y = hawker_center)
hc_npc_count <- as.data.frame(hc_npc_intersection %>% group_by(NPC_NAME) %>% count())[, 1:2]
names(hc_npc_count)[names(hc_npc_count) == 'n'] <- 'HC_count'
## Count the number of TA per NPC
ta_npc_intersection <- st_intersection(x = npc_crime, y = tourist_attraction)
ta_npc_count <- as.data.frame(ta_npc_intersection %>% group_by(NPC_NAME) %>% count())[, 1:2]
names(ta_npc_count)[names(ta_npc_count) == 'n'] <- 'TA_count'
## Join npc_crime with counts of CC and HC
npc_crime <- merge(npc_crime, cc_npc_count, by = 'NPC_NAME', all.x=TRUE)
npc_crime <- merge(npc_crime, hc_npc_count, by = 'NPC_NAME', all.x=TRUE)
npc_crime <- merge(npc_crime, ta_npc_count, by = 'NPC_NAME', all.x=TRUE)
## Get density of CC and HC per npc
npc_crime["CC_density"] <- npc_crime["CC_count"] / npc_crime["NPC.Area.in.km2"]
npc_crime["HC_density"] <- npc_crime["HC_count"] / npc_crime["NPC.Area.in.km2"]
npc_crime["TA_density"] <- npc_crime["TA_count"] / npc_crime["NPC.Area.in.km2"]

# Intersect NPC and planning area
intersection_full <- st_intersection(planning_area_30, npc_crime)
intersection_full$'intersected_area_km2' <- raster::area(as(intersection_full, 'Spatial')) / 1000000
# Get only necessary columns
intersection <- intersection_full[c("PLN_AREA_N", "Name", "Elderly.Ratio",
                                    "Chinese.Ratio", "Malay.Ratio", "Indian.Ratio",
                                    "Others.Ratio", "Male.Ratio", "Female.Ratio",
                                    "Population.Density", "HDB.Housing.Density",
                                    "Private.Housing.Density",
                                    "Households.Below.Median.Income.Density",
                                    "Households.Above.Median.Income.Density",
                                    "NPC_NAME", "geometry", "NPC.Area.in.km2",
                                    "intersected_area_km2")]
intersection$intersected_area_proportion <- intersection$intersected_area_km2 / intersection$NPC.Area.in.km2

# Plot NPC and intersection 
tm_shape(npc_crime) +
  tm_borders("black") +
  tm_fill("white") +
tm_shape(intersection) +
  tm_fill("gray") +
  tm_borders("brown") +
tm_shape(npc_crime) +
  tm_borders("black", lwd = 3)

#################################################
# Scaling variables from planning areas to NPCs #
#################################################

# Plot intersections within Bukit Merah East NPC
tm_shape(npc_crime[npc_crime$NPC_NAME == 'Bukit Merah East Neighbourhood Police Centre',]) +
  tm_borders("black") +
  tm_fill("white") +
tm_shape(intersection[intersection$NPC_NAME == 'Bukit Merah East Neighbourhood Police Centre',]) +
  tm_fill("gray") +
  tm_borders("brown") +
tm_shape(npc_crime[npc_crime$NPC_NAME == 'Bukit Merah East Neighbourhood Police Centre',]) +
  tm_borders("black", lwd = 3)

# Calculate coverage percentage
proportion <- aggregate(intersection$intersected_area_proportion, list(intersection$NPC_NAME), FUN=sum) 
proportion

# List of Ratio and Density columns
variable_column_list <- c("Elderly.Ratio", "Chinese.Ratio", "Malay.Ratio", "Indian.Ratio",
                          "Others.Ratio", "Male.Ratio", "Female.Ratio", "Population.Density", 
                          "HDB.Housing.Density", "Private.Housing.Density",
                          "Households.Below.Median.Income.Density",
                          "Households.Above.Median.Income.Density")
# Empty final data frame
final_data_frame <- data.frame(matrix(ncol = length(variable_column_list) + 1, nrow = 0))
# List of columns in the final data frame. Add "Scaled" to the Ratio and Density columns.
colnames(final_data_frame) <- append("NPC_NAME", paste0("Scaled.", variable_column_list))

# Scale all variables 
# Scaled variable = Sum(Proportion of intersected area * Variable in planning area) / Sum(Proportion of intersected area)
for (npc in npc_crime$NPC_NAME) {
  intersection_within_that_npc <- as.data.frame(intersection[intersection$NPC_NAME == npc,])
  row.names(intersection_within_that_npc) <- NULL
  npc_row <- c(npc)
  for (i in 3:(length(variable_column_list)+2)) {
    proportion_sum = 0
    proportion_variable = 0
    for (j in 1:nrow(intersection_within_that_npc)){
      proportion_sum = proportion_sum + intersection_within_that_npc$intersected_area_proportion[j]
      proportion_variable = proportion_variable + 
                  intersection_within_that_npc$intersected_area_proportion[j] *
                  intersection_within_that_npc[j, i]
    }
    npc_row <- append(npc_row, proportion_variable / proportion_sum)
  }
  npc_row_df <- data.frame(matrix(ncol = length(variable_column_list) + 1, nrow = 0))
  npc_row_df[1, ] = npc_row
  colnames(npc_row_df) <- append("NPC_NAME", paste0("Scaled.", variable_column_list))
  final_data_frame <- rbind(final_data_frame, npc_row_df)
  final_data_frame <- final_data_frame[!duplicated(final_data_frame),]
}

# Convert scaled columns to numeric
for (col in paste0("Scaled.", variable_column_list)) {
  final_data_frame[, col] <- as.numeric(unlist(final_data_frame[, col]))
}

# Join final_data_frame back to npc_crime
final_data_frame <- merge(final_data_frame, npc_crime, by = "NPC_NAME")

# Join final_data_frame with coverage
final_data_frame <- st_as_sf(merge(final_data_frame, coverage, by = "NPC_NAME", all.x = TRUE))

final_data_frame_spatial <- as_Spatial(final_data_frame)

tm_shape(final_data_frame_spatial) + 
  tm_fill("crime.rate.npc.area")

writeOGR(final_data_frame_spatial, dsn = "C:/Users/user/Desktop/Study/BT4015/Project/Data",
         layer = "NPC_all_attributes", driver="ESRI Shapefile")

final_data_frame_columns <- as.data.frame(colnames(final_data_frame))
colnames(final_data_frame_columns) <- c("column_names")
write.csv(final_data_frame_columns, file = "C:/Users/user/Desktop/Study/BT4015/Project/Data/NPC_all_attributes_col_names.csv")

