###################
# Import packages #
###################

library(tmap)
library(tmaptools)
library(SpatialEpi)
library(sf)
library(spdep)
library(raster)
library(plyr)
library(rgdal)
library(gmodels)
library(dplyr)
library(geosphere)
library(sfheaders)
library(readxl)
library(GISTools)
library(rgeos)
library(spgwr)

##############
# Read files #
##############
npc <- st_as_sf(readOGR("C:/Users/Yi Zhen/Desktop/Y4S1/BT4015/Project/Datasets/NPC_all_attributes.shp"))
col_names <- read.csv("C:/Users/Yi Zhen/Desktop/Y4S1/BT4015/Project/Datasets/NPC_all_attributes_col_names.csv")
colnames(npc) = col_names$column_names
npc_polygon <- readOGR("C:/Users/Yi Zhen/Desktop/Y4S1/BT4015/Project/Datasets/NPC_all_attributes.shp")

###########################
# Spatial Autocorrelation #
###########################

# Create a SpatialLinesDataFrame showing the Queen's case contiguities
npc.nb <- poly2nb(npc_polygon, queen=TRUE)
npc.net <- nb2lines(npc.nb,coords=coordinates(npc_polygon))

# Change projection from NA to the NPC shp file's projection 
P4S.latlon <- CRS("+proj=longlat +datum=WGS84")
P4S.latlon <- st_crs(npc.net)

# Plot the network of neighbours 
tm_shape(npc) + tm_borders(col='lightgrey') + 
  tm_shape(npc.net) + tm_lines(col='red')

# Define "neighboring" polygons using Queen's case
nb <- poly2nb(npc, queen=TRUE)

# Check neighbors for the first polygon 
nb[[1]]

# Assign weights to each neighbouring polygon
lw <- nb2listw(nb)

# Check weights of first polygon's neighbours
lw$weights[1]

# Global Moran's I using analytical method
moran.test(npc$crime.rate.npc.area,lw)

# Global Moran's I using Monte Carlo method
MC<- moran.mc(npc$crime.rate.npc.area, lw, nsim=599)

MC

# Plot the distribution (note that this is a density plot instead of a histogram)
plot(MC, main="", las=1)

# Compute the Local Moran's I
npc$lI <- localmoran(npc$crime.rate.npc.area,lw)[,1]

# Plot the Local Moran's I coefficients on a map
tm_shape(npc) + 
  tm_polygons(col='lI',title="Local Morans I",legend.format=list(flag="+")) +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.7,
            legend.bg.alpha = 1, legend.outside = TRUE)+ 
  tm_scale_bar(position=c("right", "bottom")) + 
  tm_compass(type="8star", position=c("right", "top"), show.labels = 3, size=2, text.size=0.5)

# Obtain the Local Moran's I p-values
npc$lI.pvalue <- localmoran(npc$crime.rate.npc.area ,lw)[,5]

# Plot the p-values on a map
tm_shape(npc) + 
  tm_polygons(col='lI.pvalue',title="p-value",breaks=c(0,0.01,0.05,1),
              border.col = "black",
              palette = "-Greens") +
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.7,
            legend.bg.alpha = 1, legend.outside = TRUE)+ 
  tm_scale_bar(position=c("right", "bottom")) + 
  tm_compass(type="8star", position=c("right", "top"), show.labels = 3, size=2, text.size=0.5)


##############
# Regression #
##############

# Fill NA values
npc[is.na(npc)] <- 0

tm_shape(npc) +
  tm_fill("crime.rate.npc.area")

# Neighbors (Queen's case)
npc_neighbors <- poly2nb(npc)
npc_neighbors

# Convert the neighbour list to a listw object
npc_listw <- nb2listw(npc_neighbors)
npc_listw

# Remove dependent columns 

### Extract numerical columns of interest
ratio_columns_npc <- npc[, c("Scaled.Elderly.Ratio", "Scaled.Chinese.Ratio", "Scaled.Malay.Ratio",
                           "Scaled.Indian.Ratio", "Scaled.Others.Ratio", "Scaled.Male.Ratio", 
                           "Scaled.Female.Ratio", "Scaled.Population.Density", 
                           "Scaled.Private.Housing.Density", "Scaled.HDB.Housing.Density",
                           "Scaled.Households.Below.Median.Income.Density",
                           "Scaled.Households.Above.Median.Income.Density",
                           "CC_density", "HC_density", "TA_density",
                           "CC_percent", "HC_percent", "TA_percent",
                           "crime.rate.npc.area")]
### Convert to normal npc
ratio_columns_npc <- ratio_columns_npc %>% st_drop_geometry()
### Correlation test
correlation <- cor(ratio_columns_npc)
colnames(correlation) <- c("Elderly", "Chinese", "Malay", "Indian", "Others", "Male", 
                           "Female", "Population", "Private Housing", "HDB", "Below Median",
                           "Above Median", "CC Density", "HC Density", "TA Density",
                           "CC Percent", "HC Percent", "TA Percent", "Crime Rate (Target)")
rownames(correlation) <- c("Elderly", "Chinese", "Malay", "Indian", "Others", "Male", 
                           "Female", "Population", "Private Housing", "HDB", "Below Median",
                           "Above Median", "CC Density", "HC Density", "TA Density",
                           "CC Percent", "HC Percent", "TA Percent", "Crime Rate (Target)")
corrplot::corrplot(correlation, type = "lower", order = "hclust", 
                   tl.col = "black", tl.srt = 45)
### Return pairs of variables with correlation higher than a certain threshold
correlation_above_threshold <- function(correlation_npc, threshold) {
  row_count = 0
  for (row in rownames(correlation_npc)) {
    row_count = row_count + 1
    column_count = 0
    for (column in colnames(correlation_npc)) {
      column_count = column_count + 1
      if ((row_count < column_count) & abs(correlation_npc[row_count, column_count]) > threshold) {
        print(c(row, column, correlation_npc[row_count, column_count]))
      }
    }
  }
}
correlation_above_threshold(correlation, 0.7)

# Run models after removing dependent columns

### Spatial AutoRegression, which will not be used due to insignificant Spatial Autocorrelation
sar.mod1 <- spautolm(crime.rate.npc.area ~ Scaled.Elderly.Ratio + Scaled.Malay.Ratio +
                       Scaled.Indian.Ratio + Scaled.Others.Ratio + 
                       Scaled.Female.Ratio + 
                       Scaled.Households.Below.Median.Income.Density +
                       CC_density + TA_density + 
                       HC_percent + TA_percent, 
                     listw = npc_listw,
                     data = npc)
summary(sar.mod1)

### Linear Regression
#### 1st iteration
glm.mod1 <- glm(crime.rate.npc.area ~ Scaled.Elderly.Ratio + Scaled.Malay.Ratio +
                  Scaled.Indian.Ratio + Scaled.Others.Ratio + 
                  Scaled.Female.Ratio + 
                  Scaled.Households.Below.Median.Income.Density +
                  CC_density + TA_density + 
                  HC_percent + TA_percent,
                data = npc)
summary(glm.mod1)

#### 2nd iteration (without TA density)
glm.mod2 <- glm(crime.rate.npc.area ~ Scaled.Elderly.Ratio + Scaled.Malay.Ratio +
                  Scaled.Indian.Ratio + Scaled.Others.Ratio + 
                  Scaled.Female.Ratio + 
                  Scaled.Households.Below.Median.Income.Density +
                  CC_density + 
                  HC_percent + TA_percent,
                data = npc)
summary(glm.mod2)

#### 3rd iteration (without Below Median Income)
glm.mod3 <- glm(crime.rate.npc.area ~ Scaled.Elderly.Ratio + Scaled.Malay.Ratio +
                  Scaled.Indian.Ratio + Scaled.Others.Ratio + 
                  Scaled.Female.Ratio + 
                  CC_density + 
                  HC_percent + TA_percent,
                data = npc)
summary(glm.mod3)

#### 4th iteration (without Indian Ratio)
glm.mod4 <- glm(crime.rate.npc.area ~ Scaled.Elderly.Ratio + Scaled.Malay.Ratio +
                  Scaled.Others.Ratio + 
                  Scaled.Female.Ratio + 
                  CC_density + 
                  HC_percent + TA_percent,
                data = npc)
summary(glm.mod4)

#### 5th iteration (without HC Percent)
glm.mod5 <- glm(crime.rate.npc.area ~ Scaled.Elderly.Ratio + Scaled.Malay.Ratio +
                  Scaled.Others.Ratio + 
                  Scaled.Female.Ratio + 
                  CC_density + 
                  TA_percent,
                data = npc)
summary(glm.mod5)

#### 6th iteration (without Elderly Ratio)
glm.mod6 <- glm(crime.rate.npc.area ~ Scaled.Malay.Ratio +
                  Scaled.Others.Ratio + 
                  Scaled.Female.Ratio + 
                  CC_density + 
                  TA_percent,
                data = npc)
summary(glm.mod6)

#### 7th iteration (without TA percent)
glm.mod7 <- glm(crime.rate.npc.area ~ Scaled.Malay.Ratio +
                  Scaled.Others.Ratio + 
                  Scaled.Female.Ratio + 
                  CC_density,
                data = npc)
summary(glm.mod7)

#### 8th iteration (without Female Ratio)
glm.mod8 <- glm(crime.rate.npc.area ~ Scaled.Malay.Ratio +
                  Scaled.Others.Ratio + 
                  CC_density,
                data = npc)
summary(glm.mod8)
