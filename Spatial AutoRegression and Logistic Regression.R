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
df <- st_as_sf(readOGR("C:/Users/user/Desktop/Study/BT4015/Project/Data/NPC_all_attributes.shp"))
colnames <- read.csv("C:/Users/user/Desktop/Study/BT4015/Project/Data/NPC_all_attributes_col_names.csv")
colnames(df) <- colnames$column_names

# Fill NA values
df[is.na(df)] <- 0

tm_shape(df) +
  tm_fill("crime.rate.npc.area")

# Neighbors (Queen's case)
df_neighbors <- poly2nb(df)
df_neighbors
# Convert the neighbour list to a listw object
df_listw <- nb2listw(df_neighbors)
df_listw


##############
# Regression #
##############


# Remove dependent columns 

### Extract numerical columns of interest
ratio_columns_df <- df[, c("Scaled.Elderly.Ratio", "Scaled.Chinese.Ratio", "Scaled.Malay.Ratio",
                           "Scaled.Indian.Ratio", "Scaled.Others.Ratio", "Scaled.Male.Ratio", 
                           "Scaled.Female.Ratio", "Scaled.Population.Density", 
                           "Scaled.Private.Housing.Density", "Scaled.HDB.Housing.Density",
                           "Scaled.Households.Below.Median.Income.Density",
                           "Scaled.Households.Above.Median.Income.Density",
                           "CC_density", "HC_density", "TA_density",
                           "CC_percent", "HC_percent", "TA_percent",
                           "crime.rate.npc.area")]
### Convert to normal df
ratio_columns_df <- ratio_columns_df %>% st_drop_geometry()
### Correlation test
correlation <- cor(ratio_columns_df)
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
correlation_above_threshold <- function(correlation_df, threshold) {
  row_count = 0
  for (row in rownames(correlation_df)) {
    row_count = row_count + 1
    column_count = 0
    for (column in colnames(correlation_df)) {
      column_count = column_count + 1
      if ((row_count < column_count) & abs(correlation_df[row_count, column_count]) > threshold) {
        print(c(row, column, correlation_df[row_count, column_count]))
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
                    listw = df_listw,
                    data = df)
summary(sar.mod1)

### Linear Regression
#### 1st iteration
glm.mod1 <- glm(crime.rate.npc.area ~ Scaled.Elderly.Ratio + Scaled.Malay.Ratio +
                  Scaled.Indian.Ratio + Scaled.Others.Ratio + 
                  Scaled.Female.Ratio + 
                  Scaled.Households.Below.Median.Income.Density +
                  CC_density + TA_density + 
                  HC_percent + TA_percent,
                data = df)
summary(glm.mod1)

#### 2nd iteration (without TA density)
glm.mod2 <- glm(crime.rate.npc.area ~ Scaled.Elderly.Ratio + Scaled.Malay.Ratio +
                  Scaled.Indian.Ratio + Scaled.Others.Ratio + 
                  Scaled.Female.Ratio + 
                  Scaled.Households.Below.Median.Income.Density +
                  CC_density + 
                  HC_percent + TA_percent,
                data = df)
summary(glm.mod2)

#### 3rd iteration (without Below Median Income)
glm.mod3 <- glm(crime.rate.npc.area ~ Scaled.Elderly.Ratio + Scaled.Malay.Ratio +
                  Scaled.Indian.Ratio + Scaled.Others.Ratio + 
                  Scaled.Female.Ratio + 
                  CC_density + 
                  HC_percent + TA_percent,
                data = df)
summary(glm.mod3)

#### 4th iteration (without Indian Ratio)
glm.mod4 <- glm(crime.rate.npc.area ~ Scaled.Elderly.Ratio + Scaled.Malay.Ratio +
                  Scaled.Others.Ratio + 
                  Scaled.Female.Ratio + 
                  CC_density + 
                  HC_percent + TA_percent,
                data = df)
summary(glm.mod4)

#### 5th iteration (without HC Percent)
glm.mod5 <- glm(crime.rate.npc.area ~ Scaled.Elderly.Ratio + Scaled.Malay.Ratio +
                  Scaled.Others.Ratio + 
                  Scaled.Female.Ratio + 
                  CC_density + 
                  TA_percent,
                data = df)
summary(glm.mod5)

#### 6th iteration (without Elderly Ratio)
glm.mod6 <- glm(crime.rate.npc.area ~ Scaled.Malay.Ratio +
                  Scaled.Others.Ratio + 
                  Scaled.Female.Ratio + 
                  CC_density + 
                  TA_percent,
                data = df)
summary(glm.mod6)

#### 7th iteration (without TA percent)
glm.mod7 <- glm(crime.rate.npc.area ~ Scaled.Malay.Ratio +
                  Scaled.Others.Ratio + 
                  Scaled.Female.Ratio + 
                  CC_density,
                data = df)
summary(glm.mod7)

#### 8th iteration (without Female Ratio)
glm.mod8 <- glm(crime.rate.npc.area ~ Scaled.Malay.Ratio +
                  Scaled.Others.Ratio + 
                  CC_density,
                data = df)
summary(glm.mod8)
