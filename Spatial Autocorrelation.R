library(sf)
library(tmap)
library(rgdal)
library(spdep)
library(tmaptools)
library(SpatialEpi)
library(spdep)
# Autocorrelation 

# Read shp file and rename columns of shp file
npc <- st_as_sf(readOGR("C:/Users/Yi Zhen/Desktop/Y4S1/BT4015/Project/Datasets/NPC_all_attributes.shp"))
col_names <- read.csv("C:/Users/Yi Zhen/Desktop/Y4S1/BT4015/Project/Datasets/NPC_all_attributes_col_names.csv")
colnames(npc) = col_names$column_names
npc_polygon <- readOGR("C:/Users/Yi Zhen/Desktop/Y4S1/BT4015/Project/Datasets/NPC_all_attributes.shp")

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
