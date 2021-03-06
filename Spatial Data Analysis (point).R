###################
# Import packages #
###################

library(tmap)
library(rgdal)
library(sf)
library(GISTools)
library(dplyr)
require(rgdal)
library(maptools)
library(raster)
library(spatstat)

tmap_mode('plot')

##############
# Read files #
##############

# for buffer and percentage cover
NPC_points <- st_as_sf(readOGR(dsn = "C:/Users/Zi Ying/Dropbox/Y4S1/BT4015/BT4015/Data/spf_establishments_2018", layer = "SPF_ESTABLISHMENTS_2018"))
NPC_polygon <- readOGR(dsn = "C:/Users/Zi Ying/Dropbox/Y4S1/BT4015/BT4015/Data/npc boundary", layer = "singapore-police-force-npc-boundary-kml-polygon")
pop_stats_by_npc <- st_as_sf(readOGR("C:/Users/Zi Ying/Dropbox/Y4S1/BT4015/BT4015/Data/NPC_all_attributes.shp"))
col_names <- read.csv("C:/Users/Zi Ying/Dropbox/Y4S1/BT4015/BT4015/Data/NPC_all_attributes_col_names.csv")
colnames(pop_stats_by_npc) = col_names$column_names

# read in hawker centre, cc and tourist attraction point data
HC <- st_as_sf(readOGR("C:/Users/Zi Ying/Dropbox/Y4S1/BT4015/BT4015/Data/hawker-centres.shp"))
CC <- st_as_sf(readOGR("C:/Users/Zi Ying/Dropbox/Y4S1/BT4015/BT4015/Data/community-clubs.shp"))
TA <- st_as_sf(readOGR("C:/Users/Zi Ying/Dropbox/Y4S1/BT4015/BT4015/Data/tourism.shp"))

#poisson point process for pop density raster
pop_stats_npc  <- readOGR("C:/Users/Zi Ying/Dropbox/Y4S1/BT4015/BT4015/Data/NPC_all_attributes.shp")

##################
# Basic cleaning #
##################

# check proj4string
st_crs(NPC_points)
NPC_polygon@proj4string@projargs

# different, rectify
NPC_points <- st_transform(NPC_points, crs = NPC_polygon@proj4string@projargs)

# change to sf object to use st_intersection
NPC_polygon_sf = st_as_sf(NPC_polygon)

# check if polygons are valid
tmap_options(check.and.fix = TRUE)
st_is_valid(NPC_polygon_sf)
NPC_polygon_sf <- st_make_valid(NPC_polygon_sf)
st_is_valid(NPC_polygon_sf)

tm_shape(NPC_polygon_sf) + tm_polygons(col="pink") + tm_shape(NPC_points)+tm_dots("blue")


# remove airport and jurong island polygon
NPC_polygon_sf = NPC_polygon_sf[!(NPC_polygon_sf$Name == "kml_25" | NPC_polygon_sf$Name == "kml_3"),]
nrow(NPC_polygon_sf)
View(NPC_polygon_sf)
tm_shape(NPC_polygon_sf) + tm_polygons(col="pink")

# remove airport and police coast guard point
NPC_points = NPC_points[!(NPC_points$NO == "94"| NPC_points$NO == "99"),]
nrow(NPC_points)
View(NPC_points)

# find intersection of npc points and npc boundaries
# get list of points in each npc
out <- st_intersection(NPC_polygon_sf, NPC_points)
points_in_poly <- out[c("Name", "NPC_NAME", "NO", "BLDG", "TYPE")]
points_in_poly = points_in_poly[order(points_in_poly$NPC_NAME),]

tm_shape(NPC_polygon_sf) + tm_polygons(col="pink") + tm_shape(points_in_poly)+tm_dots("red") 

# read in combined attributes based on NPC shp file
tm_shape(pop_stats_by_npc) + tm_fill("Scaled.Population.Density", title = "Population Density") + 
  tm_shape(points_in_poly)+tm_dots("red", size = 0.1)+
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.5,
            legend.bg.alpha = 1, legend.outside = TRUE) + 
  tm_scale_bar(position=c("right", "bottom")) + 
  tm_compass(type="8star", position=c("right", "top"), show.labels = 3, size=2, text.size=0.5)


######################################
# Descriptive Analysis of NPC Points #
######################################

# Density
# merge points_in_poly with pop_stats_by_npc to get area of each polygon 

pop_stats_area <- as.data.frame(pop_stats_by_npc[c('NPC_NAME', 'NPC.Area.in.km2', 'crime.rate.npc.area')])
npc_with_area <- merge(pop_stats_area, points_in_poly, by = 'NPC_NAME', all.x=TRUE)
npc_count <- as.data.frame(npc_with_area %>% group_by(NPC_NAME) %>% count())[, 1:2]
names(npc_count)[names(npc_count) == 'n'] <- 'NPC_COUNT'

npc_count_with_area <- merge(npc_count, pop_stats_area, by = 'NPC_NAME', all.x=TRUE)
npc_count_with_area$NPC_Density <- npc_count_with_area$NPC_COUNT/npc_count_with_area$NPC.Area.in.km2

tm_shape(st_as_sf(npc_count_with_area)) +tm_fill("crime.rate.npc.area", title = "Crime Rate by NPC") + 
  tm_bubbles(size="NPC_Density", col="blue")+
  tm_borders('black') + 
  tm_layout(legend.title.size = 1,
            legend.text.size = 0.7,
            legend.bg.alpha = 1, legend.outside = TRUE)+ 
  tm_scale_bar(position=c("right", "bottom")) + 
  tm_compass(type="8star", position=c("right", "top"), show.labels = 3, size=2, text.size=0.5)

# Quadrat by population density covariate
# make sure the data is in the correct format for the spatstat package
# try using utm projection to convert coordinates to a projected one
pop_stats_by_npc.utm <- spTransform(pop_stats_npc, CRS("+proj=utm +zone=48N +datum=WGS84 +no_defs"))

# polygon: make pop_stats_by_npc owin type 
pop_stats_by_npc_owin  <- as.owin(pop_stats_by_npc.utm)
pop_stats_by_npc_owin.km <- rescale(pop_stats_by_npc_owin, 1000)

# point: police station and police post
intersected_points_spatial <- as(points_in_poly, "Spatial")

intersected_points_spatial.utm <- spTransform(intersected_points_spatial, CRS("+proj=utm +zone=48N +datum=WGS84 +no_defs"))

intersected_points_ppp  <- as.ppp(intersected_points_spatial.utm)
marks(intersected_points_ppp) <- NULL
Window(intersected_points_ppp) <- pop_stats_by_npc_owin
intersected_points_ppp.km <- rescale(intersected_points_ppp, 1000, "km")

# get population density raster layer
r <- raster(pop_stats_by_npc.utm)
nrow(r) <- 500
ncol(r) <- 500

r.polys <- rasterize(pop_stats_by_npc.utm, r, pop_stats_by_npc.utm@data[["Scl_P_D"]], update = TRUE, updateValue = "NA")

plot(r.polys)

pop <- as.im(r.polys)
hist(pop, main=NULL, las=1)
pop.km <- rescale(pop, 1000, "km")

pop.lg <- log(pop)
hist(pop.lg, main=NULL, las=1) # not much difference in shape? is it really better?
pop.lg.km <- rescale(pop.lg, 1000, "km")

brk  <- c( -Inf, 8, 9, 10, Inf)  # Define the breaks
Zcut <- cut(pop.lg.km, breaks=brk, labels=1:4)  # Classify the raster
E    <- tess(image=Zcut)  # Create a tesselated surface
plot(E, main="", las=1)
Q   <- quadratcount(intersected_points_ppp.km, tess = E)  # Tally counts
Q.d <- intensity(Q)  # Compute density
Q.d
cl <- interp.colours(c("lightyellow", "orange" ,"red"), E$n)
plot(intensity(Q, image=TRUE), las=1, col=cl, main=NULL)
plot(intersected_points_ppp.km, pch=20, cex=0.6, col=rgb(0,0,0,.5), add=TRUE)


##########
# Buffer #
##########

## HC, CC, TA Coverage in each NPC buffer

# Buffer for each police station/police post
npc_point_buf <- st_buffer(points_in_poly, dist = 1000)

# undissolved buffer
tm_shape(NPC_polygon_sf) + tm_borders('black') + 
  tm_shape(npc_point_buf) + tm_polygons("blue", alpha = 0.5) +
  tm_shape(points_in_poly)+tm_dots("red", size = 0.1) + 
  tm_layout(title= 'Undissolved Buffer 1km radius around each NPC', title.position = c('right', 'top'), title.size = 0.8) +tm_layout(legend.title.size = 1,
             legend.text.size = 0.7,
             legend.bg.alpha = 1, legend.outside = TRUE)+ 
  tm_scale_bar(position=c("right", "bottom")) + 
  tm_compass(type="8star", position=c("right", "top"), show.labels = 3, size=2, text.size=0.5)

npc_point_buf <- st_union(npc_point_buf)
npc_point_buf <- sf::st_make_valid(npc_point_buf)

# plot hawker centre
tm_shape(NPC_polygon_sf) + tm_borders('black') + 
  tm_shape(npc_point_buf) + tm_polygons("blue", alpha = 0.5)+ 
  tm_shape(HC) + tm_dots("black", size = 0.1) + 
  tm_layout(title= 'Hawker Centres in Dissolved NPC Buffer', title.position = c('right', 'top'), title.size = 0.8)+ tm_layout(legend.title.size = 0.5,
            legend.text.size = 0.7,
            legend.bg.alpha = 1, legend.outside = TRUE)+ 
  tm_scale_bar(position=c("right", "bottom")) + 
  tm_compass(type="8star", position=c("right", "top"), show.labels = 3, size=2, text.size=0.5)
# count of hc, cc, ta in each buffer
hc_buf_intersection <- st_intersection(HC, npc_point_buf) #polygon, point
hc_covered <- st_intersection(NPC_polygon_sf, hc_buf_intersection)

# tm_shape(NPC_polygon_sf) + tm_borders("yellow")+ tm_shape(hc_covered) + tm_dots("purple")
hc_buf_count <- as.data.frame(hc_covered %>% group_by(NPC_NAME) %>% count())[, 1:2]
names(hc_buf_count)[names(hc_buf_count) == 'n'] <- 'HC_in_buffer'

# plot CC
tm_shape(NPC_polygon_sf) + tm_borders('black') + 
  tm_shape(npc_point_buf) + tm_polygons("blue", alpha = 0.5)+ 
  tm_shape(CC) + tm_dots("pink", size = 0.1) + 
  tm_layout(title= 'Community Centres in Dissolved NPC Buffer', title.position = c('right', 'top'), title.size = 0.8) + tm_layout(legend.title.size = 1,
            legend.text.size = 0.7,
            legend.bg.alpha = 1, legend.outside = TRUE)+ 
  tm_scale_bar(position=c("right", "bottom")) + 
  tm_compass(type="8star", position=c("right", "top"), show.labels = 3, size=2, text.size=0.5)

# CC
cc_buf_intersection <- st_intersection(CC, npc_point_buf) #polygon, point
cc_covered <- st_intersection(NPC_polygon_sf, cc_buf_intersection)

cc_buf_count <- as.data.frame(cc_covered %>% group_by(NPC_NAME) %>% count())[, 1:2]
names(cc_buf_count)[names(cc_buf_count) == 'n'] <- 'CC_in_buffer'

# plot TA
tm_shape(NPC_polygon_sf) + tm_borders('black') + tm_shape(npc_point_buf) + tm_polygons("blue", alpha = 0.5)+ tm_shape(TA) + tm_dots("orange", size = 0.1) + tm_layout(title= 'Tourist Attractions in Dissolved NPC Buffer', title.position = c('right', 'top'), title.size = 0.8) + tm_layout(legend.title.size = 1,
            legend.text.size = 0.7,
            legend.bg.alpha = 1, legend.outside = TRUE)+ 
  tm_scale_bar(position=c("right", "bottom")) + 
  tm_compass(type="8star", position=c("right", "top"), show.labels = 3, size=2, text.size=0.5)

# TA
ta_buf_intersection <- st_intersection(TA, npc_point_buf) #polygon, point
ta_covered <- st_intersection(NPC_polygon_sf, ta_buf_intersection)

ta_buf_count <- as.data.frame(ta_covered %>% group_by(NPC_NAME) %>% count())[, 1:2]
names(ta_buf_count)[names(ta_buf_count) == 'n'] <- 'TA_in_buffer'

# Merge the counts into 1 df based on NPC point name
hc_cc_ta_in_buf <- merge(hc_buf_count, cc_buf_count, by = 'NPC_NAME', all.x=TRUE)
hc_cc_ta_in_buf <- merge(hc_cc_ta_in_buf, ta_buf_count, by = 'NPC_NAME', all.x=TRUE)
hc_cc_ta_in_buf[is.na(hc_cc_ta_in_buf)] <- 0

# Count of hc, cc, ta in each NPC polygon

hc_npc_intersection <- st_intersection(NPC_polygon_sf, HC) #polygon, point
hc_npc_count <- as.data.frame(hc_npc_intersection %>% group_by(NPC_NAME) %>% count())[, 1:2]
names(hc_npc_count)[names(hc_npc_count) == 'n'] <- 'HC_in_NPC_polygon'

cc_npc_intersection <- st_intersection(NPC_polygon_sf, CC) #polygon, point
cc_npc_count <- as.data.frame(cc_npc_intersection %>% group_by(NPC_NAME) %>% count())[, 1:2]
names(cc_npc_count)[names(cc_npc_count) == 'n'] <- 'CC_in_NPC_polygon'

ta_npc_intersection <- st_intersection(NPC_polygon_sf, TA) #polygon, point
ta_npc_count <- as.data.frame(ta_npc_intersection %>% group_by(NPC_NAME) %>% count())[, 1:2]
names(ta_npc_count)[names(ta_npc_count) == 'n'] <- 'TA_in_NPC_polygon'

final <- merge(hc_cc_ta_in_buf, hc_npc_count, by = 'NPC_NAME', all.x=TRUE)
final <- merge(final, cc_npc_count, by = 'NPC_NAME', all.x=TRUE)
final <- merge(final, ta_npc_count, by = 'NPC_NAME', all.x=TRUE)

final$HC_percent <- final$HC_in_buffer/final$HC_in_NPC_polygon*100
final$CC_percent <- final$CC_in_buffer/final$CC_in_NPC_polygon*100
final$TA_percent <- final$TA_in_buffer/final$TA_in_NPC_polygon*100

final[is.na(final)] <- 0

write.csv(final, "Percentage Cover of HC, CC, TA in each point buffer.csv")


##########################
# ANN Hypothesis Testing #
##########################

# do hypothesis testing for population density
# model with pop density as the function
n <- 599L
ann.r <- vector(length=n)
for (i in 1:n){
  rand.p <- rpoint(n=intersected_points_ppp.km$n, f=pop.km, win=pop_stats_by_npc_owin.km) 
  ann.r[i] <- mean(nndist(rand.p, k=1))
}

plot(rand.p, pch=16, main=NULL, cols=rgb(0,0,0,0.5))

# observed
ann.p <- mean(nndist(intersected_points_ppp.km, k=1))
ann.p

# histogram
hist(ann.r, main=NULL, las=1, col="bisque", xlim=range(ann.p, ann.r))
abline(v=ann.p, col="blue")
text(1.06, 130, toString(round(ann.p, 5)))

# p-value
N.greater <- sum(ann.r > ann.p)
p <- min(N.greater + 1, n + 1 - N.greater) / (n +1)
p


#########################
# Poisson Point Process #
#########################

# fit the model that assumes that the process' intensity is not a function of population density
PPM0 <- ppm(intersected_points_ppp.km ~ 1)
PPM0

PPM1 <- ppm(intersected_points_ppp.km ~ pop.lg.km)
PPM1

# observed intensity
intersected_points_ppp.km$n / area(pop_stats_by_npc_owin.km)

# compare models using likelihood ratio test
# to see if they are significantly different from each other
anova(PPM0, PPM1, test="LRT")

