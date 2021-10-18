library(tmap)
library(rgdal)
library(sf)
library(GISTools)
require(rgdal)
NPC_points <- st_as_sf(readOGR(dsn = "C:/Users/Zi Ying/Dropbox/Y4S1/BT4015/BT4015/Data/spf_establishments_2018", layer = "SPF_ESTABLISHMENTS_2018"))
NPC_polygon <- readOGR(dsn = "C:/Users/Zi Ying/Dropbox/Y4S1/BT4015/BT4015/Data/npc boundary", layer = "singapore-police-force-npc-boundary-kml-polygon")

#check proj4string
st_crs(NPC_points)
NPC_polygon@proj4string@projargs

#different, rectify
NPC_points <- st_transform(NPC_points, crs = NPC_polygon@proj4string@projargs)

#change to sf object to use st_intersection
NPC_polygon_sf = st_as_sf(NPC_polygon)

#check if polygons are valid
tmap_options(check.and.fix = TRUE)
st_is_valid(NPC_polygon_sf)
NPC_polygon_sf <- st_make_valid(NPC_polygon_sf)
st_is_valid(NPC_polygon_sf)

tmap_mode('view')
tm_shape(NPC_polygon_sf) + tm_polygons(col="pink") + tm_shape(NPC_points)+tm_dots("blue")


#remove airport polygon

NPC_polygon_sf = NPC_polygon_sf[!NPC_polygon_sf$Name == "kml_25",]
nrow(NPC_polygon_sf)
View(NPC_polygon_sf)
tm_shape(NPC_polygon_sf) + tm_polygons(col="pink")

#remove airport and police coast guard point
NPC_points = NPC_points[!(NPC_points$NO == "94"| NPC_points$NO == "99"),]
nrow(NPC_points)
View(NPC_points)
tm_shape(NPC_points)+tm_dots("blue")


#find intersection of npc points and npc boundaries
#get list of points in each npc

out <- st_intersection(NPC_polygon_sf, NPC_points)
points_in_poly <- out[c("Name", "NPC_NAME", "NO", "BLDG", "TYPE")]
points_in_poly = points_in_poly[order(points_in_poly$NPC_NAME),]
tm_shape(NPC_polygon_sf) + tm_polygons(col="pink") + tm_shape(NPC_points)+tm_dots("blue")
