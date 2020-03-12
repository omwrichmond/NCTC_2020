# This code is for playing with spatial data
require(sf)
require(raster)
require(tidyverse)
require(mapview)

# Get some Starkey data for HUC boundary and stream
bndy <- st_read(dsn = "C:/Users/orichmond/Documents/TEACHING/CSP1004_Data_Wrangling_with_R_2020/Spatial", layer = "StarkeyBoundary")
strm <- st_read(dsn = "C:/Users/orichmond/Documents/TEACHING/CSP1004_Data_Wrangling_with_R_2020/Spatial", layer = "StarkeyStreams")

# Add collared elk dataset
pts <- read_delim("C:/Users/orichmond/Documents/TEACHING/CSP1004_Data_Wrangling_with_R_2020/Data/271_E_2012_G_JuneSubset.txt", delim = "\t") %>% #Select columns we need
  select(AID, Pres, geometry) %>% #Make it spatially aware
  st_as_sf(wkt = "geometry", crs = "+init=epsg:26911")
class(pts)

# Create a map
mapview(bndy) + mapview(strm) + mapview(pts, zcol = "Pres")

# Add raster data for NLCD 2011
nlcd <- raster("C:/Users/orichmond/Documents/TEACHING/CSP1004_Data_Wrangling_with_R_2020/Spatial/nlcdRaw.tif")

# Create a map
mapview(nlcd)
plot(nlcd) #base R plot function is dumb (does NOT reproject on the fly). This is useful because you can determine when layers are not in the same coordinate system.
plot(pts$geometry, add = TRUE)
#Things are lining up the way they are supposed to be. !!!You don't want to do any spatial analysis when the layers are in different spatial coordinate reference systems!!!

mapview(nlcd) + mapview(strm, zcol = "Name") + mapview(pts)

#Ask some spatial analysis questions
#What is the distance of elk to forests? What is the distance of elk to streams? What land cover types are the elk using? 

#The NLCD raster will be our "template"

#First we need to reclassify NLCD to be Forest/Not Forest
freq(nlcd)
#21 and 22 are urban
#42 is coniferous forest
#52 is scrub/shrub
#71 is grassland
#82 is pasture
#95 is ?

#So we want 42 to be forest and all others to be non-forest

#Create reclass table, make forest = 1, rest NA
rcls <- data.frame(is = c(21, 22, 42, 52, 71, 82, 95), becomes = c(NA, NA, 1, NA, NA, NA, NA))

#Now, reclass the raster
forest <- reclassify(nlcd, rcl = as.matrix(rcls))
plot(forest)

#Create a distance raster of meters from forest edge
forDist <- distance(forest)
plot(forDist)

#4 tenets of good raster analysis
#-Same cell cell size
#-Same origin
#-Same extent
#-Same coordinate system

#Rasterize the stream layer
strm_rast <- rasterize(as(strm, "Spatial"), nlcd, field = 1)
plot(strm_rast)

#Create a distance raster of meters to stream
strmDist <- distance(strm_rast)
plot(strmDist)

#Now stack the rasters together
rs <- stack(nlcd, forDist, strmDist)
#No errors, so it means all of the rasters have the same origin, cell size, extent and spatial coordinate system

plot(rs)

#Rename rasters in the stack
names(rs)[2:3] <- c("ForestDist", "StrmDist")

plot(rs)

#Create pseudo-absence points (for where elk are not)
rpts <- st_sample(bndy, size = nrow(pts) * 2) %>%
  st_sf(data.frame(AID = "Random", Pres = 0)) %>%
  mutate(geometry = st_as_text(.)) %>%
  st_drop_geometry() %>%
  st_as_sf(wkt = "geometry", crs = "+init=epsg:26911")

mapview(rpts) + mapview(bndy)

mySamp <- pts %>%
  rbind(rpts)

plot(mySamp$geometry)

#Extract raster values to points: attribute all of the elk points with the data from all three layers in the raster stack
elk_attributed <- data.frame(raster::extract(rs, as(mySamp, "Spatial")))

#Append attributes back to points layer
mySamp <- mySamp %>%
  bind_cols(elk_attributed)

#Now we have a data frame with our response (presence/absence) and possible explanatory variables (nlcd, distance to forest, distance to streams)

#Coerce nlcd to a factor
mySamp <- mySamp %>%
  mutate(nlcdRaw = as.factor(nlcdRaw))

#Let's fit a logistic regression model
mod <- glm(Pres ~ nlcdRaw + ForestDist + StrmDist, data = mySamp, family = binomial(link = "logit"))

#Look at the results
summary(mod)

#Take the model and predict back to the landscape given the landcover data that we have

pred <- predict(rs, model = mod, type = "response")
plot(pred)
