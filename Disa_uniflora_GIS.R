# GIS exercise in R, using Disa uniflora as my study spp.
# Install sf
install.packages("sf")
library(sf)
library(tidyverse)

# Reading in data with sf
veg <- st_read("cape_peninsula/cape_peninsula/veg/Vegetation_Indigenous.shp")

# Checking co-ordinate reference system
st_crs(veg)

#Checking the plots work
plot(veg)
#Plotting only the National vegetation types
plot(veg[3])

# Using ggplot because it looks better
ggplot() +
  geom_sf(data=veg, aes(fill = `National_`))

# I want to crop my map so I can zoom in on the Cape Peninsula.
#Making a vector with the desired coordinates in meters according to TMLo19
newextent <- c(-66642.18, -3809853.29, -44412.18, -3750723.29)
newextent
#Giving them names
names(newextent) <- c("xmin", "ymin", "xmax", "ymax")
newextent

# Cropping the map
veg <- st_crop(veg, newextent)

# Making the zoomed in map
ggplot() + geom_sf(data=veg, aes(fill = `National_`))

#Merging all the redundant names
split_veg <- c("Peninsula Granite Fynbos - North",
               "Peninsula Granite Fynbos - South",
               "Cape Flats Dune Strandveld - West Coast",
               "Cape Flats Dune Strandveld - False Bay")
#Indexing to select attributes
vegsub <- veg[which(veg$National_%in% split_veg),]
vegsub$National_ <- str_replace_all(vegsub$National_,
                                    c("Peninsula Granite Fynbos - North" = "Peninsula Granite Fynbos",
                                      "Peninsula Granite Fynbos - South" = "Peninsula Granite Fynbos",
                                      "Cape Flats Dune Strandveld - West Coast" = "Cape Flats Dune Strandveld",
                                      "Cape Flats Dune Strandveld - False Bay" = "Cape Flats Dune Strandveld"))
ggplot() + geom_sf(data=vegsub, aes(fill = `National_`))

#Disolving the internal borders
vegsub %>% group_by(National_) %>%
  summarize() %>%
  ggplot() + geom_sf(aes(fill = National_))

# Reading in data from iNat
install.packages("rinat")
library(rinat)

#Lesser double collared sunbird
cc <- get_inat_obs(taxon_name = "Cinnyris chalybeus",
                   maxresults = 1000)
# Orange breasted sunbird
av <- get_inat_obs(taxon_name = "Anthobaphes violacea",
                   maxresults = 1000)

# Filtering to make sure I only get the research grade observations
cc <- cc %>% filter(positional_accuracy<46 &
                      latitude<0 &
                      !is.na(latitude) &
                      captive_cultivated == "false" &
                      quality_grade == "research")
av <- av %>% filter(positional_accuracy<46 &
                      latitude<0 &
                      !is.na(latitude) &
                      captive_cultivated == "false" &
                      quality_grade == "research")


#I need to turn this from a data frame into a spatial object
du <- st_as_sf(du, coords = c("longitude", "latitude"), crs = 4326)
cc <- st_as_sf(cc, coords = c("longitude", "latitude"), crs = 4326)
av <- st_as_sf(av, coords = c("longitude", "latitude"), crs = 4326)
class(du)

#Plotting the points!
ggplot() + geom_sf(data = cc)

# Adding basemaps to my plot
install.packages("rosm")
install.packages("ggspatial")
install.packages("leaflet")
library(rosm)
library(ggspatial)
library(leaflet)
library(prettymapr)

####Plot a basic map####
ggplot() +
  annotation_map_tile(type = "osm", progress = "none") +
  geom_sf(data = cc, color = "#2E8B57") 

ggplot() +
  annotation_map_tile(type = "osm", progress = "none") +
  geom_sf(data = av, color = "#FF7F00")


#The interactive map
intmap <-leaflet() %>%
  addTiles(group = "Default") %>%
  addCircleMarkers(data = cc,
                   group = "Cinnyris chalybeus",
                   radius = 1,
                   color = "green") %>%
  addCircleMarkers(data = av,
                   group = "Anthobaphes violacea",
                   radius = 1,
                   color = "orange") %>%
  addLegend(position = "bottomright",
            colors = c("orange","green"),
            labels = c("Orange breasted","Double collared"))
intmap

#Getting the remnants layer
vegr <- st_read("cape_peninsula/cape_peninsula/veg/Vegetation_Indigenous_Remnants.shp")
hmm <- st_intersection(du, vegr)
st_crs(du)

#Making the crs the same for both
av2 <- st_transform(av, st_crs(vegr))
cc2 <- st_transform(cc, st_crs(vegr))

# Another attempt at plotting data
ggplot() +
  annotation_map_tile(type = "osm", progress = "none") +
  geom_sf(data=cc, color = "#2E8B57") +
  geom_sf(data=av, color = "#FF7F00")

#### Attempt at reading in elevation data ####
install.packages("terra")
library(terra)
elevation <-  rast("cape_peninsula/cape_peninsula/CoCT_10m.tif")
elevation
# checking the CRS
crs(elevation)
st_crs(elevation)

#Crop the elevation so that the size is reduced.
elevation <- crop(elevation, ext(c(-66642.18, -40412.18, -4019953.29, -3750723.29)))

#Plot the elevation data
plot(elevation)

# aggregate to reduce file size
ele30 <- aggregate(elevation, fact = 3, fun = mean)

#Plotting again, but in tidyverse
ele30 %>% as.data.frame(xy = TRUE)
  ggplot() +
  geom_raster(aes(x = x, y = y, fill = `10m_BA`)) # Why is this fat??

#Binding cc and av (from section 8.9) so go back to this section and work out the rest!!
ccBind <- rbind(cc, av)
#crop ccBind 
ccBind <- st_crop(ccBind, ext(c(-66642.18, -40412.18, -4019953.29, -3750723.29)))

ccBind$scientific_name <- str_replace_all(ccBind$scientific_name,
                                    c("Cinnyris chalybeus chalybeus" = "Cinnyris chalybeus",
                                      "Cinnyris chalybeus subalaris" = "Cinnyris chalybeus",
                                      "Cinnyris chalybeus albilateralis" = "Cinnyris chalybeus"))

#Extract the data points
data <- terra::extract(ele30, vect(ccBind))

#Making a box plot for spp
ccBind$elevation <- data$`10m_BA`
ccBind %>% ggplot() +
  geom_boxplot(aes(scientific_name, elevation), color = c("#FF7F00", "#2E8B57")) 

 #THIS WONT WORK EITHER


# Trying to plot many maps on the same thing
ggplot() +
  geom_contour(data = as.data.frame(ele30, xy = TRUE),
              aes(x = x, y = y, z = `10m_BA`), breaks = seq(0, 1100, 100), colour = "black") +
  geom_sf(data=ccBind, colour = "red")
  # this doesnt work

ggplot() +
  annotation_map_tile(type = "osm", progress = "none") +
  geom_contour(data = as.data.frame(ele30, xy = TRUE),
               aes(x = x, y = y, z = `10m_BA`), breaks = seq(0, 1100, 100), colour = "black") +
  geom_sf(data=ccBind, colour = "red", fill = "red")
