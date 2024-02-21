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

pc <- get_inat_obs(taxon_name = "Protea cynaroides",
                   bounds = c(-35, 18, -33.5, 18.5),
                   maxresults = 1000)
du <- get_inat_obs(taxon_name = "Disa uniflora",
                   bounds = c(-35, 18, -33.5, 18.5),
                   maxresults = 1000)
head(du)

# Filtering to make sure I only get the research grade observations
du <- du %>% filter(positional_accuracy<46 &
                      latitude<0 &
                      !is.na(latitude) &
                      captive_cultivated == "false" &
                      quality_grade == "research")
class(du)

#I need to turn this from a data frame into a spatial object
du <- st_as_sf(du, coords = c("longitude", "latitude"), crs = 4326)
class(du)

#Plotting the points!
ggplot() + geom_sf(data = du)

# Adding basemaps to my plot
install.packages("rosm")
install.packages("ggspatial")
install.packages("leaflet")
library(rosm)
library(ggspatial)
library(leaflet)

ggplot() +
  annotation_map_tile(type = "osm", progress = "none") +
  geom_sf(data=du) #this won't work

leaflet() %>%
  addTiles(group = "Default") %>%
  addCircleMarkers(data = du,
                   group = "Disa uniflora",
                   radius = 3,
                   color = "red") #Why are these soooo mismatched??

#Getting the remnants layer
vegr <- st_read("cape_peninsula/cape_peninsula/veg/Vegetation_Indigenous_Remnants.shp")
hmm <- st_intersection(du, vegr)
st_crs(du)
#Making the crs the same for both
du <- st_transform(du, st_crs(vegr))

# Another attempt at plotting data
ggplot() +
  annotation_map_tile(type = "osm", progress = "none") +
  geom_sf(data=du) #this won't work
