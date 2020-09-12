# Retrieve elevation from digital elevation model (DEM)
# Steven Moran <steven.moran@uzh.ch>; example from Peter Ranacher
# DEM data available here: https://www.ngdc.noaa.gov/mgg/topo/gltiles.html
# GLOBE model has global coverage with a spatial resolution of 1km

library(raster)
elev <- raster("global_dem.tif")

# An example
some_cities <- data.frame(city = c("Zurich", "Vienna", "Ouagadougou"),
                          lon = c(8.5417, 16.3738, -1.4900),
                          lat = c(47.376, 48.2082, 12.3217))
                       
some_cities$elevation <- extract(elev, some_cities[, c("lon", "lat")], method="simple")
some_cities

# Get elevation with Glottolog 4 data
library(dplyr)

glottolog.geo <- read.csv('https://cdstar.shh.mpg.de/bitstreams/EAEA0-3DAE-E27B-4692-0/languages_and_dialects_geo.csv')
languages.geo <- glottolog.geo %>% select(glottocode, longitude, latitude) %>% rename(lon=longitude, lat=latitude)
languages.geo <- languages.geo %>% filter(!is.na(lon))
languages.geo$elevation <- extract(elev, languages.geo[, c("lon", "lat")], method="simple")

write.csv(languages.geo, file="glottocodes_elevation.csv", row.names = FALSE)