# ==================================================================
# geo_inside_tester.r
# ==================================================================

# Add required libraries
require(maps    , quietly=TRUE)           # world maps
require(rgdal   , quietly=TRUE)           # read spatial polygons (e.g. EEZ)
require(maptools, quietly=TRUE)           # manipulating spatial data
require(geo     , quietly = TRUE)         # getting rectangles etc.
require(sp      , quietly = TRUE)         # spatial manipulation
require(sf      , quietly = TRUE)         # Support for simple features, a standardized way to encode spatial vector data.
library(broom)                            # for tidy; replacement of fortify

# source geo_inside
source("D:/GIT/gisland/R/geo_inside.R")

# read the FAO shapefiles
# http://www.fao.org/figis/geoserver/area/ows?service=WFS&request=GetFeature&version=1.0.0&typeName=area:FAO_AREAS&outputFormat=SHAPE-ZIP
fao         <- rgdal::readOGR(dsn="gis\\FAO_AREAS", layer='FAO_AREAS') 
save(fao,   file="rdata/fao.RData")

# some positions (in the southern pacific)
mypos <- data.frame(long = c(-81.18333, -80.63333, -74.71667),
                    lat  = c(-46.91667, -39.35000, -25.43333))

# running geo_inside for area works fine
mypos %>% 
  mutate(division = geo_inside(lon=long, lat=lat, map=fao, variable="F_AREA"))

# but running geo_inside for divisions gives an NA
mypos %>% 
  mutate(division = geo_inside(lon=long, lat=lat, map=fao, variable="F_DIVISION"))



