# ====================================================================================
# FITfileR codes to read fit files and add to database
# 
# To do: deal with breaks in between; calculate time between recordings; 
# ====================================================================================

library(FITfileR)    # remotes::install_github("grimbough/FITfileR")
library(tidyverse)
library(RJSONIO)     #install.packages("RJSONIO")
library(osmdata)
library(sf)

options(dplyr.summarise.inform = FALSE)

source("r/my utils.r")

# set path
my.filepath <- file.path(get_dropbox(),"Hardloop")

# load data frames
load(file=file.path(my.filepath, "rdata", "session_comb.RData"))
load(file=file.path(my.filepath, "rdata", "rec_comb.RData"))
load(file=file.path(my.filepath, "rdata", "laps_comb.RData"))

# http://joshuamccrain.com/tutorials/maps/streets_tutorial.html
# ======================================================================================
h <- data.frame(lat = 52.81355, lon=6.54193)

t <- session %>% filter(municipality=="Midden-Drenthe", sport=="running", 
                        distance >=10000, distance <10400) 

t2 <-
  rec %>% 
  filter(id %in% t$id) %>% 
  # filter(lat < 52.82) %>% 
  filter(!grepl("53920", id)) %>% 
  drop_na(start_time, lat, lon, cum_distance) %>% 
  group_by(id) %>% 
  mutate(
    xmin = round(min(lon, na.rm=TRUE), digits=2),
    xmax = round(max(lon, na.rm=TRUE), digits=2),
    ymin = round(min(lat, na.rm=TRUE), digits=2),
    ymax = round(max(lat, na.rm=TRUE), digits=2),
    distance2 = round(max(cum_distance, na.rm=TRUE)/1000, digits=1), 
    round = paste(
      paste0(distance2,"km"),
      paste(xmin, xmax, sep="-"),
      paste(ymin, ymax, sep="-"), 
      sep="\n")
  ) 

xlim <- range(t2$lon, na.rm=TRUE)
ylim <- range(t2$lat, na.rm=TRUE)

# t2 %>% 
#   ggplot(aes(x=lon, y=lat)) +
#   theme_minimal() +
#   coord_quickmap(xlim=xlim, ylim=ylim) +
#   geom_point(aes(group=as.Date(start_time)), size=0.5) +
#   geom_point(data=h, size=1.2, colour="red") +
#   facet_wrap(~round, ncol=10)


t2_sf <-
  t2 %>% 
  sf::st_as_sf(coords = c("lon", "lat"), crs = 4326) %>% 
  dplyr::group_by(id, round) %>% 
  dplyr::summarise() %>%
  sf::st_cast("POINT")
  # sf::st_cast("LINESTRING")

bbox <- sf::st_bbox(t2_sf)

q <- 
  opq (bbox=bbox) %>%
  add_osm_feature(key = "highway", 
                  value = c("motorway", "primary", "secundary", "tertiary", "unclassified")) %>%
  osmdata_sf()
# q <- 
#   getbb ("Midden-Drenthe") %>%
#   opq () %>%
#   add_osm_feature(key = "highway", 
#                   value = c("motorway", "primary", "secundary", "tertiary", "unclassified")) %>%
#   osmdata_sf()

ggplot() +
  theme_publication() +
  theme(legend.position = "none") +
  geom_sf(data = q$osm_lines, inherit.aes = FALSE, color = "black") +
  # geom_sf(data = river$osm_lines, inherit.aes = FALSE, color = "blue") +
  geom_point(data=h, aes(x=lon, y=lat), size=1.2, colour="red") +
  geom_sf(data=t2_sf, aes(colour=round), size=0.5) +
  facet_wrap(~id, nrow=3)
  
  

# plot(map$osm_lines)

ggplot() +
  theme_publication() +
  theme(legend.position="none") +
  # geom_sf(data=map) +
  geom_sf(data=t2_sf, aes(colour=id)) +
  geom_point(data=h, aes(x=lon, y=lat), size=1.2, colour="red") +
  facet_wrap(~round, ncol=8)

# leaflet solution
# map <- leaflet()  %>% addTiles() 
# map <- map %>%   
#   # addCircleMarkers(data=df1, radius = 8, color = 'red', fill = TRUE, label = ~as.character(row_rank), labelOptions=c(noHide=TRUE)) %>%
#   addPolylines(data=t2, lng = ~lon, lat = ~lat, group=id)
# map




