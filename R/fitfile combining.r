# ====================================================================================
# FITfileR codes to read fit files and add to database
# 
# To do: deal with breaks in between; calculate time between recordings; 
# ====================================================================================

library(FITfileR)    # devtools::install_github("grimbough/FITfileR")
library(tidyverse)
library(leaflet)
library(tidygeocoder)
library(RJSONIO)     #install.packages("RJSONIO")
library(OpenStreetMap)

options(dplyr.summarise.inform = FALSE)

source("r/my utils.r")

# set path
my.filepath <- file.path(get_dropbox(),"Hardloop")

# load data frames
rec        <- loadRData(file=file.path(my.filepath, "rdata", "rec.RData"))
session    <- loadRData(file=file.path(my.filepath, "rdata", "session.RData"))
lap        <- loadRData(file=file.path(my.filepath, "rdata", "laps.RData"))

rec_gm     <- loadRData(file=file.path(my.filepath, "rdata", "rec_gm.RData"))
session_gm <- loadRData(file=file.path(my.filepath, "rdata", "session_gm.RData"))
lap_gm     <- loadRData(file=file.path(my.filepath, "rdata", "laps_gm.RData"))

# session %>% filter(id %in% session_gm$id) %>% View()
# session_gm %>% filter(id %in% session$id) %>% View()

# CONTINUE HERE 14/10/2022
session_comb <- 
  bind_rows(loadRData(file=file.path(my.filepath, "rdata", "session_sp.RData")) ) %>% 
  bind_rows(loadRData(file=file.path(my.filepath, "rdata", "session_tt.RData")) ) %>% 
  bind_rows(session ) %>% 
  bind_rows(session_gm %>% filter(!id %in% session$id))  

rec_comb <- 
  bind_rows(loadRData(file=file.path(my.filepath, "rdata", "rec_sp.RData")) ) %>% 
  bind_rows(loadRData(file=file.path(my.filepath, "rdata", "rec_tt.RData")) ) %>% 
  bind_rows(rec ) %>% 
  bind_rows(rec_gm %>% filter(!id %in% rec$id))  

session_comb <- session_comb %>% distinct(id, sport, start_time, .keep_all = TRUE) 

save(session_comb, file=file.path(my.filepath, "rdata", "session_comb.RData"))
save(rec_comb,     file=file.path(my.filepath, "rdata", "rec_comb.RData"))

load(file=file.path(my.filepath, "rdata", "session_comb.RData"))
load(file=file.path(my.filepath, "rdata", "rec_comb.RData"))


# ======================================================================================
h <- data.frame(lat = 52.81355, lon=6.54193)

t <- session_comb %>% filter(municipality=="Midden-Drenthe", sport=="running", distance >=9800, distance <10400) 

t2 <-
  rec_comb %>% 
  filter(id %in% t$id) %>% 
  # filter(lat < 52.82) %>% 
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

t2 %>% 
  ggplot(aes(x=lon, y=lat)) +
  theme_minimal() +
  coord_quickmap(xlim=xlim, ylim=ylim) +
  geom_point(aes(group=as.Date(start_time)), size=0.5) +
  geom_point(data=h, size=1.2, colour="red") +
  facet_wrap(~round, ncol=10)



# first standardize the spatial objects
points   <- 
  sf::st_as_sf(t2, coords = c("lon", "lat"), crs = 4326)
polys = points %>% 
  dplyr::group_by(id) %>% 
  dplyr::summarise() %>%
  sf::st_cast("POLYGON")

plot(points)
plot(polys)
polys %>% group_by(id) %>% sf::st_bbox()

class(points)

shape <- sf::st_transform(shape, crs = 4326)



map <- OpenStreetMap::openmap(c(ylim[2],xlim[1]), c(ylim[1],xlim[2]), zoom = NULL,
               type = c("osm"),
               mergeTiles = TRUE)
class(map)

library( leaflet )
library( magrittr )
library(ggmap)


# subscr<-data.frame(lat=c(55.381640),
#                    lon=c(10.433600))

s  
go_tar_heels <- ggmap::get_stamenmap(bbox = c(left = xlim[1], 
                                              bottom = ylim[1], 
                                              right = xlim[2], 
                                              top = ylim[2]),
                                     zoom = 14, 
                                     maptype = "watercolor")
bbox <- c(left = xlim[1], 
                 bottom = ylim[1], 
                 right = xlim[2], 
                 top = ylim[2])

# plot on the background, using standard ggplot syntax
ggmap::ggmap(go_tar_heels) +
  geom_point(data = t2, aes(x = lon, y = lat), col = "red")

  autoplot.OpenStreetMap(map)  %>% longlat(.)
  theme_publication() +
  geom_point(data=t2, aes(x=lon, y=lat), size=0.2) +
  labs(title = "Plot over OpenStreetMap")

  # download
  map <- ggmap::get_googlemap(center = "Europe", zoom = 3, 
                       style = paste0("feature:administrative.country|",
                                      "element:labels|visibility:off"),
                       filename = "Map",
                       language = "en-EN") # you might want to adjust the language settings
  
  # see what you've got
  ggmap(map)
  
  # edit map
  ggmap(map)+
    
    # do some scaling (make it smaller)
    scale_x_continuous(limits = c(-12, 42), expand = c(0, 0)) +
    scale_y_continuous(limits = c(35, 70), expand = c(0, 0))+
    
    # remove unwanted information
    theme(axis.title.x    = element_blank(),
          axis.title.y    = element_blank(),
          axis.line       = element_blank(),
          axis.ticks      = element_blank(),
          axis.text       = element_blank(),
          plot.title      = element_blank(),
          plot.background = element_blank())
save(session_comb, file=file.path(my.filepath, "rdata", "session_comb.RData"))
save(rec_comb,     file=file.path(my.filepath, "rdata", "rec_comb.RData"))
# save(lap,     file=file.path(my.filepath, "rdata", "laps.RData"))
# writexl::write_xlsx(session, path=file.path(my.filepath,"excel","sessions.xlsx"))
