# ====================================================================================
# FITfile test 
# codes to read fit files and add to database
# 
# To do: deal with breaks in between; calculate time between recordings; 
# ====================================================================================

library(FITfileR)    # devtools::install_github("grimbough/FITfileR")
library(tidyverse)
library(leaflet)
library(tidygeocoder)
library(RJSONIO)     #install.packages("RJSONIO")

options(dplyr.summarise.inform = FALSE)

source("r/my utils.r")

# set path
my.filepath <- file.path(get_dropbox(),"Hardloop","tomtom")
bn          <- "cycle-20210711T050451.fit"
my.fit      <- file.path(my.filepath, bn)

ff      <- try(readFitFile(my.fit), silent=TRUE)

sp      <- getMessagesByType(ff, "session")$sport
mydate  <- as.Date(getMessagesByType(ff, "session")$timestamp)
mydate2 <- format(mydate, "%Y%m%d") 
id      <- paste(format(getMessagesByType(ff, "session")$start_time, "%Y%m%dT%H%M%S"), sp)

calculate_pace <- function(distance, duration) {
  pace_raw    <- (duration/60) / (distance/1000) 
  pace_min    <- floor((duration/60) / (distance/1000))
  pace_sec    <- floor((pace_raw - pace_min) *60)
  pace        <- paste0(stringr::str_pad(pace_min, width=2, pad="0"),
                                ":",
                                stringr::str_pad(pace_sec, width=2, pad=0))
  return(pace)
}

calculate_km_hour <- function(distance, duration) {
  km_hour <- (distance/1000) / (duration/3600)
}

# add to session summaries
s <- 
  getMessagesByType(ff, "session") %>% 
  rename(end_time = timestamp) %>% 
  mutate(id=id) %>% 
  tidygeocoder::reverse_geocode(
    lat  = start_position_lat,
    long = start_position_long,
    address=addr,
    full_results = TRUE,
    method = "osm",
    quiet = TRUE,
    verbose=FALSE,
    progress_bar = FALSE) %>% 
  rename(lat=start_position_lat, lon=start_position_long, distance=total_distance) %>% 
  dplyr::select(one_of("id", "start_time", "end_time", "lat", "lon",
                "total_elapsed_time", "total_timer_time", "distance", 
                "enhanced_avg_speed", "enhanced_max_speed", "total_calories",
                "total_ascent", "total_descent", "num_laps",
                "sport", "avg_heart_rate", "max_heart_rate", "total_training_effect",
                "municipality", "country"))

l <- 
  getMessagesByType(ff, "lap") %>% 
  dplyr::select(one_of("timestamp", "start_time", "start_position_lat", "start_position_long", "end_position_lat", "end_position_long",
                "total_elapsed_time", "total_timer_time", "total_distance", 
                "avg_heart_rate", "max_heart_rate")) %>% 
  mutate(id=id) %>% 
  mutate(end_time = timestamp-1) %>% 
  mutate(lap = row_number()) %>% 
  mutate(km_hour = calculate_km_hour(total_distance, total_timer_time)) %>% 
  mutate(pace    = calculate_pace(total_distance, total_timer_time)) %>% 
  dplyr::select(-timestamp) %>% 
  left_join(dplyr::select(s,
                          id, municipality, country),
            by="id") %>% 
  relocate(id, start_time, end_time) 
  
r1 <- 
  s %>% 
  dplyr::select(start_time, lat, lon) %>% 
  mutate(distance=0) %>% 
  bind_rows(
    records(ff) %>% 
      bind_rows() %>% 
      rename(start_time=timestamp, lat=position_lat, lon=position_long) %>% 
      drop_na(lat, lon)
  )

r2 <-
  sqldf::sqldf("select r1.start_time, l.lap from r1
                join l on r1.start_time >= l.'start_time' and 
                          r1.start_time <= l.'end_time'") %>% 
  as_tibble()

r <-
  left_join(r1, r2, by="start_time") %>% 
  mutate(duration = as.numeric(start_time - lag(start_time))) %>% 
  rename(cumdistance = distance) %>% 
  mutate(distance = cumdistance - lag(cumdistance)) %>% 
  mutate(km_hour = (distance/1000) / (duration/3600)  ) %>% 
  mutate(id=id) %>% 
  left_join(dplyr::select(s,
                          id, municipality, country),
            by="id") %>% 
  relocate(id, start_time) 

ul <- r %>% 
  summarise(
    lon  = min(lon, na.rm=TRUE)-0.01,
    lat  = max(lat, na.rm=TRUE)+0.01
  ) %>% 
  pivot_longer(names_to = "variable", values_to = "value", c(lat, lon)) %>% 
  dplyr::select(value) %>% 
  unlist() %>% 
  as.numeric()

lr <- r %>% 
  ungroup() %>% 
  summarise(
    lon  = max(lon, na.rm=TRUE)+0.01,
    lat  = min(lat, na.rm=TRUE)-0.01
  ) %>% 
  pivot_longer(names_to = "variable", values_to = "value", c(lat, lon)) %>% 
  dplyr::select(value) %>% 
  unlist() %>% 
  as.numeric()

# plot
library(ggmap)
ggmap::register_google(key = "AIzaSyBBAupviqeSeVBFBNF15eTfAQ2gW2Z9ZI0")

al1 = ggmap::get_map(location = c((ul[[2]]+lr[[2]])/2, (lr[[1]]+ul[[1]])/2), zoom=13, maptype = 'roadmap')
al1MAP = ggmap::ggmap(al1)
al1MAP +
  theme_bw() +
  # geom_point(data=r, aes(x=lon, y=lat, colour=factor(lap))) +
  geom_point(data=r, aes(x=lon, y=lat, colour=km_hour)) +
  geom_point(data=l, aes(x=start_position_long, y=start_position_lat), colour="black") +
  viridis::scale_color_viridis(discrete=FALSE, option="magma", direction=-1)

  
