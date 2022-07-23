# ====================================================================================
# FITfile Garmin exports
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
my.filepath <- file.path(get_dropbox(),"Hardloop")

# load data frames
# rec     <- loadRData(file=file.path(my.filepath, "rdata", "rec.RData"))
# session <- loadRData(file=file.path(my.filepath, "rdata", "session.RData"))
# lap     <- loadRData(file=file.path(my.filepath, "rdata", "laps.RData"))

# make filelist for zip files
my.zip <- list.files(path=file.path(my.filepath, "garmin"), pattern = "*.zip", full.names = TRUE, recursive = FALSE) 

# unzip all zip files and then process the fit file (if any)
for (i in 1:length(my.zip)) {
  utils::unzip(my.zip[[i]], exdir = dirname(my.zip[[i]]))
}

# make filelist for fit files
my.fit <- list.files(path=file.path(my.filepath, "garmin"), pattern = "*.fit", full.names = TRUE, recursive = FALSE) 

i <- 1
rec <- session <- lap <- data.frame(stringsAsFactors = FALSE)
for (i in 2:length(my.fit)) {
# for (i in 1:5) {
    
  invisible(gc())
  
  bn      <- basename(my.fit[[i]])
  ff      <- try(readFitFile(my.fit[[i]]), silent=TRUE)
  
  if (class(ff) != "try-error") {
    
    print(paste(i,my.fit[[i]]))
    
    # ff<- readFitFile(file.path(dirname(my.fit[[59]]), "verwerkt", basename(my.fit[[59]])))
    # ff<- readFitFile(my.fit)
    
    sp      <- getMessagesByType(ff, "session")$sport
    mydate  <- as.Date(getMessagesByType(ff, "session")$timestamp)
    mydate2 <- format(mydate, "%Y%m%d") 
    id      <- paste(format(getMessagesByType(ff, "session")$start_time, "%Y%m%dT%H%M%S"), sp)
    
    
    # add to session summaries
    session <- 
      session %>% 
      # filter(!grepl(bn,filename)) %>% 
      
      # add data
      bind_rows(getMessagesByType(ff, "session") %>% 
                  bind_rows() %>% 
                  mutate(filename=bn) %>%
                  mutate(id=id) %>% 
                  arrange(desc(start_time)) %>% 
                  relocate(id, filename) 
                  
                  # 
                  # tidygeocoder::reverse_geocode(
                  #   lat  = start_position_lat,
                  #   long = start_position_long,
                  #   address=addr,
                  #   full_results = TRUE,
                  #   method = "osm"
                  # ) 
      )  
    
    # add to records (with lat long)
    rec     <- 
      rec %>% 
      
      # check if run already exists; if so remove
      # filter(!grepl(bn,filename)) %>% 
      
      # add data
      bind_rows(getMessagesByType(ff, "record") %>% 
                  bind_rows() %>%
                  mutate(filename=bn) %>%
                  mutate(id=id) %>% 
                  mutate(date=as.Date(timestamp)) %>% 
                  rename(start_time=timestamp)
      )  
    
    lap     <- 
      lap %>% 
      
      # check if run already exists; if so remove
      # filter(!grepl(bn,filename)) %>% 
      
      # add data
      bind_rows(getMessagesByType(ff, "lap") %>% 
                  mutate(filename=bn) %>%
                  mutate(id=id) %>% 
                  mutate(sport=sp) %>% 
                  rename(end_time=timestamp) %>% 
                  mutate(date=as.Date(end_time)) %>% 
                  arrange(desc(date),end_time) %>% 
                  # group_by(filename, sport, sub_sport) %>% 
                  # mutate(km_hour = avg_speed * (60*60) / 1000) %>% 
                  # mutate(change  = km_hour / lag(km_hour) - 1) %>% 
                  # mutate(test    = ifelse(abs(change)>0.1,1,0)) %>% 
                  # mutate(test    = ifelse(is.na(test),1, test)) %>% 
                  # mutate(lap     = cumsum(test)) %>% 
                  
                  ungroup()
                
     ) 
    
    # rename fit files
    file.rename(from= file.path(dirname(my.fit[[i]]), basename(my.fit[[i]])),
                to  = file.path(dirname(my.fit[[i]]), paste0(id,".fit")) )

  } else {
    print(paste(my.fit[[i]], "File error"))
  } # end of try-error
    
} # end of loop over fit files

# intersect(names(session), names(session_tt))
# setdiff(names(session), names(session_tt))
# setdiff(names(session_tt), names(session))

session_gm <-
  session %>% 
  rename(
    end_time = timestamp,
    lat      = start_position_lat,
    lon      = start_position_long,
    distance = total_distance,
    ascent   = total_ascent,
    descent  = total_descent
  ) %>% 
  mutate(
    date     = as.Date(start_time),
    duration = ifelse(is.na(total_timer_time), total_elapsed_time, total_timer_time),
    km_hour  = calculate_km_hour(distance, duration),
    pace     = calculate_pace(distance, duration),
    source   = "garmin"
  ) %>% 
  tidygeocoder::reverse_geocode(
    lat  = lat,
    long = lon,
    address=addr,
    full_results = TRUE,
    method = "osm"
  ) %>%  
  dplyr::select(id, sport, date, start_time, end_time, lat, lon, duration, distance, ascent,  descent, num_laps, 
                km_hour, pace, avg_heart_rate, municipality, country, filename, source)

  


# load(file=file.path(my.filepath, "rdata", "session_gm.RData"))
# load(file=file.path(my.filepath, "rdata", "laps.RData"))
# load(file=file.path(my.filepath, "rdata", "rec.RData"))

# load(file=file.path(my.filepath, "rdata", "session_tt.RData"))
# load(file=file.path(my.filepath, "rdata", "rec_tt.RData"))

# NEED TO FIX THE SITUATION OF THE SAME START AND PREVIOUS ENDTIME
lap_gm <- 
  lap %>% 
  group_by(id) %>% 
  mutate(
    lap      = row_number(),
    # end_time = ifelse(end_time == lead(end_time), end_time-1, end_time)
  ) %>% 
  relocate(id, filename, sport, lap, start_time, end_time) %>% 
  rename(
    lat      = start_position_lat,
    lon      = start_position_long,
    distance = total_distance,
    ascent   = total_ascent,
    descent  = total_descent
  ) %>% 
  mutate(
    duration = ifelse(is.na(total_timer_time), total_elapsed_time, total_timer_time),
    km_hour  = calculate_km_hour(distance, duration),
    pace     = calculate_pace(distance, duration),
    source   = "garmin"
  ) %>% 
  filter(!is.na(start_time)) %>% 
  dplyr::select(id, sport, date, lap, start_time, end_time, lat, lon, duration, distance, ascent,  descent, 
                km_hour, pace, avg_heart_rate, filename, source) %>% 
  ungroup() %>% 
  distinct()

# lap_gm %>% group_by(id) %>% filter(start_time == lag(end_time)) %>% View()

# setdiff(names(session_gm), names(lap_gm))

r1 <- 
  rec %>% 
  relocate(id, start_time) %>% 
  rename(
    lat      = position_lat,
    lon      = position_long,
    altitude = enhanced_altitude
  ) %>% 
  mutate(
    cum_distance = distance,
    distance = cum_distance - lag(cum_distance),
    duration = as.numeric(start_time - lag(start_time)),
    km_hour  = calculate_km_hour(distance, duration),
    pace     = calculate_pace(distance, duration),
    source   = "garmin"
  ) %>% 
  drop_na(lat, lon) %>% 
  left_join(dplyr::select(session_gm, id, sport), by="id") %>% 
  dplyr::select(id, sport, date, start_time, lat, lon, duration, distance, altitude, 
                km_hour, pace, heart_rate, filename, source) %>% 
  arrange(id, start_time) %>% 
  distinct()

# setdiff(names(session_gm), names(r1))

# NEED YTO CHECK WHY r2 has more records than r1; see above; due to start and previous endtime being the same

r2 <-
  sqldf::sqldf("select r1.start_time, lap_gm.lap from r1
                join lap_gm on r1.start_time >= lap_gm.start_time and 
                               r1.start_time <= lap_gm.end_time") %>% 
  as_tibble() %>% 
  distinct()

r2 %>% group_by(start_time, lap) %>% mutate(n=n()) %>% filter(n>1) %>% arrange(start_time, lap) %>% View()

rec_gm <-
  left_join(r1, r2, by="start_time") %>% 
  relocate(id, sport, lap, start_time) 

rec_gm %>% group_by(id, start_time) %>% mutate(n=n()) %>% filter(n>1) %>% arrange(id, start_time, lap) %>% View()

# save files
save(session_gm, file=file.path(my.filepath, "rdata", "session_gm.RData"))
save(lap_gm,     file=file.path(my.filepath, "rdata", "laps_gm.RData"))
save(rec_gm,     file=file.path(my.filepath, "rdata", "rec_gm.RData"))


bind_rows(session_tt, session_gm) %>% 
  filter(sport %in% c("cycling", "running")) %>% 
  mutate(year = lubridate::year(start_time)) %>% 
  ggplot(aes(x=start_time, y=km_hour)) +
  geom_point(aes(colour=sport)) +
  facet_grid(sport~year, scales="free")

rec_gm %>% 
  filter(id=="20211106T084555 cycling") %>% 
  ggplot(aes(x=lon, y=lat)) + theme_publication() + geom_point(aes(colour=factor(lap)))



# load(file=file.path(my.filepath, "rdata", "rec.RData"))
# load(file=file.path(my.filepath, "rdata", "session.RData"))
# load(file=file.path(my.filepath, "rdata", "laps.RData"))

s1 <- 
  session %>% 
  # filter(as.Date(start_time) == as.Date("2017-07-21") ) %>% 
  mutate(id = paste(format(start_time, "%Y%m%dT%H%M%S"), sport)) %>% 
  dplyr::select(id, filename) 

s2 <-
  session %>% 
  right_join(s1, by="filename") %>% 
  relocate(id, filename) %>% 
  distinct() %>% 
  rename(end_time = timestamp) %>% 
  mutate(km_hour = calculate_km_hour(total_distance, total_timer_time)) %>% 
  mutate(pace    = calculate_pace(total_distance, total_timer_time)) %>% 
  dplyr::select(id, start_time, end_time, lat=start_position_lat, lon = start_position_long,
                total_elapsed_time, total_timer_time, distance=total_distance, 
                enhanced_avg_speed, enhanced_max_speed, total_calories,
                total_ascent, total_descent, num_laps,
                km_hour, pace, 
                sport, avg_heart_rate, max_heart_rate, total_training_effect,
                municipality, country, filename)
s2 %>% filter(lubridate::year(start_time)==2017) %>% View()

l1 <- 
  lap %>% 
  right_join(s1, by="filename") %>% 
  dplyr::select(id, start_time, end_time, start_position_lat, start_position_long, end_position_lat, end_position_long,
                total_elapsed_time, total_timer_time, total_distance, enhanced_avg_speed, enhanced_max_speed,
                avg_heart_rate, max_heart_rate, filename) %>% 
  mutate(total_distance = ifelse(total_distance == 0, NA, total_distance)) %>% 
  group_by(id) %>% 
  mutate(
    end_time = end_time-1,
    lap      = row_number(),
    km_hour  = calculate_km_hour(total_distance, ifelse(is.na(total_timer_time), total_elapsed_time, total_timer_time)),
    pace     = calculate_pace(total_distance, ifelse(is.na(total_timer_time), total_elapsed_time, total_timer_time))) %>% 
  left_join(s2 %>% 
              filter(num_laps==1) %>% 
              dplyr::select(id, end_time2=end_time),
            by = "id") %>% 
  left_join(dplyr::select(s2,
                          id, municipality, country),
            by="id") %>% 
  relocate(id, start_time, end_time) %>% 
  filter(!is.na(start_time))

# r1 <- 
#   rec %>% 
#   mutate(filename = ifelse(is.na(filename), fn, filename)) %>% 
#   right_join(s1, by="filename") %>% 
#   relocate(id, filename) %>% 
#   distinct() %>% 
#   drop_na(timestamp)

r1 <- 
  s2 %>% 
  dplyr::select(start_time, lat, lon) %>% 
  mutate(distance=0) %>% 
  bind_rows(
    rec %>% 
      bind_rows() %>% 
      mutate(filename = ifelse(is.na(filename), fn, filename)) %>% 
      left_join(s1, by="filename") %>% 
      rename(start_time=timestamp, lat=position_lat, lon=position_long) %>% 
      drop_na(lat, lon) 
  ) %>% 
  arrange(id, start_time)

# r1 %>% filter(is.na(id)) %>% View()
# s2 %>% filter(grepl("20210711", filename)) %>% View()
# r1 %>% filter(grepl("20210711", filename)) %>% View()

r2 <-
  sqldf::sqldf("select r1.start_time, l1.lap from r1
                join l1 on r1.start_time >= l1.'start_time' and 
                          r1.start_time <= l1.'end_time'") %>% 
  as_tibble()

r <-
  left_join(r1, r2, by="start_time") %>% 
  mutate(duration = as.numeric(start_time - lag(start_time))) %>% 
  rename(cumdistance = distance) %>% 
  mutate(distance = cumdistance - lag(cumdistance)) %>% 
  mutate(km_hour = (distance/1000) / (duration/3600)  ) %>% 
  mutate(id=id) %>% 
  mutate(lap = ifelse(is.na(lap), 1, lap)) %>% 
  left_join(dplyr::select(s,
                          id, municipality, country),
            by="id") %>% 
  relocate(id, start_time) 

# janitor::compare_df_cols(r1, r2, return="mismatch")
# dplyr::all_equal(r1, r2)

s2 %>% ggplot(aes(x=start_time, y=distance)) + geom_point(aes(colour=sport))



# this compares the content of two data.frames
diffdf::diffdf(r1,r2)

ul <- r1 %>% 
  filter(id=="20211229T051858 running") %>% 
  ungroup() %>% 
  summarise(
    ul_long = min(position_long, na.rm=TRUE)-0.01,
    ul_lat  = max(position_lat, na.rm=TRUE)+0.01
  ) %>% 
  pivot_longer(names_to = "variable", values_to = "value", c(ul_lat, ul_long)) %>% 
  dplyr::select(value) %>% 
  unlist() %>% 
  as.numeric()

lr <- r1 %>% 
  filter(id=="20211229T051858 running") %>% 
  ungroup() %>% 
  summarise(
    lr_long = max(position_long, na.rm=TRUE)+0.01,
    lr_lat  = min(position_lat, na.rm=TRUE)-0.01
  ) %>% 
  pivot_longer(names_to = "variable", values_to = "value", c(lr_lat, lr_long)) %>% 
  dplyr::select(value) %>% 
  unlist() %>% 
  as.numeric()

t <- r1 %>% filter(id=="20211229T051858 running") 

# mp <- OpenStreetMap::openmap(ul,lr)
# plot(mp)

# OpenStreetMap::autoplot.OpenStreetMap(mp, expand=FALSE) +
#   theme_bw() +
#   geom_segment(data=t, aes(x=position_long, y=position_lat, xend=lead(position_long), yend=lead(position_lat))) +
#   scale_size(range = c(0, 2))

library(ggmap)
ggmap::register_google(key = "AIzaSyBBAupviqeSeVBFBNF15eTfAQ2gW2Z9ZI0")
# al1 = get_map(location = c(left = ul[[2]], bottom = lr[[1]], right = lr[[2]], top = ul[[1]]), maptype = 'roadmap')
al1 = get_map(location = c((ul[[2]]+lr[[2]])/2, (lr[[1]]+ul[[1]])/2), zoom=15, maptype = 'roadmap')
al1MAP = ggmap(al1)
al1MAP +
  theme_bw() +
  geom_segment(data=t, aes(x=position_long, y=position_lat, xend=lead(position_long), yend=lead(position_lat))) 
  



# # plot of specific tracks
# lap %>%
#   filter(date > as.Date("2021-12-02") ) %>%
#   mutate(lap = stringr::str_pad(lap, width=2, pad="0")) %>%
#   ggplot(aes(x=start_position_long, y=start_position_lat)) +
#   theme_bw() +
#   geom_segment(aes(xend=end_position_long, yend=end_position_lat, colour=as.character(lap))) +
#   geom_segment(aes(xend=end_position_long, yend=end_position_lat, colour=as.character(lap), size=km_hour)) +
#   geom_point(aes(colour=lap)) +
#   scale_size(range = c(0, 2))


# session <-
#   session %>%
#   # slice(n()) %>%
#   tidygeocoder::reverse_geocode(
#     lat  = start_position_lat,
#     long = start_position_long,
#     address=addr,
# 
#     full_results = TRUE,
#     method = "osm"
#   ) %>% 
#   dplyr::select(-place_id, 
#                 -licence,
#                 -osm_type,
#                 -osm_id,
#                 -osm_lat,
#                 -osm_lon,
#                 -house_number,
#                 -road,
#                 -village,
#                 -state,
#                 -region,
#                 -postcode,
#                 -country_code,
#                 -boundingbox,
#                 -leisure,
#                 -city,
#                 -building,
#                 -residential,
#                 -hamlet,
#                 -suburb,
#                 -town,
#                 -amenity,
#                 -neighbourhood,
#                 -tourism,
#                 -isolated_dwelling,
#                 -county,
#                 -railway)


# my.gpx <- list.files(my.filepath, pattern = "gpx", full.names = TRUE)
# 
# tomtom_gps <- data.frame(stringsAsFactors = FALSE)
# 
# i <- 1
# for (i in 1:length(my.files)) {
#   fn      <- basename(my.files[[i]])
#   mytry<- try(trackeR::readGPX(file = my.files[[i]], timezone = "GMT"), silent=TRUE)
#   
#   if (class(mytry) != "try-error") {
#     
#     print(paste(i,my.files[[i]]))
#     
#     tomtom_gps <-
#       tomtom_gps %>% 
#       bind_rows(
#         trackeR::readGPX(file = my.files[[i]], timezone = "GMT") %>% 
#           mutate(filename=basename(my.files[[i]])) %>% 
#           mutate(date=lubridate::date(time))
#       )
#     
#   } else {
#     print(paste(i,my.files[[i]], "File error"))
#   }
#   
# }

