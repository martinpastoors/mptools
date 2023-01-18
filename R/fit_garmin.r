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
for (i in 1:length(my.fit)) {
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

# laptimes, while fixing situation that start_time and lag(end_time) are identical
lap_gm <- 
  lap %>% 
  relocate(id, filename, sport, start_time, end_time) %>% 
  group_by(id) %>% 
  mutate(
    lap        = row_number(),
    start_time = ifelse(lap > 1 & start_time == lag(end_time), start_time+lubridate::seconds(1), start_time),
    start_time = lubridate::as_datetime(start_time)) %>% 
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

# lap %>% filter(grepl("20211106T084555", id)) %>% group_by(id) %>% filter(start_time == lag(end_time)) %>%  View()
# lap_gm %>% filter(is.na(start_time)) %>% View()
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

# allocate laps to the individual records using sqldf 
r2 <-
  sqldf::sqldf("select r1.start_time, lap_gm.lap from r1
                join lap_gm on r1.start_time >= lap_gm.start_time and 
                               r1.start_time <= lap_gm.end_time") %>% 
  as_tibble() %>% 
  distinct()

# r2 %>% group_by(start_time, lap) %>% mutate(n=n()) %>% filter(n>1) %>% arrange(start_time, lap) %>% View()

# combine r1 and r2 again
rec_gm <-
  left_join(r1, r2, by="start_time") %>% 
  relocate(id, sport, lap, start_time) 

# rec_gm %>% filter(is.na(lap)) %>% distinct(id) %>% View()


# combine with existing rdata sets
session_gm <- 
  bind_rows(
    loadRData(file.path(my.filepath, "rdata", "session_gm.RData")),
    session_gm
  )

lap_gm <- 
  bind_rows(
    loadRData(file.path(my.filepath, "rdata", "laps_gm.RData")),
    lap_gm
  )

rec_gm <- 
  bind_rows(
    loadRData(file.path(my.filepath, "rdata", "rec_gm.RData")),
    rec_gm
  )

# save files
save(session_gm, file=file.path(my.filepath, "rdata", "session_gm.RData"))
save(lap_gm,     file=file.path(my.filepath, "rdata", "laps_gm.RData"))
save(rec_gm,     file=file.path(my.filepath, "rdata", "rec_gm.RData"))

# load(file=file.path(my.filepath, "rdata", "rec_tt.RData"))
# load(file=file.path(my.filepath, "rdata", "session_tt.RData"))
# load(file=file.path(my.filepath, "rdata", "laps_tt.RData"))

bind_rows(session_gm) %>% 
  filter(sport %in% c("cycling", "running")) %>% 
  mutate(year = lubridate::year(start_time)) %>% 
  ggplot(aes(x=start_time, y=km_hour)) +
  geom_point(aes(colour=sport)) +
  facet_grid(sport~year, scales="free")

# rec_gm %>% 
#   filter(id=="20211106T084555 cycling") %>% 
#   ggplot(aes(x=lon, y=lat)) + theme_publication() + geom_point(aes(colour=factor(lap)))

rec_gm %>% 
  filter(date >= lubridate::ymd("2022-05-01")) %>% 
  filter(lat>52.7, lat<53, lon < 6.6) %>% 
  filter(sport=="running") %>% 
  ggplot(aes(x=lon, y=lat)) + 
  theme_publication() + 
  theme(legend.position="none") +
  geom_point(aes(colour=id))



