# trackR codes to read gpx files

library(trackeR)
library(tidyverse)

options(dplyr.summarise.inform = FALSE)

# TO DO: add starttime, endtime
# TO DO: add sport

source("r/my utils.r")


my.filepath <- file.path(get_dropbox(),"Hardloop")
# my.files   <- file.path(my.filepath, "archive", "SportTracks export 20200403.tcx")
# fn      <- basename(my.files[[i]])

#tst <- readJSON(file.path(my.filepath, "archive","martin.pastoors_0_summarizedActivities.json")) 
tst <- read_directory(file.path(my.filepath, "tomtom","test")) 


sporttracks <-
  trackeR::readTCX(my.files) %>% 
  sporttracks_gps %>% 
  
  filter(time != as.Date("0001-01-01")) %>% 
  arrange(time) %>% 
  mutate(date=lubridate::date(time)) %>% 
  distinct() %>% 
  group_by(date) %>% 
  mutate(
    dd = ifelse(distance==0, 0, distance - lag(distance)),
    dt = ifelse(distance==0, 0, as.numeric(time - lag(time))),
    activity = ifelse(distance==0,1,0)) %>% 
  mutate(activity = cumsum(activity)) %>% 
  mutate(activity = paste(format(date, "%Y%m%d"),activity, sep="_"))

sporttracks_gps_check <- sporttracks_gps %>% filter(lubridate::year(date) > 2020)
sporttracks_gps       <- sporttracks_gps %>% filter(lubridate::year(date) <= 2020)

# sporttracks_gps <-
#   sporttracks_gps %>% 
#   mutate(activity = ifelse(distance==0,1,0)) %>% 
#   group_by(date) %>% 
#   mutate(activity = cumsum(activity)) %>% 
#   mutate(activity = paste(format(date, "%Y%m%d"),activity, sep="_")) %>% 
#   ungroup()

sporttracks_hr <-
  sporttracks_gps %>% 
  group_by(activity) %>% 
  mutate(hrzone = cut(heart_rate, breaks=c(0,125,145,160,175,200), right=TRUE)) %>% 
  group_by(activity, hrzone) %>% 
  summarise(duration = sum(dt, na.rm=TRUE)) %>% 
  mutate(time = hms::as_hms(duration)) %>% 
  dplyr::select(-duration) %>% 
  tidyr::pivot_wider(names_from=hrzone, values_from=time) %>% 
  setNames(gsub("NA","noHR",names(.)))

# sporttracks_hr %>% 
#   pivot_longer(names_to="hrzone",values_to="time", 2:6) %>% 
#   separate(activity, into=c("date","index"), sep="_" ) %>% 
#   mutate(date=lubridate::ymd(date)) %>% 
#   ggplot(aes(x=date, y=time)) +
#   theme_publication() +
#   geom_bar(aes(fill=hrzone), stat="identity")

load(file=file.path(my.filepath, "rdata", "sporttracks_gps.RData"))
load(file=file.path(my.filepath, "rdata", "sporttracks_hr.RData"))

# generate records object
rec_sp <-
  sporttracks_gps %>% 
  separate(activity, into=c("act", "lap"), sep="_") %>% 
  mutate(lap = as.integer(lap)) %>% 
  rename(
    cum_distance = distance,
    distance     = dd,
    duration     = dt,
    start_time   = time,
    lat          = latitude,
    lon          = longitude) %>% 
  group_by(act, lap) %>% 
  mutate(
    km_hour      = calculate_km_hour(distance, duration),
    pace         = calculate_pace(distance, duration),
    ascent       = ifelse(altitude >= lag(altitude), altitude-lag(altitude), 0),
    descent      = ifelse(altitude <  lag(altitude), lag(altitude)-altitude, 0),
    source       = "sporttracks"
  ) %>% 
  group_by(act) %>% 
  mutate(id = format(min(start_time, na.rm=TRUE), "%Y%m%dT%H%M%S")) %>% 
  ungroup()

# generate session object
session_sp <-
  rec_sp %>% 
  # filter(row_number() <= 1000) %>% 
  group_by(id, source) %>% 
  mutate(
    lat          = dplyr::first(lat),
    lon          = dplyr::first(lon)
  ) %>% 
  group_by(id, source, lat, lon) %>% 
  summarise(
    distance   = sum(distance, na.rm=TRUE),
    duration   = sum(duration, na.rm=TRUE),
    ascent       = sum(ascent, na.rm=TRUE),
    descent      = sum(descent, na.rm=TRUE),
    start_time = min(start_time, na.rm=TRUE),
    end_time   = max(start_time, na.rm=TRUE),
    num_laps   = max(lap, na.rm=TRUE),
    avg_heart_rate = mean(heart_rate, na.rm=TRUE)
  ) %>% 
  ungroup() %>% 
  mutate(
    km_hour      = calculate_km_hour(distance, duration),
    pace         = calculate_pace(distance, duration),
    date         = as.Date(start_time),
    filename     = "sporttracks_gps.RData",
    sport = case_when(
      km_hour <= 6                      ~ "walking",
      km_hour >  6  & km_hour <= 15     ~ "running",
      km_hour >  15                     ~ "cycling",
      TRUE                              ~ "other")
  ) %>% 
  mutate(id = paste(id, sport)) %>% 
  tidygeocoder::reverse_geocode(
    lat  = lat,
    long = lon,
    address=addr,
    full_results = TRUE,
    method = "osm"
  ) %>% 
  dplyr::select(id, sport, date, start_time, end_time, lat, lon, duration, distance, ascent,  descent, num_laps, 
                km_hour, pace, avg_heart_rate, municipality, country, filename, source)

# setdiff(names(session_gm), names(session_sp))

# generate id object
ids <-
  session_sp %>% 
  # dplyr::select(id, municipality, country) %>% 
  dplyr::select(id) %>% 
  separate(id, into=c("id2", "sport"), sep=" ", remove=FALSE)

# redo records object with id's
rec_sp <-
  rec_sp %>% 
  left_join(ids, by=c("id"="id2")) %>% 
  dplyr::select(-id) %>% 
  rename(id=id.y)

# generate laps object
lap_sp <-
  rec_sp %>% 
  group_by(id, sport, lap, source) %>% 
  mutate(
    lat          = dplyr::first(lat),
    lon          = dplyr::first(lon)
  ) %>% 
  group_by(id, sport, lap, lat, lon, source) %>% 
  summarise(
    distance   = sum(distance, na.rm=TRUE),
    duration   = sum(duration, na.rm=TRUE),
    ascent       = sum(ascent, na.rm=TRUE),
    descent      = sum(descent, na.rm=TRUE),
    start_time   = min(start_time, na.rm=TRUE),
    end_time     = max(start_time, na.rm=TRUE),
    filename     = "sporttracks_gps.RData"
  ) %>% 
  mutate(
    km_hour      = calculate_km_hour(distance, duration),
    pace         = calculate_pace(distance, duration)
  )

  
# plots
bind_rows(session_tt, session_gm, session_sp) %>% 
  mutate(yday = lubridate::yday(date)) %>% 
  filter(sport %in% c("cycling", "running")) %>% 
  mutate(year = lubridate::year(start_time)) %>% 
  ggplot(aes(x=yday, y=km_hour)) +
  geom_point(aes(colour=source)) +
  scale_x_continuous(limits=c(1,365)) +
  facet_grid(sport~year, scales="free_y")

bind_rows(session_tt, session_gm, session_sp) %>% 
  mutate(yday = lubridate::yday(date)) %>% 
  mutate(distance = distance/1000) %>% 
  filter(sport %in% c("cycling", "running")) %>% 
  mutate(year = lubridate::year(start_time)) %>% 
  ggplot(aes(x=yday, y=distance)) +
  geom_point(aes(colour=source)) +
  scale_x_continuous(limits=c(1,365)) +
  facet_grid(sport~year, scales="free_y")


# save(sporttracks_gps, file=file.path(my.filepath, "rdata", "sporttracks_gps.RData"))
# save(sporttracks_hr, file=file.path(my.filepath, "rdata", "sporttracks_hr.RData"))
# save(sporttracks_session, file=file.path(my.filepath, "rdata", "sporttracks_session.RData"))

# save
save(session_sp, file=file.path(my.filepath, "rdata", "session_sp.RData"))
save(rec_sp, file=file.path(my.filepath, "rdata", "rec_sp.RData"))
save(lap_sp, file=file.path(my.filepath, "rdata", "laps_sp.RData"))


