# trackR codes to read gpx files

library(trackeR)
library(tidyverse)

options(dplyr.summarise.inform = FALSE)

# TO DO: add starttime, endtime
# TO DO: add sport

source("r/my utils.r")

my.filepath <- file.path(get_dropbox(),"Hardloop")
my.files <- list.files(file.path(my.filepath,"tomtom"), pattern = "running|cycling|hiking|freestyle|trail", full.names = TRUE)

# df <- trackeR::read_directory(file.path(my.filepath,"tomtom")) %>% as.data.frame(df) # wel snel; maar slaat filename niet op. 

df <- data.frame(stringsAsFactors = FALSE)

# i <- 1
for (i in 1:length(my.files)) {
  fn      <- basename(my.files[[i]])
  ff      <- try(trackeR::readGPX(file = my.files[[i]], timezone = "GMT", speedunit="m_per_s", distanceunit = "m" ), silent=TRUE)

  if (class(ff) != "try-error") {
    
    print(paste(i,my.files[[i]]))
    
    df <-
      df %>% 
      bind_rows(
        ff %>% 
          mutate(filename=fn) %>% 
          mutate(date=lubridate::date(time)) %>% 
          mutate(sport = stringr::word(fn,1,sep="_")) %>% 
          mutate(id = paste(format(min(time, na.rm=TRUE), "%Y%m%dT%H%M%S"), sport)) %>% 
          rename(start_time=time)
      )
    
  } else {
    print(paste(i,my.files[[i]], "File error"))
  }
}

# intersect(names(df), names(rec))
# setdiff(names(df), names(rec))
# setdiff(names(rec), names(df))

rec_tt <-
  df %>% 
  group_by(id) %>% 
  mutate(
    cum_distance = distance * 1000,
    distance     = cum_distance - lag(cum_distance),
    duration     = as.numeric(start_time - lag(start_time)),
    km_hour      = calculate_km_hour(distance, duration),
    pace         = calculate_pace(distance, duration),
    ascent       = ifelse(altitude >= lag(altitude), altitude-lag(altitude), 0),
    descent      = ifelse(altitude <  lag(altitude), lag(altitude)-altitude, 0),
    source       = "tomtom"
  ) %>% 
  dplyr::select(-one_of(c("speed","cadence_running", "cadence_cycling","power","temperature"))) %>% 
  rename(lat=latitude, lon=longitude)

session_tt <-
  rec_tt %>% 
  group_by(id, filename, sport, date, source) %>% 
  mutate(
    lat1 = ifelse(row_number() == 1, lat, NA),
    lon1 = ifelse(row_number() == 1, lon, NA)
  ) %>% 
  group_by(id, filename, sport, date, source) %>% 
  summarise(
    end_time   = max(start_time, na.rm=TRUE),
    start_time = min(start_time, na.rm=TRUE),
    lat        = max(lat1, na.rm=TRUE),
    lon        = max(lon1, na.rm=TRUE),
    distance   = sum(distance, na.rm=TRUE),
    duration   = sum(duration, na.rm=TRUE), 
    avg_heart_rate = mean(heart_rate, na.rm=TRUE),
    ascent     = sum(ascent, na.rm=TRUE),
    descent    = sum(descent, na.rm=TRUE)
  ) %>% 
  mutate(
    km_hour      = calculate_km_hour(distance, duration),
    pace         = calculate_pace(distance, duration),
  ) %>% 
  
  # takes a long time
  tidygeocoder::reverse_geocode(
    lat  = lat,
    long = lon,
    address=addr,
    full_results = TRUE,
    method = "osm"
  ) %>%  
  dplyr::select(id, sport, date, start_time, end_time, lat, lon, duration, distance, ascent,  descent,
                km_hour, pace, avg_heart_rate, municipality, country, filename, source)

save(session_tt, file=file.path(my.filepath, "rdata", "session_tt.RData"))
save(rec_tt, file=file.path(my.filepath, "rdata", "rec_tt.RData"))

session_tt %>% 
  filter(sport %in% c("cycling", "running")) %>% 
  mutate(year = lubridate::year(start_time)) %>% 
  ggplot(aes(x=start_time, y=distance)) +
  geom_point(aes(colour=sport)) +
  facet_grid(sport~year, scales="free")

session_tt %>% 
  filter(sport %in% c("cycling", "running")) %>% 
  mutate(year = lubridate::year(start_time)) %>% 
  ggplot(aes(x=start_time, y=km_hour)) +
  geom_point(aes(colour=sport)) +
  facet_grid(sport~year, scales="free")

# tomtom_hr <-
#   tomtom_gps %>% 
#   group_by(activity) %>% 
#   mutate(dt = ifelse(row_number()==1,0,time-lag(time))) %>% 
#   mutate(hrzone = cut(heart_rate, breaks=c(0,125,145,160,175,200), right=TRUE)) %>% 
#   group_by(activity, hrzone) %>% 
#   summarise(duration = sum(dt, na.rm=TRUE)) %>% 
#   mutate(time = hms::as_hms(duration)) %>% 
#   dplyr::select(-duration) %>% 
#   tidyr::pivot_wider(names_from=hrzone, values_from=time) %>% 
#   setNames(gsub("NA","noHR",names(.)))
# 
# save(tomtom_hr, file=file.path(my.filepath, "rdata", "tomtom_hr.RData"))
# load(file=file.path(my.filepath, "rdata", "tomtom_hr.RData"))
# 
# tomtom_session <-
#   tomtom_gps %>% 
#   group_by(date, activity) %>% 
#   summarise(
#     longitude = mean(longitude, na.rm=TRUE),
#     latitude  = mean(latitude, na.rm=TRUE),
#     heartrate = mean(heart_rate, na.rm=TRUE),
#     heartrate_max = max(heart_rate, na.rm=TRUE),
#     seconds  = as.integer(difftime(max(time, na.rm=TRUE),min(time, na.rm=TRUE),units="secs")) ,
#     hours    = as.numeric(difftime(max(time, na.rm=TRUE),min(time, na.rm=TRUE),units="hours")) ,
#     time     = hms::as_hms(seconds),
#     distance = max(distance, na.rm=TRUE)/1000
#   ) %>% 
#   mutate(
#     speed = distance / hours,
#     pace  = hms::as_hms(as.integer(seconds / distance))
#   ) %>% 
#   dplyr::select(-seconds, -hours) %>% 
#   left_join(tomtom_hr, by="activity")
# 
# save(tomtom_session, file=file.path(my.filepath, "rdata", "tomtom_session.RData"))
# 
# writexl::write_xlsx(tomtom_session, path=file.path(my.filepath,"excel","tomtom_sessions.xlsx"))
# 
# # =================================================================================================
# 
# # Read old sporttracks information
# 
# # =================================================================================================
# 
# sporttracks_gps <-
#   # trackeR::readTCX(file.path(my.filepath,"SportTracks export 20200403.tcx")) %>% 
#   sporttracks_gps %>% 
#   
#   filter(time != as.Date("0001-01-01")) %>% 
#   arrange(time) %>% 
#   mutate(date=lubridate::date(time)) %>% 
#   distinct() %>% 
#   group_by(date) %>% 
#   mutate(
#     dd = ifelse(distance==0, 0, distance - lag(distance)),
#     dt = ifelse(distance==0, 0, as.numeric(time - lag(time))),
#     activity = ifelse(distance==0,1,0)) %>% 
#   mutate(activity = cumsum(activity)) %>% 
#   mutate(activity = paste(format(date, "%Y%m%d"),activity, sep="_"))
# 
# sporttracks_gps_check <- sporttracks_gps %>% filter(lubridate::year(date) > 2020)
# sporttracks_gps       <- sporttracks_gps %>% filter(lubridate::year(date) <= 2020)
# 
# # sporttracks_gps <-
# #   sporttracks_gps %>% 
# #   mutate(activity = ifelse(distance==0,1,0)) %>% 
# #   group_by(date) %>% 
# #   mutate(activity = cumsum(activity)) %>% 
# #   mutate(activity = paste(format(date, "%Y%m%d"),activity, sep="_")) %>% 
# #   ungroup()
# 
# sporttracks_hr <-
#   sporttracks_gps %>% 
#   group_by(activity) %>% 
#   mutate(hrzone = cut(heart_rate, breaks=c(0,125,145,160,175,200), right=TRUE)) %>% 
#   group_by(activity, hrzone) %>% 
#   summarise(duration = sum(dt, na.rm=TRUE)) %>% 
#   mutate(time = hms::as_hms(duration)) %>% 
#   dplyr::select(-duration) %>% 
#   tidyr::pivot_wider(names_from=hrzone, values_from=time) %>% 
#   setNames(gsub("NA","noHR",names(.)))
# 
# # sporttracks_hr %>% 
# #   pivot_longer(names_to="hrzone",values_to="time", 2:6) %>% 
# #   separate(activity, into=c("date","index"), sep="_" ) %>% 
# #   mutate(date=lubridate::ymd(date)) %>% 
# #   ggplot(aes(x=date, y=time)) +
# #   theme_publication() +
# #   geom_bar(aes(fill=hrzone), stat="identity")
# 
# sporttracks_session <-
#   sporttracks_gps %>% 
#   group_by(date, activity) %>% 
#   summarise(
#     longitude = mean(longitude, na.rm=TRUE),
#     latitude  = mean(latitude, na.rm=TRUE),
#     heartrate = mean(heart_rate, na.rm=TRUE),
#     heartrate_max = max(heart_rate, na.rm=TRUE),
#     seconds  = as.integer(difftime(max(time, na.rm=TRUE),min(time, na.rm=TRUE),units="secs")) ,
#     hours    = as.numeric(difftime(max(time, na.rm=TRUE),min(time, na.rm=TRUE),units="hours")) ,
#     time     = hms::as_hms(seconds),
#     distance = max(distance, na.rm=TRUE)/1000
#   ) %>% 
#   mutate(
#     speed = distance / hours,
#     pace  = hms::as_hms(as.integer(seconds / distance))
#   ) %>% 
#   dplyr::select(-seconds, -hours) %>% 
#   left_join(sporttracks_hr, by="activity")
# 
# 
# save(sporttracks_gps, file=file.path(my.filepath, "rdata", "sporttracks_gps.RData"))
# save(sporttracks_hr, file=file.path(my.filepath, "rdata", "sporttracks_hr.RData"))
# save(sporttracks_session, file=file.path(my.filepath, "rdata", "sporttracks_session.RData"))
# 
# writexl::write_xlsx(sporttracks_session, path=file.path(my.filepath,"excel","sporttrack_sessions.xlsx"))
# 
# save(sporttracks_gps_check, file=file.path(my.filepath, "rdata", "sporttracks_gps_check.RData"))
# 
# load(file=file.path(my.filepath, "rdata", "sporttracks_gps.RData"))
# load(file=file.path(my.filepath, "rdata", "sporttracks_hr.RData"))
