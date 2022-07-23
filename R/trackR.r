# trackR codes to read tcx files

library(trackeR)
library(tidyverse)
library(leaflet)

options(dplyr.summarise.inform = FALSE)

# TO DO: add starttime, endtime
# TO DO: add sport

source("r/my utils.r")

my.filepath <- file.path(get_dropbox(),"Hardloop")
my.files <- list.files(file.path(my.filepath,"archive"), pattern = "running|cycling|hiking|freestyle", full.names = TRUE)
# my.files <- list.files(file.path(my.filepath,"archive"), pattern = "running", full.names = TRUE)

tomtom_gps <- data.frame(stringsAsFactors = FALSE)

i <- 1
for (i in 1:length(my.files)) {
# for (i in 1:50) {
  fn      <- basename(my.files[[i]])
  mytry<- try(trackeR::readGPX(file = my.files[[i]], timezone = "GMT"), silent=TRUE)
  
  if (class(mytry) != "try-error") {
    
    print(paste(i,my.files[[i]]))
    
    tomtom_gps <-
      tomtom_gps %>% 
      bind_rows(
        trackeR::readGPX(file = my.files[[i]], timezone = "GMT") %>% 
          mutate(filename=basename(my.files[[i]])) %>% 
          mutate(date=lubridate::date(time))
      )
    
  } else {
    print(paste(i,my.files[[i]], "File error"))
  }
  
}

# tomtom_gps <- 
#   tomtom_gps %>% 
#   group_by(date) %>% 
#   mutate(
#     dd = ifelse(distance==0, 0, distance - lag(distance)),
#     dt = ifelse(distance==0, 0, as.numeric(time - lag(time))),
#     activity = ifelse(distance==0,1,0)) %>% 
#   mutate(activity = cumsum(activity)) %>% 
#   mutate(activity = paste(format(date, "%Y%m%d"),activity, sep="_")) %>% 
#   mutate(sport = stringr::word(filename, 1, sep="_"))

save(tomtom_gps, file=file.path(my.filepath, "rdata", "tomtom_gps.RData"))
load(file=file.path(my.filepath, "rdata", "tomtom_gps.RData"))

tomtom_hr <-
  tomtom_gps %>% 
  group_by(activity) %>% 
  mutate(dt = ifelse(row_number()==1,0,time-lag(time))) %>% 
  mutate(hrzone = cut(heart_rate, breaks=c(0,125,145,160,175,200), right=TRUE)) %>% 
  group_by(activity, hrzone) %>% 
  summarise(duration = sum(dt, na.rm=TRUE)) %>% 
  mutate(time = hms::as_hms(duration)) %>% 
  dplyr::select(-duration) %>% 
  tidyr::pivot_wider(names_from=hrzone, values_from=time) %>% 
  setNames(gsub("NA","noHR",names(.)))

save(tomtom_hr, file=file.path(my.filepath, "rdata", "tomtom_hr.RData"))
load(file=file.path(my.filepath, "rdata", "tomtom_hr.RData"))

tomtom_session <-
  tomtom_gps %>% 
  group_by(date, activity) %>% 
  summarise(
    longitude = mean(longitude, na.rm=TRUE),
    latitude  = mean(latitude, na.rm=TRUE),
    heartrate = mean(heart_rate, na.rm=TRUE),
    heartrate_max = max(heart_rate, na.rm=TRUE),
    seconds  = as.integer(difftime(max(time, na.rm=TRUE),min(time, na.rm=TRUE),units="secs")) ,
    hours    = as.numeric(difftime(max(time, na.rm=TRUE),min(time, na.rm=TRUE),units="hours")) ,
    time     = hms::as_hms(seconds),
    distance = max(distance, na.rm=TRUE)/1000
  ) %>% 
  mutate(
    speed = distance / hours,
    pace  = hms::as_hms(as.integer(seconds / distance))
  ) %>% 
  dplyr::select(-seconds, -hours) %>% 
  left_join(tomtom_hr, by="activity")

save(tomtom_session, file=file.path(my.filepath, "rdata", "tomtom_session.RData"))

writexl::write_xlsx(tomtom_session, path=file.path(my.filepath,"excel","tomtom_sessions.xlsx"))

# =================================================================================================

# Read old sporttracks information

# =================================================================================================

sporttracks_gps <-
  # trackeR::readTCX(file.path(my.filepath,"SportTracks export 20200403.tcx")) %>% 
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

sporttracks_session <-
  sporttracks_gps %>% 
  group_by(date, activity) %>% 
  summarise(
    longitude = mean(longitude, na.rm=TRUE),
    latitude  = mean(latitude, na.rm=TRUE),
    heartrate = mean(heart_rate, na.rm=TRUE),
    heartrate_max = max(heart_rate, na.rm=TRUE),
    seconds  = as.integer(difftime(max(time, na.rm=TRUE),min(time, na.rm=TRUE),units="secs")) ,
    hours    = as.numeric(difftime(max(time, na.rm=TRUE),min(time, na.rm=TRUE),units="hours")) ,
    time     = hms::as_hms(seconds),
    distance = max(distance, na.rm=TRUE)/1000
  ) %>% 
  mutate(
    speed = distance / hours,
    pace  = hms::as_hms(as.integer(seconds / distance))
  ) %>% 
  dplyr::select(-seconds, -hours) %>% 
  left_join(sporttracks_hr, by="activity")


save(sporttracks_gps, file=file.path(my.filepath, "rdata", "sporttracks_gps.RData"))
save(sporttracks_hr, file=file.path(my.filepath, "rdata", "sporttracks_hr.RData"))
save(sporttracks_session, file=file.path(my.filepath, "rdata", "sporttracks_session.RData"))

writexl::write_xlsx(sporttracks_session, path=file.path(my.filepath,"excel","sporttrack_sessions.xlsx"))

save(sporttracks_gps_check, file=file.path(my.filepath, "rdata", "sporttracks_gps_check.RData"))

load(file=file.path(my.filepath, "rdata", "sporttracks_gps.RData"))
load(file=file.path(my.filepath, "rdata", "sporttracks_hr.RData"))
