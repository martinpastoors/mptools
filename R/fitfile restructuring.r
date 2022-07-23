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

options(dplyr.summarise.inform = FALSE)

source("r/my utils.r")

# set path
my.filepath <- file.path(get_dropbox(),"Hardloop")

# load data frames
rec     <- loadRData(file=file.path(my.filepath, "rdata", "rec.RData"))
session <- loadRData(file=file.path(my.filepath, "rdata", "session.RData"))
lap     <- loadRData(file=file.path(my.filepath, "rdata", "laps.RData"))


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

session <- s2
rec     <- r
lap     <- l1

save(session, file=file.path(my.filepath, "rdata", "session.RData"))
save(rec,     file=file.path(my.filepath, "rdata", "rec.RData"))
save(lap,     file=file.path(my.filepath, "rdata", "laps.RData"))
writexl::write_xlsx(session, path=file.path(my.filepath,"excel","sessions.xlsx"))


