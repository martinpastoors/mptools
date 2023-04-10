# ====================================================================================
# FITfileR codes to read fit files and add to database
# 
# To do: deal with breaks in between; calculate time between recordings; 
# ====================================================================================

library(FITfileR)    # remotes::install_github("grimbough/FITfileR")
library(tidyverse)
# library(leaflet)
# library(tidygeocoder)
library(RJSONIO)     #install.packages("RJSONIO")
# library(OpenStreetMap)
library(osmdata)
# library(rJava)
library(ggthemes)

options(dplyr.summarise.inform = FALSE)

source("r/my utils.r")

# set path
my.filepath <- file.path(get_dropbox(),"Hardloop")


load(file=file.path(my.filepath, "rdata", "session_comb.RData"))
load(file=file.path(my.filepath, "rdata", "rec_comb.RData"))
load(file=file.path(my.filepath, "rdata", "laps_comb.RData"))

# Check sessions
session %>% 
  mutate(year = lubridate::year(start_time)) %>% 
  group_by(source, year) %>% 
  summarise(nobs = n()) %>% 
  ggplot(aes(x=year, y=source)) +
  theme_publication() +
  geom_point(aes(colour=source)) +
  geom_text(aes(label=nobs))

session %>% 
  filter(grepl("^fit", filename)) %>% 
  arrange(id) %>% 
  View()

# session <-
#   session %>% 
#   mutate(source = ifelse(is.na(source) & grepl("ACTIVITY", filename), "garmin", source)) 

# Updating and fixing session
t <- 
  session %>% 
  mutate(date     = ifelse(is.na(date), as.Date(start_time), date)) %>% 
  mutate(date     = as.Date(date, origin="1970-01-01")) %>% 
  mutate(duration = ifelse(is.na(duration), as.numeric(end_time - start_time), duration)) %>% 
  mutate(duration = ifelse(duration==0, as.numeric(NA), duration)) %>% 
  mutate(distance = ifelse(distance==0, as.numeric(NA), distance)) %>% 
  mutate(num_laps = ifelse(num_laps == -Inf, as.numeric(NA), num_laps)) %>% 
  mutate(km_hour  = ifelse(is.nan(km_hour), as.numeric(NA), km_hour)) %>% 
  mutate(km_hour  = ifelse(km_hour == 0, as.numeric(NA), km_hour)) %>% 
  mutate(pace     = ifelse(pace == "NaN:NaN" |pace=="Inf:NaN", as.character(NA), pace)) %>% 
  mutate(num_laps = ifelse(num_laps==0, 1, num_laps)) %>% 
  mutate(lat      = ifelse(lat == 180, as.numeric(NA), lat)) %>% 
  mutate(lon      = ifelse(lon == 180, as.numeric(NA), lon)) %>% 
  mutate(avg_heart_rate = ifelse(is.nan(avg_heart_rate), as.numeric(NA), avg_heart_rate)) %>% 
  dplyr::select(-any_of(c("total_elapsed_time", "total_timer_time", "enhanced_avg_speed",
                          "enhanced_max_speed","total_calories", "total_ascent",
                          "total_descent", "max_heart_rate","total_training_effect",
                          "city")))
t1 <-
  t %>% 
  filter(is.na(municipality) & !is.na(lat) & !is.na(lon)) %>% 
  dplyr::select(id, lat, lon) %>% 
  tidygeocoder::reverse_geocode(
    lat  = lat,
    long = lon,
    address=addr,
    full_results = TRUE,
    method = "osm"
  ) %>% 
  mutate(municipality = case_when(
    !is.na(city)          ~ city,
    !is.na(municipality)  ~ municipality,
    !is.na(village)       ~ village,
    !is.na(town)          ~ town,
    !is.na(city_district) ~ city_district,
    !is.na(state)         ~ state
  )) %>% 
  dplyr::select(id, municipality, country)

session <-
  t %>% 
  left_join(t1, by="id") %>% 
  mutate(
    municipality = ifelse(is.na(municipality.x), municipality.y, municipality.x),
    country      = ifelse(is.na(country.x), country.y, country.x)
  ) %>% 
  dplyr::select(-c("municipality.x", "municipality.y", "country.x", "country.y"))

save(session, file=file.path(my.filepath, "rdata", "session_comb.RData"))

skimr::skim(t)
count_not_finite(t)
count_zeroes(t)
unique(t$num_laps)

inspectdf::inspect_num(t) %>% inspectdf::show_plot()
inspectdf::inspect_imb(t) %>% inspectdf::show_plot()
inspectdf::inspect_cat(t) %>% inspectdf::show_plot()

t %>% filter(is.na(duration) | is.nan(duration) | is.infinite(duration)|duration==0) %>% View()
t %>% filter(is.na(lat) | is.na(lon)) %>% View()
t %>% filter(is.nan(km_hour)) %>% View()
t %>% filter(is.nan(pace)) %>% View()
t %>% filter(km_hour==0) %>% View()
t %>% filter(km_hour < 1) %>% View()
t %>% filter(num_laps == 0) %>% View()
t %>% separate(pace, into=c("minutes","seconds"), sep=":") %>% 
  mutate(minutes = as.numeric(minutes)) %>% 
  filter(minutes > 40) %>% 
  View()
t %>% filter(is.na(municipality), !is.na(city)) %>% View()
hist(t$km_hour)
count_not_na(t)


rec %>% 
  filter(date == lubridate::dmy("08-03-2014")) %>% 
  # View()
  ggplot(aes(x=lon, y=lat)) +
  theme_publication() +
  coord_quickmap() +
  geom_point(aes(colour=start_time))

rec %>% 
  filter(date == lubridate::dmy("08-03-2014")) %>% 
  dplyr::select(start_time, lat, lon) %>% 
  tidyr::pivot_longer(names_to="variable", values_to="data", lat:lon) %>% 
  
  ggplot(aes(x=start_time, y=data)) +
  theme_publication() +
  geom_point(aes(colour=start_time)) +
  facet_wrap(~variable, scales="free_y")


# save data frames
save(rec    , file=file.path(my.filepath, "rdata", "rec_comb.RData"))
save(session, file=file.path(my.filepath, "rdata", "session_comb.RData"))
save(laps   , file=file.path(my.filepath, "rdata", "laps_comb.RData"))
