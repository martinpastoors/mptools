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
  # mutate(source = ifelse(is.na(source) & grepl("^fit", filename), "garmin", source)) %>% 
  # filter(grepl("^fit", filename)) %>% 
  # filter(source=="garmin") %>% 
  filter(grepl("ACTIVITY", filename)) %>% 
  arrange(id) %>% 
  View()

session <-
  session %>% 
  mutate(source = ifelse(is.na(source) & grepl("ACTIVITY", filename), "garmin", source)) 

session %>% 
  group_by(date, sport) %>% 
  filter(n() > 1) %>% 
  arrange(date, id) %>% 
  View()


# save data frames
save(rec    , file=file.path(my.filepath, "rdata", "rec_comb.RData"))
save(session, file=file.path(my.filepath, "rdata", "session_comb.RData"))
save(laps   , file=file.path(my.filepath, "rdata", "laps_comb.RData"))
