# ====================================================================================
# FITfileR codes to read fit files and add to database
# 
# To do: deal with breaks in between; calculate time between recordings; 
# ====================================================================================

library(FITfileR)    # remotes::install_github("grimbough/FITfileR")
library(tidyverse)
library(RJSONIO)     #install.packages("RJSONIO")
library(osmdata)
library(sf)

options(dplyr.summarise.inform = FALSE)

source("r/my utils.r")

# set path
my.filepath <- file.path(get_dropbox(),"Hardloop")

# load data frames
load(file=file.path(my.filepath, "rdata", "session_comb.RData"))
load(file=file.path(my.filepath, "rdata", "rec_comb.RData"))
load(file=file.path(my.filepath, "rdata", "laps_comb.RData"))

# summary per year
session %>% 
  mutate(year = lubridate::year(date)) %>% 
  group_by(year, sport) %>% 
  summarise(
    n              = n(),
    distance       = sum(distance, na.rm=TRUE),
    duration       = sum(duration, na.rm=TRUE),
    km_hour        = mean(km_hour, na.rm=TRUE),
    avg_heart_rate = mean(avg_heart_rate, na.rm=TRUE)
  ) %>% 
  View()


session %>% 
  mutate(year = lubridate::year(date)) %>% 
  filter(sport=="running") %>% 
  group_by(year, sport) %>% 
  summarise(
    n              = n(),
    distance       = sum(distance, na.rm=TRUE),
    duration       = sum(duration, na.rm=TRUE),
    km_hour        = mean(km_hour, na.rm=TRUE),
    avg_heart_rate = mean(avg_heart_rate, na.rm=TRUE)
  ) %>% 
  tidyr::pivot_longer(names_to = "variable", values_to = "data", n:avg_heart_rate) %>% 
  ggplot(aes(x=year, y=data)) +
  theme_publication() +
  geom_point() +
  geom_line( ) +
  facet_wrap(~variable, scales="free_y")


