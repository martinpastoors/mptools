# ====================================================================================
# auto.r
# 
# read auto data 
# ====================================================================================

library(tidyverse)
library(ggthemes)
library(readxl)
# install.packages("googledrive")
library(googledrive)

options(dplyr.summarise.inform = FALSE)

source("r/my utils.r")

# set path
my.filepath <- file.path(get_dropbox(),"Hardloop")

# drive_find()
drive_download(file="Auto verbruik.xlsx", overwrite=TRUE)
t <- 
  readxl::read_excel("Auto verbruik.xlsx") %>% 
  lowcase() %>% 
  mutate(
    yday  = lubridate::yday(datum),
    month = lubridate::month(datum),
    year  = lubridate::year(datum)
  ) %>% 
  drop_na(datum) %>% 
  mutate(kmafgelegd = ifelse(is.na(kmafgelegd), 0, kmafgelegd)) %>% 
  group_by(year) %>% 
  mutate(km = cumsum(kmafgelegd))

t %>% 
  filter(year >= 2022) %>% 
  ggplot(aes(x=yday, y=km, group=year)) +
  theme_publication() +
  geom_line(aes(colour=as.character(year))) +
  scale_x_continuous(breaks=seq(0,365,50))

skimr::skim(t)
count_not_finite(t)
count_zeroes(t)
unique(t$num_laps)

inspectdf::inspect_num(t) %>% inspectdf::show_plot()
inspectdf::inspect_imb(t) %>% inspectdf::show_plot()
inspectdf::inspect_cat(t) %>% inspectdf::show_plot()


