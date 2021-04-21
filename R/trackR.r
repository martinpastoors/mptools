# trackR codes to read tcx files

library(trackeR)
library(tidyverse)
library(leaflet)

options(dplyr.summarise.inform = FALSE)

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

save(tomtom_gps, file=file.path(my.filepath, "rdata", "tomtom_gps.RData"))
load(file=file.path(my.filepath, "rdata", "tomtom_gps.RData"))

tomtom_hr <-
  tomtom_gps %>% 
  group_by(date, filename) %>% 
  mutate(time2 = ifelse(row_number()==1,0,time-lag(time))) %>% 
  mutate(hrzone = cut(heart_rate, breaks=c(0,125,145,160,175,300), right=TRUE)) %>% 
  group_by(date, filename, hrzone) %>% 
  summarise(duration = sum(time2, na.rm=TRUE)) %>% 
  mutate(time = hms::as.hms(duration))

tomtom_hr %>% 
  filter(grepl("running",filename)) %>% 
  ggplot(aes(x=date, y=time)) +
  theme_publication() +
  geom_bar(aes(fill=hrzone), stat="identity")

save(tomtom_hr, file=file.path(my.filepath, "rdata", "tomtom_hr.RData"))
load(file=file.path(my.filepath, "rdata", "tomtom_hr.RData"))

tomtom_session <-
  tomtom_gps %>% 
  group_by(filename) %>% 
  summarise(
    longitude = mean(longitude, na.rm=TRUE),
    latitude = mean(latitude, na.rm=TRUE),
    heartrate = mean(heart_rate, na.rm=TRUE),
    heartrate_max = max(heart_rate, na.rm=TRUE),
    sec = sum(time2, na.rm=TRUE),
    dist = max(distance, na.rm=TRUE)
  ) %>% 
  mutate(min=sec/60)

save(tomtom_session, file=file.path(my.filepath, "rdata", "tomtom_session.RData"))


# =================================================================================================

# Read old sporttracks information; the gpx file does not have heartrate; how to read the tcx file?

# =================================================================================================

t <- trackeR::readGPX(file.path(my.filepath,"SportTracks export 20200403.gpx"))

filepath <- system.file("extdata/tcx/", "2013-06-01-183220.TCX.gz", package = "trackeR")
setwd("c:/users/marti/Dropbox/Hardloop")
filepath <- system.file("run-20210110T083628.tcx", package = "trackeR")
runDF <- readTCX(file = filepath, timezone = "GMT")

# data(runs, package = "trackeR")
plot(runDF, session = 1:5, what = c("speed", "pace", "altitude"))
plot(runDF, session = 1:5)
plot(runDF)
