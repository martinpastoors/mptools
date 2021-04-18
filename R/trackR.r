# trackR codes to read tcx files

library(trackeR)
library(tidyverse)
library(leaflet)

options(dplyr.summarise.inform = FALSE)

source("r/my utils.r")

my.filepath <- file.path(get_dropbox(),"Hardloop")
my.files <- list.files(file.path(my.filepath,"archive"), pattern = "running", full.names = TRUE)

t <- xml2::read_xml(file.path(my.filepath,"SportTracks export 20200403.tcx"))
sporttracks_gps <- data.frame(stringsAsFactors = FALSE)

for (i in 1:length(my.files)) {
  print(i)
  sporttracks_gps <-
    sporttracks_gps %>% 
    bind_rows(
      trackeR::readGPX(file = my.files[[i]], timezone = "GMT") %>% 
        mutate(date = lubridate::date(time)) %>% 
        group_by(date) %>% 
        mutate(
          deltat = ifelse(row_number()==1,0,time - lag(time)),
          deltad = ifelse(row_number()==1,0,distance - lag(distance)),
          cumt = cumsum(deltat),
          cumd = cumsum(deltad)
        )
    )
}

sporttracks_gps <-

sporttracks_gps <-
  sporttracks_gps %>% 
  mutate(date = lubridate::date(time)) %>% 
  group_by(date) %>% 
  mutate(
    deltat = ifelse(row_number()==1,0,time - lag(time)),
    deltad = ifelse(row_number()==1,0,distance - lag(distance)),
    cumt = cumsum(deltat),
    cumd = cumsum(deltad)
  )

sporttracks_session <-
  sporttracks_gps %>% 
  group_by(date) %>% 
  summarise(
    longitude = mean(longitude, na.rm=TRUE),
    latitude = mean(latitude, na.rm=TRUE),
    heartrate = mean(heart_rate, na.rm=TRUE),
    heartrate_max = max(heart_rate, na.rm=TRUE),
    sec = max(cumt, na.rm=TRUE),
    dist = max(cumd, na.rm=TRUE)
  )

save(sporttracks_gps, file=file.path(my.filepath, "rdata", "sporttracks_gps.RData"))


filepath <- system.file("extdata/tcx/", "2013-06-01-183220.TCX.gz", package = "trackeR")
setwd("c:/users/marti/Dropbox/Hardloop")
filepath <- system.file("run-20210110T083628.tcx", package = "trackeR")
runDF <- readTCX(file = filepath, timezone = "GMT")

# data(runs, package = "trackeR")
plot(runDF, session = 1:5, what = c("speed", "pace", "altitude"))
plot(runDF, session = 1:5)
plot(runDF)
