# FITfileR codes to read fit files

library(FITfileR)    # devtools::install_github("grimbough/FITfileR")
library(tidyverse)
library(leaflet)

options(dplyr.summarise.inform = FALSE)

source("r/my utils.r")

my.filepath <- file.path(get_dropbox(),"Hardloop")
my.files <- 
  list.files(path=file.path(get_dropbox(),"Hardloop"), pattern = "*.fit", full.names = TRUE) %>% 
  data.frame(stringsAsFactors = FALSE) %>% 
  setNames("filename") %>% 
  mutate(basename=basename(filename)) %>% 
  separate(basename, into=c("sport","datetime"), remove=FALSE) %>% 
  arrange(datetime) %>% 
  dplyr::select(filename) %>% 
  unlist()

# load data frames

session <- loadRData(file=file.path(my.filepath, "rdata", "session.RData"))
rec     <- loadRData(file=file.path(my.filepath, "rdata", "rec.RData"))
lap     <- loadRData(file=file.path(my.filepath, "rdata", "laps.RData"))
tmp     <- readxl::read_xlsx(path=file.path(my.filepath,"excel","sessions.xlsx"))

# i <- 1
# for (i in 190:200) {
for (i in 1:length(my.files)) {
  
  fn      <- basename(my.files[[i]])
  mytry<- try(readFitFile(my.files[[i]]), silent=TRUE)

  if (class(mytry) != "try-error") {
    
    print(paste(i,my.files[[i]]))
    
    ff<- readFitFile(my.files[[i]])
    
    session <- 
      session %>% 
      
      # check if run already exists; if so remove
      filter(!fn %in% filename) %>% 
      
      # add data
      bind_rows(getMessagesByType(ff, "session") %>% 
                  bind_rows() %>% 
                  mutate(filename=fn) %>%
                  separate(filename, into=c("sporttype","fn"), sep="-|_", remove=FALSE) %>% 
                  arrange(desc(start_time)) 
                )  
    
    rec     <- 
      rec %>% 
      
      # check if run already exists; if so remove
      filter(!fn %in% filename) %>% 
      
      # add data
      bind_rows(getMessagesByType(ff, "record") %>% 
                  bind_rows() %>%
                  mutate(filename=fn) %>%
                  separate(filename, into=c("sporttype","fn"), sep="-|_", remove=FALSE) %>% 
                  mutate(date=as.Date(timestamp)) %>% 
                  arrange(desc(date),timestamp) 
                )  
    
    lap     <- 
      lap %>% 
      
      # check if run already exists; if so remove
      filter(!fn %in% filename) %>% 
      
      # add data
      bind_rows(getMessagesByType(ff, "lap") %>% 
                  bind_rows() %>% 
                  mutate(filename=fn) %>%
                  separate(filename, into=c("sporttype","fn"), sep="-|_", remove=FALSE) %>% 
                  mutate(date=as.Date(timestamp)) %>% 
                  arrange(desc(date),timestamp) %>% 
                  group_by(filename, sport, sub_sport) %>% 
                  mutate(km_hour = avg_speed * (60*60) / 1000) %>% 
                  mutate(change  = km_hour / lag(km_hour) - 1) %>% 
                  mutate(test    = ifelse(abs(change)>0.1,1,0)) %>% 
                  mutate(test    = ifelse(is.na(test),1, test)) %>% 
                  mutate(lap     = cumsum(test)) %>% 
                  group_by(filename, fn, date, sport, sub_sport, lap) %>% 
                  summarise(
                    start_time = min(start_time),
                    end_time   = max(timestamp),
                    start_position_lat = dplyr::first(start_position_lat),
                    start_position_long = dplyr::first(start_position_long),
                    end_position_lat = dplyr::last(end_position_lat),
                    end_position_long = dplyr::last(end_position_long),
                    total_elapsed_time = sum(total_elapsed_time, na.rm=TRUE),
                    total_calories     = sum(total_calories, na.rm=TRUE),
                    avg_heart_rate = mean(avg_heart_rate, na.rm=TRUE),
                    max_heart_rate = max(max_heart_rate, na.rm=TRUE),
                    total_distance = sum(total_distance, na.rm=TRUE),
                    km_hour = mean(km_hour, na.rm=TRUE)
                  )
                ) 
  } else {
    print(paste(i,my.files[[i]], "File error"))
  }
  
  # move file
  file.copy(my.files[[i]], file.path(get_dropbox(),"Hardloop","verwerkt"))
  file.remove(my.files[[i]])
}


# save files
save(session, file=file.path(my.filepath, "rdata", "session.RData"))
save(rec,     file=file.path(my.filepath, "rdata", "rec.RData"))
save(lap,     file=file.path(my.filepath, "rdata", "laps.RData"))
writexl::write_xlsx(session, path=file.path(my.filepath,"excel","sessions.xlsx"))



# plot of specific tracks
lap %>%
  filter(date > as.Date("2021-04-01") ) %>% 
  mutate(lap = stringr::str_pad(lap, width=2, pad="0")) %>% 
  ggplot(aes(x=start_position_long, y=start_position_lat)) +
  theme_bw() +
  # geom_segment(aes(xend=end_position_long, yend=end_position_lat, colour=as.character(lap))) +
  geom_segment(aes(xend=end_position_long, yend=end_position_lat, colour=as.character(lap), size=km_hour)) +
  geom_point(aes(colour=lap)) +
  scale_size(range = c(0, 2)) +
  facet_wrap(~date)


# ==================================================================


library(trackeR)
library(tidyverse)

my.filepath <- "c:/users/marti/Dropbox/Hardloop"
my.files <- list.files("c:/users/marti/Dropbox/Hardloop", pattern = "*.tcx", full.names = TRUE)
test     <- readFitFile(my.files[[1]])
runDF    <- readTCX(file = my.files[[1]], timezone = "GMT")

filepath <- system.file("extdata/tcx/", "2013-06-01-183220.TCX.gz", package = "trackeR")
setwd("c:/users/marti/Dropbox/Hardloop")
filepath <- system.file("run-20210110T083628.tcx", package = "trackeR")
runDF <- readTCX(file = filepath, timezone = "GMT")

# data(runs, package = "trackeR")
plot(runDF, session = 1:5, what = c("speed", "pace", "altitude"))
plot(runDF, session = 1:5)
plot(runDF)
