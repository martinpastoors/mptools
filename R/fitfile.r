
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

# FITfileR codes to read fit files

devtools::install_github("grimbough/FITfileR")

library(FITfileR)
library(tidyverse)
library(leaflet)

my.filepath <- "c:/users/marti/Dropbox/Hardloop"
my.files <- list.files("c:/users/marti/Dropbox/Hardloop", pattern = "*.fit", full.names = TRUE)

# test     <- readFitFile(my.files[[1]])
# getMessagesByType(test, "session") %>% bind_rows() %>% View()
# getMessagesByType(test, "sport") %>% bind_rows() %>% View()
# getMessagesByType(test, "lap") %>% bind_rows() %>% View()
# getMessagesByType(test, "session") %>% bind_rows() %>% View()
# records(test) %>% 
#   bind_rows() %>% 
#   arrange(timestamp) %>% 
#   dplyr::select(position_long, position_lat) %>% 
#   as.matrix() %>%
#   leaflet(  ) %>%
#   addTiles() %>%
#   addPolylines( )

session <- rec <- lap <- data.frame(stringsAsFactors = FALSE)
# i <- 200
# for (i in 190:200) {
for (i in 1:length(my.files)) {
  print(i)
  fn      <- basename(my.files[[i]])
  ff      <- readFitFile(my.files[[i]])
  session <- bind_rows(session, getMessagesByType(ff, "session") %>% bind_rows() %>% mutate(filename=fn))
  rec     <- bind_rows(rec, getMessagesByType(ff, "record") %>% bind_rows() %>% mutate(filename=fn))
  lap     <- bind_rows(lap, getMessagesByType(ff, "lap") %>% bind_rows() %>% mutate(filename=fn, lap=row_number()))
}

save(session, file=file.path(my.filepath, "session.RData"))
save(rec,     file=file.path(my.filepath, "rec.RData"))
save(lap,     file=file.path(my.filepath, "laps.RData"))

writexl::write_xlsx(session, path=file.path(my.filepath,"sessions.xlsx"))

ggmap::get_map(source = 'stamen')
glimpse(rec)
range(rec$position_long, na.rm=TRUE)

rec %>% 
  filter(position_long < 179) %>% 
  ggplot(aes(x=position_long, y=position_lat)) +
  theme_bw() +
  theme(legend.position = "none") +
  geom_point(aes(colour=filename))

for (nm in listMessageTypes())
session %>% 
  dplyr::select(start_position_long, start_position_lat) %>% 
  as.matrix() %>%
  leaflet(  ) %>%
  addTiles() %>%
  leaflet::addMarkers( ) 
  # leaflet::addPolygons()

rec %>% 
  dplyr::select(position_long, position_lat) %>% 
  as.matrix() %>%
  leaflet(  ) %>%
  addTiles() %>%
  leaflet::addPolylines()

rec %>%
  filter(grepl("run",filename)) %>% 
  select(timestamp, heart_rate, filename) %>%
  ggplot() + 
  theme_bw() +
  theme(legend.position = "none") +
  geom_line(aes(x = timestamp, y = heart_rate, col = filename)) +
  facet_wrap(~filename, scales="free_x")

save(df, file=file.path(my.filepath, "meta.RData"))
writexl::write_xlsx(df, path=file.path(my.filepath,"meta.xlsx"))

rec <- data.frame(stringsAsFactors = FALSE)
# for (i in 1:length(my.files)) {
for (i in 190:210) {
  print(paste(i, length(records(readFitFile(my.files[[i]])))))
  # df <- bind_rows(df, records(readFitFile(my.files[[i]])) %>% bind_rows() )  
}
session(readFitFile(my.files[[200]])) %>% bind_rows() %>% View()

laps(readFitFile(my.files[[200]])) %>% bind_rows() %>% View()
hrv(readFitFile(my.files[[200]])) %>% bind_rows() %>% View()
getMessagesByType(readFitFile(my.files[[200]]), "lap") %>% bind_rows() %>% View()
records(readFitFile(my.files[[i]]))[[2]] %>% View()
length(records(readFitFile((my.files[[1]]))))
        
setwd("C:/Users/marti/Dropbox/Hardloop")
