# FITfileR codes to read fit files

library(FITfileR)    # devtools::install_github("grimbough/FITfileR")
library(tidyverse)
library(leaflet)

source("../prf/R/my utils.r")
my.filepath <- "c:/users/marti/Dropbox/Hardloop"
my.files <- list.files(path=file.path(get_dropbox(),"Hardloop"), pattern = "*.fit", full.names = TRUE)


# empty data frames
session <- rec <- lap <- data.frame(stringsAsFactors = FALSE)

# i <- 221
# for (i in 190:200) {
for (i in 1:length(my.files)) {
  print(i)
  fn      <- basename(my.files[[i]])
  ff      <- readFitFile(my.files[[i]])
  session <- bind_rows(session, getMessagesByType(ff, "session") %>% bind_rows() %>% mutate(filename=fn))
  rec     <- bind_rows(rec, getMessagesByType(ff, "record") %>% bind_rows() %>% mutate(filename=fn))
  lap     <- bind_rows(lap, getMessagesByType(ff, "lap") %>% bind_rows() %>% mutate(filename=fn, lap=row_number()))
}

rec <-
  rec %>% 
  drop_na(position_lat, position_long) %>% 
  mutate(date=as.Date(timestamp)) %>% 
  arrange(desc(date),timestamp) 

session <-
  session %>% 
  arrange(desc(start_time)) 

lap <-
  lap %>% 
  mutate(date=as.Date(timestamp)) %>% 
  arrange(desc(date),timestamp) 

# save files
save(session, file=file.path(my.filepath, "FITfileR", "session.RData"))
save(rec,     file=file.path(my.filepath, "FITfileR", "rec.RData"))
save(lap,     file=file.path(my.filepath, "FITfileR", "laps.RData"))
writexl::write_xlsx(session, path=file.path(my.filepath,"FITfileR","sessions.xlsx"))

# load files
load(file=file.path(my.filepath, "FITfileR", "session.RData"))
load(file=file.path(my.filepath, "FITfileR", "rec.RData"))
load(file=file.path(my.filepath, "FITfileR", "laps.RData"))
tmp <- readxl::read_xlsx(path=file.path(my.filepath,"FITfileR","sessions.xlsx"))

i <- 221
fn      <- basename(my.files[[i]])
ff      <- readFitFile(my.files[[i]])
l       <- getMessagesByType(ff, "lap") %>% bind_rows() %>% mutate(filename=fn, lap=row_number()) %>% 
  mutate(km_hour = avg_speed * (60*60) / 1000) %>% 
  mutate(change  = km_hour / lag(km_hour) - 1) %>% 
  mutate(changed = ifelse(abs(change) > 0.1, 1, 0)) %>% 
  mutate(test    = lag(lap, default = vctrs::vec_cast(lap, integer())[1])) 

  mutate(lap2    = ifelse(row_number()==1, 1, 0)) %>%  
  mutate(lap2    = ifelse(row_number() >1, lag(lap2) + changed, lap2))   
  

l %>% ggplot(aes(x=start_time, y=change)) + geom_line() + geom_point() +
  geom_hline(yintercept=0.15, linetype="dashed") +
  geom_hline(yintercept=-0.15, linetype="dashed")

data <- tribble(
  ~month, ~index,
  "Jan",  100.5,
  "Feb",  110.5,
  "Mar",  99.8
)

data %>%
  mutate(test = lag(index, default = vctrs::vec_cast(index, double())[2])) 

data %>%
  mutate(test = lag(index, default = vctrs::vec_cast(index, double())[1])) %>% 
  mutate(excel_formula = index/lag(index, default = vctrs::vec_cast(index, double())[1])) %>% 
  mutate(excel_formula = lag(excel_formula, default = 1) * excel_formula * 100) %>% 
  print.data.frame()

attr(l$avg_speed, "units")
16.666666667 / l$avg_speed
l$avg_speed * (60*60) / 1000

ggmap::get_map(source = 'stamen')t1 <-
  lap %>% 
  filter(date == as.Date("2021-04-11")) %>% 
  group_by

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
