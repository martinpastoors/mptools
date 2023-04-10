# ====================================================================================
# FITfile Garmin exports
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
rec     <- loadRData(file=file.path(my.filepath, "rdata", "rec_comb.RData"))
session <- loadRData(file=file.path(my.filepath, "rdata", "session_comb.RData"))
laps    <- loadRData(file=file.path(my.filepath, "rdata", "laps_comb.RData")) 
  
# make filelist for zip files
my.zip <- list.files(path=file.path(my.filepath, "garmin"), pattern = "*.zip", full.names = TRUE, recursive = FALSE) 

# unzip all zip files and then process the fit file (if any)
for (i in 1:length(my.zip)) {
  utils::unzip(my.zip[[i]], exdir = dirname(my.zip[[i]]))
}

# make filelist for fit files
my.fit <- list.files(path=file.path(my.filepath, "garmin"), pattern = "*.fit", full.names = TRUE, recursive = FALSE) 

# i <- 1
# for (i in 1:length(my.fit)) {
for (i in 15:length(my.fit)) {
    
  invisible(gc())
  
  bn      <- basename(my.fit[[i]])
  ff      <- try(readFitFile(my.fit[[i]]), silent=TRUE)
  
  if (class(ff) != "try-error") {
    
    print(paste(i,my.fit[[i]]))
    
    # ff<- readFitFile(file.path(dirname(my.fit[[59]]), "verwerkt", basename(my.fit[[59]])))
    # ff<- readFitFile(my.fit)
    
    sp      <- getMessagesByType(ff, "session")$sport
    mydate  <- as.Date(getMessagesByType(ff, "session")$timestamp)
    mydate2 <- format(mydate, "%Y%m%d") 
    id      <- paste(format(getMessagesByType(ff, "session")$start_time, "%Y%m%dT%H%M%S"), sp)
    
    if (id %notin% session$id) {
      
      # add to session summaries
      session <- 
        session %>% 

        # add data
        bind_rows(
          getMessagesByType(ff, "session") %>% 
            bind_rows() %>% 
            mutate(filename=bn) %>%
            mutate(id=id) %>% 
            arrange(desc(start_time)) %>% 
            relocate(id, filename) %>% 
            rename(
              end_time = timestamp,
              lat      = start_position_lat,
              lon      = start_position_long,
              distance = total_distance,
              ascent   = total_ascent,
              descent  = total_descent
            ) %>% 
            mutate(
              date     = as.Date(start_time),
              duration = ifelse(is.na(total_timer_time), total_elapsed_time, total_timer_time),
              km_hour  = calculate_km_hour(distance, duration),
              pace     = calculate_pace(distance, duration),
              source   = "garmin"
            ) %>% 
            tidygeocoder::reverse_geocode(
              lat  = lat,
              long = lon,
              address=addr,
              full_results = TRUE,
              method = "osm"
            ) %>%
            # rename(any_of(c("municipality"="city"))) %>% 
            dplyr::select(any_of(c("id", "sport", "date", "start_time", "end_time", "lat", "lon", 
                                  "duration", "distance", "ascent",  "descent", "num_laps", 
                                  "km_hour", "pace", "avg_heart_rate", "city", "municipality", 
                                  "country", "filename", "source")))
        )  
  
      # laps
      laps     <- 
        laps %>% 

        # add data
        bind_rows(
          getMessagesByType(ff, "lap") %>% 
          mutate(filename=bn) %>%
          mutate(id=id) %>% 
          mutate(sport=sp) %>% 
          rename(end_time=timestamp) %>% 
          mutate(date=as.Date(end_time)) %>% 
          arrange(desc(date),end_time) %>% 
          ungroup() %>% 
          dplyr::relocate(id, filename, sport, start_time, end_time) %>% 
          group_by(id) %>% 
          mutate(
            lap        = row_number(),
            start_time = ifelse(lap > 1 & start_time == lag(end_time), start_time+lubridate::seconds(1), start_time),
            start_time = lubridate::as_datetime(start_time)) %>% 
          relocate(id, filename, sport, lap, start_time, end_time) %>% 
          rename(
            lat      = start_position_lat,
            lon      = start_position_long,
            distance = total_distance,
            ascent   = total_ascent,
            descent  = total_descent
          ) %>% 
          mutate(
            duration = ifelse(is.na(total_timer_time), total_elapsed_time, total_timer_time),
            km_hour  = calculate_km_hour(distance, duration),
            pace     = calculate_pace(distance, duration),
            source   = "garmin"
          ) %>% 
          filter(!is.na(start_time)) %>% 
          dplyr::select(id, sport, date, lap, start_time, end_time, lat, lon, duration, distance, ascent,  descent, 
                        km_hour, pace, avg_heart_rate, filename, source) %>% 
          ungroup() %>% 
          distinct()
        ) 
      
      
      # add to recs (with lat long) via r1 and r2 to couple laps to the records
      r1 <- 
        getMessagesByType(ff, "record") %>% 
        bind_rows() %>%
        mutate(filename=bn) %>%
        mutate(id=id) %>% 
        mutate(date=as.Date(timestamp)) %>% 
        rename(start_time=timestamp) %>% 
        relocate(id, start_time) %>% 
        rename(
          lat      = position_lat,
          lon      = position_long,
          altitude = enhanced_altitude
        ) %>% 
        mutate(
          cum_distance = distance,
          distance = cum_distance - lag(cum_distance),
          duration = as.numeric(start_time - lag(start_time)),
          km_hour  = calculate_km_hour(distance, duration),
          pace     = calculate_pace(distance, duration),
          source   = "garmin"
        ) %>% 
        drop_na(lat, lon) %>% 
        left_join(dplyr::select(session, id, sport), by="id") %>% 
        dplyr::select(id, sport, date, start_time, lat, lon, duration, distance, altitude, 
                      km_hour, pace, heart_rate, filename, source) %>% 
        arrange(id, start_time) %>% 
        distinct()
      
      # allocate laps to the individual records using sqldf 
      r2 <-
        sqldf::sqldf("select r1.start_time, laps.lap from r1
                      join laps on r1.start_time >= laps.start_time and 
                                   r1.start_time <= laps.end_time") %>% 
        as_tibble() %>% 
        distinct()
      
      # combine r1 and r2 again
      rec <-
        rec %>% 
        bind_rows(
          left_join(r1, r2, by="start_time") %>% 
          relocate(id, sport, lap, start_time) 
        ) 
      
      # rename fit files
      file.rename(from= file.path(dirname(my.fit[[i]]), basename(my.fit[[i]])),
                  to  = file.path(dirname(my.fit[[i]]), paste0(id,".fit")) )
      
    } # end of if statement whether ID already exists
    

  } else {
    print(paste(my.fit[[i]], "File error"))
  } # end of try-error
    
} # end of loop over fit files

# save data frames
save(rec    , file=file.path(my.filepath, "rdata", "rec_comb.RData"))
save(session, file=file.path(my.filepath, "rdata", "session_comb.RData"))
save(laps   , file=file.path(my.filepath, "rdata", "laps_comb.RData"))

# intersect(names(session), names(session_tt))
# setdiff(names(session), names(session_tt))
# setdiff(names(session_tt), names(session))


