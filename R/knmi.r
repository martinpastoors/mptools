# knmi

download_knmi_data_day <- function(stations, variables, start_date, end_date,
                                    output_file = "knmi_download.txt") {
  
  # start_date <- parse_date_time(start_date,
  #                               orders = c("ymd_HMS", "ymd", "ym", "y"))
  # end_date <- parse_date_time(end_date,
  #                             orders = c("ymd_HMS", "ymd", "ym", "y"))
  # lubridate::hour(end_date) <- 23 # make sure to have all hours of the day
  
  # http://www.knmi.nl/kennis-en-datacentrum/achtergrond/data-ophalen-vanuit-een-script
  res <- POST(url = "http://projects.knmi.nl/klimatologie/uurgegevens/getdata_uur.cgi",
              body = list(stns = paste(stations, collapse = ":"),
                          vars = paste(variables, collapse = ":"),
                          start = format(start_date, "%Y%m%d%H"),
                          end = format(end_date, "%Y%m%d%H")
              ))
  
  write(content(res, "text", encoding = "UTF-8"), output_file)
  
  res
}