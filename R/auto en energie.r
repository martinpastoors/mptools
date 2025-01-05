# ====================================================================================
# auto en energie.r
# 
# read auto data 
# ====================================================================================

library(tidyverse)
library(ggthemes)
library(readxl)
library(googledrive)   # install.packages("googledrive")
library(ggrepel)

options(dplyr.summarise.inform = FALSE)

source("r/my utils.r")

dropboxdir <- file.path(get_dropbox(), "Martin en Pina")

energie <-
  readxl::read_excel(file.path(dropboxdir, "energie en water", "Energie verbruik - Gecombineerd.xlsx"), 
                     sheet="data",
                     range="A1:X500") %>% 
  lowcase() %>% 
  filter(!is.na(datum)) %>% 
  mutate(
    yday  = lubridate::yday(datum),
    decade= 10*floor(jaar/10)
  ) %>% 
  arrange(desc(woning), datum)  
  
  # group_by(woning, year) %>% 
  # mutate(
  #   km   = cumsum(kmafgelegd),
  #   euro = cumsum(eurogetankt),
  #   liters=cumsum(litersgetankt)
  # )

# options(gargle_verbosity = "debug")
# googledrive::drive_auth()
# googledrive::drive_find()

# drive_download(file="Auto verbruik.xlsx", overwrite=TRUE)

# Auto
auto <-
  readxl::read_excel(file.path(dropboxdir, "auto verbruik.xlsx")) %>% 
  lowcase() %>% 
  drop_na(datum) %>% 
  drop_na(kmafgelegd) %>% 
  filter(auto != "Corsa 2021 blauw") %>% 
  dplyr::select(c(1:5,15)) %>% 
  
  # bind google drive document
  bind_rows(
    readxl::read_excel(file.path(dropboxdir, "energie en water", "Auto verbruik.xlsx")) %>% 
    lowcase() %>% 
    drop_na(datum) %>% 
    drop_na(kmafgelegd) %>% 
    dplyr::select(c(1:5,11))    
  ) %>% 
  mutate(
    yday  = lubridate::yday(datum),
    maand = lubridate::month(datum),
    jaar  = lubridate::year(datum),
    decade= 10*floor(jaar/10)
  ) %>% 
  arrange(datum) %>% 
  filter(jaar >= 2003) %>% 
  filter(jaar %notin% 2014:2018)  # missing Corsa Eco data


trein <- 
  read.csv(file.path(dropboxdir,"energie en water", "trein.csv")) %>% 
  filter(jaar >= 2005) %>%
  mutate(trein_euro = ifelse(is.na(trein_euro), 0, -trein_euro)) 


# auto %>% group_by(jaar) %>% summarise(km = sum(kmafgelegd, na.rm=TRUE)) %>% View()

# skimr::skim(t)
# count_not_finite(t)
# count_zeroes(t)
# unique(t$num_laps)

# inspectdf::inspect_num(t) %>% inspectdf::show_plot()
# inspectdf::inspect_imb(t) %>% inspectdf::show_plot()
# inspectdf::inspect_cat(t) %>% inspectdf::show_plot()

# plot energie, auto en trein
bind_rows(
  energie %>% 
    group_by(jaar) %>% 
    summarise(
      stroomproductie = sum(brutoproductiekwh, na.rm=TRUE),
      stroomverbruik  = sum(`nettostroom-verbruikkwhberekend`, na.rm=TRUE),
      gasverbruik     = sum(gasverbruikm3berekend, na.rm=TRUE),
      waterverbruik   = sum(waterverbruikm3berekend, na.rm=TRUE) 
    ) %>%   
    tidyr::pivot_longer(names_to = "variable", 
                        values_to = "data", 
                        stroomproductie:waterverbruik) %>% 
    drop_na(data),
  
  auto %>% 
    group_by(jaar) %>% 
    summarise(
      kmafgelegd = as.integer(sum(kmafgelegd, na.rm=TRUE)),
      benzineverbruik=as.integer(sum(litersgetankt, na.rm=TRUE))
    ) %>%   
    tidyr::pivot_longer(names_to = "variable", 
                        values_to = "data", 
                        kmafgelegd:benzineverbruik) %>% 
    drop_na(data),
  
  trein %>%   
    group_by(jaar) %>% 
    summarise(trein_euro = sum(trein_euro, na.rm=TRUE)) %>% 
    tidyr::pivot_longer(names_to = "variable", 
                        values_to = "data", 
                        trein_euro) %>% 
    drop_na(data)
  ) %>% 
  tidyr::complete(jaar, variable) %>% 
  mutate(variable = factor(variable, 
                           levels=c("stroomproductie", 
                                    "stroomverbruik", 
                                    "gasverbruik", 
                                    "waterverbruik", 
                                    "benzineverbruik",
                                    "kmafgelegd",
                                    "trein_euro"))) %>% 

  ggplot(aes(x=jaar, y=data)) +
  theme_publication() +
  theme(legend.position="none") +
  geom_point() +
  geom_line() +
  labs(title="energie en water") +
  expand_limits(y=0) +
  facet_wrap(~variable, scales="free_y")



# plot energie, auto en trein per maand
tmp <-
  bind_rows(
  energie %>% 
    group_by(jaar, maand) %>% 
    summarise(
      stroomproductie = sum(brutoproductiekwh, na.rm=TRUE),
      stroomverbruik  = sum(`nettostroom-verbruikkwhberekend`, na.rm=TRUE),
      gasverbruik     = sum(gasverbruikm3berekend, na.rm=TRUE),
      waterverbruik   = sum(waterverbruikm3berekend, na.rm=TRUE) 
    ) %>%   
    tidyr::pivot_longer(names_to = "variable", 
                        values_to = "data", 
                        stroomproductie:waterverbruik) %>% 
    drop_na(data),
  
  auto %>% 
    group_by(jaar, maand) %>% 
    summarise(
      kmafgelegd = as.integer(sum(kmafgelegd, na.rm=TRUE)),
      benzineverbruik=as.integer(sum(litersgetankt, na.rm=TRUE))
    ) %>%   
    tidyr::pivot_longer(names_to = "variable", 
                        values_to = "data", 
                        kmafgelegd:benzineverbruik) %>% 
    drop_na(data),
  
  trein %>%   
    group_by(jaar, maand) %>% 
    summarise(trein_euro = sum(trein_euro, na.rm=TRUE)) %>% 
    tidyr::pivot_longer(names_to = "variable", 
                        values_to = "data", 
                        trein_euro) %>% 
    drop_na(data)
  ) %>% 
  filter(jaar >= 2021) %>% 
  # tidyr::complete(jaar, maand, variable) %>% 
  arrange(variable, jaar, maand) %>% 
  group_by(variable, jaar) %>% 
  mutate(cumdata = cumsum(data)) %>% 
  mutate(variable = factor(variable, 
                           levels=c("stroomproductie", 
                                    "stroomverbruik", 
                                    "gasverbruik", 
                                    "waterverbruik", 
                                    "benzineverbruik",
                                    "kmafgelegd",
                                    "trein_euro"))) 

tmp %>%   
  ggplot(aes(x=maand, y=data, group=jaar)) +
  theme_publication() +
  geom_point(aes(colour=as.character(jaar))) +
  geom_line(aes(colour=as.character(jaar), linewidth=as.character(jaar))) +
  labs(title="energie en water", colour="jaar") +
  expand_limits(y=0) +
  scale_x_continuous(breaks=seq(1,12,1)) +
  scale_linewidth_manual( values = c(rep(0.5,3), 1) ) +
  guides(linewidth = "none") +
  facet_wrap(~variable, scales="free_y")

tmp %>%   
  ggplot(aes(x=maand, y=cumdata, group=jaar)) +
  theme_publication() +
  geom_point(aes(colour=as.character(jaar))) +
  geom_line(aes(colour=as.character(jaar), linewidth=as.character(jaar))) +
  labs(title="energie en water", colour="jaar") +
  expand_limits(y=0) +
  scale_x_continuous(breaks=seq(1,12,1)) +
  scale_linewidth_manual( values = c(rep(0.5,3), 1) ) +
  guides(linewidth = "none") +
  facet_wrap(~variable, scales="free_y")


