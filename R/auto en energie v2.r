# ====================================================================================
# auto en energie v2.r
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

# ------------------------------------------------------------------------------
# Energie en water
# ------------------------------------------------------------------------------

energie <-
  readxl::read_excel(file.path(dropboxdir, "energie en water", "Energie verbruik.xlsx"), 
                     sheet="data",
                     range="A1:AB500") %>% 
  lowcase() %>% 
  filter(!is.na(datum)) %>% 
  mutate(
    decade= 10*floor(jaar/10)
  ) 
  
energie_per_month <-
  energie %>% 
  filter(jaar >= 2021) %>% 
  dplyr::select(decade, jaar, maand,
                stroomproductie = stroomproductiepermaand,
                stroomverbruik  = stroomverbruikpermaand,
                gasverbruik     = gasverbruikpermaand,
                waterverbruik   = waterverbruikpermaand) %>% 
  tidyr::pivot_longer(names_to = "variabele", values_to = "data", stroomproductie:waterverbruik) %>% 
  drop_na(data) %>% 
  drop_na(maand) %>% 
  group_by(variabele, decade, jaar, maand) %>% 
  summarise(data = sum(data, na.rm=TRUE) ) %>% 
  group_by(variabele, decade, jaar) %>% 
  mutate(cumdata = cumsum(data))

energie_per_year <-
  bind_rows(
    energie %>%
      filter(jaar == max(jaar)) %>%
      group_by(decade, jaar) %>% 
      summarise(
        stroomproductie = sum(stroomproductiepermaand, na.rm=TRUE),
        stroomverbruik  = sum(stroomverbruikpermaand, na.rm=TRUE),
        gasverbruik     = sum(gasverbruikpermaand, na.rm=TRUE),
        waterverbruik   = sum(waterverbruikpermaand, na.rm=TRUE)),
    energie %>% 
      filter(jaar < max(jaar)) %>% 
      dplyr::select(
        decade, jaar,
        stroomproductie = stroomproductieperjaar,
        stroomverbruik  = stroomverbruikperjaar,
        gasverbruik     = gasverbruikperjaar,
        waterverbruik   = waterverbruikperjaar)
  ) %>% 
  tidyr::pivot_longer(names_to = "variabele", values_to = "data", stroomproductie:waterverbruik) %>%
  ungroup() %>% 
  drop_na(data) %>% 
  mutate(huidigjaar = ifelse(jaar == max(jaar, na.rm=TRUE), TRUE, FALSE))


# ------------------------------------------------------------------------------
# AUTO data
# ------------------------------------------------------------------------------

# google drive_find()
drive_download(file="Auto verbruik.xlsx", overwrite=TRUE)

# Auto algemeen
auto <-
  
  # historic data
  readxl::read_excel(file.path(dropboxdir, "auto verbruik.xlsx")) %>% 
  lowcase() %>% 
  drop_na(datum) %>% 
  drop_na(kmafgelegd) %>% 
  filter(auto != "Corsa 2021 blauw") %>% 
  dplyr::select(c(1:5,15)) %>% 
  
  # bind google drive document
  bind_rows(
    readxl::read_excel("Auto verbruik.xlsx") %>% 
    lowcase() %>% 
    drop_na(datum) %>% 
    drop_na(kmafgelegd) %>% 
    dplyr::select(c(1:5,11))    
  ) %>% 
  mutate(
    maand = lubridate::month(datum),
    jaar  = lubridate::year(datum),
    decade= 10*floor(jaar/10)
  ) %>% 
  arrange(datum) %>% 
  
  # quick fix for missing liters or euros
  mutate(
    litersgetankt = ifelse(is.na(litersgetankt), lag(litersgetankt) * kmafgelegd/lag(kmafgelegd), litersgetankt),
    eurogetankt   = ifelse(is.na(eurogetankt), lag(eurogetankt) * kmafgelegd/lag(kmafgelegd), eurogetankt)
  ) %>% 
  
  group_by(jaar) %>% 
  mutate(
    km   = cumsum(kmafgelegd),
    euro =cumsum(eurogetankt),
    liters=cumsum(litersgetankt)
  ) %>% 
  filter(jaar >= 2003) %>% 
  filter(jaar %notin% 2014:2018)  # missing Corsa Eco data

# auto per month 

auto_per_month <-
  auto %>% 
  group_by(decade, jaar, maand) %>% 
  summarise(
    auto_km    = as.integer(sum(kmafgelegd, na.rm=TRUE)),
    auto_euro  = as.integer(sum(eurogetankt, na.rm=TRUE)),
    auto_liter =as.integer(sum(litersgetankt, na.rm=TRUE))
  ) %>% 
  tidyr::pivot_longer(names_to = "variabele", values_to = "data", auto_km:auto_liter) %>%
  group_by(variabele, decade, jaar) %>% 
  mutate(cumdata = cumsum(data))

auto_per_year <-
  auto %>%
  group_by(decade, jaar) %>% 
  summarise(
    auto_km    = sum(kmafgelegd, na.rm=TRUE),
    auto_euro  = sum(eurogetankt, na.rm=TRUE),
    auto_liter = sum(litersgetankt, na.rm=TRUE)
  ) %>% 
  tidyr::pivot_longer(names_to = "variabele", values_to = "data", auto_km:auto_liter) %>%
  drop_na(data) %>% 
  ungroup() %>% 
  mutate(huidigjaar = ifelse(jaar == max(jaar, na.rm=TRUE), TRUE, FALSE))

# ------------------------------------------------------------------------------
# TREIN data
# ------------------------------------------------------------------------------

trein_per_month <- read.csv(file.path(dropboxdir,"energie en water", "trein.csv")) %>% 
  filter(jaar>= 2005) %>% 
  mutate(variabele="trein_euro") %>% 
  mutate(data=ifelse(is.na(data), 0, -data)) %>% 
  group_by(variabele, jaar) %>% 
  mutate(cumdata=cumsum(data))

trein_per_year <- 
  trein_per_month %>%   
  group_by(variabele, jaar) %>% 
  summarise(data=sum(data))

comb_per_month <- bind_rows(energie_per_month, auto_per_month, trein_per_month)
comb_per_year  <- bind_rows(energie_per_year, auto_per_year, trein_per_year)

# skimr::skim(t)
# count_not_finite(t)
# count_zeroes(t)
# unique(t$num_laps)

# inspectdf::inspect_num(t) %>% inspectdf::show_plot()
# inspectdf::inspect_imb(t) %>% inspectdf::show_plot()
# inspectdf::inspect_cat(t) %>% inspectdf::show_plot()

# plot auto, trein en energie km
comb_per_year %>% 
  drop_na(data) %>% 
  tidyr::complete(jaar, variabele) %>% 

  ggplot(aes(x=jaar, y=data)) +
  theme_publication() +
  theme(legend.position="none") +
  geom_line() +
  geom_point(aes(colour=huidigjaar)) +
  geom_smooth(se=FALSE, linewidth=0.5) +
  labs(title="auto, energie, water") +
  expand_limits(y=0) +
  facet_wrap(~variabele, scales="free_y")

# cumulatief per jaar en maand

comb_per_month %>%
  filter(jaar >= 2019) %>%
  drop_na(jaar) %>% 
  drop_na(data) %>% 
  # tidyr::complete(jaar, maand, variabele) %>%

  ggplot(aes(x=maand, y=cumdata, group=jaar)) +
  theme_publication() +
  # theme(legend.position="none") +
  # geom_point() +
  geom_line(aes(colour=as.character(jaar))) +
  labs(title="auto, energie en water") +
  expand_limits(y=0) +
  facet_wrap(~variabele, scales="free_y")

