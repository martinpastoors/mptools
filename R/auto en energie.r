# ====================================================================================
# auto.r
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

# Energie en water
energie <-
  readxl::read_excel(file.path(dropboxdir, "energie en water", "Energie verbruik.xlsx"), 
                     sheet="data",
                     range="A1:AB500") %>% 
  lowcase() %>% 
  filter(!is.na(datum))


# google drive_find()
drive_download(file="Auto verbruik.xlsx", overwrite=TRUE)

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
    readxl::read_excel("Auto verbruik.xlsx") %>% 
    lowcase() %>% 
    drop_na(datum) %>% 
    drop_na(kmafgelegd) %>% 
    dplyr::select(c(1:5,11))    
  ) %>% 
  mutate(
    yday  = lubridate::yday(datum),
    month = lubridate::month(datum),
    year  = lubridate::year(datum),
    decade= 10*floor(year/10)
  ) %>% 
  arrange(datum) %>% 
  
  # quick fix for missing liters or euros
  mutate(
    litersgetankt = ifelse(is.na(litersgetankt), lag(litersgetankt) * kmafgelegd/lag(kmafgelegd), litersgetankt),
    eurogetankt   = ifelse(is.na(eurogetankt), lag(eurogetankt) * kmafgelegd/lag(kmafgelegd), eurogetankt)
  ) %>% 
  
  group_by(year) %>% 
  mutate(
    km   = cumsum(kmafgelegd),
    euro =cumsum(eurogetankt),
    liters=cumsum(litersgetankt)
  ) %>% 
  filter(year >= 2003) %>% 
  filter(year %notin% 2014:2018)  # missing Corsa Eco data

# per month  
auto_per_month <-
  auto %>% 
  group_by(decade, year, month) %>% 
  summarise(
    km = as.integer(sum(kmafgelegd, na.rm=TRUE)),
    euro= as.integer(sum(eurogetankt, na.rm=TRUE)),
    liters=as.integer(sum(litersgetankt, na.rm=TRUE))
  ) %>% 
  group_by(decade, year) %>% 
  mutate(
    km_cumsum = cumsum(km),
    euro_cumsum= cumsum(euro),
    liters_cumsum=cumsum(liters)
  )

auto_per_year <-
  auto %>% 
  group_by(decade, year) %>% 
  summarise(
    km = as.integer(sum(kmafgelegd, na.rm=TRUE)),
    euro= as.integer(sum(eurogetankt, na.rm=TRUE)),
    liters=as.integer(sum(litersgetankt, na.rm=TRUE))
  ) 

auto_per_month <-
  auto %>% 
  group_by(decade, year, month) %>% 
  summarise(
    km = as.integer(sum(kmafgelegd, na.rm=TRUE)),
    euro= as.integer(sum(eurogetankt, na.rm=TRUE)),
    liters=as.integer(sum(litersgetankt, na.rm=TRUE))
  ) %>% 
  group_by(decade, year) %>% 
  mutate(
    km_cumsum = cumsum(km),
    euro_cumsum= cumsum(euro),
    liters_cumsum=cumsum(liters)
  )

# plot auto in april
auto_per_month %>% 
  filter(month==4) %>% 
  ungroup() %>% 
  mutate(avg = mean(km_cumsum[year>= 2010 & year <= 2020])) %>% 
  mutate(km_rel_to_avg = km_cumsum/avg-1) %>%  
  ggplot(aes(x=year, y=km_cumsum)) +
  theme_publication() +
  geom_bar(stat="identity") +
  geom_line(aes(y=avg)) +
  geom_text(aes(label=scales::percent(km_rel_to_avg, accuracy=1)), vjust=0)

t <-
  auto_per_month %>% 
  group_by(decade, year) %>% 
  filter(month == min(month))

tt <-
  auto_per_month %>% 
  group_by(decade, year) %>% 
  filter(month == max(month))

ttt <-
  auto_per_month %>% 
  ungroup() %>% 
  filter(year==max(year)) %>% 
  filter(month == max(month)) %>% 
  dplyr::select(month) %>% 
  left_join(auto_per_month, by="month")

# plot auto km
auto_per_month %>% 
  ggplot(aes(x=month, y=km_cumsum, group=year)) +
  theme_publication() +
  theme(legend.position="none") +
  geom_line(aes(colour=as.character(year))) +
  scale_x_continuous(breaks=seq(1,12,1)) +
  # ggrepel::geom_text_repel(data=t,
  #                          aes(label=year, colour=as.character(year)),
  #                          hjust=1, size=3, segment.size=0.15, segment.linetype="dashed", nudge_x=-2, direction="y",
  #                          min.segment.length = 0) +
  # ggrepel::geom_text_repel(data=tt,
  #                          aes(label=km_cumsum, colour=as.character(year)),
  #                          hjust=0, size=3, segment.size=0.15, segment.linetype="dashed", nudge_x=5, direction="y",
  #                          min.segment.length = 0) +
  geom_point(data=ttt,
             aes(colour=as.character(year))) +
  ggrepel::geom_text_repel(data=ttt,
                           aes(label=paste(km_cumsum, paste0("(", year,")")), colour=as.character(year)),
                           hjust=0, size=3, segment.size=0.15, segment.linetype="dashed", nudge_x=5, direction="y",
                           min.segment.length = 0) +
  labs(title="cumulatieve kilometers per jaar") +
  facet_wrap(~decade)


skimr::skim(t)
count_not_finite(t)
count_zeroes(t)
unique(t$num_laps)

inspectdf::inspect_num(t) %>% inspectdf::show_plot()
inspectdf::inspect_imb(t) %>% inspectdf::show_plot()
inspectdf::inspect_cat(t) %>% inspectdf::show_plot()

names(energie)[grepl("jaar", names(energie))]

# plot auto en energie km
energie %>% 
  left_join(dplyr::select(auto_per_year, jaar=year, litersbenzine=liters),
            by="jaar") %>% 
  dplyr::select(jaar, stroomproductieperjaar, stroomverbruikperjaar, 
                gasverbruikperjaar, waterverbruikperjaar, litersbenzine) %>% 
  tidyr::pivot_longer(names_to = "variable", values_to = "data", 
                      stroomproductieperjaar:litersbenzine) %>% 
  drop_na(data) %>% 
  tidyr::complete(jaar, variable) %>% 
  mutate(variable = factor(variable, 
                           levels=c("stroomproductieperjaar", "stroomverbruikperjaar", 
                                    "gasverbruikperjaar", "waterverbruikperjaar", 
                                    "litersbenzine"))) %>% 
  # View()

  ggplot(aes(x=jaar, y=data)) +
  theme_publication() +
  theme(legend.position="none") +
  geom_point() +
  geom_line() +
  labs(title="energie en water") +
  expand_limits(y=0) +
  facet_wrap(~variable, scales="free_y")


