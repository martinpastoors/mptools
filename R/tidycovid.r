# devtools::install_github("joachim-gassen/tidycovid19")

library(tidyverse)
library(tidycovid19)
library(zoo)
library(readxl)
library(patchwork)

df <- 
  tidycovid19::download_merged_data(cached = TRUE, silent = TRUE) %>% 
  mutate(
    new_cases_per_million = 100000*(confirmed - lag(confirmed))/population,
    new_cases_smoothed_per_million = zoo::rollmean(new_cases_per_million, 7, na.pad=TRUE, align="right")
  )
  
download.file("https://covid.ourworldindata.org/data/owid-covid-data.csv", 
              "C:/TEMP/owid-covid-data.csv", mode = "w", cacheOK = TRUE)

owid <- 
  read.csv("C:/TEMP/owid-covid-data.csv") %>% 
  mutate(date = lubridate::ymd(date))

# OxCGRT <- 
#   read.csv("https://github.com/OxCGRT/covid-policy-tracker/blob/master/data/OxCGRT_withnotes_2021.csv")

data_ends <- 
  owid %>% 
  filter(iso_code %in% c("DEU","NLD", "BEL", "FRA", "ESP","DNK")) %>% 
  group_by(iso_code) %>% 
  filter(date == max(date, na.rm=TRUE)) 

owid %>%
  filter(iso_code %in% c("DEU","NLD", "BEL", "FRA", "ESP","DNK")) %>% 
  filter(date >= lubridate::ymd("2021-01-01")) %>% 
  ggplot(aes(x = date)) +
  theme_bw() +
  # geom_bar(aes(y = new_cases_per_million), stat = "identity", fill = "lightblue") +
  geom_line(aes(y =new_cases_smoothed_per_million), color ="red") +
  geom_step(aes(y=stringency_index)) +
  ggrepel::geom_text_repel(data=data_ends, aes(y=stringency_index, label=stringency_index)) +
  
  geom_point(aes(y=people_fully_vaccinated_per_hundred),
             colour="blue") +
  scale_y_continuous(
    name = "First Axis", limits=c(0,1000), 
    sec.axis = sec_axis(trans=~./1000, name="Second Axis")
  ) +
  expand_limits(y=0) +
  facet_wrap(~iso_code)

owid %>%
  filter(iso_code %in% c("DEU","NLD", "BEL", "FRA", "ESP","DNK")) %>% 
  filter(date >= lubridate::ymd("2021-01-01")) %>% 
  dplyr::select(iso_code, date, 
                new_cases_smoothed_per_million, stringency_index, people_fully_vaccinated_per_hundred) %>% 
  pivot_longer(names_to = "variable", values_to = "value", 
               new_cases_smoothed_per_million:people_fully_vaccinated_per_hundred) %>% 
  filter(value >= 0) %>% 

  ggplot(aes(x = date)) +
  theme_bw() +
  theme(legend.position = "none") +
  geom_line(aes(y =value, colour=variable)) +
  # geom_point(aes(y=value, colour=variable), size=0.3) +
  expand_limits(y=0) +
  facet_grid(variable~iso_code, scales="free_y")

t <-
  owid %>%
  filter(iso_code %in% c("DEU","NLD", "BEL", "FRA", "ESP","DNK")) %>% 
  filter(date >= lubridate::ymd("2021-01-01")) %>% 
  dplyr::select(iso_code, date, 
                new_cases_smoothed_per_million, stringency_index, people_fully_vaccinated_per_hundred) %>% 
  pivot_longer(names_to = "variable", values_to = "value", 
               stringency_index:people_fully_vaccinated_per_hundred) %>% 
  filter(value >= 0, new_cases_smoothed_per_million > 0) 

p1 <-
  t %>% 
  ggplot(aes(x = date)) +
  theme_bw() +
  theme(legend.position = "none") +
  theme(axis.text.x = element_blank()) +
  geom_line(aes(y =new_cases_smoothed_per_million), colour="black") +
  ggrepel::geom_label_repel(data=t %>% group_by(iso_code) %>% filter(date==max(date, na.rm=TRUE)), 
                            aes(y=new_cases_smoothed_per_million, label=round(new_cases_smoothed_per_million))) +
  expand_limits(y=0) +
  labs(y="", x="", title = "new_cases_smoothed_per_million") + 
  facet_wrap(~iso_code, nrow=1)

p2 <-
  t %>% 
  ggplot(aes(x = date)) +
  theme_bw() +
  theme(legend.position = "bottom") +
  theme(strip.background = element_blank(), strip.text = element_blank()) + 
  geom_line(aes(y =value, colour=variable)) +
  ggrepel::geom_label_repel(data=t %>% group_by(iso_code, variable) %>% filter(date==max(date, na.rm=TRUE)), 
                            aes(y=value, label=round(value), colour=variable)) +
  expand_limits(y=0) +
  labs(x="", y="index") +
  facet_wrap(~iso_code, nrow=1)

p1 / p2

owid %>%
  filter(iso_code %in% c("DEU","NLD", "BEL", "FRA")) %>% 
  filter(date >= lubridate::ymd("2021-01-01")) %>% 
  ggplot(aes(x = date)) +
  theme_bw() +
  # geom_bar(aes(y = new_cases_per_million), stat = "identity", fill = "lightblue") +
  geom_line(aes(y =new_cases_smoothed_per_million, colour=iso_code)) +
  geom_step(aes(y=10*stringency_index, colour=iso_code)) 

owid %>%
  filter(iso_code %in% c("DEU","NLD", "BEL")) %>% 
  filter(date >= lubridate::ymd("2021-01-01")) %>% 
  group_by(iso_code) %>% 
  fill(everything()) %>% 
  dplyr::select(date, iso_code, new_cases_smoothed_per_million, 
                stringency_index, people_fully_vaccinated_per_hundred) %>% 
  pivot_longer(names_to = "variable", values_to = "value", 
               new_cases_smoothed_per_million:people_fully_vaccinated_per_hundred) %>% 
  ggplot(aes(x = date)) +
  theme_bw() +
  geom_line(aes(y =value, colour=iso_code)) +
  expand_limits(y=0)+
  facet_wrap(~variable, ncol=1, scales="free_y") 



owid_nld <-
  owid %>% 
  mutate(people_fully_vaccinated_per_hundred=ifelse(is.na(people_fully_vaccinated_per_hundred), 
                                                    lag(people_fully_vaccinated_per_hundred),
                                                    people_fully_vaccinated_per_hundred))

filter(owid, iso_code=="NLD") %>% 
  dplyr::select(date, people_fully_vaccinated_per_hundred) %>% 
  drop_na() %>% 
  ggplot(aes(date, people_fully_vaccinated_per_hundred)) +
  theme_bw() +
  geom_line()


glimpse(owid)

df %>%
  filter(iso3c == "NLD") %>%
  ggplot(aes(x = date)) +
  geom_bar(aes(y = new_cases_per_million), stat = "identity", fill = "lightblue") +
  geom_line(aes(y = ave_new_cases), color ="red") +
  geom_step(data=filter(owid, iso_code=="NLD"),
  aes(y=1*stringency_index)) +
  # geom_path(data=owid_nld %>% drop_na(),
  #           aes(x=date, y=100*people_fully_vaccinated_per_hundred),
  #           colour="blue",inherit.aes=FALSE) +
  theme_minimal()

owid_nld %>%
  mutate(
    ave_new_cases = zoo::rollmean(new_cases_per_million, 7, na.pad=TRUE, align="right")
  ) %>% 
  
  ggplot(aes(x = date)) +
  geom_bar(aes(y = new_cases_per_million), stat = "identity", fill = "lightblue") +
  geom_line(aes(y = ave_new_cases), color ="red") +
  geom_step(data=filter(owid, iso_code=="NLD"), aes(y=10*stringency_index)) +
  geom_point(aes(y=10*people_fully_vaccinated_per_hundred),
            colour="blue") +
  theme_minimal()

owid %>%
  filter(iso_code%in% c("NLD","DEU","BEL","DNK")) %>% 
  filter(date >= lubridate::ymd("2021-07-01")) %>% 
  ggplot(aes(x = date)) +
  geom_point(aes(y = new_cases_per_million, colour=iso_code, group=iso_code)) +
  geom_line(aes(y = new_cases_smoothed_per_million, colour=iso_code, group=iso_code)) +
  theme_minimal()

