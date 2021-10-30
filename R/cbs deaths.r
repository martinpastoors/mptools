# cbs deaths

# Load needed packages
library(tidyverse)
library(cbsodataR)

tables_en <- cbs_get_toc(Language="en")
tables_en %>% filter(grepl("death", tolower(Title))) %>% View()

tables_nl <- cbs_get_toc(Language="nl")
tables_nl %>% filter(grepl("dood", tolower(Title))) %>% View()

cbs7233ENG <-
  cbsodataR::cbs_get_data("7233ENG") %>%
  cbs_add_label_columns() %>% 
  mutate(year = factor(substr(Periods, start = 1, stop = 4))) %>% 
  mutate(Age_label = as.character(Age_label)) %>% 
  filter(Sex == "T001038") %>% 
  filter(Age_label != "Total") %>% 
  filter(CausesOfDeath_label == "A00-Y99 Total") %>% 
  mutate(Sex_label = factor(Sex_label)) %>% 
  mutate(Age_label = ifelse(Age_label %in% c("0 year","1 to 4 years", "5 to 9 years",
                                             "10 to 14 years","15 to 19 years"), 
                            "0-19 years", Age_label)) %>% 
  group_by(Sex_label, Age_label, year) %>% 
  summarise(deaths = sum(Deaths_1, na.rm=TRUE)) 

# save(cbs7233ENG, file="cbs7233ENG.RData")
# load(file="cbs7233ENG.RData")

cbs7233ENG %>% 
  ggplot(aes(x=year, y=deaths)) +
  theme_bw() +
  geom_bar(aes(fill=Age_label), stat="identity") +
  facet_wrap(~Age_label, scales="free")


# Import data from CBS using the table identifier
db_cbs <- 
  cbsodataR::cbs_get_data("70895ENG") %>%
  cbs_add_label_columns() %>% 
  mutate(
    year = factor(substr(Periods, start = 1, stop = 4)), # extract year
    wk  = factor(as.numeric(substr(Periods, start = 7, stop = 8)))) %>% #extract weeks
  filter(
    Sex == "T001038", # only keep data for both sexes
    # Age31December == "10000",
    wk %in% 1:52) %>% # only keep weeks 1 through 52
  select(Sex_label, Age31December_label, Deaths_1, year, wk) %>% 
  mutate(Sex_label = factor(Sex_label)) %>% 
  group_by(Sex_label, Age31December_label, year) %>% 
  arrange(Sex_label,Age31December_label,  year, wk) %>% 
  mutate(cumdeath = cumsum(Deaths_1)) %>% 
  mutate(linesize = ifelse(year %in% c("2020","2021"), "thick", "narrow"))

db_cbs %>% 
  filter(year %in% 2016:2021) %>% 
  filter(Age31December_label != "Total") %>% 
  
  ggplot(aes(x=factor(wk), y=cumdeath, group=year)) +
  # plot lines
  geom_line(aes(colour=year, size=linesize)) +
  scale_size_manual(values=c("thick"=1.5, "narrow"=0.5)) +
  labs(title = "Deaths per week",
       x = "Week number",
       y = "Deaths",
       col = "Years") +
  # place legend and customise text size to make it all fit
  theme(legend.position = "bottom",
        legend.text = element_text(size = 7),
        text = element_text(size = 7)) + 
  guides(col = guide_legend(nrow = 1)) +
  # facet_wrap(~ Age31December_label, scales = "free", nrow=1)
  facet_wrap(~ Age31December_label, nrow=1)

db_cbs %>% 
  filter(year %in% 2010:2021) %>% 
  filter(Age31December_label != "Total") %>% 
  
  ggplot(aes(x=factor(wk), y=Deaths_1, group=year, size=linesize)) +
  # plot lines
  geom_line(aes(colour=year, size =linesize)) +
  scale_size_manual(values=c("thick"=1.5, "narrow"=0.5)) +
  labs(title = "Deaths per week",
       x = "Week number",
       y = "Deaths",
       col = "Years") +
  # place legend and customise text size to make it all fit
  theme(legend.position = "bottom",
        legend.text = element_text(size = 7),
        text = element_text(size = 7)) + 
  guides(col = guide_legend(nrow = 1)) +
  # facet_wrap(~ Age31December_label, scales = "free", nrow=1)
  facet_wrap(~ Age31December_label, nrow=1)

glimpse(db_cbs)
