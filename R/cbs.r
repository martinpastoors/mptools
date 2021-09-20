# Eenmalig uitvoeren:
# install.packages("cbsodataR")

library(cbsodataR)
library(tidyverse)

# Downloaden van tabeloverzicht
toc <- cbs_get_toc()
# View(toc)
# toc %>% filter(grepl("landbouw", tolower(Title))) %>% View()

# Downloaden van gehele tabel (kan een halve minuut duren)
tabel    <- "80781ned"
dwnld    <- cbs_get_data(tabel) 
metadata <- cbs_get_meta(tabel)

# View(metadata$TableInfos)
# View(metadata$DataProperties)
# View(metadata$CategoryGroups)
# View(metadata$RegioS)
# View(metadata$Perioden)

# Hoofdgroepen
t1 <- 
  metadata$DataProperties %>% 
  dplyr::select(odata.type, ID, Position, ParentID, Title, Description) %>% 
  filter(odata.type == "Cbs.OData.TopicGroup") %>% 
  rename(hoofdgroep=Title, hoofdgroep2 = Description) %>% 
  dplyr::select(-Position, -odata.type)

# Deelgroepen
t2 <- 
  metadata$DataProperties %>% 
  dplyr::select(Key, ID, Position, ParentID, Title, Description, Unit) %>% 
  filter(!is.na(Position) & !is.na(ID)) %>% 
  filter(!Key %in% c("RegioS", "Perioden") ) %>% 
  # rename(subgroep=Title, subgroep2 = Description) %>% 
  dplyr::select(-ID, -Position)

# Metadata uitgeklapt
metadata_df <-
  t2 %>% 
  left_join(t1, by=c("ParentID"="ID")) %>% 
  dplyr::select(-ParentID) %>% 
  rename(
    ParentID = ParentID.y,
    subgroep = hoofdgroep,
    subgroep_desc = hoofdgroep2) %>% 
  left_join(dplyr::select(t1,
                          -ParentID), 
            by=c("ParentID"="ID")) %>% 
  rename(hoofdgroep_desc = hoofdgroep2)
  
my_names <- c("Westerkwartier", "Zuidhorn", "Leek", "Marum", "Grootegast")
# my_names <- c("Midden-Drenthe") 


  
# dataset samenstellen
data <-
  dwnld %>% 
  
  # add naam van gemeente
  left_join(metadata$RegioS, by=c("RegioS"="Key")) %>% 
  rename(naam = Title) %>% 
  
  # filter op namenn
  filter(naam %in% my_names) %>% 
  
  # Make long
  ungroup() %>% 
  pivot_longer(names_to="variabele", values_to="data", 3:151) %>% 
  dplyr::select(-Description) %>% 
  
  # add beschrijving van variabelen
  left_join(metadata_df, by=c("variabele"="Key")) %>% 
  
  # add jaar
  mutate(jaar = as.integer(substr(Perioden, 1, 4))) %>% 
  
  group_by(naam, variabele, jaar) %>% 
  separate(variabele, into=c("var", "varnr"), sep="_")

# TEMP
# unique(data$hoofdgroep)
# data %>% filter(hoofdgroep=="Rundvee") %>% View()
# dwnld %>% 
#   left_join(metadata$RegioS, by=c("RegioS"="Key")) %>% 
#   rename(naam = Title) %>% 
#   filter(naam %in% my_names) %>%
#   View()
# data %>% filter(varnr=="90") %>% View()

# alle dieren
data %>% 
  filter(
    tolower(hoofdgroep) == "aantal dieren",
    grepl("totaal", tolower(var) )) %>% 
  mutate(var2 = paste(subgroep, var,Unit,sep= "_")) %>% 
  # View()

  ggplot(aes(x=jaar, y=data)) +
  theme_bw() +
  geom_bar(aes(fill=naam), stat="identity") +
  facet_wrap(~var2, scales="free_y")

# alle dieren (totalen)
data %>% 
  filter(
    tolower(hoofdgroep) == "aantal dieren",
    grepl("totaal", tolower(var) )) %>% 
  mutate(var2 = paste(subgroep, var,Unit,sep= "_")) %>% 
  # View()
  
  ggplot(aes(x=jaar, y=data)) +
  theme_bw() +
  geom_bar(aes(fill=subgroep), stat="identity") +
  facet_wrap(~var, scales="free_y")

# rundvee
data %>% 
  filter(grepl("rundvee", tolower(var))) %>% 
  mutate(var2 = paste(var,Unit,sep= "_")) %>% 
  
  ggplot(aes(x=jaar, y=data)) +
  theme_bw() +
  geom_bar(aes(fill=naam), stat="identity") +
  facet_grid(hoofdgroep~var2, scales="free_y")

# rundvee per bedrijf
data %>% 
  filter(grepl("rundvee", tolower(var))) %>% 
  mutate(var2 = paste(var,Unit,sep= "_")) %>% 
  group_by(hoofdgroep, var2, jaar) %>% 
  summarise(data = sum(data, na.rm=TRUE)) %>% 
  group_by(var2, jaar) %>% 
  mutate(data2 = lag(data)) %>% 
  mutate(dierenperbedrijf = data/data2) %>% 
  filter(!is.na(data2)) %>% 

  ggplot(aes(x=jaar, y=dierenperbedrijf)) +
  theme_bw() +
  geom_line() +
  geom_point() +
  expand_limits(y=0) +
  facet_wrap(~var2, scales="free_y")

# varkens
data %>% 
  filter(grepl("varken", tolower(var))) %>% 
  filter(grepl("totaal", tolower(var))) %>% 
  mutate(var2 = paste(var,Unit,sep= "_")) %>% 
  
  ggplot(aes(x=jaar, y=data)) +
  theme_bw() +
  geom_bar(aes(fill=naam), stat="identity") +
  facet_grid(hoofdgroep~var2, scales="free_y")

# cultuurgrond
data %>% 
  filter(grepl("cultuurgrond", tolower(var))) %>% 
  filter(Unit=="are") %>% 
  mutate(var2 = paste(var,Unit,sep= "_")) %>% 
  
  ggplot(aes(x=jaar, y=data)) +
  theme_bw() +
  geom_bar(aes(fill=naam), stat="identity") +
  facet_grid(hoofdgroep~var2, scales="free_y")

# bloembollen
data %>% 
  filter(grepl("bloembollenenknollen", tolower(var))) %>% 
  filter(Unit=="are") %>% 
  mutate(var2 = paste(var,Unit,sep= "_")) %>% 
  
  ggplot(aes(x=jaar, y=data/100)) +
  theme_bw() +
  geom_bar(aes(fill=naam), stat="identity") +
  facet_grid(hoofdgroep~var2, scales="free_y")

# grasland
data %>% 
  filter(grepl("grasland", tolower(var))) %>% 
  filter(Unit=="are") %>% 
  mutate(var2 = paste(var,Unit,sep= "_")) %>% 
  
  ggplot(aes(x=jaar, y=data)) +
  theme_bw() +
  geom_bar(aes(fill=naam), stat="identity") +
  facet_grid(hoofdgroep~var2, scales="free_y")

# alle dieren
data %>% 
  filter(grepl("aantal dieren", tolower(hoofdgroep))) %>% 
  filter(grepl("totaal", tolower(var))) %>% 
  mutate(var2 = paste(var,Unit,sep= "_")) %>% 
  
  ggplot(aes(x=jaar, y=data)) +
  theme_bw() +
  geom_bar(aes(fill=naam), stat="identity") +
  facet_wrap(hoofdgroep~var2, scales="free_y")


# WIJKEN EN BUURTEN

# Downloaden van gehele tabel (kan een halve minuut duren)
tabel <- "84799NED"
buurt2020     <- cbs_get_data(tabel) 
buurt2020meta <- cbs_get_meta(tabel)

View(buurt2020meta$DataProperties)

