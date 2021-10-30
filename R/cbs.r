# Eenmalig uitvoeren:
# install.packages("cbsodataR")


library(cbsodataR)
library(tidyverse)

# Downloaden van tabeloverzicht
toc <- cbs_get_toc()
# View(toc)
# toc %>% filter(grepl("landbouw", tolower(Title))) %>% View()
toc %>% filter(grepl("personenauto", tolower(Title))) %>% View()

# Downloaden van gehele tabel (kan een halve minuut duren)
tabel    <- "80781ned"
dwnld    <- 
  cbs_get_data(tabel)  %>% 
  cbs_add_label_columns() %>% 
  cbs_add_date_column() 
metadata <- 
  cbs_get_meta(tabel)

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
  # left_join(metadata$RegioS, by=c("RegioS"="Key")) %>% 
  rename(naam = RegioS_label) %>% 
  
  # filter op namenn
  filter(naam %in% my_names) %>% 
  
  # Make long
  ungroup() %>% 
  pivot_longer(names_to="variabele", values_to="data", 7:155) %>% 
  # dplyr::select(-Description) %>% 
  
  # add beschrijving van variabelen
  left_join(metadata_df, by=c("variabele"="Key")) %>% 
  
  # add jaar
  mutate(jaar = as.integer(as.character(Perioden_label))) %>% 
  
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


# Downloaden van gehele tabel (kan een halve minuut duren)
tabel    <- "80428ned"
dwnld    <- 
  cbs_get_data(tabel)  %>% 
  cbs_add_label_columns() 
metadata <- 
  cbs_get_meta(tabel)

# View(metadata$TableInfos)
# View(metadata$DataProperties)
# View(metadata$CategoryGroups)
# View(metadata$RegioS)
# View(metadata$Perioden)
# names(dwnld)


extra <- 
  readxl::read_xlsx("C:/Users/marti/Dropbox/CBS 2001 motorvoertuigenpark.xlsx") %>% 
  pivot_longer(names_to = "Brandstofsoort_label", values_to = "value", totaal:lpg) %>% 
  filter(Brandstofsoort_label != "totaal") %>% 
  mutate(variable = "autos") %>% 
  filter(jaar < 2001)
  
data <-
  dwnld %>% 
  filter(LeeftijdVoertuig_label == "Totaal leeftijd voertuig") %>% 
  filter(Eigendomssituatie_label == "Totaal") %>% 
  filter(Brandstofsoort_label != "Totaal") %>% 
  mutate(Brandstofsoort_label = tolower(Brandstofsoort_label)) %>% 
  mutate(jaar = as.integer(as.character(Perioden_label))) %>% 
  group_by(jaar, Brandstofsoort_label) %>% 
  summarise(
    km = sum(KilometersDoorNederlandseVoertuigen_2, na.rm=TRUE),
    kminnederland = sum(KilometersInNederland_5, na.rm=TRUE),
    autos = sum(NederlandsePersonenautoSInGebruik_10, na.rm=TRUE)) %>% 
  pivot_longer(names_to = "variable", values_to = "value", km:autos) %>% 
  mutate(Brandstofsoort_label = tolower(Brandstofsoort_label)) %>% 
  mutate(Brandstofsoort_label = ifelse(grepl("benzine", Brandstofsoort_label), "benzine", Brandstofsoort_label)) %>% 
  filter(value > 0) %>% 
  bind_rows(extra)

  # bind_rows(
  #   data.frame(
  #     variable = rep("autos",3),
  #     jaar = c(1986, 1986, 1986),
  #     Brandstofsoort_label = c("Benzine/overige", "Diesel","LPG"),
  #     value = c(3741000, 380000, 520000)
  #   )) %>% 
  # bind_rows(
  #   data.frame(
  #     variable = rep("autos",3),
  #     jaar = c(1995, 1995, 1995),
  #     Brandstofsoort_label = c("Benzine/overige", "Diesel","LPG"),
  #     value = c(4639000, 614000, 380000)
  #   )) 
  

data %>% 
  ggplot(aes(x=jaar, y=value)) +
  theme_bw() +
  geom_bar(aes(fill=Brandstofsoort_label), stat="identity") +
  facet_wrap(~variable, scales="free_y")