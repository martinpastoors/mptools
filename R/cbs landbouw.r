# Eenmalig uitvoeren:
# install.packages("cbsodataR")

options(dplyr.summarise.inform = FALSE)

library(cbsodataR)
library(tidyverse)

source("R/my utils.r")

# Downloaden van tabeloverzicht
toc <- cbs_get_toc()
# View(toc)
toc %>% filter(grepl("landbouw", tolower(Title))) %>% View()
# toc %>% filter(grepl("personenauto", tolower(Title))) %>% View()
# toc %>% filter(grepl("broedvogel", tolower(Title))) %>% View()
# toc %>% filter(grepl("bodem", tolower(Title))) %>% View()

# Downloaden van gehele tabel (kan een halve minuut duren)
tabel    <- "80781ned"
# tabel    <- "84498NED" # broedvogel index

dwnld    <- 
  cbs_get_data(tabel)  %>% 
  cbs_add_label_columns() %>% 
  cbs_add_date_column() 
metadata <- 
  cbs_get_meta(tabel)

save(dwnld, metadata, file="cbs80781ned.RData")
# load(file="cbs80781ned.RData")

# View(metadata$TableInfos)
# View(metadata$DataProperties)
# View(metadata$CategoryGroups)
# View(metadata$RegioS)
# View(metadata$Perioden)

# Hoofdgroep
t1 <- 
  metadata$DataProperties %>% 
  filter(Type == "TopicGroup", is.na(ParentID)) %>% 
  dplyr::select(hoofdID=ID, Hoofdgroep=Title, DescriptionHoofdgroep=Description) 

# Subgroepen
t2 <- 
  metadata$DataProperties %>% 
  filter(Type == "TopicGroup", !is.na(ParentID)) %>% 
  dplyr::select(ID, ParentID, Title, Description) 


topic <-
  metadata$DataProperties %>% 
  filter(Type == "Topic") %>% 
  filter(ID >= 161) %>% 
  dplyr::select(Key, Topic=Title, DescriptionTopic=Description, Unit, ParentID) 

t <-
  topic %>% 
  left_join(t2, by=c("ParentID"="ID" )) %>% 
  left_join(t2, by=c("ParentID.y" = "ID"))

# Samengevoegd
t <-
  t1 %>% 
  
  full_join(t2, 
             by=c("hoofdID"="ParentID")) %>% 
  drop_na(Hoofdgroep) %>% 
  rename("subID"="ID", "subTitle"="Title", "subDescription"="Description") %>% 
  
  full_join(t2, 
             by=c("subID"="ParentID"))  %>% 
  drop_na(Hoofdgroep) %>% 
  rename("subSubID"="ID", "subSubTitle"="Title", "subSubDescription"="Description") %>% 
  
  full_join(t2, 
            by=c("subSubID"="ParentID")) %>% 
  drop_na(Hoofdgroep) %>% 
  
  left_join(topic,
            by=c("subID"="ParentID")) %>% 
  

metadata_df <-
  topic %>% 
  
  left_join(tg, by=c("ParentID"= "ID")) %>% 
  dplyr::select(-ParentID) %>% 
  rename(ParentID=ParentID.y) %>% 
  
  left_join(tg, by=c("ParentID"= "ID")) %>% 
  dplyr::select(-ParentID) %>% 
  rename(ParentID=ParentID.y) %>% 
  
  right_join(t3, by=c("hoofdID"="ParentID")) %>% 
# left_join(t4, by=c("subgroupID"="ParentID"))

# Onderwerpen
t1 <- 
  metadata$DataProperties %>% 
  # filter(Type == "Topic", ID %in% 111:196) %>% 
  filter(Type == "Topic") %>% 
  dplyr::select(ID, Position, ParentID, Key, Topic=Title, DescriptionTopic=Description, Unit) 

# Hoofdgroep
t2 <- 
  metadata$DataProperties %>% 
  filter(Type == "TopicGroup", is.na(Position), is.na(ParentID)) %>% 
  dplyr::select(hoofdID=ID, Hoofdgroep=Title, DescriptionHoofdgroep=Description) 

# Subgroepen
t3 <-
  metadata$DataProperties %>% 
  filter(Type == "TopicGroup", is.na(Position), !is.na(ParentID)) %>% 
  filter(!ParentID %in% .$ID) %>% 
  dplyr::select(subgroupID=ID, ParentID, Subgroep=Title, DescriptionSubgroep=Description) 

# SubSubgroepen
t4 <-
  metadata$DataProperties %>% 
  filter(Type == "TopicGroup", is.na(Position), !is.na(ParentID)) %>% 
  filter(ParentID %in% .$ID) %>% 
  dplyr::select(subsubgroupID=ID, ParentID, SubSubgroep=Title, DescriptionSubSubgroep=Description) 

metadata_df <-
  t2 %>% 
  left_join(t3, by=c("hoofdID"="ParentID")) %>% 
  left_join(t4, by=c("subgroupID"="ParentID")) %>%  
  left_join(t1, by=c("subsubgroupID" = "ParentID"))


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
  
  # TEMP
  drop_na(hoofdID) %>% 
  
  # add jaar
  mutate(jaar = as.integer(as.character(Perioden_label))) %>% 
  
  group_by(naam, variabele, jaar) %>% 
  separate(variabele, into=c("var", "varnr"), sep="_") %>% 
  
  lowcase()



# alle dieren
data %>% 
  filter(
    tolower(subgroep) == "aantal dieren",
    grepl("totaal", tolower(var) )) %>% 
  mutate(var = gsub("Totaal","", var)) %>% 
  # View()

  ggplot(aes(x=jaar, y=data)) +
  theme_bw() +
  geom_bar(aes(fill=naam), stat="identity") +
  labs(y="", x="", title="Aantal dieren") +
  facet_wrap(~var, scales="free_y")

#bedrijven
data %>% 
  filter(
    tolower(subgroep) == "aantal bedrijven",
    grepl("totaal", tolower(var) )) %>% 
  mutate(var = gsub("Totaal","", var)) %>% 
  # View()
  
  ggplot(aes(x=jaar, y=data)) +
  theme_bw() +
  geom_bar(aes(fill=naam), stat="identity") +
  labs(y="", x="", title="Aantal bedrijven") +
  facet_wrap(~var, scales="free_y")

# aantal dieren per bedrijf
data %>% 
  filter(
    tolower(subgroep) %in% c("aantal bedrijven", "aantal dieren"),
    grepl("totaal", tolower(var) )) %>% 
  mutate(var = gsub("Totaal","", var)) %>% 
  mutate(subgroep = gsub("Aantal ","", subgroep)) %>% 
  
  group_by(var, jaar, subgroep) %>%
  summarise(data = sum(data, na.rm=TRUE)) %>% 
  ungroup() %>% 

  pivot_wider(names_from = subgroep, values_from = data) %>% 
  mutate(dieren_bedrijf = dieren/bedrijven) %>% 
  # View()
  
  ggplot(aes(x=jaar, y=dieren_bedrijf)) +
  theme_bw() +
  geom_bar(stat="identity") +
  labs(y="", x="", title="Dieren per bedrijf") +
  facet_wrap(~var, scales="free_y")

data %>% ungroup() %>% filter(tolower(subgroep)=="aantal dieren") %>% distinct(topic) %>% View()

data %>% 
  filter(
    tolower(hoofdgroep) == "aantal dieren",
    grepl("totaal", tolower(var) )) %>% 
  mutate(var2 = paste(subgroep, var,Unit,sep= "_")) %>% 
  group_by(jaar, var) %>% 
  summarise(data = sum(data, na.rm=TRUE)) %>% 
  mutate(var = gsub("Totaal","",var)) %>% 
  filter(jaar %in% c(2000, 2021)) %>% 
  mutate(jaar= paste0("j", jaar)) %>% 
  pivot_wider(names_from = jaar, values_from = data) %>% 
  mutate(change = j2021/j2000-1) %>% 
  mutate(var = forcats::fct_reorder(factor(var), change)) %>% 
  # View()
  
  ggplot(aes(x=var, y=change)) +
  theme_bw() +
  geom_bar(stat="identity") 

data %>% 
  filter(
    tolower(subgroep) == "aantal bedrijven",
    grepl("hokdieren|graasdieren", tolower(Title) )) %>% 
  group_by(jaar, Title) %>% 
  summarise(data = sum(data, na.rm=TRUE)) %>% 
  # mutate(var = gsub("\\, totaal","",var)) %>% 
  filter(jaar %in% c(2000, 2021)) %>% 
  mutate(jaar= paste0("j", jaar)) %>% 
  pivot_wider(names_from = jaar, values_from = data) %>% 
  mutate(change = j2021/j2000-1) %>% 
  mutate(var = forcats::fct_reorder(factor(Title), change)) %>% 
  # View()
  
  ggplot(aes(x=Title, y=change)) +
  theme_bw() +
  geom_bar(stat="identity") 

t1 <-
  data %>% 
  filter(
    tolower(hoofdgroep) == "aantal dieren",
    grepl("totaal", tolower(var) )) %>% 
  mutate(var2 = paste(subgroep, var,Unit,sep= "_")) %>% 
  group_by(jaar, var) %>% 
  summarise(data = sum(data, na.rm=TRUE)) %>% 
  mutate(var = gsub("Totaal","",var)) %>% 
  filter(jaar %in% c(2000, 2021)) %>% 
  mutate(jaar= paste0("j", jaar)) %>% 
  pivot_wider(names_from = jaar, values_from = data) %>% 
  mutate(change = j2021/j2000-1) %>% 
  mutate(var = ifelse(grepl("paarden", tolower(var)), "Paarden", var)) %>% 
  mutate(var = forcats::fct_reorder(factor(var), change)) %>% 
  mutate(type = "aantal dieren")
  # View()
  
t2 <-
  data %>% 
  filter(
    tolower(subgroep) == "aantal bedrijven",
    grepl("hokdieren|graasdieren", tolower(Title) )) %>% 
  group_by(jaar, Title) %>% 
  summarise(data = sum(data, na.rm=TRUE)) %>% 
  # mutate(var = gsub("\\, totaal","",var)) %>% 
  filter(jaar %in% c(2000, 2021)) %>% 
  mutate(jaar= paste0("j", jaar)) %>% 
  pivot_wider(names_from = jaar, values_from = data) %>% 
  mutate(change = j2021/j2000-1) %>% 
  mutate(Title = ifelse(grepl("graasdieren", tolower(Title)), "Graasdieren", Title)) %>% 
  mutate(Title = ifelse(grepl("hokdieren", tolower(Title)), "Hokdieren", Title)) %>% 
  mutate(var = forcats::fct_reorder(factor(Title), change)) %>% 
  mutate(type = "aantal bedrijven")

bind_rows(t1, t2) %>% 
  mutate(label = paste(var, scales::percent(change, accuracy=1))) %>% 
  ggplot(aes(x=var, y=change)) +
  theme_bw() +
  theme(axis.text.x = element_blank()) +
  theme(axis.ticks.x = element_blank()) +
  geom_hline(yintercept=0) +
  geom_bar(stat="identity") +
  labs(title="percentuele verandering in 2021 t.o.v. 2000", x="", y="rel. verandering t.o.v. 2000") +
  scale_y_continuous(labels=scales::percent) +
  geom_text(aes(label=label, y = change + 0.05 * sign(change))) +
  facet_grid(.~type, scales="free_x", space="free_x")


save(data, dwnld, metadata, metadata_df, file="csb_80781ned.RData")

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

data <-
  dwnld %>% 
  mutate(jaar = as.numeric(as.character(Perioden_label))) 

t <-
  data %>% 
  dplyr::group_by(Broedvogels_label) %>% 
  filter((Broedvogelindex_1==100 & row_number() == 1) |
         (Broedvogelindex_1==100 & row_number() == n())) 

data %>% 
  ggplot(aes(x=jaar, y=Broedvogelindex_1)) +
  theme_bw() +
  geom_hline(yintercept=100, colour="red") +
  geom_point(data=t, colour="red") +
  geom_line() +
  expand_limits(y=0)+
  facet_wrap(~Broedvogels_label, scales="free_y")

dwnld %>% filter(grepl("zilverreiger", tolower(Broedvogels_label))) %>% View()
