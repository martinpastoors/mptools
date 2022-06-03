# Eenmalig uitvoeren:
# install.packages("cbsodataR")

options(dplyr.summarise.inform = FALSE)

library(cbsodataR)
library(tidyverse)

source("R/my utils.r")

# Downloaden van tabeloverzicht
toc <- cbs_get_toc()
toc %>% filter(grepl("bevolking", tolower(Title))) %>% View()

# Downloaden van gehele tabel (kan een halve minuut duren)
tabel    <- "03759ned"

dwnld    <- 
  cbs_get_data(tabel)  %>% 
  cbs_add_label_columns()  
  # cbs_add_date_column() 

metadata <- 
  cbs_get_meta(tabel)

save(dwnld, metadata, file=paste0("cbs",tabel,".RData"))
load(file=paste0("cbs",tabel,".RData"))
# load(file=paste0("cbs","85217NED",".RData"))
load(file=paste0("cbs","03759ned",".RData"))


# View(metadata$TableInfos)
# View(metadata$DataProperties)
# View(metadata$CategoryGroups)
# View(metadata$RegioS)
# View(metadata$Perioden)
# unique(metadata$DataProperties$Type)


# Hoofdgroep
t1 <- 
  metadata$DataProperties %>% 
  filter(Type == "TopicGroup", is.na(Position), is.na(ParentID)) %>% 
  dplyr::select(hoofdID=ID, Hoofdgroep=Title, DescriptionHoofdgroep=Description) 

# Groep
t2 <- 
  metadata$DataProperties %>% 
  filter(Type == "Topic") %>% 
  dplyr::select(ID, Position, ParentID, Key, Topic=Title, DescriptionTopic=Description, Unit) 

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
  t1 %>% 
  right_join(t2, by=c("hoofdID"= "ParentID")) 
  # left_join(t3, by=c("hoofdID"="ParentID")) %>% 
  # left_join(t4, by=c("subgroupID"="ParentID")) %>%  


my_names <- c("Westerkwartier", "Zuidhorn", "Leek", "Marum", "Grootegast")
# my_names <- c("Midden-Drenthe") 


  
# dataset samenstellen
data <-
  dwnld %>% 
  
  # add naam van gemeente
  rename(naam = Gemeentenaam_1) %>% 
  mutate(naam = gsub("\\s+$","", naam)) %>% 
  
  # filter op namenn
  filter(naam %in% my_names) %>% 
  filter(grepl("Gemeente", SoortRegio_2)) %>% 
  
  # Make long
  ungroup() %>% 
  pivot_longer(names_to="variabele", values_to="data", 6:ncol(dwnld)) %>% 
  # dplyr::select(-Description) %>% 
  
  # add beschrijving van variabelen
  left_join(metadata_df, by=c("variabele"="Key")) %>% 
  
  # TEMP
  drop_na(hoofdID) %>% 
  
  # add jaar
  # mutate(jaar = as.integer(as.character(Perioden_label))) %>% 
  mutate(jaar = 2017) %>% 
  
  group_by(naam, variabele, jaar) %>% 
  separate(variabele, into=c("var", "varnr"), sep="_") %>% 
  
  lowcase()

install.packages("treemapify")
library(treemapify)

data %>% 
  filter(grepl("totaal", tolower(var))) %>% 
  filter(data > 0) %>% 
  group_by(topic) %>% 
  summarise(data=sum(data, na.rm=TRUE)) %>% 
  ungroup() %>% 
  mutate(topic = gsub("Totaal |Totaal  ","", topic)) %>% 
  arrange(desc(topic)) %>% 
  mutate(prop = 100*(data / sum(data, na.rm=TRUE))) %>% 
  mutate(ypos = cumsum(prop)- 0.5*prop ) %>% 
  
  ggplot(aes(x="", y=prop, fill=topic)) +
  # theme_void() +
  geom_bar(stat="identity", width=1, color="white") +
  coord_polar("y", start=0) +
  geom_text(aes(y = ypos, label = prop), color = "white", size=6) +
  scale_fill_brewer(palette="Set1")
   

data %>% 
  filter(grepl("totaal", tolower(var))) %>% 
  filter(data > 0) %>% 
  group_by(naam, topic) %>% 
  summarise(data=sum(data, na.rm=TRUE)) %>% 
  group_by(naam) %>% 
  mutate(topic = gsub("Totaal |Totaal  ","", topic)) %>% 
  arrange(desc(topic)) %>% 
  mutate(prop = 100*(data / sum(data, na.rm=TRUE))) %>% 
  mutate(ypos = cumsum(prop)- 0.5*prop ) %>% 
  
  ggplot(aes(area = prop, fill = topic, label = paste(substr(topic,1,10), as.integer(prop)))) +
  theme(legend.position = "none")+
  geom_treemap() +
  geom_treemap_text() +
  facet_wrap(~naam)

