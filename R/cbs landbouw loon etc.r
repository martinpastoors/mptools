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
tabel <- "71904ned" # landbouw vanaf 1851
tabel <- "84298NED"

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

properties <- metadata$DataProperties

topicgroups <- 
  properties %>% 
  filter(Type == "TopicGroup") %>% 
  dplyr::select(ID, ParentID, Title_tg=Title, Description_tg=Description)

metadata_df <-
  properties %>% 
  filter(Type == "Topic") %>% 
  dplyr::select(Key, ID, Position, ParentID, Variable=Title, Description, Unit) %>% 
  
  # level1
  left_join(topicgroups, by=c("ParentID" = "ID")) %>% 
  dplyr::select(-ParentID, ParentID=ParentID.y, Title_1=Title_tg, Description_1 = Description_tg) %>% 

  # level2
  left_join(topicgroups, by=c("ParentID" = "ID")) %>% 
  dplyr::select(-ParentID, ParentID=ParentID.y, Title_2=Title_tg, Description_2 = Description_tg) %>% 

  # level3
  left_join(topicgroups, by=c("ParentID" = "ID")) %>% 
  dplyr::select(-ParentID, ParentID=ParentID.y, Title_3=Title_tg, Description_3 = Description_tg) %>% 
  
  # level4
  left_join(topicgroups, by=c("ParentID" = "ID")) %>% 
  dplyr::select(-ParentID, -ParentID.y, Title_4=Title_tg, Description_4 = Description_tg) %>% 
  
  tidyr::pivot_longer(names_to = "tempvar", values_to = "data", Title_1:Description_4) %>% 
  drop_na(data) %>% 
  tidyr::separate(tempvar, into=c("text","id"), sep="_") %>% 
  mutate(id = as.integer(id) ) %>% 
  
  group_by(Key, text) %>% 
  mutate(id = abs(id - max(id, na.rm=TRUE))) %>% 
  
  tidyr::unite("tempvar", text:id, sep="_") %>% 
  
  pivot_wider(names_from = tempvar, values_from = data) %>% 
  rename(hoofdcategorie = Title_0, hoofdcategorie_desc = Description_0) %>% 
  {if("Title_1" %in% names(.)) rename(., unittype       = Title_1, unittype_desc       = Description_1) else .} %>% 
  {if("Title_2" %in% names(.)) rename(., subcategorie   = Title_2, subcategorie_desc   = Description_2) else .} %>% 
  {if("Title_3" %in% names(.)) rename(., subsubcategorie= Title_3) else .}
  


# dataset samenstellen
data <-
  dwnld %>% 
  
  # add naam van gemeente
  # left_join(metadata$RegioS, by=c("RegioS"="Key")) %>% 
  # rename(naam = RegioS_label) %>% 
  
  # filter op namenn
  # filter(naam %in% my_names) %>% 
  
  # Make long
  ungroup() %>% 
  pivot_longer(names_to="Key", values_to="data", 5:ncol(.)) %>% 
  # dplyr::select(-Description) %>% 
  
  # add beschrijving van variabelen
  left_join(metadata_df, by=c("Key"="Key")) %>% 
  
  # add jaar
  mutate(jaar = as.integer(as.character(Perioden_label))) %>% 
  
  lowcase()


# unique(data$unittype)
unique(data$variable)
# data %>% filter(grepl("bedrij", tolower(variable))) %>% View()
# data %>% filter(grepl("melk", tolower(variable))) %>% View()
# data %>% filter(grepl("vlees", tolower(variable))) %>% View()



data %>% 
  filter( !is.na(data)) %>% 
  filter(key=="InkomenUitBedrijfsuitoefening_153") %>% 

  ggplot(aes(x=jaar, y=data)) +
  theme_bw() +
  geom_bar(aes(fill=key), stat="identity") +
  labs(y="", x="") +
  facet_wrap(~key, scales = "free_y")

