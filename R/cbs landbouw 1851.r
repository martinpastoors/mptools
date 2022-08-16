# Eenmalig uitvoeren:
# install.packages("cbsodataR")

options(dplyr.summarise.inform = FALSE)

library(cbsodataR)
library(tidyverse)

source("R/my utils.r")

# Downloaden van tabeloverzicht
toc <- cbs_get_toc()
# View(toc)

# toc %>% filter(grepl("landbouw", tolower(Title))) %>% View()
# toc %>% filter(grepl("personenauto", tolower(Title))) %>% View()
# toc %>% filter(grepl("broedvogel", tolower(Title))) %>% View()
# toc %>% filter(grepl("bodem", tolower(Title))) %>% View()

# Downloaden van gehele tabel (kan een halve minuut duren)
tabel    <- "80781ned"
# tabel    <- "84498NED" # broedvogel index
tabel <- "71904ned" # landbouw vanaf 1851

dwnld    <- 
  cbs_get_data(tabel)  %>% 
  cbs_add_label_columns() %>% 
  cbs_add_date_column() 
metadata <- 
  cbs_get_meta(tabel)

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
  rename(unittype       = Title_1, unittype_desc       = Description_1) %>% 
  {if("Title2" %in% names(.)) rename(subcategorie   = Title_2, subcategorie_desc   = Description_2) else .} %>% 
  {if("Title3" %in% names(.)) rename(subsubcategorie= Title_3) else .}
  


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
  pivot_longer(names_to="Key", values_to="data", 5:204) %>% 
  # dplyr::select(-Description) %>% 
  
  # add beschrijving van variabelen
  left_join(metadata_df, by=c("Key"="Key")) %>% 
  
  # add jaar
  mutate(jaar = as.integer(as.character(Perioden_label))) %>% 
  
  lowcase()

save(data, file=paste0(tabel,".RData"))
load(file=paste0(tabel,".RData"))

unique(data$unittype)
unique(data$variable)
data %>% filter(grepl("bedrij", tolower(variable))) %>% View()
data %>% filter(grepl("melk", tolower(variable))) %>% View()
data %>% filter(grepl("vlees", tolower(variable))) %>% View()


t <-
  data %>% 
  filter( !is.na(data)) %>% 
  filter(tolower(unittype) == "runderen" | 
         variable          == "Melk afgeleverd aan fabrieken"|
         variable          == "Bedrijven met rundvee") %>% 
  filter(jaar >= 1950) %>% 
  
  mutate(variable = ifelse(grepl("runderen", tolower(unittype)), "runderen", variable)) %>% 
  mutate(variable = ifelse(grepl("^Melk", variable), "melkproductie", variable)) %>% 
  mutate(variable = ifelse(grepl("^Bedrijven",variable), "bedrijven", variable)) %>% 
  
  bind_rows(data %>% filter(!is.na(data), title2 == "Melk- en fokvee") %>% mutate(variable = "melkkoeien")) %>% 
  
  group_by(variable, jaar) %>% 
  summarise(data = sum(data, na.rm=TRUE)) %>% 
  filter(data > 0) %>% 
  ungroup() %>% 
  
  pivot_wider(names_from = variable, values_from = data) %>% 
  
  mutate(runderen_bedrijf = runderen / bedrijven) %>% 
  mutate(melkproductie_bedrijf = melkproductie / bedrijven) %>% 
  mutate(melkproductie_melkkoe    = melkproductie / melkkoeien) %>% 
  
  pivot_longer(names_to = "variable", values_to = "data", bedrijven:melkproductie_melkkoe) %>% 
  filter(variable != "melkkoeien") %>% 
  
  mutate(variable = ifelse(grepl("bedrijven", variable), "bedrijven (x 1 000)", variable)) %>% 
  
  mutate(data     = ifelse(variable=="runderen",  data/1000, data)) %>% 
  mutate(variable = ifelse(variable=="runderen",  "runderen (x 1 000 000)", variable)) %>% 

  mutate(data     = ifelse(variable=="melkproductie",  data/1000, data)) %>% 
  mutate(variable = ifelse(variable=="melkproductie",  "melkproductie (miljard kg)", variable)) %>% 
  
  mutate(variable = factor(variable, levels=c("bedrijven (x 1 000)",    "melkproductie_bedrijf",
                                              "runderen (x 1 000 000)", "runderen_bedrijf", 
                                              "melkproductie (miljard kg)",          "melkproductie_melkkoe"))) 



t %>% 
  filter(variable %in% c("bedrijven (x 1 000)",
                         "runderen (x 1 000 000)",
                         "melkproductie (miljard kg)",
                         "runderen_bedrijf")) %>% 
  ggplot(aes(x=jaar, y=data)) +
  theme_bw() +
  theme(legend.position = "none") +
  # geom_bar(aes(fill=variable), stat="identity", width=0.8) +
  geom_bar(fill="gray20", stat="identity", width=0.8) +
  labs(y="", x="") +
  scale_x_continuous(breaks=seq(1950,2020, 10)) +
  facet_wrap(~variable, scales = "free_y", ncol=1)





# overzicht bedrijven, rundvee en melk geleverd aan fabrieken
data %>% 
  filter( !is.na(data)) %>% 
  filter(tolower(unittype) == "runderen" | 
           variable == "Melk afgeleverd aan fabrieken"|
           variable == "Bedrijven met rundvee") %>% 
  mutate(hoofdcategorie = paste(hoofdcategorie, unit)) %>% 
  mutate(hoofdcategorie = ifelse(variable=="Melk afgeleverd aan fabrieken", paste(variable, unit), hoofdcategorie)) %>% 
  mutate(hoofdcategorie = ifelse(variable=="Bedrijven met rundvee", paste(variable, unit), hoofdcategorie)) %>% 
  mutate(variable = ifelse(variable %in% c("Melk afgeleverd aan fabrieken","Bedrijven met rundvee"), NA, variable)) %>% 
  filter(jaar >= 1950) %>% 
  ggplot(aes(x=jaar, y=data)) +
  theme_bw() +
  geom_bar(aes(fill=variable), stat="identity") +
  labs(y="", x="") +
  facet_wrap(~hoofdcategorie, scales = "free_y")

# overzicht (melk) dieren per bedrijf en melk productie per bedrijf
t <-
  data %>% 
  filter( !is.na(data)) %>% 
  filter(tolower(unittype) == "runderen" ) %>%
  filter(grepl("^melk", tolower(variable))) %>% 
  group_by(jaar) %>% 
  mutate(data = 1000 * data) %>% 
  summarise(data = sum(data, na.rm=TRUE)) %>% 
  mutate(variable = "melkvee") %>% 
  
  # melkproductie
  bind_rows(
    data %>% 
    filter(variable == "Melk afgeleverd aan fabrieken") %>% 
    filter( !is.na(data)) %>% 
    mutate(data = 1000000 * data) %>% 
    group_by(jaar) %>% 
    summarise(data = sum(data, na.rm=TRUE)) %>% 
    mutate(variable = "melkproductie")
  ) %>% 
  
  # rundvee bedrijven
  bind_rows(
    data %>% 
      filter(variable == "Bedrijven met rundvee") %>% 
      filter( !is.na(data)) %>% 
      mutate(data = 1000 * data) %>% 
      group_by(jaar) %>% 
      summarise(data = sum(data, na.rm=TRUE)) %>% 
      mutate(variable = "bedrijven")
  ) %>% 
  
  filter(jaar >= 1950) %>% 
  filter(data > 0) %>% 
  pivot_wider(names_from = variable, values_from = data) %>% 
  
  mutate(dieren_bedrijf = melkvee / bedrijven) %>% 
  mutate(productie_bedrijf = melkproductie / bedrijven) %>% 
  mutate(productie_koe     = melkproductie / melkvee) 

t2a <-
  t %>% 
  pivot_longer(names_to = "variable", values_to = "data", melkvee:productie_koe) %>% 
  mutate(decade = cut(jaar, breaks=seq(1950,2020,10), include.lowest=TRUE, dig.lab=10)) %>% 
  mutate(decade = as.character(decade)) %>% 
  mutate(decade = gsub("\\[|\\(|\\]","", decade)) %>% 
  mutate(decade = ifelse(is.na(decade), "2020", decade)) %>% 
  group_by(decade, variable) %>% 
  summarise(data = mean(data, na.rm=TRUE))

t2 <-
  t2a %>% 
  left_join(
    t2a %>% group_by(variable) %>% filter(row_number()==1) %>% dplyr::select(variable, first=data), 
    by="variable"
  ) %>% 
  mutate(index = data/first) %>% 
  mutate(perc  = (data/first-1))

# dieren per bedrijf etc. 
t %>% 
  dplyr::select(jaar, dieren_bedrijf, productie_bedrijf, productie_koe) %>% 
  pivot_longer(names_to = "variable", values_to = "data", dieren_bedrijf:productie_koe) %>% 

  ggplot(aes(x=jaar, y=data)) +
  theme_bw() +
  geom_bar(aes(fill=variable), stat="identity") +
  labs(y="", x="") +
  facet_wrap(~variable, scales = "free_y")

# gemiddelden per decade
t2 %>% 
  ungroup() %>% 
  separate(decade, into=c("start", "end"), sep=",") %>% 
  mutate(across(c(start, end), as.numeric)) %>% 
  mutate(end = ifelse(is.na(end), start+10, end)) %>% 
  group_by(variable) %>% 
  mutate(index2 = lag(index)) %>% 
  # View()

  ggplot(aes(x=start)) +
  theme_bw() +
  theme(legend.position = "none") +
  # geom_point(aes(colour=variable)) +
  geom_segment(aes(xstart=start, xend=start, y=index, yend=index2), colour="gray", size=0.2) +
  geom_segment(aes(xstart=start, xend=end, y=index, yend=index, colour=variable), size=1.2) +
  # geom_segment(aes(xstart=start, xend=end, ystart=perc, yend=perc, colour=variable), size=1.2) +
  labs(y="", x="") +
  expand_limits(y=0) +
  scale_y_continuous(breaks = scales::pretty_breaks(n=10)) +
  # scale_y_continuous(labels = scales::percent_format(accuracy=1)) +
  facet_wrap(~variable, scales = "free_y")

#alle dieren
data %>% 
  filter(jaar >= 1950) %>% 
  filter(tolower(hoofdcategorie) == "dieren") %>% 
  mutate(unittype = ifelse(is.na(unittype), variable, unittype)) %>% 
  group_by(jaar, unittype) %>% 
  summarise(data=sum(data, na.rm=TRUE)) %>% 
  
  # View()
  
  ggplot(aes(x=jaar, y=data)) +
  theme_bw() +
  theme(legend.position = "none") +
  geom_bar(aes(fill=unittype), stat="identity") +
  labs(y="", x="", title="Aantal dieren * 1000") +
  facet_wrap(~unittype, scales="free_y")

