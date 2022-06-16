library(tidyverse)

options(dplyr.summarise.inform = FALSE)

source("r/my utils.r")

q <- 
  readxl::read_xlsx("C:/TEMP/ruwe data enquête Landbouwbeleid MP.xlsx",
                    sheet="nl",
                    col_names=TRUE,
                    skip=0,
                    col_types="text") %>% 
  lowcase() %>% 
  setNames(gsub("#","q", names(.))) %>% 
  tidyr::pivot_longer(names_to = "antwoord", values_to = "data", cols=c(3:ncol(.))) 


groep <-
  q %>% 
  filter(q %in% c("2")) %>% 
  setNames(gsub("data","groep",names(.)))  %>% 
  mutate(groep = stringr::word(groep,1)) %>% 
  mutate(groep = gsub(",","",groep)) %>% 
  mutate(groep = ifelse(groep %in% c("Landbouw","Landschap","Overheid"), groep, "Overig")) %>% 
  dplyr::select(-vragen) %>% 
  drop_na()

lftd <-
  q %>% 
  filter(q %in% c("4")) %>% 
  setNames(gsub("data","lftd",names(.))) %>%
  dplyr::mutate(lftd = gsub("Jonger dan","<", lftd)) %>% 
  dplyr::mutate(lftd = gsub("of ouder","plus", lftd)) %>% 
  dplyr::select(-vragen) %>% 
  ungroup() %>% 
  mutate(lftd = ifelse(antwoord %in% c("1","2"), "jonger dan 50", lftd)) %>% 
  mutate(lftd = ifelse(antwoord %in% c("3","4"), "50 en ouder", lftd)) %>% 
  drop_na()

d <- 
  readxl::read_xlsx("C:/TEMP/ruwe data enquête Landbouwbeleid MP.xlsx",
                    sheet="Data",
                    col_names=TRUE,
                    skip=0,
                    col_types="text") %>% 
  tidyr::pivot_longer(names_to = "q", values_to = "antwoord", cols=c(2:ncol(.))) 


pers <-
  d %>% 
  left_join(groep, by=c("q","antwoord")) %>% 
  left_join(lftd, by=c("q","antwoord")) %>% 
  filter(!is.na(groep) | !is.na(lftd)) %>% 
  dplyr::select(-q, -antwoord) %>% 
  tidyr::pivot_longer(names_to="variable", values_to = "data", groep:lftd) %>% 
  drop_na() %>% 
  tidyr::pivot_wider(names_from = variable, values_from = data)

x <-
  d %>% 
  filter(q %notin% c("2", "2_Open", "4")) %>% 
  left_join(pers, by="id") %>% 
  left_join(q, by=c("q","antwoord")) %>% 
  tidyr::separate(q, into=c("q1","q2"), sep="_", remove=FALSE) %>% 
  dplyr::mutate(q1 = as.integer(q1), q2 = as.integer(q2)) %>% 
  drop_na(antwoord) %>% 
  dplyr::mutate(belangrijk = ifelse(antwoord %in% 4:5, TRUE, FALSE))

summ <-
  x %>% 
  group_by(q1, q2, vragen, groep, lftd, belangrijk) %>% 
  summarise(count=n()) %>% 
  group_by(q1, q2, vragen, groep, lftd) %>% 
  mutate(prop=count/sum(count, na.rm=TRUE)) %>% 
  filter(belangrijk == TRUE) 

summ %>%
  mutate(title=paste(str_pad(q1,width=2, pad="0"),
                     str_pad(q2, width=2,pad="0"),
                     vragen)) %>% 
  ggplot(aes(x=groep, y=prop, fill=groep)) +
  theme_bw() +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  theme(strip.text = element_text(hjust = 0)) +
  geom_bar(stat="identity") +
  labs(title=paste("Question", question)) +
  facet_wrap( ~ title)



writexl::write_xlsx(summ, path="C:/TEMP/landbouwenquete new.xlsx")

  
