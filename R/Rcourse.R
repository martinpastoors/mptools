############################################################################
##
## TCRENV 2016
##
###########################################################################

# ------------------------------------------------------------------------
# 01 Coding R - Bjarki
# 29/2/2016
# ------------------------------------------------------------------------

# make a vector
(x1<-seq(1,10))
# the same
(x1<-1:10)
(x2<-seq(0,1,length=11))
(x3<-seq(0,1,by=0.1))

rep(1,10)
rep(c(1,2,3),2)
rep(c(1,2,3),each=2)

# length of a vector
length(x1)

(x3<-c(6,3,8,9,4))
# sort the elements of the vector
sort(x3)
# find the rank of each element
rank(x3)
# gives an index for reordering
order(x3)

# join two vectors
c(x1,x2)
cbind(x1,x2)
rbind(x1,x2)

x <- rnorm(100)
mean(x)
median(x)
quantile(x)

# vector indices ------------------------------

x4<-c(4,7,5,3,4,8,9)
# first element of x4
x4[1]
x3[5]
# first and fourth
x4[c(1,4)]
# use logicals
x4[x4>=7]
x4[x4 != 3]
x4[x4>7 & x4 != 9]

# indices that satify a condition
which(x4>=7)
which(x4!=3)   # != not equal to
which(x4==3)   # == equal to

# get everything except the first element
x4[-1]
# get everything except the first element and second element
x4[-c(1,2)]

# dealing with NA
x5 <- c(1,54,7,NA)
is.na(x5)
x5[!is.na(x5)]

# Data frames ----------------------------

# Read the minke whale data
minke <- read.csv("http://www.hafro.is/~bthe/minke.csv")
minke["length"]
head(minke)
head(minke[c("date.caught","length")])
head(minke[,c(2,7)])

install.packages("stringr")

# ------------------------------------------------------------------------
# 02 GGPLOT - Einar
# 29/2/2016
# ------------------------------------------------------------------------

# install.packages(c("geo","dplyr","tidyr", "mapproj"))

library(geo)
library(ggplot2)
library(dplyr)  # not used here?
library(tidyr)  # not used here?
library(maps)
library(mapdata)
library(mapproj)
library(ggmap)  # to get e.g. google or OSM map

# Read in data Minke whale data
d <- read.csv("http://www.hafro.is/~einarhj/data/minke.csv",
              stringsAsFactors = FALSE) 
dim(d)
str(d) #give me the structure

# Read in cod assessment data
d2 <- read.csv("http://data.hafro.is/assmt/2015/cod/summary.csv") 
str(d2)

# Simple GGplot
ggplot(d, aes(x = age, y = length)) + 
  geom_point()

# different syntax, same results
ggplot(d, aes(x = age, y = length)) + geom_point()
ggplot(d, aes(age, length))         + geom_point()
ggplot()                            + geom_point(data = d, aes(age, length))
ggplot(data = d)                    + geom_point(aes(x = age, y = length))
ggplot(d)                           + geom_point(aes(age, length))
d %>% ggplot()                      + geom_point(aes(age, length))

# generating a ggplot object
p <- ggplot(d, aes(age, length)) + geom_point()
class(p)
str(p)

# plotting with separate colouring
p <- ggplot(d)
p + geom_point(aes(age, length, colour = sex))
p + geom_point(aes(age, length, colour = area))


# Manual control of colours:
p <- ggplot(d)
p + geom_point(aes(age, length, colour = sex)) +
  scale_colour_manual(values = c("orange","brown"))
p + geom_point(aes(age, length, colour = area)) +
  scale_colour_manual(values = c("orange","brown"))

# Exercise
p + geom_point(aes(age, length, colour = maturity))
p + geom_point(aes(age, length, shape = maturity))

# Different formatting
p + geom_point(aes(age, length, size = stomach.volume))
p + geom_point(aes(age, length, colour = sex, size = stomach.volume))

# Reveal overlays in the data
# Alpha = 1 is no transparency, Alpha = 0 is fully transparent. 
p + geom_point(aes(age, length, size = stomach.volume), alpha = 0.6)
p + geom_point(aes(age, length, colour= sex, size = stomach.volume), alpha = 0.6)

p + geom_point(aes(age, length, size = stomach.volume), alpha = 0.3, col = "red")

# Facetting
ggplot(d) + 
  geom_point(aes(age, length, colour = sex)) + 
  facet_wrap(~ area)

# facet grid approach
ggplot(d) + 
  geom_point(aes(age, length, colour = sex)) + 
  facet_grid(area ~ maturity)

# need to remove the NAs before the ggplot is called. 
x <- d[!is.na(d$maturity),] #give me all the rows where this vector is true
ggplot(x) + 
  geom_point(aes(age, length, colour = sex)) + 
  facet_grid(area ~ maturity)

# Add multiple layers; e.g. smoother layers
p <- ggplot(d, aes(age, length))
p + geom_point() + geom_smooth()
p + geom_point() + geom_smooth(span = 0.1)

# linear smoothing
p + geom_point() + geom_smooth(method = "lm")

# loess smoothing with parameters
loess(age~length, d)
scatter.smooth (d$age,d$length)

# Boxplots and violin plots
p <- ggplot(d, aes(sex, length))
p + geom_boxplot()
p + geom_violin()

# Jitter to randomply displace points a little bit
p + geom_boxplot() + geom_jitter()
p + geom_violin() + geom_boxplot() + geom_jitter()
p + geom_violin() + geom_boxplot() + geom_jitter(width=0.25, height=0.25)

# Layer: frequency polygons
p <- ggplot(d, aes(length))
p + geom_freqpoly()
p + geom_freqpoly(aes(colour = sex))

# Layer: histograms
p + geom_histogram()
p + geom_histogram(aes(fill = sex), bins=15)
p + geom_histogram(aes(fill = sex), bins=30)
p + geom_histogram(aes(fill = sex)) + facet_wrap(~ sex, ncol = 1)

# Layer: barchart
ggplot(d, aes(maturity)) + geom_bar()
ggplot(d, aes(maturity)) + geom_bar() + facet_wrap(~area, ncol=1)

# Layer: line (on icelandic cod!)
ggplot(d2, aes(Year, Rec)) + geom_line()
ggplot(d2, aes(Year, Rec)) + geom_line() + expand_limits(y = 0)
ggplot(d2, aes(Year, Rec)) + geom_line() + expand_limits(y = 0) + geom_smooth(span=0.5)

# Exercise
# line graph of biomass
ggplot(d2, aes(Year, B4plus)) + geom_line() + expand_limits(y = 0) +
  scale_y_continuous(breaks=seq(0,2500, by=500)) +
  geom_line(aes(y=SSB), col="red")

# NOTE: it is not possible to have data on a second axis !!!

# bargraph of landings
ggplot(d2, aes(Year, Landings)) + geom_bar(stat="identity")

# Layer: path
ggplot(d2, aes(SSB, Fbar)) + geom_path()
ggplot(d2, aes(SSB, Fbar)) + geom_path() + 
  geom_point(aes(colour = Year), size = 3)

# LIMITS
p <- ggplot(d, aes(maturity, length))
p + geom_jitter()
p + geom_jitter() + ylim(600, 800)
p + geom_jitter() + ylim(NA, 800) # setting only one limit
p + geom_jitter() + ylim(600,800) + xlim("immature","mature")
# careful when using boxplots on limited graphs; boxplots changes. It takes away
# all the data outside the range. 
# Solution
p + geom_boxplot() + coord_cartesian(ylim = c(600, 800))

# Mini GIS
?island
str(island)
p <- ggplot(island, aes(lon, lat)) + labs(x = NULL, y = NULL)
p + geom_point()
p + geom_path() #follow the path
p + geom_path() + coord_map() + 
  geom_point(data=d, aes(lon,lat, colour=sex, size = age), alpha=0.6) 

# Map data of the world
m <- map_data("world")
sort(unique(m$region))

ggplot(m) +
  geom_polygon(aes(long, lat, group = group)) +
  coord_quickmap()
ggplot(m) +
  geom_path(aes(long, lat, group = group)) +
  coord_quickmap()

#higher resolution
m <- map_data("worldHires") 
m1 <- filter(m, long > 5 & long < 15 & lat > 54, lat < 58)
ggplot(m1) +
  geom_polygon(aes(long, lat, group = group)) +
  coord_quickmap()

ggplot(m1) +
  geom_path(aes(long, lat, group = group)) +
  coord_quickmap()

# Get a fixed position map e.g. OSM, google
m2 <- ggmap::get_map(location=c(-20,65), zoom=6, source="osm")
ggmap(m2)
ggmap(m2) + 
  geom_point(data=d, aes(lon,lat, colour=sex, size = age), alpha=0.6) 



# ------------------------------------------------------------------------
# 05 DPLYR and TIDYR 
# ------------------------------------------------------------------------

library(tidyr)
library(dplyr)
library(pander)

minke <- read.csv("http://www.hafro.is/~einarhj/data/minke.csv",
                  stringsAsFactors = FALSE) 

## FILTERING

minke.2003males <- dplyr::filter(minke,year==2003,sex !="Female")
View(minke.2003males)

## create a dataset with only females
minke.females <- 
  filter(minke,sex=='Female')

## or non-pregnant
minke.nonpregnant <-
  filter(minke,sex=='Female', maturity != 'pregnant')


## SELECTING (columns)

## select id, age, length and sex
minke.redux <-
  dplyr::select(data=minke,id,age,length,sex)

## change variable names to capitals as well. 
minke.AgeLength <- select (minke, Age=age, Length=length)

## select all column except stomach volume and weight
minke.noStom <-
  select(minke,-contains('stomach'))

## select id and all column between length and sex
minke.min <-
  select(minke,c(id,length:sex))

minke.neg <-
  select(minke,-date)


## ARRANGING 

## arrange by length (ascending)
minke.lasc <-
  dplyr::arrange(minke,length)

## arrange by length (descending)
minke.ldesc <-
  arrange(minke,desc(length))

## arrange by age and then length (ascending)
minke.alasc <-
  arrange(minke,age,length)

## combining arrange with select
head(arrange(select(minke, id, age, length),age,length))



## MUTATING: e.g. create a number of columns

## add new column in the traditional r style
minke$approx.weight <- 3.85*1e-06*minke$length^3.163

## add new column using mutate
minke <-
  dplyr::mutate(minke,
         approx.weight = 3.85*1e-06*length^3.163)

## not really useful until you add more than one
minke <-
  mutate(minke,
         approx.weight = 3.85*1e-06*length^3.163,
         # 
         adj.weight = ifelse(is.na(weight),approx.weight, weight))

library(lubridate)
minke.mutate <- mutate(minke,
                       date2 = ymd_hms(date),
                       month = month(date2),
                       year = year(data2))

## SUMMARISE

## create summaries
dplyr::summarise(minke,num.obs = n())

## you can have as many as you like
summarise(filter(minke, sex == 'Female', year == 2003),
          num.obs = n(),
          num.fem = sum(sex == 'Female'),
          num.large = sum(length > 650))

summarise(filter(minke, sex == 'Female', year == 2003),
          num.obs = n(),
          num.fem = sum(stomach.weight, na.rm=TRUE),
          num.large = sum(length > 650))

## But not particularly interesting until one can group the data
minke.2 <- group_by(minke, area, sex)
summarise(minke.2,
          num.obs = n(),
          num.fem = sum(sex == 'Female'),
          num.large = sum(length > 650))

## have to submit but to github Hadley!!


## this is the same as splitting things up
minke.N <- filter(minke,area=='North')
summarise(minke.N,
          num.obs = n(),
          num.fem = sum(sex == 'Female'),
          num.large = sum(length > 650))


minke.S <- filter(minke,area=='South')
summarise(minke.S,
          num.obs = n(),
          num.fem = sum(sex == 'Female'),
          num.large = sum(length > 650))

## TIDYR
## RESHAPING tools

## GATHER: gather columns into rows

## read in data in a wide format and convert to long format
catatage <-
  read.csv('http://data.hafro.is/assmt/2015/cod/catage.csv')


catlong <-
  # takes df, takes name of the header values, takes values, 
  gather(catatage, #df
         key=age, #names of new variables
         value=number, # values
         X3:X14) #apply to what columns

catlong <-
  mutate(catlong,
         age = as.numeric(gsub('X', '', age)))


## SPREAD: from long to wide, Spread rows into columns

## Traditional way of getting from long to wide
minke.y.a <- group_by(minke, year, area)
num.minke.y.a <- summarise(minke.y.a, num = n())

spread(num.minke.y.a,year,num)

catwide <- spread(catlong, key=age,value=number)

## !!!! This is very useful for generating report tables
catwide2 <- spread(catlong, key=Year, value=number)
pandoc.table(catwide2, split.tables=80)


## SEPARATE into different columns

minke.by.day <-
  separate(minke,
           date,
           c('yr', 'm', 'd'),
           sep = '-') #split date based on hyphens

minke.by.day <-
  separate(minke.by.day, 
           d, 
           c('d2', 'time'), 
           sep = ' ') #split date based on space

minke.by.day <-
  separate(minke.by.day, 
           time, 
           c('hh', 'mm', 'ss'), 
           sep = ':') #split date based on semicolon

# In one go
separate(select(minke,date), 
         date,
         c('yr', 'mo', 'da', "hh", "mm", "ss")) 

## unite: Unite several columns into one

## combine
minke.m.s <-
  unite(minke,mat.sex,c(sex,maturity),sep='-')


## CHAINING operations -----------------------------

## chaining operations together can be quite complex
summarise(
  group_by(
    filter(minke,!is.na(weight)),
    sex),
  num.whale=n(),
  m.weight = mean(weight)
)

## using the %>% operator 
minke %>%
  filter(!is.na(weight)) %>%
  group_by(sex) %>%
  summarise(num.whale = n(),
            m.weight = mean(weight))

## lets do more
minke %>%
  filter(area == 'North') %>%
  group_by(maturity) %>%
  summarise(n=n(),
            num.na = sum(is.na(age)),
            m.age = mean(age,na.rm=TRUE),
            sd.age = sd(age,na.rm=TRUE)) %>%
  mutate(prop = n/sum(n)) %>%
  arrange(prop)

## play with ungroup
minke %>%
  group_by(maturity) %>%
  mutate(lnorm.in = length/mean(length)) %>%
  ungroup() %>%
  mutate(lnorm.out = length/mean(length)) %>%
  select(whale.id,maturity,lnorm.in,lnorm.out)


## class excercise
## num caught by year
minke %>%
  group_by(year) %>%
  summarise(num.caught = n())

## num which are females
minke %>%
  group_by(year) %>%
  summarise(num.caught = n(),
            num.fem = sum(sex=='Female'),
            prop.fem = mean(sex=='Female')) %>%
  mutate(prop2 = num.fem/num.caught)

## mean age and sd by maturity
minke %>%
  group_by(maturity) %>%
  summarise(ml = mean(length),
            sdl = sd(length),
            ma = mean(age,na.rm=TRUE),
            sda = sd(age,na.rm=TRUE))


##
minke %>%
  group_by(year,area) %>%
  summarise(num = n()) %>%
  spread(year,num)

# number of whales caught each year
m1  <- group_by(minke, year)
m1a <- summarise(m1,Ntot=n())

# proportion caught of which are females each year
m2  <- filter(m1, sex =="Female")
m2a <- cbind(summarise(m2,Nfem=n()),m1a$Ntot)
m2b <- mutate(m2a, FemProp = Nfem/m1a$Ntot)

# Quicker way
summarise(m1,propfem = mean(sex == "Female"))

# Calculate the mean length and age along with standard deviation 
# grouped by maturity
m3  <- group_by(minke, maturity)
summarise(
  m3,
  ml = mean(length),
  sl = sd(length),
  ma = mean(age, na.rm = TRUE),
  sa = sd(age, na.rm = TRUE)
)

# Using %>% and spread, calculate number of whales caught by area (rows) and year (columns)
minke %>%
  group_by(area,year) %>%
  summarise(Ntot=n()) %>%
  spread(key=year,Ntot)


# ------------------------------------------------------------------------
# 06 Applied data and graph grammar
# Icelandic survey raising example
# ------------------------------------------------------------------------

library(dplyr)
library(tidyr)
library(ggplot2)
library(geo)
library(maps)
library(mapdata)
library(lubridate)

Station <-
  #  read.csv("http://www.hafro.is/~einarhj/data/tcrenv2016/Station.csv") %>%
  read.csv("Station.csv") %>%
  mutate(year = year(date1)) %>%
  select(id, year, towlength, lon = lon1, lat = lat1) %>%
  tbl_df()                                 # print in simple format

Subsampling <-
  #  read.csv("http://www.hafro.is/~einarhj/data/tcrenv2016/Subsampling.csv") %>%
  read.csv("Subsampling.csv") %>%
  tbl_df()

Length <-
  #  read.csv("http://www.hafro.is/~einarhj/data/tcrenv2016/Length.csv") %>%
  read.csv("Length.csv") %>%
  select(id, species, length, n) %>%
  tbl_df()

# 1. Pick a year
Species <- 1               # pick a species
Year <- 2015               # pick a year

# 2. Subset (filter) the station table (only the year I want)
st <-
  Station %>% 
  filter(year %in% Year) %>% 
  select(id, towlength, lon, lat) 

Station %>% group_by(year) %>% summarise(count=n())

# plot the stations
ggplot(st, aes(lon,lat)) + geom_point()

# plot towlength histogram
ggplot(st, aes(towlength)) + geom_histogram()


# 3. Subset (filter) the station table
# The year is not stored in the Length dataframe.
# So limit the data by using the station id in the st dataframe
le <-
  Length %>% 
  filter(id %in% st$id,           # ID in length table same as station table
         species == Species) %>%  # only required species
  select(id, length, n)           # keep only three variables

# number of length measurement
sum(le$n)


# 4. Get the subsampling information
ss <-
  Subsampling %>% 
  filter(id %in% st$id,
         species == Species) %>% 
  select(id, n.total, n.measured)

Subsampling %>% group_by(species) %>% summarize(count=sum(n.total))

# and calculate the raising factor
ss <-
  ss %>% 
  mutate(r = n.total/n.measured,
         r = ifelse(is.na(r), 1, r)) %>%  # just in case
  select(id, r)                           # only columns needed

# plot raising factor histogram
ggplot(ss, aes(r)) + geom_histogram()

# how many samples with r > 1
summarise(ss,sum(r>1))
sum(ss$r>1)

# 5. Raise the numbers measured
# Here we join the subsampling table (ss) with the length table (le)
# If you don't specify the by statement 
le <-
  le %>%
  left_join(ss) %>%    # Left join: add it to the left of the x
  mutate(n = n * r)    # Apply raising factor


# Filter the data such that you “see” the first few records were length was raised
# … and calculate the weight
le <-
  le %>% 
  mutate(wt = (n * 0.01 * length^3)/1000) # kilos already multiplied by number of fish


# 6. Calculate the catch per tow
# Each id represents a tow, hence we can group by it and then summarise

d <-
  le %>% 
  group_by(id) %>%         # group by ID
  summarise(n = sum(n),    # summarize by number of fish and weight 
            wt = sum(wt))

nrow(d)
nrow(st) # there are stations where no cod was caught

# 7. Join the calculation with the station table
# We do this because the station table is the only place were the tow postion 
# is recorded

d <-
  st %>% 
  left_join(d) %>% 
  mutate(n = ifelse(is.na(n), 0, n)/1e3,    # replace NAs by 0
         wt = ifelse(is.na(wt), 0, wt)/1e3)

# raise for towlength
d <-
  d %>% 
  mutate(n  = n  * 4 / towlength, 
         wt = wt * 4 / towlength)


# 8. Plot the results
ggplot(d, aes(lon, lat)) + 
  geom_point(aes(size = wt), alpha = 0.3, colour = "red") +
  scale_size_area(max_size = 20, breaks = c(0, .1, .5, 1, 3)) +
  labs(x = NULL, y = NULL, size = "tonnes/tow") +
  coord_quickmap()

iceland <- map_data("worldHires", region = "iceland")
ggplot(d, aes(lon, lat)) + 
  theme_bw() +
  geom_polygon(data=iceland, aes(long, lat, group=group), fill = "grey90") +
  geom_point(aes(size = wt), alpha = 0.3, colour = "red") +
  scale_size_area(max_size = 20, breaks = c(0, .1, .5, 1, 3)) +
  labs(x = NULL, y = NULL, size = "tonnes/tow") +
  coord_quickmap()

# CONDENSED VERSION

Species <- 1
Year <- 2015
st <-
  Station %>%
  filter(year %in% Year) %>%
  select(id, towlength, lon, lat)
d <-
  Length %>%
  filter(id %in% st$id,
         species %in% Species) %>%
  select(id, length, n) %>%
  left_join(
    Subsampling %>%                            # Note: here we do the raising
      filter(species %in% Species) %>%         #  "inside" the left_join function
      mutate(r = n.total / n.measured,
             r = ifelse(is.na(r), 1, r)) %>%
      select(id, r)
  ) %>%
  mutate(n = n * r,
         wt = (n * 0.01 * length ^ 3) / 1000) %>%  # kilos
  group_by(id) %>%                                 # summarise per tow
  summarise(n = sum(n),
            wt = sum(wt)) %>%
  right_join(st) %>%                               # join with station table
  mutate(n = ifelse(is.na(n), 0, n) / 1e3,         #  note the right_join
         wt = ifelse(is.na(wt), 0, wt) / 1e3) %>% 
  mutate(n  = n  * 4 / towlength, 
         wt = wt * 4 / towlength)


# CONVERTING THE CODE INTO A FUNCTION

catch_per_tow <- function(Year, Species) {
  st <-
    Station %>%
    filter(year %in% Year) %>%
    select(id, towlength, lon, lat)
  d <-
    Length %>%
    filter(id %in% st$id,             # this ensures we only work with SMB length
           species %in% Species) %>%
    select(id, length, n) %>%
    left_join(
      Subsampling %>%
        filter(species %in% Species) %>%
        mutate(r = n.total / n.measured,
               r = ifelse(is.na(r), 1, r)) %>%
        select(id, r),
      by =  c("id")
    ) %>%
    mutate(n = n * r,
           wt = (n * 0.01 * length ^ 3) / 1000) %>%
    group_by(id) %>%
    summarize(n = sum(n),
              wt = sum(wt)) %>%
    right_join(st, by = c("id")) %>%
    mutate(n = ifelse(is.na(n), 0, n) / 1e3,
           wt = ifelse(is.na(wt), 0, wt) / 1e3)
  
  return(d)
}

source("R/catch_per_tow.r")

catch_per_tow(Year = 2015, Species = 1, TowCorrect = TRUE) %>%
  ggplot(aes(lon, lat)) +
  theme_bw() +
  geom_polygon(data = iceland, aes(long, lat, group = group), fill = "grey90") +
  geom_point(aes(size = wt), alpha = 0.3, colour = "red") +
  scale_size_area(max_size = 20, breaks = c(0, .1, .25, 0.5, 1)) +
  labs(x = NULL, y = NULL, size = "tonnes/tow") +
  coord_quickmap()

# alternative presentation 

d <- catch_per_tow(Year = 2015, Species = 2, TRUE) 
d2 <-
  d %>%
  mutate(statrec = geo::d2ir(lat, lon)) %>%   # convert to rectangles
  group_by(statrec) %>%
  summarise(count = n(),
            n = mean(n),
            wt = mean(wt)) %>%
  mutate(lon = geo::ir2d(statrec)$lon,        # set positions to centre rect.
         lat = geo::ir2d(statrec)$lat)

ggplot(d2) +
  # set theme
  theme_bw() +
  # add rectangle density (by weight)
  geom_tile(aes(lon, lat, fill = wt)) +
  # change the colouring of the tiles
  scale_fill_continuous(low = "yellow", high = "red") +
  # add iceland
  geom_polygon(data = iceland, aes(long, lat, group = group), fill = "grey90") +
  # add the number of observations
  geom_text(aes(lon, lat, label = round(count, 0)), size = 2) +
  # add the station locations
  geom_point(data = d, aes(lon, lat), col = "blue", size = 0.05, alpha = 0.5) +
  # select coordinates and mapping
  coord_quickmap(range(d$lon), range(d$lat)) +
  # do labelling
  labs(x = NULL, y = NULL, fill = "t/tow")

# B. Many years, one species
# Exercises
# Modify the function such that it can handle multiple years.
# Create a bubble plot of some selected years (e.g. every tenth year) for some species

# 1. Simple mean and median

# calculate indices by year; create long df
d <- 
  catch_per_tow(Year = 1985:2015, Species = 1) %>% 
  group_by(year) %>% 
  summarise(Median = median(wt),
            Mean = mean(wt)) %>% 
  gather(key = Statistic, value = value, -year)

# plot the species
d %>% 
  ggplot(aes(year, value, colour = Statistic)) +
  geom_point() +
  geom_line()

# now for all species
# calculate indices by year; create long df
d <- 
  catch_per_tow(Year = 1985:2015, Species = 1:60) %>% 
  group_by(species, year) %>% 
  summarise(Median = median(wt),
            Mean = mean(wt)) %>% 
  gather(key = Statistic, value = value, Median:Mean)

# plot the species
d %>% 
  ggplot(aes(year, value, colour = Statistic)) +
  theme_bw() +
  geom_point() +
  geom_line() +
  facet_wrap(~species, scale="free_y")


# ------------------------------------------------------------------------
# Getting data into R - BJarki
# ------------------------------------------------------------------------

## read in the minke data properly
minke <-
  read.table(
    file = "http://www.hafro.is/~einarhj/data/tcrenv2016/Station.csv",
    header = TRUE,
    dec = '.',    ## decimal sign
    sep = ',',    ## column separator symbol
    skip = 0,     ## num lines at top to skip
    stringsAsFactors = FALSE,
    comment.char = '#'    ## indicating comments
  )  

## more low level:
minke.lines <- readLines("http://www.hafro.is/~einarhj/data/tcrenv2016/Station.csv", n=4)

## read the header in using scan
## this is useful for the PFA length data !!!!!!!
header <- scan(text = minke.lines[4],
               what = 'character',sep=',')

minke <- read.table(text = minke.lines[-c(1:4)],
                    sep = ',',dec='.')

## use the header read with scan to name the columns
names(minke) <- header

## simple sanity checks
head(minke)
tail(minke)
dim(minke)
names(minke)
summary(minke)
str(minke)
glimpse(minke) 

## one can write the data to file
write.table(minke,
            file = 'minke-class.csv', ## file name
            col.names = TRUE,    ## write header
            row.names = FALSE,   ## write row names
            quote = FALSE,       ## characters qouted?
            sep = ',',
            dec = '.')
## or simply
write.csv(minke,file='minke.csv')

## location of files
minke <-
  read.csv('http://www.hafro.is/~bthe/minke.csv')

catatage <-
  read.csv('http://data.hafro.is/assmt/2015/cod/catage.csv')

## excel files
install.packages("readxl")
library(readxl)
library(openxlsx)

?read_excel

minke.xls <- read_excel("minke.xlsx")
pfa       <- read_excel("C:/DATA/DATA/Selfsampling/PFA 20160217 selfsampling data.xlsm",
                        sheet = 2)
excel_sheets(pfa)

haul  <- read.xlsx("C:/DATA/DATA/Selfsampling/PFA 20160217 selfsampling data.xlsm", 
                   sheet="Trek", detectDates = TRUE, skipEmptyRows = TRUE)


## write excel files
library(openxlsx)

write.xlsx(minke,file='minke.xlsx')

fish <- read.csv2('lec1/fish.csv')

write.xlsx(list("minke"=minke,'fish'=fish),file='minkeandfish.xlsx')

## list sheets
excel_sheets('minkeandfish.xlsx')

## read in only the fish data
fish.xls <- read_excel('minkeandfish.xlsx',sheet = 'fish')

## database connectivity
library(dplyr)

## setup a connection to a database
## by creating it
db <- src_sqlite('minke.db',create = TRUE)

## dump data into it
tmp <- copy_to(db,minke,'minke_tbl')

## query the database
minke.tbl <- tbl(db,'minke_tbl')

## run sql on the database
num.minke <- tbl(db,sql('select count(1) from minke_tbl'))


## describe the table (analogous to str in R)
tbl_vars(minke.tbl)
glimpse(minke.tbl)

## list all tables in a data base
db_list_tables(db$con)

## for more db stuff refer to
?db_list_tables

## DATRAS

# install.packages("httr")
# install.packages("xml2")
library("httr")
library("xml2")

source('R/datras.R')

## get station data
st <- get_datras(record = "HH", survey = 'NS-IBTS',
                 year = 2000, quarter = 1)

## get length data
le <- get_datras(record = "HL", survey = 'NS-IBTS',
                 year = 2000, quarter = 1)

## get age data
age <- get_datras(record = "CA", survey = 'NS-IBTS',
                  year = 2000, quarter = 1)



# ------------------------------------------------------------------------
# Friday 4 March 2016
# Fitting statistical models
# ------------------------------------------------------------------------
# Statistics
# https://cran.r-project.org/doc/contrib/Faraway-PRA.pdf
# https://www.openintro.org/stat/?stat_book=os

library(dplyr)  
library(tidyr)  
library(ggplot2)
library(broom) # Package broom for dealing with model fits. 

cod.ca <- 
  read.csv("~/TCRENC 2016/Course/example-ca.csv") %>% 
  tbl_df()

ggplot(cod.ca,aes(length,ungutted)) + geom_point()

ggplot(cod.ca,aes(log(length),log(ungutted))) + geom_point()

# linear regression

# Often one sees the length weight relationship charaterised as:
#   W = a  Lb
# Now this is a non-linear relationship (if b ̸= 1) but is easy to linearise
# using log:
#   log(W) = log(a) + b  log(L)
wlFun <- function(dat, a, b) {
  p <- ggplot(dat, aes(log(length), log(ungutted))) +
       geom_point() +
       geom_abline(intercept = log(a), slope = b) +
       ylim(c(0, 10)) + xlim(c(0, 10))
  ss <- sum((log(dat$ungutted) - log(a) -
               b * log(dat$length)) ^ 2)
  return(list(p = p, ss = ss))
}

wlFun(cod.ca,0.01,3)

# lm (y~x, data)
fit <- lm(log(ungutted)~log(length), cod.ca)
summary(fit)
coefficients(fit)[1]
wlFun(cod.ca,exp(coefficients(fit)[1]),coefficients(fit)[2])

# Interaction
fit2 <- lm(log(ungutted)~log(length)*sex, cod.ca)
summary(fit2)
plot(fit2)
AIC(fit2)   # just the AIC value of the model
step(fit2)  # estimate which parameters contribute to the AIC


# Interaction and extra variable
fit3 <- lm(log(ungutted)~log(length)*sex + liver, cod.ca)
summary(fit3)
# plot(fit3)
step(fit3, na.rm=TRUE)


# Estimate a maturity ogive using a logistic model; proportions at length and age

cod.ca <- 
  mutate(cod.ca,
         mat1 = ifelse(maturity==1,0,1)) %>% 
  filter(!is.na(mat1))
fit.mat <- glm(mat1~length, family="binomial", data=cod.ca)
summary(fit.mat)
plot(fit.mat)


broom::tidy(fit.mat)
output <- broom::augment(fit.mat, type.predict = "response")
ggplot(output, aes(length, .fitted)) + 
  geom_line()

L50 <- output %>% 
  filter(.fitted <= 0.50) %>% 
  summarise(L50 = max(length))

ggplot(output, aes(length, .fitted)) + 
  geom_line() +
  geom_jitter(data=cod.ca, aes(length,mat1), alpha=0.1, height=0.1) + 
  geom_vline(xintercept=64)

# Fit by age
fit.mat2 <- glm(mat1~age, family="binomial", data=cod.ca)
output2 <- broom::augment(fit.mat2, type.predict = "response")
ggplot(output2, aes(age, .fitted)) + 
  geom_line() +
  geom_jitter(data=cod.ca, aes(age,mat1), alpha=0.1, height=0.11, width=0.5) + 
  geom_vline(xintercept=5)

output2 %>% 
  filter(.fitted <= 0.50) %>% 
  summarise(A50 = max(age)) # does not work very wel because of integer values


# Non-linear models (e.g. VBGF) - nls()

minke <- read.csv("http://www.hafro.is/~bthe/minke.csv")
age.data <- filter(minke, !is.na(age))
minke.vonB.par <-
  nls(length ~ Linf * (1 - exp(-K * (age - t0))),
      data = age.data,
      start = list(Linf = 1100, K = 0.1, t0 = -1))
minke.vonB.par

# fisheries package in R (too many things at the same time) ??
# variables that are not in the data will be estimated

vonB.par <-
  nls(length ~ Linf * (1 - exp(-K * (age - t0))),
      data = cod.ca,
      start = list(Linf = 100, K = 0.1, t0 = -1))
summary(vonB.par)

# confidence intervals
confint(vonB.par)

head(tmp)
confint(vonB.par)
tidy(vonB.par)
tmp <- augment(vonB.par)

ggplot(tmp, aes(age, .fitted)) + 
  geom_line(colour="blue") +
  geom_point(data=cod.ca, aes(age,length), alpha=0.1, colour="red") + 
  geom_hline(yintercept=149, colour="blue") 

# plot residuals vs. age
ggplot(tmp, aes(age, .resid)) + 
  geom_point() 

install.packages("devtools")
library(devtools)
install_github("einarhjorleifsson/wices")  # install wices through github

