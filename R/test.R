#Making a crayola of the North Sea plaice survey

library(dplyr)
library(tidyr)
library(RColorBrewer)
library(ggplot2)

x <- readLines("DATA/pleiv fleet.txt")
x1 <- x[7:17]
write(x1, "garbage")
x1 <- read.table("garbage", sep = "\t", header = FALSE)
x2 <- x[22:40]
write(x2, "garbage")
x2 <- read.table("garbage", sep = "\t", header = FALSE)
x <- rbind(x1,x2)
colnames(x) = c("dummy",1:9)
x$year <- c(1985:2014)
d <- x[,2:ncol(x)]
d <- gather(d, #df
            key=age, #names of new variables
            value=number, # values
            1:9) #apply to what columns

source("R/crayola.r")

crayola(d,"MyTitle")
