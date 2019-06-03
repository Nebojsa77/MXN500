#Lab 1
#load Library
library(tidyverse)

#Read in file
eco<- read_csv("ecology.csv")

#Get Dimensions
dim(eco)
names(eco)

#pull observations
eco$pH[7]
eco$Temp[0:7]
eco$Temp

#get moderate aquatic life in Spine

eco.pine <- filter(eco, Location == "South Pine River")
table(eco.pine$AquaticLife)

#cross tabling location and SuscErosion
eco.group <- group_by(eco, Location, SuscErosion)
count(eco.group)

#mean for whole dataset

summarise(eco, mean.temp = mean(Temp))

summarise(eco, mean.temp = mean(Location))

#location turbidity
eco.groupl <- group_by(eco, Location)

summarise(eco.groupl, mean.turbidity = mean(Turbidity),
          sd.temp = sd(Temp))

eco.group <- select(eco, Location, SuscErosion)
table(eco.group)
