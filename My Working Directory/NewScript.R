library(tidyverse)
fish_data <- read_csv("fish.csv")
head(fish_data)
tail(fish_data)
dim(fish_data)
names(fish_data)

#display only the Weights of Fish
fish_data$Weight            #OR
pull(fish_data, Weight)

summarise(fish_data, mean.Weight = mean(Weight, na.rm = T))
summarise(fish_data, median.Weight = median(Weight, na.rm = T))

#get the names of each species of fish in dataset, personally Distinct looks nicer
distinct(fish_data, Species)        #OR
unique(fish_data$Species)

#count how many of each species, count will group by variable specified for the count, but can store this into another dataframe
fishcounts <- count(fish_data, Species)
summarise (fishcounts, median.count = median(n),
           Min.Count = min(n),
           Max.Count = max(n))
#summary of fish Length3

summarise(fish_data, min.length = min(Length3, na.rm=T),
          median.length = median(Length3, na.rm=T),
          max.length = max(Length3, na.rm=T))

#creating a subset of a variable
fishsubset <- filter(fish_data, Species == "Bream")

summarise(fishsubset, Mean.Length = mean(Length3))




