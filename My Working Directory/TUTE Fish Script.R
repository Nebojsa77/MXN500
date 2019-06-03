fish<- read_csv("fish.csv")
head(fish)
tail(fish)
dim(fish)
names(fish)
distinct(fish, Species)
unique(fish$Species)
pull(fish, Weight)
fish$Weight
summarise(fish, mean.weight = mean(Weight, na.rm=T))
summarise(fish, median.weight = median(Weight, na.rm=T))

#calculating to number of observations per species
fish.count <- count(fish, Species)
summarise(fish.count, median.count = median(n))
summarise(fish.count, min.count = min(n))
summarise(fish.count, max.count = max(n))

#fish length summary stats
summarise(fish, min.length = min(Length3), median.length = median(Length3), max.length = max(Length3))

#filter only bream
fish.sub <- filter(fish, Species == "Bream")

#length only bream
summarise(fish.sub, mean.length = mean(Length3))

#group by species and sex
fish.group <- group_by(fish, Species, Sex)

fish.gm <- summarise(fish.group, mean.l = mean(Length3))

#spreading the data into key values
spread(fish.gm, key=Sex, value=mean.l)


#standard deviation of group, in order to arrange this in the future need to make a new variable
fish.gsd <- summarise(fish.group, sd.l = sd(Length3, na.rm=T))

#arrange dataset into accending sd
arrange(fish.gsd, sd.l)
arrange(fish.gsd, desc(sd.l))

#doing multiple summaries
fish.gsum <- summarise(fish.group, `Mean Length` =mean(Length3), `Standard Deviation` = sd(Length3), n=n())
fish.gsum


fishgsum.weight <- summarise(fish.group, mean.Weight =mean(Weight), Standard.Deviation = sd(Weight), n=n())

#writing out my results

write_csv(x = fish.gsum, path = "Fish Summarry.csv")






