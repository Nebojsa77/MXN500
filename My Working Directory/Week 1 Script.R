df <- read_csv("granite.csv")

head(df)

airq <- airquality
view(airq)


weights <- c(55, 68,60,52,65,64,70,61,63,75)
mean(weights)

mean(df$Angle)

sort(weights, decreasing = T)

#Airquality ANALYSIS
head(airq$Month)
tail(airq)
dim(airq)
ncol(airq)
nrow(airq)
table(airq$Month)

summarise(airq, Mean.Ozone = mean(Ozone, na.rm=T))

airq.may <- filter(airq, Month ==5)

table(airq.may$Month)
count(airq.may, Month)
airq.may$Ozone
summarise(airq.may, Mean.Ozone = mean(Ozone, na.rm=T),
          sd.Ozone = sd(Ozone, na.rm = T))

airq.month <- group_by(airq, Month)
summarise(airq.month, mean.Ozone = mean(Ozone,na.rm=T),
          Ozone.sd = sd(Ozone, na.rm=T),
          Ozone.median = median(Ozone, na.rm=T))

ldeaths <- data.frame(matrix(ldeaths, ncol=12, byrow=T))
names(ldeaths) <- month.abb
ldeaths <- mutate(ldeaths, Year = 1974:1979)

ldeaths.long <- gather(ldeaths, key=Month, value = Deaths,
                       -Year)
