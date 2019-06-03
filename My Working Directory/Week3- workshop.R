#Week 3 Workshop
library(carData)
data(Burt)
view(Burt)
names(Burt)

class(Burt$class)
Burt$class <- factor(Burt$class, levels = c("high", "medium", "low"))
#graphs

ggplot(data=Burt, aes(x=IQbio, y=IQfoster)) +geom_point(aes(colour=class), size=2.5) +labs(x="IQ of biologically raised kids", y="IQ of fostered kids" )+
  theme_bw() 

#inport vessel berths
ves <- read_csv("inport.csv")
names(ves)
dim(ves)
summary(ves)
view(ves)

#counting the number of unique ships, use the n_distinct function

n_distinct(ves$Ship)

#counting each ship at different Berth
group_by(ves, Port) %>%
count(Port)


#tryin to get dates
ves$`Dep Date` <- as.Date(ves$`Dep Date`, format = "%a %d %b %Y") 
ves$`Arr Date` <- as.Date(ves$`Arr Date`, format = "%a %d %b %Y") 
head(ves)

ves <- mutate(ves, waiting = Sys.Date() - `Arr Date`)

#moving onto port dataset

PI <- read_csv('PortInfo.csv')

#cleaning the dataset
names(PI)
head(PI)
dim(PI)

PI <- mutate(PI,`Port Type` = str_replace(`Port Type`, "PortType.", ""))

tail(PI)

#joining the dataframes
names(ves)
names(PI)

### altport <- left_join(PI, ves, by = c("Port Name" = "Port"))

AllPort <- left_join(ves, PI, by = c("Port" = "Port Name"))
names(AllPort)
#moving onto maps

library(ggmap)
qld_map <- get_stamenmap(bbox = c(left = 137, bottom = -30, right = 155, top = -8),   
                         zoom = 4, maptype  = c("terrain-background"))
ggmap(qld_map)

#adding points to the map

ggmap(qld_map)+ geom_point(data = AllPort, aes(x = Longitude, y = Latitude))

#adding Labels
ggmap(qld_map)+ geom_point(data = AllPort, aes(x = Longitude, y = Latitude))+
  geom_text(data = AllPort, aes(x = Longitude, y = Latitude, label = Port), hjust = -0.1, size = 2)

#visualising amount of ships
ggmap(qld_map)+ geom_point(data = AllPort, aes(x = Longitude, y = Latitude))+
  geom_jitter(data = AllPort, aes(x = Longitude, y = Latitude),
  width = 1, height = 1, colour = "blue", size = 0.5)





  


