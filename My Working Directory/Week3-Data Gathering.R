hml <- data.frame(category = c("High", "Medium", "Low"))
summary(hml)

class(hml$category)

hml$category <- factor(hml$category, levels = c("Low", "Medium", "High"))

summary(hml)
class(hml$category)

#doing my own

htb <- data.frame(category = c("Hello", "There", "Bud"))
summary(htb)

htb$category <- factor(htb$category, levels = c("Hello", "There", "Bud"))
summary(htb)
class(htb$category)

#numbers in data frame

num <- data.frame(numbers = c("3.14", "1500", "-12"))
summary(num)
class(num$numbers)

num$numbers <- as.numeric(num$numbers)
summary(num)
class(num$numbers)
#obviuosly dont need to do this, can just omit the strings and they will be treated as numbers

#Data Wrangling

library(tidyverse)
library(Lahman)
data(Batting)

#Filter so it only contsains data for manny ramirez

manny <- filter(Batting, playerID == "ramirma02")
view(manny)
dim(manny)
names(manny)

manny %>% summarise(
  span = paste(min(yearID), max(yearID), sep = "-"),
  numYears = n_distinct(yearID),
  numTeams = n_distinct(teamID),
  BA = sum(H)/sum(AB),
  tH = sum(H), tHR = sum(HR), tRBI = sum(RBI))

#team performnce without pipe

manny_g <- group_by(manny, teamID)
summarise(manny_g, 
          span = paste(min(yearID), max(yearID), sep="-"), #paste gives us a character output
          numYears = n_distinct(yearID),
          numTeams = n_distinct(teamID),
          BA = sum(H)/sum(AB), 
          tH = sum(H), 
          tHR = sum(HR), 
          tRBI = sum(RBI))

#seeing how manny performed in each team

manny %>%
  group_by(teamID) %>%
  summarise(
    span = paste(min(yearID), max(yearID), sep = "-"),
    numYears = n_distinct(yearID),
    numTeams = n_distinct(teamID),
    BA = sum(H)/sum(AB),
    tH = sum(H), tHR = sum(HR), tRBI = sum(RBI)) %>%
  arrange(span)


Master %>% filter(nameLast == "Ramirez" & nameFirst == "Manny")

#inner join
df_inner <- Batting %>% 
  filter(playerID == "ramirma02") %>%
  inner_join(Master, by = c("playerID"="playerID"))

#left join
df_left <- Batting %>% 
  filter(playerID == "ramirma02") %>%
  left_join(Master, by = c("playerID"="playerID"))

#left join gives at least as many rows as there are in the orginal dataset
df_left2 <- filter(Batting, playerID == "ramirma02"), by= c("playerID" = "playerID"))
