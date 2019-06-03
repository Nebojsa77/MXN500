#Workshop 10

sleep <- sleep
help(sleep)
#extra = hours of sleep
#group = drug given
#Id = patient ID

ggplot(sleep, aes(x=ID, y=extra)) +geom_point() +theme_bw()

ggplot(sleep, aes(x=ID, y=extra, colour = group)) +geom_point() +theme_bw()

#boxplot
ggplot(sleep, aes(x=group, y=extra)) + geom_boxplot() +
  theme_bw() + coord_flip()

#summary stats for each group
sleep.group <- group_by(sleep, group)
summarise_at(sleep.group,
             vars(extra),
             funs(Mean = mean, SE = std.error, Min = min, Max = max))

#TS for each group
t.s.1 <- (.75-0)/(0.566)
t.s.2 <- (2.33)/(0.633)

group1 <- filter(sleep, group==1)
group2 <- filter(sleep, group ==2)
t.test(x = sleep.group$extra, alternative = "two.sided", mu = 0, )
t.test(x = group2$extra, alternative = "two.sided", mu = 0, )

#activity 2
#fitting the linear model
drug_lm <- lm(data = sleep, extra ~ group)
tidy(drug_lm)
summary(drug_lm)

#activity 3 

chick <- ChickWeight
help("ChickWeight")
summary(chick)
count(chick)

#different chicks
n_distinct(chick$Chick)

#different diets
n_distinct(chick$Diet)

#displayig weight of chicks over time
ggplot(chick, aes(x=Time, y=weight)) +geom_point(alpha=0.5) + theme_bw() +geom_smooth()
ggplot(chick, aes(x=Time, y=weight, colour = Diet)) +geom_point(alpha=0.5) + theme_bw() +geom_smooth(se=F)

c_group <- group_by(chick, Chick)
ChickEnd <-  filter(c_group,Time == max(Time))
ChickStart <- filter(c_group, Time == min(Time))

#renaming colums
ChickEnd$TimeEnd <- ChickEnd$Time
ChickEnd$Time <- c()

ChickStart$TimeStart <- ChickStart$Time
ChickStart$Time <- c()

ChickEnd$WeightEnd <- ChickEnd$weight
ChickEnd$weight <- c()

ChickStart$WeightStart <- ChickStart$weight
ChickStart$weight <- c()

#Joining the datata

ChickFull <- inner_join(ChickStart, ChickEnd, by = c("Diet", "Chick"))

#adding a column
AverageWeight <- mutate(ChickFull, )