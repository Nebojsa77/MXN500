#Workshop 7 - Hypothesis Testing
survey = data.frame(Type = c("Auto", "Manual", "Auto", "Manual"),
                    `Likes Driving` = c("Yes", "Yes", "No", "No"), 
                    Count = c(10, 4, 4, 0))

class(survey$Count)
#need to convert to long for Fisher

# The arguments to spread():
# - data: Data object
# - key: Name of column containing the new column names
# - value: Name of column containing values

survey_wide <- spread(survey, Type, Count)

rownames(survey_wide) <- survey_wide$Likes.Driving
survey_wide$Likes.Driving <- c()
fisher.test(as.matrix(survey_wide), alternative = "greater")


#ACTIVITY 2

#counting Lollies for the Chi-Square Goodness-of-fit test
lollies <- data.frame( Colour = c("Yellow", "Red", "Blue", "Purple"), Count = c(4,3,3,3))

#need to convert to wide
lol_wide <- spread(lollies, Colour, Count)

#doesnt meet expectations but yolo
chisq.test(lol_wide) #observed has to be in our wide format


#ACTIVITY 3
#train data
tw <- read_csv("TrainWait.csv")

#plot to show the variability 
ggplot(data = tw, aes(x = `Waiting Time`)) +
  geom_histogram(fill = "grey", colour = "black") +
  theme_classic()

#plotting a uniform CDF with [0,15]
ggplot(data = data.frame(x = c(0,15)), aes( x= x))+
  stat_function(fun = "punif", args = list(min = -1, max = 16), size = 1)+
  theme_bw()

#using ecdf to plot data

ordered_tw <- order(tw$`Waiting Time`)

tcdf <-ecdf(tw$`Waiting Time`)

#Plotting the ECDF
ggplot(data.frame(x = c(0,15)), aes(x = x))+
  stat_function(fun = "punif", args = list(min = 0, max = 15), size = 1)+
  stat_function(fun = tcdf, size = 1)+
  theme_bw()

#making unif func
p_cdf <- function(x){return(punif(q=x, min = 0, max = 15))}

#doing ks test
ks.test(x = tw$`Waiting Time`, y = "p_cdf")

#performing an AD test
library(goftest)
ad.test(tw$`Waiting Time`, null = "punif", min = 0, max = 15)

#ACTIVITY 4
chisq.test(survey_wide)


