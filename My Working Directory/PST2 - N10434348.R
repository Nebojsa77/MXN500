#PST 2
pst <- read_csv("PST2data.csv")

#load Libraries
library(tidyverse)
library(GGally)
library(broom)
#filter out my student N
pst <- filter(pst, Student != "n10434348")
#getting rid of student Col
pst$Student <- c()

#Section A - Data and Visualisation (20%)
#A1 Data Structure (10%)

#getting min and maxes
summary(pst)

#Creating TimeSince Col
pst$TimeSince <- pst$Year - 1970

#A2 Graphocal Summaries
ggpairs(pst) + theme_bw()

#B1 
#Time VS Clock Speed
#no log
#ggplot(pst, aes(x=TimeSince, y = `Clock (MHz)`)) +geom_point() +
#  geom_smooth(method = lm) + theme_bw() +labs(x = "Time Since 1970", y = "log(Clock(MHz))", title = "Clock Speed Over Time")

#y & x log
#ggplot(pst, aes(x=log(TimeSince), y = log(`Clock (MHz)`))) +geom_point() +
#  geom_smooth(method = lm) + theme_bw() +labs(x = "Time Since 1970", y = "log(Clock(MHz))", title = "Clock Speed Over Time")

#ylog
ggplot(pst, aes(x=TimeSince, y = log(`Clock (MHz)`))) +geom_point() +
  geom_smooth(method = lm) + theme_bw() +labs(x = "Time Since 1970", y = "log(Clock(MHz))", title = "Clock Speed Over Time")

#B2 Linear Model (10%)
clock_lm <- lm(data = pst, log(`Clock (MHz)`) ~ TimeSince)
tidy(clock_lm, conf.int = 95)

#getting R^2
glance(clock_lm)

#B3 Analysis of Residuals (10%)
clock_fort <- fortify(clock_lm)
ggplot(data = clock_fort, aes(x= .fitted, y = .resid)) + labs(y= "Residuals", x= "Fitted", title = "Fitted VS Residuals") +geom_point() +geom_smooth() +theme_bw()

#plotting how residuals vary with power density
clock_fort$Power <- pst$`Power Density`
ggplot(data = clock_fort, aes(x= Power, y = .resid)) + labs(x= "Power Density", y= "Residuals", title = "Residuals VS Power Density") +geom_point() +geom_smooth() +theme_bw()

#comparing standardised residuals to a normal curve
ggplot(data = clock_fort, aes(sample = .stdresid)) +
  stat_qq() + geom_abline() +theme_bw() + labs(x= "Theoretical", y= "Sample", title = "Plot for Checking Normality of Errors")

#Section C - Advanced Regression (30%)
#C1 Multiple Explanatory Variables (10%)

clock_lm2 <- lm(data=pst, log(`Clock (MHz)`) ~ TimeSince + `Power Density`)
tidy(clock_lm2, conf.int = 95)
glance(clock_lm2)

#C2 Residual Analysis (10%)
#checking residuals
clock_fort2 <- fortify(clock_lm2)
ggplot(data = clock_fort2, aes(x= .fitted, y = .resid)) + labs(y= "Residuals", x= "Fitted", title = "Fitted VS Residuals") +geom_point() +geom_smooth() +theme_bw()

#Checking normality of stdresid
ggplot(data=clock_fort2, aes(sample=.stdresid)) +
  stat_qq(geom="point") + geom_abline() + labs(x= "Theoretical", y = "Sample", title = "Normality of Standardised Residuals") +
  coord_equal() + theme_bw()

#C3 Model Choice (10%)

anova(clock_lm, clock_lm2)
