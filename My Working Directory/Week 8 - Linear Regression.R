#LINEAR REGRESSION
charles <- read_csv("charles.csv")

ggplot(data = charles, aes(x= Temperature, y = Volume)) +geom_smooth(method = "lm") + geom_point() + 
  theme_bw() 

#creating a linear model
charles_lm <- lm(data=charles, Volume ~ Temperature) #lm(data, Y ~ X)
charles_lm #lm stands for linear model #Volume as a function of Temperature

#Glance from broom pacakage gives a useful summary
library(broom)
glance(charles_lm) #sigma here is the best estimated for residuals

#running summary on Linear Models gives alot of info
charles_summ <- summary(charles_lm)
charles_summ

#tidy gves mor info on our parameters
charles_parameters <- tidy(charles_lm)
charles_parameters

#pass arguments into tidy so that it calcs confidence levels for us
charles_parameters <- tidy(charles_lm, conf.int = TRUE, conf.level = 0.95)
select(charles_parameters, term, estimate, conf.low, conf.high)

#fitted function extracts fitted values
y.hat <- fitted(charles_lm)
y.hat

#residuals gives us the error
epsilon <- residuals(charles_lm)
epsilon

#visualising residuals
charles_fort <- fortify(charles_lm)
round(charles_fort, 4)

#its common to show the residuals
ggplot(data=charles_fort, aes(x=.fitted, y=.resid)) + geom_point() +
  theme_bw() + xlab("Fitted values") + ylab("Residuals")

#iris data

ggplot(data = iris, aes(x = Petal.Length, y = Petal.Width)) + geom_smooth(method = "lm") + #method = lm makes it a linear line
  geom_point(aes(colour = Species)) +
  scale_x_continuous(limits = c(-1, 7)) + theme_bw()

iris_lm <- lm(data = iris, Petal.Width ~ Petal.Length)
iris_lm
summary(iris_lm)
tidy(iris_lm, conf.int = TRUE, conf.level = 0.95)
#if conf interval doesnt cover zero we can reject the null hypothesis.

#analyseing our residuals
#can investigate unexplained variability by running one as a colour
iris_fort<- fortify(iris_lm)
iris_fort$Species <- iris$Species 
ggplot(data = iris_fort, aes(x= .fitted, y = .resid)) + geom_point(aes(colour = Species)) +geom_smooth() +theme_bw()


#Checking other assumptions
#checking normality density is useful here,
ggplot(data = iris_fort, aes(x = .resid)) +geom_histogram(aes(y = ..density..)) +
  stat_function(fun = "dnorm", 
                args = list(mean = mean(iris_fort$.resid), sd = sd(iris_fort$.resid)), size = 2)

ggplot(data = iris_fort, aes(sample = .stdresid)) +
  stat_qq() + geom_abline() +theme_bw()
#Belinda says this may not be normally distributed
#therefore need to do AD test

library(goftest)
ad.test(iris_fort$.stdresid, null = "pnorm", mean = 0, sd = 1)
