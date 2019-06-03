library(tidyverse)

# working with weight and heights dataset

hw <- read_csv("HeightWeight.csv")

# BMI <-mutate(hw, BMI = Height/Weight) doesnt work as the measurwements make no sense

#creating a histogram of the spread of height
summarise(hw, mean_hw = mean(Height), sd_hw = sd(Height))

ggplot(data = hw, aes(x=Height)) +geom_histogram(aes(y=..density..), 
                                  colour = "black", fill = "grey", bins = 40) +theme_bw() +
  stat_function(fun = dnorm, args = list(mean = 67.9, sd = 1.94)) 

#getting the quantile

quantile(hw$Height, probs = 0.9)


#getting the height where 10% of the pop is smaller than it

qnorm(p = 0.1, mean = 67.9, sd = 1.94, lower.tail = T)

#getting the height where 10% of the pop is taller than it

qnorm(p = 0.1, mean = 67.9, sd = 1.94, lower.tail = F)

qnorm(p = 0.5, mean = 67.9, sd = 1.94, lower.tail = T)

#for what proportion of people would we expect to see a height less than 63 inches

pnorm(q = 63, mean =67.9, sd = 1.94 )

#calcing the upper tail
pnorm(q = 64, mean =67.9, sd = 1.94, lower.tail = F )

#chances of having our max height
max(hw$Height)
pnorm(q = 73.9, mean =67.9, sd = 1.94, lower.tail = F )

#probability of having someone with a height of zero
pnorm(q = 0, mean =67.9, sd = 1.94)

#simming 100 values for a standard curve
set.seed(113)

sim <- data.frame(x = rnorm(n=100, mean=0, sd=1))

#plotting it
ggplot(data = sim, aes(x=x)) +geom_histogram(aes(y=..density..), 
                              colour = "black", fill = "grey", bins = 30) +theme_bw() +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1)) 

#checking to see of it is normally distributed using quantile-quantile plots

ggplot(data=sim, aes(sample=x)) + stat_qq(alpha = 0.4) +
  geom_abline(intercept = 0, slope = 1) + theme_bw() + coord_equal()

#STANDARDISATION
standardise <- function(x){return( (x-mean(x))/sd(x) )}

hw <- mutate(hw, Height.std = standardise(Height))

#working wiht our standardized Height data
ggplot(data = hw, aes(x=Height.std)) +geom_histogram(aes(y=..density..), 
                                      colour = "black", fill = "grey", bins = 40) +theme_bw() +
  stat_function(fun = dnorm, args = list(mean = 0, sd = 1)) 

#histogram
ggplot(data=hw, aes(sample=Height.std)) + stat_qq(alpha = 0.3) +
  geom_abline(intercept = 0, slope = 1) + theme_bw() + coord_equal()

#quantile-quantile plot
ggplot(data=hw, aes(sample=Height.std)) + stat_qq(alpha = 0.3) +
  geom_abline(intercept = 0, slope = 1) + theme_bw() + coord_equal()


sim <- data.frame(x = rnorm(n=100000, mean=0, sd=1))

ggplot(data=sim, aes(sample=x)) + stat_qq(alpha = 0.1) +
  geom_abline(intercept = 0, slope = 1) + theme_bw() + coord_equal()



#Dice function from last week
dice <- function(obvs){
  return(round(runif(n = obvs, min = 1, max = 6)))}


