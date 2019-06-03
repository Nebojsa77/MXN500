library(tidyverse)

#plotting the X^2 function with different DF
ggplot(data.frame(x = c(0,10)), aes(x = x))+
  stat_function(fun = dchisq, args = list(df = 2), colour = 'red', size = 2)+
  stat_function(fun = dchisq, args = list(df = 3), colour = 'blue', size = 2)+
  stat_function(fun = dchisq, args = list(df = 5), colour = 'green', size = 2)+
  theme_bw() 

#reading in the data
students <- read.csv('Students.csv')

#filterig out unknowns as X^2 wont work with them
students <- filter(students, SES_QUARTILE != "Unknown", DESTINATION != "Unknown")

#to see if they are of class factor or not
summary(students)

#need to change from character to factor so that R knows they are categorical variables
students$SES_QUARTILE <- factor(students$SES_QUARTILE,
                                levels = c("High", "Medium","Low - Medium", "Lowest"))
students$DESTINATION <- factor(students$DESTINATION,
                               levels = c("Bachelor degree", "Employed","Certificates/diplomas",
                                          "Apprenticeship/traineeship", "Looking for work","NILFET"))
#summary STATS
summary(students$SES_QUARTILE)
summary(students$DESTINATION)

#plot to see if there is a relationship between SES and what students do after HS
ggplot(data = students, aes(x=DESTINATION, fill = factor(SES_QUARTILE))) +
  geom_bar(position = "dodge") + # using bar over hist cause they are categorical var not continuous
  scale_x_discrete(name = "Destination",
                   labels = c("Bach", "Emp","Cert/dip","App","LFW","NILFET")) +
  scale_fill_discrete(name = "Socio-Economic Status") +
  theme_classic()

#turning our data into a two way table
observed <- table(students$SES_QUARTILE,students$DESTINATION) #creates the table but we still need to assign colnames
colnames(observed) <- c("Bach","Emp","Cert/dip","App/train", "LFW","NILFET")
observed

#before we can get expected we need to get row and col totals
total_ses <- summarise(group_by(students,SES_QUARTILE), count = n()) #creates dataset that has counts of SES
total_dest <- summarise(group_by(students,DESTINATION), count = n()) #creates dataset that has counts of DEST

total_ses <- matrix(total_ses$count)   #singles out the counted values and converts to matricies
total_dest <- matrix(total_dest$count) #singles out the counted values and converts to matricies

expected <- data.frame(total_ses%*%t(total_dest)/sum(total_ses)) #converts everything back into a DF after using the shortcuts we get from matricies
#the t represents the transpose, i.e. flipping total_dest on its side into a row from column
# %*% means we do matrix multiplucation instead of normal

round(expected,2) #Here we are only rounding to make it nicer to look at.

contribution <- (observed - expected)^2/expected
round(contribution,2) #Again, just to make it easier to read

#calcing contribution to test statistic
ts <- sum(contribution)
ts

#getting the degrees of Freedom
df  <- (nrow(observed)-1)*(ncol(observed)-1)
df

#finding p value
pval <- pchisq(q = ts, df = df, lower.tail = F) #chisq is the code for X^2 distribution
pval

#running Pearsons Chi-aquared test in r
Xsq <- chisq.test(observed) #observed has to be in our wide format
Xsq

#verifying our expected counts
Xsq$expected

#working with the Diuce dataset
dice <- read_csv('Dice.csv')
ggplot(data = dice, aes(x = Roll))+
  geom_histogram(fill = 'coral', colour = 'black')+
  theme_bw()

#testing to see if the dice is fair
observed <- table(dice$Roll) #the observed values
expected <- rep(x = 0.25*50, 4) #the expected values if it was accurately reped by distribution or PMF

contribution <- (observed - expected)^2/expected
round(contribution,2) #Again, just to make it easier

ts <- sum(contribution)#remeber the TS is the SUM of (observed − expected)^2 / expected
ts

df <- (nrow(observed)-1) # df = k-1    #i.e. levels of cat var - 1
df

pchisq(q = ts, df, lower.tail = F)

chisq.test(x = observed) #is observed as needs to be a table and not a dataframe

#working with CDF's
set.seed(600)
rand_data <- data.frame( x= round(runif(10),2))
orig_rand <- rand_data
rand_data$x

#i havent ordered my data so it isnt accurate
rand_data$cumulative <- cumsum(rep(0.1, 10))
rand_data <- rand_data[order(-rand_data$cumulative),]
rand_data <- rand_data[!duplicated(rand_data$x),]
rand_data <- rand_data[order(rand_data$cumulative),]
t(rand_data)

#kolmogorov Smirnov test with IQ data
IQ <- read_csv("IQ.csv")

IQ_ecdf <- ecdf(IQ$IQ)
IQ_ecdf

ggplot(data.frame(x = c(70,140)), aes(x = x))+
  stat_function(fun = IQ_ecdf, colour = 'blue', size = 2)+
  stat_function(fun = pnorm, args = list(mean = 100, sd = 15), colour = 'red', size = 2)+
  theme_bw()

#simply performing the KS test
p_cdf <- function(x){return(pnorm(q = x, mean = 100, sd = 15))}
ks.test(x = IQ$IQ, y = "p_cdf")

#moving onto the Anderson-Darling test
library(goftest)
ad.test(IQ$IQ, null = "pnorm", mean = 100, sd = 15)
