#PST 1
rm(list=ls())
#After setting working directory, load my packages
library(tidyverse)
library(forcats)
library(broom)
library(goftest)
#SET UP
#need to use read.csv so that it utomatically factors my stuff, this will group it together instead of it being characters
random <- read.csv("PST1Random.csv") 
my_origin <- filter(random, Username == "n10434348")
view(my_origin)
#my origin is SLC

data <- read.csv("PST1Data.csv") 
my_data <- filter(data, Origin == "SLC")

#removing Origin and OriginStateName variables
my_data[ ,c('Origin', 'OriginStateName')] <- list(NULL)

airport_codes <- read.csv("PST1AirportCodes.csv")
airport_codes <- rename(airport_codes, Dest = Airport.Code )
data_codes <- inner_join(my_data, airport_codes, by = "Dest")

airline_codes <- read.csv("PST1AirlineCodes.csv")
airport_codes <- rename(airport_codes, Reporting_Airline = Airline.Code )
colnames(airline_codes)[colnames(airline_codes)=="Airline code"] <- "Reporting_Airline"
all_data <- inner_join(data_codes, airline_codes, by = "Reporting_Airline")

#SECTION A1 DATA STRUCTURE
#getting the classes
summary(all_data)

#changing into the right classes
all_data$FlightDate <- as.Date(all_data$FlightDate, "%d/%m/%y")

#Summary stats on departure delays, need to clean the NA values first
all_clean_data <- na.omit(all_data)
Dep <- summarise(all_clean_data, mean = mean(DepDelay), median = median(DepDelay), sd = sd(DepDelay), n=n())

#grouping by airline
airline_group <-group_by(all_clean_data, `Airline name`)
summarise(airline_group, mean = mean(DepDelay), median = median(DepDelay), sd = sd(DepDelay), n=n())
airline_group_sum <- summarise(airline_group, mean = mean(DepDelay), median = median(DepDelay), sd = sd(DepDelay), n=n())
#then i just need to order by n in the dataframe to select the top 5 airlines


##SECTION A2 GRAPHICALLY EXCELLENT STUDIES
#relationship between departure delay and arrival delay
ggplot(all_clean_data, aes(x = DepDelay, y = ArrDelay)) +geom_smooth(method = "lm") + geom_point(colour = "black", alpha = 0.3) +
  labs(x = "Departure Delay (min)", y = "Arrival Delay (min)", title = "Departure Delay VS Arrival Delay") + 
  theme_bw()

ggplot(all_clean_data, aes(x = DepDelay, y = ArrDelay)) +geom_point(colour = "black", alpha = 0.3) + geom_smooth(method = "lm") +
  xlim(-20, 100) + ylim(-55, 200) +
  labs(x = "Departure Delay (min)", y = "Arrival Delay (min)", title = "Departure Delay VS Arrival Delay") + theme_bw()

#showing the distribution of Departure Delays
ggplot(data = all_clean_data, aes(x = DepDelay)) +geom_histogram(bins = 30, colour = "black", fill = "grey") +
  scale_y_log10() + labs(x = "Departure Delay (min)",  title = "Distribution of Departure Delay", y = "Count" )+ theme_classic()


ggplot(data = all_clean_data, aes(x = DepDelay)) +geom_histogram(bins = 50, colour = "black", fill = "grey") +
  xlim(-20,100) + scale_y_log10() + labs(x = "Departure Delay (min)", y = "Count", title = "Distribution of Departure Delay" )+ theme_classic()

#Showing the distribution of Arrival Delays
ggplot(data = all_clean_data, aes(x = ArrDelay)) +geom_histogram(bins = 30, colour = "black", fill = "grey") +
  scale_y_log10() + labs(x = "Arrival Delay (min)", y = "Count", title = "Distribution of Arrival Delay" )+ theme_classic()


ggplot(data = all_clean_data, aes(x = ArrDelay)) +geom_histogram(bins = 40, colour = "black", fill = "grey") +
  xlim(-50,100) + scale_y_log10() + labs(x = "Arrival Delay (min)", y = "Count", title = "Distribution of Arrival Delay" )+ theme_classic()

#showing how Departure Delay varies by airline
lumped_data <-all_clean_data #creating a new dataset incase i dont want to keep my data lumped
lumped_data$`Airline name` <- fct_lump(lumped_data$`Airline name`, n = 3)

#Histogram
ggplot(lumped_data, aes(x = DepDelay)) +geom_histogram(colour = "black", fill = "grey") + 
  xlim(-20,20) + facet_wrap(~`Airline name`) + labs(y = "Count", x = "Departure Delay (min)", title = "Departure Delay By Airline") +theme_bw()

#showing how arrival delay is affected by destination
lumped_data$Dest <- fct_lump(lumped_data$Dest, n = 3)

ggplot(lumped_data, aes(x = ArrDelay)) +geom_histogram(colour = "black", fill = "grey") + 
  xlim(-40,40) + facet_wrap(~Dest) + scale_y_log10() +
  labs(y = "Count", x = "Arrival Delay (min)", title = "Arrival Delay By Destination") +theme_bw()


#SECTION B1

#SETUP  
all_clean_data$Arr_Cat <- cut(all_clean_data$ArrDelay, breaks = c(-Inf, 0, Inf), labels = c("Not Late", "Late"))
two_airlines <- filter(all_clean_data, `Airline name` == c("Delta Air Lines" , "SkyWest Airlines"))

#summariseing for SkyWest
two_airlines_s <- filter(two_airlines, `Airline name` == "SkyWest Airlines")
summary(two_airlines_s$Arr_Cat)

#summarising for Delta
two_airlines_d <- filter(two_airlines, `Airline name` == "Delta Air Lines")
summary(two_airlines_d$Arr_Cat)

#SECTION B2
#Getting the TS and DF
observed <- table(two_airlines$`Airline name`, two_airlines$Arr_Cat)
colnames(observed) <- c("Not Late", "Late")
observed

total_airline <- summarise(group_by(two_airlines,`Airline name`), count = n()) 
total_arrival <- summarise(group_by(two_airlines,Arr_Cat), count = n()) 

total_airline <- matrix(total_airline$count) #singles out the counted values and converts to matricies
total_arrival <- matrix(total_arrival$count) #singles out the counted values and converts to matricies

expected <- data.frame(total_airline%*%t(total_arrival)/sum(total_airline))
colnames(expected) <- c("Not Late", "Late")
expected

round(expected,2) #Here we are only rounding to make it nicer to look at.
contribution <- (observed - expected)^2/expected
round(contribution,2)

#the test statistic
ts <- sum(contribution)
ts

#the DF
df  <- (nrow(observed)-1)*(ncol(observed)-1)
df

#pval
pval <- pchisq(q = ts, df = df, lower.tail = F) #chisq is the code for X^2 distribution
pval #always want upper tail for CHI-SQ


#SECTION C - LINEAR MODEL

#creating a linear model
delay_lm <- lm(data=all_clean_data, ArrDelay ~ DepDelay) #lm(data, Y ~ X)
delay_lm

#Glance from broom pacakage gives a useful summary
glance(delay_lm)

#running summary on Linear Models gives alot of info
delay_summ <- summary(delay_lm)
delay_summ

#tidy gves mor info on our parameters
delay_parameters <- tidy(delay_lm, conf.int = TRUE, conf.level = 0.95)
delay_parameters

#REGRESSION ASSUMPTIONS
#fitted function extracts fitted values
y.hat <- fitted(delay_lm)

#residuals gives us the error
epsilon <- residuals(delay_lm)

#visualising residuals
delay_fort <- fortify(delay_lm)
round(delay_fort, 4)

#Plotting the Residuals
ggplot(data=delay_fort, aes(x=.fitted, y=.resid)) + geom_point(alpha = 0.2) + 
  geom_smooth() +
  theme_bw() + xlab("Fitted values") + ylab("Residuals") + labs(title = "Homogeneity of Error")

#getting a better look at the majority of our data
ggplot(data=delay_fort, aes(x=.fitted, y=.resid)) + geom_point(alpha = 0.5) + geom_smooth() +
  theme_bw() + xlab("Fitted values") + ylab("Residuals") + labs(title = "Homogeneity of Error") + xlim(-50, 400)

#checking assumptions, yes they are independant

#checking normality
ggplot(data = delay_fort, aes(sample = .stdresid)) +
  stat_qq() + geom_abline() +theme_bw() +labs(x= "Theoretical", y= "Sample", title = "Plot for Checking Normality")

#plotting the ECDF's
delay_ecdf <- ecdf(delay_fort$.stdresid)

ggplot(data.frame(x = c(-3,3.5)), aes(x = x))+
  stat_function(fun = delay_ecdf, aes(colour = 'ecdf'), size = 1 )+
  stat_function(fun = pnorm, args = list(mean = 0, sd = 1), aes(colour = 'cdf'), size = 1) +
  scale_colour_manual(name='', values=c('ecdf'='blue', 'cdf'='red')) + labs(title = "Residuals ecdf VS cdf") +
  theme_classic() 

# performing a KS test to test normality
cdf <- function(x){return(pnorm(q = x, mean = 0, sd = 1))}
ks.test(x = delay_fort$.stdresid, y = "cdf")
#cant use model as it residuals are not normally distributed

#the range limits of our model
min(all_clean_data$ArrDelay)
max(all_clean_data$ArrDelay)
