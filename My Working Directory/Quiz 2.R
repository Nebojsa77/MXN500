#Quiz 2

#question 2 
#it is a pdf 

#question 4 
B = matrix(c(67, 70, 80, 54, 68, 48, 49, 65, 67), nrow=3, ncol=3) 

chisq.test(B)

#Question 5
A = matrix(c(233, 77, 30, 13, 162, 68, 51, 35), nrow=4, ncol=2)

chisq.test(A)

#question 6 

lf = data.frame(Year = c(0,	10,	20,	30,	40,	50,	60,	70), `Life expectancy` =	c(57.7,	61,	64.5,	66,	68.4,	71.2,	75.6,	78.6))
lf_model <- lm(data=lf, `Life expectancy` ~ Year)
lf_model
summary(lf_model)
summarise(lf, mean= mean(`Life expectancy`))

rest_newdata <- data.frame(Year = 32)
#predict y for x
predict(object = lf_model, newdata = rest_newdata)

#question 10 
#must anwer them with a dollar sign
rest = data.frame(Bill = c(32.98,	106.27,	64.30, 70.29,	97.34, 49.72), Tip =	c(4.5, 16, 7.7, 10, 16, 5.28))

rest_lm <- lm(data=rest, Tip ~ Bill) #lm(data, Y ~ X)      
rest_lm #lm stands for linear model #Volume as a function of Temperature

summary(rest_lm)

rest_newdata <- data.frame(Bill = 85)
#predict y for x
predict(object = rest_lm, newdata = rest_newdata)
