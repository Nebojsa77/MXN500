#Lab 5

#Normal Distributions

#BMI Q's
bmi <- data.frame(x = rnorm(n=150, mean=26, sd=4))
#Q 1
ggplot(data = sims, aes(x)) +
  stat_function(fun = dnorm, args = list(mean = 26, sd = 4)) + xlim(14, 38) + theme_bw()

#probabiity that rand pers is < 18.5

pnorm(q=18.5, mean = 26, sd = 4) ## 30% chance


#between 18.5 and 25
pnorm(q=25, mean = 26, sd = 4, lower.tail = T) - pnorm(q=18.5, mean = 26, sd = 4, lower.tail = T)
## 37%

#more than 30 BMI
pnorm(q=30, mean = 26, sd = 4, lower.tail = F) ## 15%


#height of students
sqrt(25)

# m = 165, SD = 5
height <- data.frame(x = rnorm(n=150, mean=165, sd=5))

top5 <- qnorm(p=0.05, mean = 165, sd = 5, lower.tail = F)
#top = 173.2

bot5 <- qnorm(p=0.05, mean = 165, sd = 5, lower.tail = T)
#bot 5 = 156.8

ggplot(data = height, aes(x), y = ..density..) + 
  stat_function(fun = dnorm, args = list(mean = 165, sd = 5)) +
  geom_vline(xintercept = bot5, color = "red") +   geom_vline(xintercept = top5, color = "red") + theme_bw()


#for assessment is very hard to assess things we need R for. 
#things likely to be on the exam are things that we can do on pen and paper.

pnorm(q=11, mean = 20, sd = 6, lower.tail = F) - pnorm(q=25, mean = 20, sd = 6, lower.tail = F)

