#workshop 6 for real tho

library(tidyverse)
library(plotrix)

deaths <- read_csv("deaths.csv") 
#Activity 2 summarising
age.deaths <- summarise(deaths, xbar = mean(Age), s = sd(Age), n = n(), ste = std.error(deaths$Age))
#Activity 3 CI's
age.deaths<- mutate(age.deaths, q975 = qt(p = 0.975, df = n - 1))


ci_95 <- mutate(age.deaths, L = xbar + q025 * age.deaths$ste, U = xbar + q975 * age.deaths$ste)

#the 90% CI's
age.deaths<- mutate(age.deaths, q95 = qt(p = 0.95, df = n - 1))
age.deaths<- mutate(age.deaths, q05 = qt(p = 0.05, df = n - 1))

ci_90 <- mutate(age.deaths, L = xbar + q05 * age.deaths$ste, U = xbar + q95 * age.deaths$ste)

#the 99% CI's
age.deaths<- mutate(age.deaths, q995 = qt(p = 0.995, df = n - 1))
age.deaths<- mutate(age.deaths, q005 = qt(p = 0.005, df = n - 1))

ci_99 <- mutate(age.deaths, L = xbar + q005 * age.deaths$ste, U = xbar + q995 * age.deaths$ste)

#Activity 4 t-test

t.s. <- (age.deaths$xbar-45)/(age.deaths$ste)


pt(q = t.s., df = (age.deaths$n - 1), lower.tail = FALSE)+
  pt(q = -t.s., df = (age.deaths$n - 1), lower.tail = TRUE)
# = 0.48141

#doing it the easy way
t.test(x = deaths$Age, alternative = "two.sided", mu = 45, conf.level = 0.95)

#Activity 5
mu <- 45
sigma <- 20

sim <- data.frame(Age = rnorm(n=5, mean = mu, sd = sigma))

sim.sum <- summarise(sim, xbar = mean(Age), s = sd(Age), ste=std.error(Age))

#do a t-test

t.test(x= sim$Age, mu=mu, conf.level=0.95, alternative="two.sided")

#checking CI at 95%
sim.sum<- mutate(sim.sum, q975 = qt(p = 0.975, df = 5 - 1))
sim.sum<- mutate(sim.sum, q025 = qt(p = 0.025, df = 5 - 1))

L = sim.sum$xbar + sim.sum$q025 * sim.sum$ste #24.8
U = sim.sum$xbar + sim.sum$q975 * sim.sum$ste #46.3

#getting 100 sims

n_sims <- 100
n_deaths <- 5

sim_data <- expand.grid(death = 1:n_deaths,sim = 1:n_sims)
sim_data <- mutate(sim_data, Age = rnorm(n=n_deaths*n_sims, mean = mu, sd = sigma))

#grouping based in the simulation
sim_data.g <- group_by(sim_data, sim)

#sumarrising the func
sim_data.sum <- summarise(sim_data.g, xbar = mean(Age), s = sd(Age), ste = std.error(Age), n=n())

#calculating the confidence intervals
sim_data.sum <- mutate(sim_data.sum, q975 = qt(p = 0.975, df = 5 - 1))
sim_data.sum <- mutate(sim_data.sum, q025 = qt(p = 0.025, df = 5 - 1))

sim_data.sum$q025 <- c()
sim_data.sum$q975 <- c()
#actually calcing the lower and upper bounds

L = sim_data.sum$xbar + sim.sum$q025 * sim_data.sum$ste 
U = sim_data.sum$xbar + sim.sum$q975 * sim_data.sum$ste

#plotting a histogram of the sample means

ggplot(data = sim_data.sum, aes(x=xbar)) + geom_histogram(colour = "black", fill = "blue") +theme_bw()

#plotting graph with colours

sim_data.sum <- mutate(sim_data.sum, mu_inside_CI = (L < mu) & (mu < U))

ggplot(data = sim_data.sum, aes(x = sim)) +
  geom_pointrange(aes(ymin=L, ymax=U, y = xbar, colour = mu_inside_CI)) + theme_bw()

