#install.packages("ALA",
#                repos="http://R-Forge.R-project.org") 
#repos is stating which repository it is from

library(ALA)
library(tidyverse)

data(fev1)

fev1_df <- filter(fev1, age > 10, age < 11) %>% #filters only 10-11 year olds
  mutate(FEV1 = exp(logFEV1)) %>% #added FEV1 Variable as data is in log form in dataset exp gets the exponent of e to our log data
  group_by(id) %>%              #groups by ID
  filter(row_number() == 1) %>% ungroup  #gets the first row from each ID and then ungroups as we dont want to analyse by the groups anymore

ggplot(data = fev1_df, aes(x = FEV1)) + 
  geom_histogram(aes(y = ..density..), bins = 15, colour = "black", fill = "grey") + 
  theme_bw() +
  stat_function(fun = dnorm, args = list(mean = 1.91, sd = 0.314)) 
#need to put y label into the geom_histogram() as the y for our stat func is diff so r cant plot em

summarise(fev1_df, mean = mean(FEV1), sd = sd(FEV1))

mean_FEV <- mean(fev1_df$FEV1)
sd_FEV <- sd(fev1_df$FEV1)

#getting the percentages from the q values
pnorm(mean = mean_FEV, sd = sd_FEV, q = 2.5, lower.tail = F)  ## 0.03040478

pnorm(mean = mean_FEV, sd = sd_FEV, q = 2.5) - pnorm(mean = mean_FEV, sd = sd_FEV, q = 1.5) 
#so 87% chance that they re between 2.5 and 1.5 lung capacity ro whatever

#getting the Q value from the percentage
qnorm(mean = mean_FEV, sd = sd_FEV, p = 0.05, lower.tail = F)

#finding the percentage of obs that have values stated
summarise(fev1_df, "FEV1 > 2.5" = mean(FEV1 >2.5), 
          "1.5 < FEV < 2.5" = mean(FEV1 <2.5 & FEV1 > 1.5))

#getting quartiles
quantile(x=fev1_df$FEV1, seq(0,1, by = 0.25))

fev1_df <- mutate(fev1_df, FEV.std = (FEV1 - mean(FEV1))/sd(FEV1))

fev1_dfqq <- ggplot(data = fev1_df, aes(sample = FEV.std)) + 
  stat_qq() + 
  coord_equal()+
  geom_abline() + #straight line with a gradient of 1
  theme_bw()

fev1_dfqq


