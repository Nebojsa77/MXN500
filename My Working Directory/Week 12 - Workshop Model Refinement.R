library(tidyverse)

df <- read_csv("ozone.csv")
df_long <- gather(df, City, Ozone, Melbourne:Hobart, factor_key = T)
df_long <- na.omit(df_long)
df <- na.omit(df)
ggplot(df_long, aes(x= Year, y= Ozone)) +geom_point(aes(colour = City)) +
  geom_smooth(aes(colour = City), se = F, method = "lm") + theme_bw()

#creating a model
df_lm <- lm(data = df_long, Ozone ~ Year)
summary(df_lm)
tidy(df_lm, conf.int = 95)
glance(df_lm)

#residual plotting
df.fort <- augment(df_lm, df_long)
ggplot(df.fort, aes(x=.fitted, y = .resid, colour = City)) + geom_point() + theme_bw()

#creating a multiple level model

df.lm2 <- lm(data= df_long, Ozone ~ Year + City)
summary(df.lm2)
tidy(df.lm2, conf.int = 95)
glance(df.lm2)

#residual plotting
df.fort2 <- augment(df.lm2, df_long)
ggplot(df.fort2, aes(x=.fitted, y = .resid)) + geom_point(aes(colour = City)) +
  geom_smooth() + theme_bw()

#creating a QQ plot

ggplot(data = df.fort2, aes(sample = .std.resid)) +
  stat_qq() + geom_abline() +theme_bw() + coord_equal() + labs(x= "Theoretical", y= "Sample", title = "Plot for Checking Normality of Errors")

#ANOVA
anova(df_lm, df.lm2) # anova(simple, complex)

#creating an interaction model

df.lm3 <- lm(data= df_long, Ozone ~ Year * City)
summary(df.lm3)
tidy(df.lm3, conf.int = 95)
glance(df.lm3)

#residual plotting
df.fort3 <- augment(df.lm3, df_long)
ggplot(df.fort3, aes(x=.fitted, y = .resid)) + geom_point(aes(colour = City)) +
  geom_smooth() + theme_bw()

#creating a QQ plot

ggplot(data = df.fort3, aes(sample = .std.resid)) +
  stat_qq() + geom_abline() +theme_bw() + coord_equal() + labs(x= "Theoretical", y= "Sample", title = "Plot for Checking Normality of Errors")

#ANOVA
anova(df.lm2, df.lm3)
