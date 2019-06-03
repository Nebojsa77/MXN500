#week 10 Lin Reg with Cat Var

library(tidyverse)
library(plotrix) # gives us standard error of the mean
help(plotrix)
eco <- read_csv("Ecologyv2.csv")
eco <- mutate(eco, Location = factor(Location))

#Boxplot
ggplot(data=eco, aes(x=Location, y=Richness)) + geom_boxplot() +
  theme_bw() + ylab("Taxon Richness") + coord_flip()

#summarising for each location with group_by
eco_by.location <- group_by(eco, Location)
summarise_at(eco_by.location,
             vars(Richness),
             funs(Mean = mean, SE = std.error))

summarise(eco_by.location, 
          Mean = mean(Richness), Se = std.error(Richness))

#creating the model
eco_cat_lm <- lm(Richness ~ Location, data=eco)
library(broom) #gives tidy func
tidy(eco_cat_lm)

#Getting Conf Int.
eco_cat_coefs <- tidy(eco_cat_lm, conf.int= TRUE)
select(eco_cat_coefs, term, conf.low, estimate, conf.high, p.value)

#testing effect
eco_pred <- distinct(eco, Location) #gets each level of categorical variable
eco_ci <- data.frame(predict(object = eco_cat_lm,
                             newdata=eco_pred,
                             interval="confidence"))
eco_pred.ci <- bind_cols(eco_pred, eco_ci)
eco_pred.ci

#testing the model as a whole
glance(eco_cat_lm)

eco_aov <- aov(data=eco, Richness ~ Location)
summary(eco_aov)

select(glance(eco_aov), p.value, sigma)
select(glance(eco_cat_lm), p.value, sigma)

#making a no intercept model
summary(eco_noint <- lm(data = eco, Richness ~ Location -1))

#showing the correct r^2 
1 - sum(residuals(eco_noint)^2) /
  sum((eco$Richness - mean(eco$Richness))^2)

#predictions
eco_pi <- data.frame(predict(object = eco_cat_lm,
                             newdata=eco_pred,
                             interval="prediction"))
eco_pred.pi <- bind_cols(eco_pred, eco_pi)
eco_pred.pi

#reordering the characters(in Location)
library(forcats) #library for reordering categorical variables (Factors)
eco <- mutate(eco, Location =
                fct_relevel(Location, "Cedar Creek")) #fct_relevel picks the baseline
pull(eco, Location)

eco <- mutate(eco, AquaticLife =
                fct_relevel(AquaticLife, #using fct_relevel to level all vars
                            "low", "mod", "high"))
pull(eco, AquaticLife)

#releveling based in order of appearence
eco <- mutate(eco, Location = fct_inorder(Location))
pull(eco, Location)

#ordering based on number
count(eco, CoverOver)

eco <- mutate(eco,
              CoverOver = fct_other(CoverOver,
                                    keep=c("none", "canopy"),
                                    other_level="other"))
count(eco, CoverOver)

#two categorical variables
# convert cyl and am to factor
mtcars <- mutate_at(mtcars,
                    .vars = vars(cyl, am),
                    .funs = factor)
# human friendly
mtcars <- mutate(mtcars,
                 trans = fct_recode(am,
                                    auto = "0",
                                    manual = "1"))
count(mtcars, trans, cyl)

#creating a two cat model
lm_cars <- lm(data=mtcars, mpg ~ trans + cyl)
select(glance(lm_cars), r.squared)

#checking for an interaction in explanatory variables
lm_cars_int <- lm(data=mtcars, mpg ~ trans * cyl)

anova(lm_cars, lm_cars_int)

#comparing model with and without transmission 
lm_cyl <- lm(data=mtcars, mpg ~ cyl)
summary(lm_cyl)
anova(lm_cyl, lm_cars)
