#Week 11
library(broom)
library(tidyverse)
library(tidyr)
library(forcats)
library(GGally)

summarise(iris, r = cor(Petal.Length, Petal.Width))
summarise(iris, r.squared = cor(Petal.Length, Petal.Width)^2)

lm.iris <- lm(data=iris, Petal.Width ~ Petal.Length)
select(glance(lm.iris), r.squared)

#Load in Data
eco <- read_csv("Ecologyv2.csv")

#testing Relationship with multiple vaiables
eco.vars <- c("Richness", "Turbidity", "DO", "Cond", "pH", "Temp") #need to have less than 15 Col
eco.pairs <- ggpairs(eco, columns = eco.vars,
                     lower=list(continuous="smooth"),
                     diag=list(continuous="densityDiag"))

eco.pairs

#creqating a correlation matrix
eco.cor <- round(cor(eco[,eco.vars]), 3)

#converting to long dataframe
cor.long <- eco.cor %>%
  as.data.frame %>%
  mutate(X = row.names(.)) %>%
  gather(., Y, value, -X) %>%
  mutate(X = fct_relevel(X, eco.vars),
         Y = fct_relevel(Y, eco.vars))

#Heatmap
eco.cor.heat <- ggplot(data=cor.long,
                       aes(x=X, y=Y)) +
  geom_raster(aes(fill=value)) +
  scale_fill_gradient2(low="blue", mid="white",
                       high="red", midpoint=0, limits=c(-1,1)) +
  theme(axis.text.x=element_text(angle=90, hjust=1, vjust=0.5)) +
  coord_equal() + xlab("") + ylab("")

eco.cor.heat

#Exploratory data analysis
ggplot(data=eco, aes(x=DO, y=Richness)) + geom_point(aes(color=Cond)) +
  scale_color_gradient(low="blue", high="red",
                       name=expression(Conductivity~(mu*S~cm^{-1}))) +
  facet_grid(. ~ Location, margins = TRUE) + theme_bw() +
  theme(legend.position="bottom") +
  xlab(expression(Dissolved~Oxygen~(mg~L^{-1})))

#Multiple explanatory variable model
fit <- lm(data=eco, Richness ~ DO + Cond)
fit.tidy <- tidy(fit, conf.int = TRUE)
fit.tidy

fit.fort <- fortify(fit)
fit.fort$Location <- eco$Location

ggplot(data=fit.fort, aes(x=.fitted, y=.resid)) +
  geom_point(aes(color=Location)) + xlab("Fitted") +
  ylab("Residuals = Observed - Fitted") + theme_bw()

#helping with predictions

DO.new <- seq(from=5, to=11, by=0.1)
Cond.new <- seq(from=200, to=600, by=10)
eco.newdata <- expand.grid(DO=DO.new, Cond=Cond.new)
eco.newdata$Richness <- predict(fit, eco.newdata)
eco.pred.gg <- ggplot(data=eco.newdata, aes(x=DO, y=Cond)) +
  geom_raster(aes(fill=Richness)) +
  scale_fill_gradient(low="red", high="blue") +
  xlab("Dissolved Oxygen") +
  ylab("Conductivity") +
  theme(legend.position="right")

eco.pred.gg

#comparing models
library(MASS)
lm.gas <- lm(data = whiteside, Gas ~ Temp * Insul)
lm.gas.parallel <- lm(data = whiteside, Gas ~ Temp + Insul)

glance(lm.gas)$r.squared
glance(lm.gas.parallel)$r.squared

anova(lm.gas.parallel, lm.gas)
