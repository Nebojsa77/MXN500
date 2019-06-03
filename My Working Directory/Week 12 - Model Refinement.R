#Model Refinement

library(tidyverse)
library(broom) #for lm stuff

animals <- read_csv("animals.csv")
eco <- read_csv("Ecologyv2.csv")
BP <- read_csv("BP.csv")

ggplot(BP, aes(x=temp, y= press)) +geom_point() +theme_bw()

BP.lm <- lm(data=BP, Pressure ~ Temperature)

fortify(BP.lm) %>% ggplot(aes(x=Temperature, y = .resid)) +
  geom_point() + theme_bw()

BP.sum <- summary(BP.lm)
BP.sum$coefficients
BP.sum$r.squared
BP.sum$sigma

BP.lm.poly <- lm(data=BP, Pressure ~ poly(Temperature,2, raw = T))
summary(BP.lm.poly)

fortify(BP.lm.poly) %>% ggplot(aes(x=.fitted, y = .resid)) +
  geom_point() + theme_bw()


augment(BP.lm, BP) %>% 
  ggplot(aes(x=Temperature, y=.resid)) + geom_point() +
  geom_smooth( method = "lm", formula = y ~ poly(x,2)) +
  facet_grid(.~Source)


BP_lm3 <- lm(data = BP, Pressure ~ Temperature + Source)
fortify(BP_lm3) %>% ggplot(aes(x=.fitted, y = .resid)) +
  geom_point() + theme_bw()



i.max <- which.max(abs(residuals(BP.lm.poly))) #abs is absoulute value(drops negative) 
i.max #in this case pbservation 12 is the one we need to drop

BP.lm.poly.outlier <- lm(data=BP[-i.max, ], #in R we use square brackets to access invdividual cells, it goes row then Col
                         Pressure ~ poly(Temperature,2, raw = T))

fortify(BP.lm.poly.outlier) %>% ggplot(aes(x=.fitted, y = .resid)) +
  geom_point() + theme_bw()

BP.lm.outlier <- lm(data=BP[-i.max, ], Pressure ~ Temperature)

anova(BP.lm.outlier, BP.lm.poly.outlier)

BP.lm.poly.int.outlier <- lm(data=BP[-i.max, ],
                             Pressure ~ poly(Temperature,2)*Source)
anova(BP.lm.poly.outlier, BP.lm.poly.int.outlier)

#physics model
BP.lm.log <- lm(data=BP[-i.max, ],
                log(Pressure) ~ Temperature)

#Biological model
bb.lm <- lm(data=animals, log(Brain) ~ log(Body))
glance(bb.lm)
summary(bb.lm)

augment(bb.lm, animals) %>% ggplot(aes(x=log(Body), y= log(Brain), colour = Era)) +
  geom_smooth(method = "lm", se = F) +
  geom_point() + theme_bw()

bb.lm.p <- lm(data=animals, log(Brain) ~ log(Body) + Era)
summary(bb.lm.p)

augment(bb.lm.p, animals) %>% ggplot(aes(x=.fitted, y = .resid, colour = Era)) +
  geom_smooth(method = "lm", se = F) +
  geom_point() + theme_bw()
library(kableExtra)
bb.lm.order <- lm(data=animals, log(Brain) ~ log(Body) +Order)

tidy(bb.lm.order, conf.int = 95) %>% kable(format = "pandoc")
