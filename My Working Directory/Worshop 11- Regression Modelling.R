#workshop 11: 
pie <-read_csv("Piemonte_data_byday.csv")

#Activity 1 – Exploratory analysis

pie$logpm <- log(pie$PM10)

#plotting histogram

ggplot(pie, aes(x=logpm)) +geom_histogram(fill = "blue", colour = "black") +theme_bw()

#getting to only explanatory VARS
exp <- pie
exp$Station.ID <- c()
exp$Date <- c()
exp$PM10 <- c()

library(GGally)
ggpairs(exp)
warnings()

#removing the spacial coordiantes
exp$A <- c()
exp$UTMX <- c()
exp$UTMY <- c()

ggpairs(exp)

#fitting a regression model

pie_lm <- lm(data=exp, logpm ~ WS)
tidy(pie_lm, conf.int = 95)
glance(pie_lm)

#residuals
library(broom)
pie.new <- augment(pie_lm, pie)

ggplot(data=pie.new, aes(x=.fitted, y=.resid)) +
  geom_point() + xlab("Fitted") + geom_smooth() +
  ylab("Residuals = Observed - Fitted") + theme_bw()

#QQ plot
ggplot(data=pie.new, aes(sample=.std.resid)) +
  stat_qq(geom="point") + geom_abline() + labs(x= "Theoretical", y = "Sample", title = "Normality of Standardised Residuals") +
  coord_equal() + theme_bw()

#
pie.new$Station.ID <- c()
pie.new$Date <- c()
pie.new$PM10 <- c()
pie.new$A <- c()
pie.new$UTMX <- c()
pie.new$UTMY <- c()
pie.new$.fitted <- c()
pie.new$.se.fit <- c()
pie.new$.hat <- c()
pie.new$.sigma <- c()
pie.new$.cooksd <- c()
pie.new$.std.resid <- c()
pie.new$WS <- c()
ggpairs(pie.new)

dplyr::select(pie, -one_of(c("Station.ID", "HMIX"))) #wayyyy more efficient

#creating a new model
pie_lm2 <- lm(data=exp, logpm ~ WS + PREC)
tidy(pie_lm2, conf.int = 95)
glance(pie_lm2)

#plotting resiuals VS fitted
pie_lm2.fort <- fortify(pie_lm2)
ggplot(data=pie_lm2.fort, aes(x=.fitted, y=.resid)) +
  geom_point() + xlab("Fitted") + geom_smooth() +
  ylab("Residuals = Observed - Fitted") + theme_bw()

#QQplot
ggplot(data=pie_lm2, aes(sample=.stdresid)) +
  stat_qq(geom="point") + geom_abline() + labs(x= "Theoretical", y = "Sample", title = "Normality of Standardised Residuals") +
  coord_equal() + theme_bw()

#ANOVA Model Comparison
anova(pie_lm, pie_lm2)

#testing for interaction
pie_lm3 <- lm(data=exp, logpm ~ WS * PREC)
tidy(pie_lm3, conf.int = 95)
glance(pie_lm3)

#residuals
pie_lm3.fort <- fortify(pie_lm3)
ggplot(data=pie_lm3.fort, aes(x=.fitted, y=.resid)) +
  geom_point() + xlab("Fitted") + geom_smooth() +
  ylab("Residuals = Observed - Fitted") + theme_bw()

#QQplot
ggplot(data=pie_lm3, aes(sample=.stdresid)) +
  stat_qq(geom="point") + geom_abline() + labs(x= "Theoretical", y = "Sample", title = "Normality of Standardised Residuals") +
  coord_equal() + theme_bw()

#ANOVA
anova(pie_lm2, pie_lm3)

#new model
pie_big <- lm(data = pie, logpm ~ WS + PREC + TEMP + HMIX + A + EMI)
tidy(pie_big, conf.int = 95)
glance(pie_big)

names(pie)
pie$PM10 <- c()
#residuals
augment(pie, pie_big)

