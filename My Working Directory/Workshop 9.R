#workshop 9 

dengue <- read_csv("dengue.csv")

ggplot(dengue, aes(x=Days, y = Load)) +geom_point() +theme_bw() + geom_smooth(se =F)

ggplot(dengue, aes(x=Days, y = Load)) +geom_point() +theme_bw() + geom_smooth(se =F) + scale_y_log10()

#Fitting a linear model

d_lm <- lm(data = dengue, log(Load) ~ Days)
d_lm
#getting pval and conf intervals
tidy(d_lm, conf.int = TRUE, conf.level = 0.95)

#Residual assumption testing
d_fort<- fortify(d_lm)
d_fort

#plotting the residuals
ggplot(data = d_fort, aes(x= .fitted, y = .resid)) + geom_point() +geom_smooth() +theme_bw()
#.resid = error | .fitted = Yhat 

#Residuals vs days
ggplot(data = d_fort, aes(x= Days, y = .resid)) + geom_point() +geom_smooth() +theme_bw()

#checking normality of residuals wiht QQ plot
ggplot(d_fort, aes(sample = .stdresid))+ stat_qq()+ geom_abline(intercept = 0, slope = 1)+
  coord_equal()+ labs(x = "q values from standard normal", #coord_equal makes it a nice square
                      y = "q values from standardised residuals")+ theme_bw()

#adding log10(Viral) to dataset
dengue$LogV <- log(dengue$Load)
dengue

#fitting a polynomial linear model
lm_dengue <- lm(data = dengue, LogV ~ poly(Days, 2, raw=T))

#checking params
tidy(lm_dengue, conf.int = TRUE, conf.level = 0.95)

#fortify poly
d_fort2 <- fortify(lm_dengue)
d_fort2

#plotting the residuals
ggplot(data = d_fort2, aes(x= .fitted, y = .resid)) + geom_point() +geom_smooth() +theme_bw()
#.resid = error | .fitted = Yhat 

#Residuals vs days
#need to bind a days col
d_fort2$Days <- dengue$Days
ggplot(data = d_fort2, aes(x= Days, y = .resid)) + geom_point() +geom_smooth() +theme_bw()

#Testing normality by plotting a QQ plot
ggplot(d_fort2, aes(sample = .stdresid))+ stat_qq()+ geom_abline(intercept = 0, slope = 1)+
  coord_equal()+ labs(x = "q values from standard normal", #coord_equal makes it a nice square
                      y = "q values from standardised residuals")+ theme_bw()

#data passed to predict func
d_newdata <- data.frame(Days = -0.7157)

#predict y for x
predict(object = lm_dengue, newdata = d_newdata)
d_predictions <- data.frame(predict(object = lm_dengue,
                                       newdata = d_newdata,
                                       interval="prediction",
                                       level = 0.95))

d_predictions <- mutate(d_predictions, lwr = exp^lwr, upr = exp^upr)

