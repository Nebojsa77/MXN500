#Week 9: Lecture - Prediction & Model Choice

data(iris)
filter(iris, Petal.Length == 7)

#Load model
iris_lm <- lm(data=iris, Petal.Width ~ Petal.Length) #lm(data, Y ~ X)
#data passed to predict func
iris_newdata <- data.frame(Petal.Length = 7)
#predict y for x
predict(object = iris_lm, newdata = iris_newdata)


#Showing my prediction
ggplot(data=iris, aes(x=Petal.Length, y=Petal.Width)) +
  geom_point(position=position_jitter(h=0.01, w=0.01), #jittering the position just makes it look nicer
             alpha=0.25) +
  xlab("Petal length (cm)") +
  ylab("Petal width (cm)") +
  theme_bw() +
  geom_smooth(method="lm", se=F, col="black", alpha=0.25) +
  geom_point(x=7,
             y=predict(object = iris_lm, newdata = iris_newdata),
             pch=19, size=2, color="red")

#making a prediction dataset
iris_newdata <- data.frame(Petal.Length = seq(1,7, by=0.1))
iris_predictions <- data.frame(predict(object = iris_lm,
                                       newdata = iris_newdata,
                                       interval="prediction",
                                       level = 0.95))
iris_predictions <- bind_cols(iris_newdata, iris_predictions)
head(iris_predictions)

#plotting my prediction dataset
ggplot(data = iris, aes(x=Petal.Length)) + #y needs to be out of ggplot bracket because other geoms dont have y axes
  geom_point(aes(y=Petal.Width), alpha = 0.25) +
  labs(x = "Petal Length (CM)", y = "Petal Width (CM)") +theme_bw() +
  geom_line(data = iris_predictions, aes(y=fit)) +
  geom_ribbon(data = iris_predictions, aes(ymin = lwr, ymax = upr), fill="lightskyblue", color=NA, alpha=0.25)

#creating my Confidence interval dataset
iris_conf <- data.frame(predict(object = iris_lm,
                                newdata = iris_newdata,
                                interval="confidence",
                                level = 0.95))
iris_conf <- bind_cols(iris_newdata, iris_conf)
head(iris_conf)

#creating my CI and PI dataset
library(purrr)

iris_both <- list(Prediction = mutate(iris_predictions) ,
                  Confidence = mutate(iris_conf)) %>%
  bind_rows(.id="Interval") %>%
  mutate(Interval = fct_rev(Interval))

#plotting my CI and PI dataset
ggplot(data=iris, aes(x=Petal.Length)) +
  geom_point(aes(y=Petal.Width),
             position=position_jitter(w=0.01, h=0.01), alpha=0.25) +
  xlab("Petal length (cm)") +
  ylab("Petal width (cm)") +
  theme_bw() +
  geom_ribbon(data=iris_both,
              aes(ymin=lwr, ymax=upr, fill=Interval),
              color=NA, alpha=0.25) +
  geom_line(data=iris_predictions, aes(y=fit)) +
  theme(legend.position="bottom") +
  scale_fill_brewer(palette="Paired")

#Polynomials example
library(alr4)
data(stopping)
stopping <- mutate(stopping,
                   Speed = Speed*1.60934, # mph to km/hr
                   Distance = Distance*0.3048) # ft to m

ggplot(data=stopping, aes(x=Speed, y=Distance)) + theme_bw() +
  geom_point() + xlab("Speed (km/hr)") + ylab("Stopping\ndistance (m)")

#because there looks like a linear relationship we can make a model
cars_linear <- lm(data=stopping, Distance ~ Speed)
summary(cars_linear) #is significant at alpha = 0.05 so lets check residuals

fortify_linear <- fortify(cars_linear)
ggplot(data=fortify_linear, aes(x=.fitted, y=.resid)) +
  geom_point() + theme_bw() + xlab("Fitted values") +
  ylab("Residuals") + geom_smooth()

#assumptions not met so we cant use this model, lets try polynomial
#we decide polynomial cause there is a curve in our data

cars_quadratic <- lm(data=stopping, Distance ~ poly(Speed, 2, raw=T))
summary(cars_quadratic)

fortify_quadratic <- fortify(cars_quadratic)

ggplot(data=fortify_quadratic, aes(x=.fitted, y=.resid)) +
  geom_point() + theme_bw() + xlab("Fitted values") +
  ylab("Residuals") + geom_smooth()

#Plotting the fanning out of residuals
library(ggalt)
ggplot(data=fortify_quadratic, aes(x=.fitted, y=.resid)) +
  geom_point() + theme_bw() + xlab("Fitted values") +
  geom_encircle(color=NA, fill="lightskyblue", alpha=0.25, s_shape = 1 , expand=0) +
  ylab("Residuals")

#Plotting the prediction of both models
cars_newdata <- data.frame(Speed = seq(0, 80))

both_preds <- list(Linear = cars_linear,
                   Quadratic = cars_quadratic) %>%
  map(~predict(object = .x, newdata=cars_newdata, interval="prediction" )) %>%
  map_df(~cbind(.x, cars_newdata), .id="Model") %>%
  rename(Distance = fit)

ggplot(data=stopping, aes(x=Speed, y=Distance)) + theme_bw() +
  geom_point() + xlab("Speed (km/hr)") + ylab("Stopping\ndistance (m)") +
  geom_line(data=both_preds) +
  geom_ribbon(data=both_preds, aes(ymin=lwr, ymax=upr), alpha=0.25) +
  facet_wrap( ~ Model)

#testing to see which model we should use
anova(cars_linear, cars_quadratic) #more complex model last always

