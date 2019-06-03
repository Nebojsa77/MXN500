#Workshop 8 - Linear Regression

hfi <- read_csv("hfi_cc_2018.csv")
head(hfi)

#filter into 2016
hfi_2016 <- filter(hfi, year == "2016")

WorldData <- map_data('world')
head(WorldData)

library(mapproj)

ggplot() +
  geom_map(data=WorldData, map=WorldData,
           aes(x = long, y = lat, group=group, map_id=region),
           fill="white", colour="#7f7f7f", size=0.5) +
  geom_map(data = hfi_2016, map = WorldData,
           aes(fill = hf_score, map_id = countries),
           colour = "#7f7f7f", size = 0.5) +
  coord_map("rectangular", lat=0, xlim=c(-180,180), ylim=c(-60, 90))+
  scale_fill_continuous(low="lightblue", high="darkblue", 
                        guide="colorbar")+
  scale_y_continuous(breaks=c())+
  scale_x_continuous(breaks=c())+
  labs(fill="HF Score", title="HF score by location", x="Longitude", y="Latitude")+
  theme_bw()

#comparing HFs to PFS
head(hfi_2016)
ggplot(data = hfi_2016, aes(x=hf_score, y=pf_score)) +geom_point() + geom_smooth(method = "lm") +theme_bw()

#HFs vs EFs
ggplot(data = hfi_2016, aes(x=hf_score, y=ef_score)) +geom_point() + geom_smooth(method = "lm") +theme_bw()

#creating am LM
personal_model <- lm(data=hfi_2016, hf_score ~ pf_score)

#tidy gves mor info on our parameters
tidy(personal_model, conf.int = TRUE, conf.level = 0.95)

#predict hf when 
predict(object = personal_model, data.frame(pf_score = 7))

glance(personal_model)

#fortify pers data

pers_fort <- fortify(personal_model)
pers_fort

mean(pers_fort$.resid)
#get mean

ggplot(data = pers_fort, aes(x= .fitted, y = .resid)) + geom_point() +geom_smooth() +theme_bw()

#plotting residuals for normality
ggplot(data = pers_fort, aes(x=.resid)) +geom_histogram(colour = "black", fill = "grey") + theme_bw()

#checking std residuals for normality
ggplot(data = pers_fort, aes(x = .stdresid)) +geom_histogram(aes(y = ..density..), colour = "black", fill = "grey") +
  stat_function(fun = "dnorm", 
                args = list(mean = 0, sd = 1, size = 2)) +
  theme_bw()

#plotting qq
ggplot(data = pers_fort, aes(sample = .stdresid)) +
  stat_qq() + geom_abline(intercept = 0, slope = 1) +theme_bw()
