library(VennDiagram)
library(ggforce)


draw.pairwise.venn(area1 = 0.8, area2 = 0.4, cross.area = 0.2,     #plotting the area for each
                   category = c("Dog People", "Cat People"),       #naming the variables
                   lty = rep("blank", 2),                          #make the circles blank in the centre
                   fill = c("red", "Blue"),                        #the colour
                   alpha = rep(0.5, 2),                            #makes the intersection abit lighter
                   cat.pos = c(0,0), cat.dist = rep(0.025, 2), scaled = F)
#cat.pos is the position of labels
#unsre what cat.dist does
#rep just repeats the stated thing the amount of times stated

df <- data.frame(n = c(1:6), p = rep(1/6,6)) #rep repeats 1/6 six times, p= is our other variable

ggplot(data = df, aes( x = n, weight = p))+       # so that we are weighting it differently
  geom_bar(color = "Black", fill = "dark blue")+ labs(x = "Roll") +
  scale_x_discrete(limits = c(seq(1,6)))+          #turns the x scale into a sequence from 1 to 6
  scale_y_continuous(name = "P(N=n)")+             # makes the scale contiuous 
  theme_bw()

#doing a function
f_t <- function(t){return(2/25 * t)}

ggplot(data = data.frame(x = c(0,5)), aes(x = x)) + stat_function(fun=f_t) +
  scale_x_continuous(name = "t")+
  scale_y_continuous(name= "f(t)") +
  theme_classic()

