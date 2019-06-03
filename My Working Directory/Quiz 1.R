y = c(0.15, 6.23, 6.35, 6.91, 3.76)
mean(y)
median(y)
sd(y)

library(tidyverse)

#problem 6
df <- ability.cov$cov
df <- data.frame(df)
df <- mutate(df, TestName = row.names(df))
df.long <- gather(df, key = Test, value = Covariance, -TestName)

df2 <- ability.cov$cov
df2 <- data.frame(df2)
df2 <- mutate(df2, TestName = row.names(df2))
df2.long <- gather(df2, key = TestName, value = Covariance)

df3 <- ability.cov$cov
df3 <- data.frame(df3)
df3 <- mutate(df, TestName = row.names(df3))
df3.long <- spread(df3, key = TestName, value = Covariance)


#problem 8
7/56
10/56

0.267857144

#problem 9

cc <- c(219, 229, 190, 190, 213, 236, 204, 222, 204, 205, 219, 204, 194, 215, 232, 235, 200, 192)

mean(cc)
sd(cc)
median(cc)
