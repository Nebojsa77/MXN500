ldeaths
ldeaths <- data.frame(matrix(ldeaths,
                             ncol = 12,
                             byrow = T))
ldeaths
names(ldeaths) <- month.abb
ldeaths <- mutate(ldeaths, Year = 1974:1979)
ldeaths.l <- gather(ldeaths, key=Month, value=Deaths, -Year)
ldyear <- group_by(ldeaths.l, Year)
summarise(ldyear, Yearly_Average = mean(Deaths))
ldmonth <- group_by(ldeaths.l, Month)
summarise(ldmonth, Monthly.Deaths = mean(Deaths))
