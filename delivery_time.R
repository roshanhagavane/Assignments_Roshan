getwd()
setwd("D:/Study Material/DataScience/Linear_Regression/Assignments/")
dt <-read.csv("delivery_time.csv")
View(dt)
attach(dt)


summary(dt)
pairs(dt)
plot(dt)
cor(dt)
## r < 0.85 ,SO moderate  correlation
library(corpcor)
cor2pcor(cor(dt))
