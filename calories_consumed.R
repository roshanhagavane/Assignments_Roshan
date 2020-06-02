calories_consumed <- read.csv("D:/Study Material/DataScience/Linear_Regression/Assignments/calories_consumed.csv")

getwd()
setwd("D:/Study Material/DataScience/Linear_Regression/Assignments/")
calories_consumed<-read.csv("calories_consumed.csv")
View(calories_consumed)
attach(calories_consumed)

summary(calories_consumed)
plot(calories_consumed)
cor(calories_consumed)
## r > 0.85 ,SO good correlation

reg <- lm(calories_consumed$Calories.Consumed ~ calories_consumed$Weight.gained..grams.) # lm(Y ~ X)
summary(reg)

pred <- predict(reg)

pred <- predict(reg)

reg$residuals
sum(reg$residuals)

mean(reg$residuals)
sqrt(sum(reg$residuals^2)/nrow(wc_at))  #RMSE

sqrt(mean(reg$residuals^2)) # another way to find RMSE value

confint(reg,level=0.95)
predict(reg,interval="predict")

# ggplot for adding regresion line for data
library(ggplot2)

?ggplot2

ggplot(data = calories_consumed, aes(x =Weight.gained..grams., y = Calories.Consumed)) + 
  geom_point(color='blue') +
  geom_line(color='red',data = calories_consumed, aes(x=Weight.gained..grams., y=pred))

?ggplot2



