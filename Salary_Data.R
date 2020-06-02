Salary_Data <- read.csv("D:/Study Material/DataScience/Linear_Regression/Assignments/Salary_Data.csv")
View(Salary_Data)

attach(Salary_Data)


summary(Salary_Data)
plot(Salary_Data)
cor(Salary_Data)

# r > 0.85 -- i.e. Good Correaltion
reg <- lm(Salary ~ YearsExperience)
summary(reg)


# Simple Linear Regression model
reg <- lm(Salary ~ YearsExperience) # lm(Y ~ X)

summary(reg)

