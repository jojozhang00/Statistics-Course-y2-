#Part A

library(readxl)
timeDF <- read_excel("Desktop/Tutorial10_timings.xlsx")

term <- timeDF$Terminals
time <- timeDF$Time
#scatterplot
plot(term, time) #default order plot(x,y)

#simple linear regression
lin <- lm(time ~ term, data=timeDF) #responses in time depend on term ( y~x )
lin
summary(lin) 
#summary() contains results of hypothesis test for bata0 and beta1, R^2, estimated error variance
#Give the estimated value of the error variance sigma^2.
#Note: R outputs the estimated value of sigma which is called Residual standard error in the output.
#Fitted regression equation is Y_bar = 3.0496 + 0.2603x

#histogram of residuals
res <- residuals(lin)
hist(res, main = paste("Histogram of residuals"), breaks = 5, xlab = "Residuals",
     xlim = range(-5,5), col="purple") #bell-shaped
#normality test - Anderson-Darling test
library(nortest)
ad.test(res)
#normal probability plot
qqnorm(res, col="blue")
qqline(res, col="red")

#plot the fitted line
plot(term, time, col="blue", pch=19, main = paste("Fitted line plot"),
     xlab = "Terminals", ylab="Time")
abline(lin, col="red") #abline() add straight lines to a plot

#plot the plot of residuals versus fitted values
fit <- fitted.values(lin)
plot(fit, res, col="blue", pch=19, main = paste("Plot of residuals vs fitted values"),
     xlab = "Fitted values", ylab="Residuals" )
#see if there is a pattern

#Compute 95% prediction and confidence intervals when the task is submitted to 50 and 70 terminals
#x0=50 or 70
pr <- data.frame(term=c(50, 70))
predict(lin, pr, interval = c("prediction"), level = 0.95)
predict(lin, pr, interval = c("confidence"), level = 0.95)

#################################################################################

#Part B
library(readxl)
tum <- read_excel("Desktop/tumour.xlsx")

#Find the fitted regression line to predict the volume of the tumour from the age of the patient
lin <- lm(Volume ~ Age, data=tum)

#Find the 90% confidence interval for the mean tumour volume for an 80 year old patient. 
#Find also the 90% prediction interval for the tumour volume of an 80 year old patient. 
#Give a practical interpretation of these intervals.
prB <- data.frame(Age = 80) #we must first store data in a data frame; name it with the column name
predict(lin, prB, interval=c("confidence"), level = 0.9) #data accpets data frame or list or environment
#mean tumour volume of 80
predict(lin, prB, interval=c("prediction"), level = 0.9)
#the tumour volume of a particular 80 year old person

#Does the tumour volume depend on the age of the patient?
#same as asking whether the slope is 0
summary(lin)
#look at the p-value of the row of slope

#State all assumptions about the errors in simple linear regression.
#Answer: The errors are independent and normally distributed with zero mean and constant variance.

#Decide whether the errors follow a normal distribution.
#histogram of residuals
res <- residuals(lin)
hist(res, main = paste("Histogram of residuals"), breaks = 10, xlab = "Residuals",
     xlim = range(-0.3,0.3), col="purple")
#normality test - Anderson-Darling test
library(nortest)
ad.test(res)
#normal probability plot
qqnorm(res, col="blue")
qqline(res, col="red")

#Decide whether the simple linear regression model seems appropriate here.
#scatterplot
plot(tum$Age, tum$Volume, col="blue", pch=19, main = paste("Scatterplot"), xlab = "Age", ylab="Volume")
#fitted line plot
plot(tum$Age, tum$Volume, col="blue", pch=19, main = paste("Fitted line plot"), xlab = "Age",
     ylab="Volume")
abline(lin, col="red")
#residuals vs fitted values plot
fit <- fitted.values(lin)
plot(fit, res, col="blue", pch=19, main = paste("Plot of residuals vs fitted values"),
     xlab = "Fitted values", ylab="Residuals")
#Answer: The plot of Residuals vs Fitted Values however suggests a pattern --> mat not be appropriate


