# part A : paried t test
library(readxl)
leaf <- read_excel("Desktop/leaf_damage.xlsx")
t.test(leaf$Treated, y=leaf$Untreated, alternative=c('g'), paired=TRUE, var.equal=FALSE,conf.level=0.95)
# paried t test 需要设定 paired=TRUE, var.equal=FALSE
# R 自动设定difference为x-y 需要注意
# The p-value of p = 0.01288 indicates that we can reject H0 at the 5% level but not at the 1% level.
# There is evidence that treated leaves have more damage, on average, than untreated leaves.

difference <- leaf$Treated - leaf$Untreated
hist(difference, main = paste("Histogram of differences"), breaks = 10,
     xlab = "Differences", col="purple", xlim = range(-0.5,1.5))
library(nortest)
ad.test(difference)
qqnorm(difference,col='blue')
qqline(difference,col='red')
#结果 The points on the graph seem reasonably close to a straight line. Also, the p-value for the AndersonDarling test is given as p = 0.7573, meaning that we cannot reject the null hypothesis (data normal)
# even at the 75% level. That is, there is no evidence that data do not come from a normal distribution.
#The assumption underlying the paired t-test (normality of the differences) appears to be justified.
# 这里p越大越好，说明paired t test潜在要求 normal分布成立

####################################################################
# part B : independent two-sample test
library(readxl)
boilers <- read_excel("Desktop/boilers.xlsx")
t.test(boilers$`Type A`, y=boilers$`Type B`, alternative=c('t'), paired=FALSE, var.equal=TRUE,conf.level=0.95)
# independent t test 需要设定 paired=FALSE 而 var.equal需要看情况
#boxplot
A <- boilers$`Type A`
B <- boilers$`Type B`
means <- c(mean(A), mean(B,na.rm=TRUE)) # na.rm是删除空值的意思
boxplot(A, B, col="purple", main = paste("Boxplots"), names=c("Type A", "Type B"),
        boxwex=0.4)
points(means, pch=9)

# check the assumption of normality of two groups
library(nortest)
qqnorm(A)
qqline(A)
ad.test(A)
# The p-values for the Anderson-Darling test are p = 0.4926 for Type A
# indicating that we cannot reject H0 (data normal) even at the 49% level in either case.
# There is no evidence against normality in either case. 
# That is, it’s reasonable to assume normality for these data.
qqnorm(B)
qqline(B)
ad.test(B)

#check the assumption of equal variances
# F test
var.test(A,B, ratio=1, alternative=c('t'),conf.level=0.95)
# 默认ratio为 A/B

#找90%CI for the population variance sigma^2 for A
library(TeachingDemos)
sigma.test(A, sigma=1, alternative=c('t'), conf.level=0.95)
sigma.test(A, alternative=c('t'), conf.level=0.90)
# sigma=多少不重要，我们只是想找CI for sigma，所以参数默认sigma=1，可省略





