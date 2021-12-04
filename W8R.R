#Part A
#ANOVA F-test
A <- salt$A
B <- salt$B
C <- salt$C
D <- salt$D
E <- salt$E

combined <- data.frame(cbind(A, B, C, D, E)) #确保了组的关系
stacked <- stack(combined) #变成两列的data，一列
anovaresults <- aov(values ~ ind, data = stacked) #～左右两边不要搞错
summary(anovaresults)

#estimate of the error variance sigma^2
Error variance may be estimated as σc2 = MSE = 40.91, from the ANOVA table.
#看上面得到的anova的summary

#normality test for residuals
res <- residuals(anovaresults)

#histogram of residuals
hist(res, main = paste("Histogram of residuals"), breaks = 10, xlab = "Residuals",
     xlim = range(-15,20), col="purple")
#看起来是symmetrical bell-shaped curve of a normal distribution

#normality test - Anderson-Darling test
library(nortest)
ad.test(res)

#normal probability plot
qqnorm(res, col="blue")
qqline(res, col="red")

#test assumption of equal variances
#tests for equal variances
bartlett.test(values ~ ind, data = stacked)
library(car)
leveneTest(values ~ ind, data = stacked)
#boxplot
means <- c(mean(A), mean(B), mean(C), mean(D), mean(E))
boxplot(A, B, C, D, E, col = "lightblue", names=c("A", "B", "C", "D", "E"))
points(means)
#box长度都差不多说明equal variance

#哪些group的means不同？
#Post-hoc Test
TukeyHSD(anovaresults)
library(PMCMRplus)
summary(lsdTest(anovaresults))
'The difference in results arises because Tukey’s method uses an adjustment for 
multiple comparisons and Fisher method does not. That is why it is recommended to 
use Tukey’s HSD test when using R.'

################################################
#Part B
#analysis of variance
G1 <- vision$Group1
G2 <- vision$Group2
G3 <- vision$Group3
combinedB <- data.frame(cbind(G1, G2, G3))
stackedB <- stack(combinedB)
anovaB <- aov(values ~ ind, data = stackedB)
summary(anovaB)

#assumption of normality and equal variance
resB <- residuals(anovaB)
hist(resB, main = paste("Histogram of residuals"), breaks = 10, xlab = "Residuals",
     xlim = range(-11,11), col="purple")
#normality test - Anderson-Darling test
library(nortest)
ad.test(resB)
#normal probability plot
qqnorm(resB, col="blue")
qqline(resB, col="red")

#equal variances test
bartlett.test(values ~ ind, data = stackedB)

#post-hoc test
TukeyHSD(anovaB)
library(PMCMRplus)
summary(lsdTest(values ~ ind, data = stackedB))

meansB <- c(mean(G1), mean(G2), mean(G3))
meansB
boxplot(G1, G2, G3, col = "purple", main = "Boxplots",
        names=c("Group 1", "Group 2", "Group 3"), boxwex=0.4)
points(meansB)




