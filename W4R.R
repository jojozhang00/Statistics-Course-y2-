#导入桌面excel文件
library(readxl)
density <- read_excel("Desktop/Tutorial4_earth_density (1).xlsx")
BMI <- read_excel("Desktop/Tutorial4_BMI (2).xlsx")

#########################################################
# QA
# 画hist图
hist(density$Density,main='histogram of density',col='purple',xlab='density')
hist(density$Density, main = paste("Histogram of Density"), xlab = "Density", col="purple")
#paste()可以将任意数量的参数组合到一起，此处只有一个字符串，可以省略

#t test 用于方差未知
t.test(density$Density,y=NULL, alternative=c('t'),mu=5.1,paired=FALSE,var.equal=FALSE,conf.level=0.95)
t.test(density$Density,y=NULL,mu=5.1) #上面一行可以省略如此行，省略默认值
# alternative是代表alternative hypothesis的情况
# alternative=c('t') 默认情况 表示！=
# alternative=c('g') 表示>
# alternative=c('l') 表示<

t.test(density$Density, y = NULL, alternative = c("l"), mu = 5.5, paired = FALSE,
       var.equal = FALSE, conf.level = 0.95)
t.test(density$Density,y=NULL,alternative=c('l'),mu=5.5)

# z-test 用于方差已知
library(BSDA) #z-test不在R里面，在BSDA里面需要单独调用
z.test(density$Density,y=NULL,alternative=c('t'),mu=5.1,sigma.x=0.22,sigma.y=NULL,conf.level=0.95)


#########################################################
# QB
hist(BMI$BMI)

#没有sigma，采用t-test
#算出99%confidence interval
t.test(BMI$BMI,y=NULL,alternative=c('t'),conf.level=0.99) 

t.test(BMI$BMI,y=NULL,alternative=c('l'),mu=18.5)
'The p-value for this test is 0.4271, so we cannot reject the null hypothesis even at 40%. There is no
evidence that the gymnasts are underweight.'

library(BSDA)
z.test(BMI$BMI,y=NULL,alternative=c('t'),mu=22.3,sigma.x=sqrt(2.56),sigma.y=NUll,conf.level=0.99)











