# Q1
# store data
# data.frame 加点，c()代表把数据存储为vector或者list
fuelDF <- data.frame('temp'=c(28,28,32.5,39,45.9,57.8,58.1,62.5),'fuel'=c(12.4,11.7,12.4,10.8,9.4,9.5,8.0,7.5))

#画散点图
plot(fuelDF$temp,fuelDF$fuel)

#用excel导入数据
library(readxl)
fuel <- read_excel("Desktop/fuel.xlsx")

#main设置标题，xlab和ylab设置行列名称
plot(fuel$temp,fuel$fuel, main='scatterplot', xlab='temp', ylab='fuel')

################################################
#Q2
"
dnorm: density function of the normal distribution
pnorm: cumulative density function of the normal distribution
qnorm: quantile function of the normal distribution
rnorm: random sampling from the normal distribution
"
#创建新变量并做简单计算
differences <- fuel$temp-fuel$fuel
differences

product <-fuel$fuel*fuel$temp
product

suqre <- fuel$fuel^2
suqre

natlog <- log(fuel$temp)
natlog

log12 <- log(fuel$temp, 12) #base可以省略不写，默认log()函数右边的数字代表底
log12 <- log(fuel$temp, base=12)
log12

#################################################
#Q3
#计算概率
# The pnorm function gives the Cumulative Distribution Function (CDF) of the Normal distribution in R,
#which is the probability that the variable X takes a value lower or equal to x.
pnorm(1.56, mean=1.2, sd=sqrt(8)) 

#计算一串概率
x <- c(0.981,0.569,2.31,0.223)
pnorm(x,mean=-1,sd=4, log=FALSE) #log=FALSE返回原始密度，这里不写对结果没有影响

#The function qnorm(), which comes standard with R, aims to do the opposite: given an area, 
#find the boundary value that determines this area.
#求z(0.05)
qnorm(0.05,mean=0,sd=1,lower.tail=FALSE) #lower.tail取左半边还是右半边
qnorm(0.95,mean=0,sd=1,lower.tail=TRUE)
#求z(0.01)
qnorm(0.1,mean=0,sd=1,lower.tail=FALSE)

#t分布
qt(0.95, 10, lower.tail = TRUE) #10是df:degree of freedom
qt(0.05, 10, lower.tail = FALSE) #0.05在表中对应P=5，即a/2=0.05（P需要除以100为所求真实值）

qt(0.01, 13, lower.tail = FALSE)
qt(0.1, 25, lower.tail = FALSE)

#F分布
qf(0.05, 5, 7, lower.tail = FALSE) #(m,n)为自由度
qf(0.05, 7, 5, lower.tail = FALSE)
qf(0.1, 12, 1, lower.tail = FALSE)
qf(0.99, 1, 12, lower.tail = FALSE)

#chi-squared
qchisq(0.01, 9, lower.tail = FALSE) #第二位为自由度
qchisq(0.95, 9, lower.tail = FALSE)
qchisq(0.1, 23, lower.tail = FALSE)
qchisq(0.99, 23, lower.tail = FALSE)

#######################################
#Q4
#plot probability density functions
#plot N(-2,25)
x<-seq(from=-15,to=+11, length.out=100000)#length.out是数列长度
# dnorm(x,mean=-2,sd=5) 所有pdf的值，cdf是pdf的积分
plot(x,dnorm(x,mean=-2,sd=5),main='Normal distributionwith mean -2 and variance 25',  type="l",col='blue')
# type是连接形式，l是用line连接，p是用point，b是既line又point

#t9分布
y1<-seq(from=-5,to=+5, length.out=100000)
plot(y1,dt(y1, 9), main="t-distribution", type="l", col ="purple")

#F_10,1分布
y2<-seq(from=0,to=+10, length.out=100000)
plot(y2,df(y2, 10,1), main="F-distribution", type="l", col ="blue")

#pdf for chisq-distribution X_10^2
y3<-seq(from=0,to=+40, length.out=100000)
plot(y3,dchisq(y3, 10), main="Chi-squared -distribution", type="l", col ="red")

#画两张图进行对比
#standard normal and t10 in one picture
y4<-seq(from=-5,to=+5, length.out=100000)
plot(y4,dt(y4, 10), main="t-distribution", type="l", col ="purple")
points(y4,dnorm(y4), main="standard normal", type="l", col ="red") #points函数将点画于已经存的坐标系上

#画三个不同自由度的t分布在一张图上
y5<-seq(from=-5,to=+5, length.out=100000)
plot(y5,dt(y5, 4), main="t-distribution", type="l", col ="purple")
points(y5,dt(y5, 6), main="t-distribution",type="l", col ="red")
points(y5,dt(y5, 8),main="t-distribution", type="l", col ="blue")

#chisquared and F(10,1) in one picture
y6<-seq(from=0,to=+30, length.out=100000)
plot(y6,dchisq(y6, 10), main="Chi-squared and F distributions", type="l",
     col ="blue", ylim = c(0,0.5))
points(y6,df(y6, 10, 1), main="F", type="l", col ="green")


