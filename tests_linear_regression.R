#Confidence Intervals for intercept beta0, slope beta1
CI_beta0_beta1 <- function(sumxi, sum_xi2, MSE, n, bar_beta0, bar_beta1, alpha){
  #initialize the parameters
  bar_sigma <- 0
  ese_beta0 <- 0
  ese_beta1 <- 0
  t_alpha <- 0
  left0 <- 0
  right0 <- 0
  left1 <- 0
  right <- 0
  # compute some basic statistics
  bar_sigma <- sqrt(MSE)
  ese_beta0 <- bar_sigma*sqrt(1/n+(sumxi/n)^2/(sum_xi2-n*(sumxi/n)^2))
  ese_beta1 <- bar_sigma/sqrt(sum_xi2-n*(sumxi/n)^2)
  t_alpha <- qt(alpha/2, n-2, lower.tail = FALSE) #alpha should be devided by 2 here
  left0 <- bar_beta0-t_alpha*ese_beta0
  right0 <- bar_beta0+t_alpha*ese_beta0
  left1 <- bar_beta1-t_alpha*ese_beta1
  right1 <- bar_beta1+t_alpha*ese_beta1
  #print the output
  print("The confidence interval for slope beta1 is:")
  print(paste("[",left1,", ",right1,"]"))
  print("The confidence interval for intercept beta0 is:")
  print(paste("[",left0,", ",right0,"]"))
}

#test
CI_beta0_beta1(144, 2116, 525.838, 11, -5.227, -3.983, 0.05)
CI_beta0_beta1(300, 4102, 100.685, 25, -8.679, -1.307, 0.01)

################################################################################

#Confidence interval for a mean response, response
CI_meanResponse_Response <- function(sumxi, sum_xi2, MSE, n, bar_beta0, bar_beta1, alpha, x0){
  h00 <- 0
  t_alpha <- 0
  bar_y0 <- 0
  left_meanR <- 0
  right_meanR <- 0
  left_R <- 0
  right_R <-0
  h00 <- 1/n + (x0-sumxi/n)^2/(sum_xi2-n*(sumxi/n)^2)
  t_alpha <- qt(alpha/2, n-2, lower.tail = FALSE)
  bar_y0 <- bar_beta0 + bar_beta1*x0
  left_meanR <- bar_y0 - t_alpha*sqrt(MSE*h00)
  right_meanR <- bar_y0 + t_alpha*sqrt(MSE*h00)
  left_R <- bar_y0 - t_alpha*sqrt(MSE*(1+h00))
  right_R <- bar_y0 + t_alpha*sqrt(MSE*(1+h00))
  print(paste((1-alpha)*100, "% prediction interval for mean response y is "))
  print(paste("[",left_meanR,", ",right_meanR,"]"))
  print(paste((1-alpha)*100, "% prediction interval for response y is "))
  print(paste("[",left_R,", ",right_R,"]"))
}

#test
CI_meanResponse_Response(205, 2685, 355.25, 18, -6.748, -3.534, 0.05, 59)
CI_meanResponse_Response(241, 3341, 134.942, 20, -4.213, -1.596, 0.05, 36)

################################################################################

#hypothesis test in linear regression
#test for slope (one-sided)
test_slope <- function(sumxi, sum_xi2, MSE, n, bar_beta0, bar_beta1, c){
  ese_beta1 <- 0
  bar_sigma <- 0
  t_alpha <- 0
  T <- 0
  bar_sigma <- sqrt(MSE)
  ese_beta1 <- bar_sigma/sqrt(sum_xi2-n*(sumxi/n)^2) 
  T <- (bar_beta1-c)/ese_beta1
  print(paste('t(0.1) is: ', qt(0.1, n-2, lower.tail = FALSE)))
  print(paste('t(0.05) is: ', qt(0.05, n-2, lower.tail = FALSE)))
  print(paste('t(0.01) is: ', qt(0.01, n-2, lower.tail = FALSE)))
  print(paste('t(0.001) is: ', qt(0.001, n-2, lower.tail = FALSE)))
  print(paste("Test statistis is: ", T))
}

#test
test_slope(322, 4416, 89.627, 27, 6.494, -1.029, -0.6)

################################################################################











