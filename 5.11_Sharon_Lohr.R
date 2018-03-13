
##
## Vivek Kumar Gupta, Question 5.11 - Sharon Lohr
##

##
##Part a solution
##
N = 828 
n = 85
M = 215

survey = data.frame("M_i" = rep(215 , 85) , "m_i" = rep(215 , 85) , "Errors" = c(4, 3 , rep(2 , 4) , rep(1 , 22 ), rep(0 , 57)))
attach(survey)
# One - stage cluster sample with equal cluster sizes 

t_hat = (N/n)*sum(survey$Errors)
var_st = var(survey$Errors)
y_bar_est = t_hat /(N * M)
#0.002024624

var_y_bar_est = (1 - n/N) * var_st/(n * (M^2))

se__y_bar_est = sqrt(var_y_bar_est)
#0.0003570679


##
##Part b solution
##
t_hat
#360.4235

var_t_hat_est = (N^2) * (1 - n/N) * var_st/n
se_t_hat_est = sqrt(var_t_hat_est)
#63.56523 

##
##Part c solution: SRS
##
n = 18275
N = 178020 

p_hat = sum(survey$Errors)/n

var_p_hat = (1 - n/N) * p_hat * (1 - p_hat)/(n - 1)
se_p_hat = sqrt(var_p_hat)
#0.0009743498

var_y_bar_est/var_p_hat
#1.285028
#We see that the Se of part c (or var of estimate) is lower than that of part a per expectations