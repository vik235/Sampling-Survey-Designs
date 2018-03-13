
##
## Vivek Kumar Gupta, Question 5.14 - Sharon Lohr
##

N = 29 
n = 4 

#M_i = c(1471 , 890 , 1021 , 1587)
M_i = c(792 , 447 , 511 , 800)
m_i = c(25 , 15 , 20 , 40)
Smokers = c(10 , 3 , 6 , 27)

Females = data.frame("M_i" = M_i , "m_i" = m_i , Smokers= Smokers)

#Two stage cluster sampling setup 

y_bar_i = Females$Smokers / Females$m_i #y_bar_i is the proportion in this case which transaltes to percentage when multiplied by 100
var_i = (1 - m_i/M_i)*y_bar_i *(1 - y_bar_i)/(m_i - 1)

y_bar_ratio_est = sum(M_i * y_bar_i) / sum(M_i)
#Estimate = 0.4311765
# Percentage = 43.11765

var_r = (1/(n -1)) * sum((M_i * y_bar_i - M_i*y_bar_ratio_est)^2)
var_y_bar_ratio_est = (1/mean(M_i)^2) * (1- n/N)*var_r/n + (1/(n*N*(mean(M_i)^2)))* sum((M_i^2) * (1 - (m_i/M_i))*var_i/m_i)
se_y_bar_ratio_est = sqrt(var_y_bar_ratio_est)
#0.09761019 


#95 CI of percentage
100* (y_bar_ratio_est + c(-1,1)*qnorm(1 - .05/2 )*se_y_bar_ratio_est)
#23.98640 62.24889