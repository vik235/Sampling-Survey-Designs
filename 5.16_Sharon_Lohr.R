##
## Vivek Kumar Gupta, Question 5.16 - Sharon Lohr
##

##
##Data setup
##
measles = read.csv('C:/Users/vigupta/OneDrive/Learning/DataScience/Statistics Texas A&M University/607/TextMaterial/Dataset/measles.csv' , header = TRUE)
measles$school = as.factor(measles$school)
attach(measles)
N = 46 
n = 10

M_i = tapply(Mitotal , school , mean)
m_i = tapply(mi , school , mean)


##
##Part a solution
##

#Reread the data after removing no returns on consent 
measle_validreturns = measles[measles$returnf != 9 & measles$form == 1, ]

M_i = tapply(measle_validreturns$Mitotal , measle_validreturns$school , mean)
m_i = tapply(measle_validreturns$mi , measle_validreturns$school , mean)

p_hat_i = tapply(measle_validreturns$returnf , measle_validreturns$school , mean)
#Estimate of % of parents who retuned the form . Note: p_hat_i is an unbiased estimator here
#where each scool is considered as SRS

100*p_hat_i

#1        2        3        4        5        6        7        8        9       10 
#50.00000 52.77778 76.47059 60.00000 46.15385 54.16667 68.18182 58.33333 65.71429 41.17647 

##
##Part b solution
##
M_i = tapply(measles$Mitotal , measles$school , mean)
m_i = tapply(measles$mi , measles$school , mean)

sampling_weight_i  = (N*M_i)/(n*m_i)

# 1        2        3        4        5        6        7        8        9       10 
#8.97000 28.81053 63.18947 26.68000 36.18667 34.59200 22.60000 18.18605 35.83158 45.34286 

##
##Part c solution
##

M_i = tapply(measle_validreturns$Mitotal , measle_validreturns$school , mean)
m_i = tapply(measle_validreturns$mi , measle_validreturns$school , mean)
var_i = (1 - m_i/M_i)*p_hat_i *(1 - p_hat_i)/(m_i - 1)
y_bar_ratio_est = sum(M_i * p_hat_i) / sum(M_i) #y_bar is same as p_hat
#0.5789482

#percentage estimate
100 * y_bar_ratio_est
#57.89482

var_r = (1/(n -1)) * sum((M_i * p_hat_i - M_i*y_bar_ratio_est)^2)
var_y_bar_ratio_est = (1/mean(M_i)^2) * (1- n/N)*var_r/n + (1/(n*N*(mean(M_i)^2)))* sum((M_i^2) * (1 - (m_i/M_i))*var_i/m_i)
se_y_bar_ratio_est = sqrt(var_y_bar_ratio_est)
#0.03449436 


#95 CI of percentage
100* (y_bar_ratio_est + c(-1,1)*qnorm(1 - .05/2 )*se_y_bar_ratio_est)
#51.13405 64.65559

##
##Part d solution
##

#Ignoring clustering and treating all the data as SRS 

M_i = tapply(measle_validreturns$Mitotal , measle_validreturns$school , mean)
m_i = tapply(measle_validreturns$mi , measle_validreturns$school , mean)

p_hat_srs = mean(measle_validreturns$returnf )
#0.569395 

n_srs  = sum(m_i)
N_srs = sum(M_i)

var_p_hat_srs_est = (1 - n_srs/N_srs) * p_hat_srs * (1 - p_hat_srs)/(n_srs - 1)

ratio = var_y_bar_ratio_est/var_p_hat_srs_est
#1.760626 

#We see that the clustering reduced the precision of the estimator.
