###
###Vivek Kumar Gupta, question A.1 Dr Wang
###

n = 21
t_yr_est = 28367 
t_x = 22919 
c_ybar_ybar = .01421
c_ybar_xbar = .01465
c_xbar_xbar = .01568
z_sq = (qnorm(1 - .05/2))^2

B_est = t_yr_est/t_x
#1.237707

#Conf Int B
B_Conf = (B_est/(1 - z_sq*c_xbar_xbar))*(1 - z_sq*c_ybar_xbar + 
                                  c(-1,1)*sqrt(c_ybar_ybar + c_xbar_xbar - 2*c_ybar_xbar - z_sq*(c_ybar_ybar*c_xbar_xbar - c_ybar_xbar^2)))
#1.211792 1.274044

t_y_conf = B_Conf*t_x
#27773.06 29199.81