####
####Vivek Kumar Gupta, Problem 6.3 Sharon Lohr 
####
####

store = c('A' , 'B' , 'C' ,'D')
psi = c( .25 , .25 , .25 , .25)
t_i = c(11 , 20 , 24 , 245)
t_psi_est = t_i / psi

#Expected value of t_psi 
t_psi_avg = sum(psi * t_psi_est)

#300 

var_t_psi = sum(psi * (t_psi_est - t_psi_avg)^2 )
#154488 

