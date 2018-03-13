####
####Vivek Kumar Gupta, Problem 6.3 Sharon Lohr 
####
####

store = c('A' , 'B' , 'C' ,'D')
psi = c( 1/16 , 2/16 , 3/16 , 10/16)
t_i = c(rep(75, 4 ))
t_psi_est = t_i / psi


#Expected value of t_psi 
t_psi_avg = sum(psi * t_psi_est)

#300 
var_t_psi = sum(psi * (t_psi_est - t_psi_avg)^2 )
#84000 

