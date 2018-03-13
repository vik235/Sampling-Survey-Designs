####
####Vivek Kumar Gupta, Problem 6.4 Sharon Lohr 
####
####

store = c('A' , 'B' , 'C' ,'D')
psi = c( 7/16 , rep(3/16 , 3))
t_i = c(11 , 20 , 24 , 245)
t_psi_est = t_i / psi

#Expected value of t_psi 
t_psi_avg = sum(psi * t_psi_est)
#300 
# Above value is same as the population total which is 300 and hence this 
#t_psi_est 
#in this sampling scheme is still an unbiased estimator of the population total

var_t_psi = sum(psi * (t_psi_est - t_psi_avg)^2 )
#235615.2

#We know that Var_T_srs is lesser than the above value and hence the sampling scheme
#with the above choices of psi is not giving a precise estimate even though it is unbiased
#We also know that the total sales in a store is intuitively proportional 
#to the size of the store which implies in general we also expect 
#to see a larger variance in bigger store as compared to the smaller 
#ones. Thus, ideally we will like to sample larger store with larger selection 
#probablity. The presented sampling scheme in this probelm is hence 
#not precise or good.

