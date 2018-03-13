####
####Vivek Kumar Gupta, Problem 6.8 Sharon Lohr
####
####

###
###Data Setup 
###

n = 10 

n = 10
M_i = c(65 , 25 , 48 , 65 , 2 , 62 , 65 , 62 , 61 , 41)
psi = c(.0805452 , .0309789 , .0594796 , .0805452 , 0.0024783 , 0.0768278 , 0.0805452 , 0.0768278 , 0.0755886 , 0.0508055)
y_ij = list(c(3 , 0, 0 , 4), c(2 , 1, 2 , 0), c(0, 0 , 1 , 0) , c(2 , 0 , 1 , 0) , c(2 , 0) ,
            c(0 , 2 , 2, 5) , c(1 , 0 , 0 , 3),c(4 , 1 , 0 , 0), c(2 , 2 , 3 , 1) ,c(2 , 5 , 12 , 3))
y_bar_i = sapply(y_ij , mean)

t_i_est = M_i*y_bar_i

t_uneqp_est = (1/n) * sum(t_i_est / psi)
#1371.9

var_t_uneqp_est = (1/n) * (1/(n - 1)) * sum(( (t_i_est / psi ) - t_uneqp_est ) ^2)
se = sqrt(var_t_uneqp_est)
#372.98