#####Vivek Kumar Gupta . 9/20/2017 Stats 607 Homework assignment 4. 
####3.22 , Sharon Lohr.
#Given data points 
#a
p = c(.1 , .03)
Wh= c(0.4, 1 - .4)
Sh = sqrt(p*(1-p))
n = 2000  
#ch are contant so we can use special case of optimal allocation i.e. Neyman allocation. 

nh = n * (Wh * Sh)/sum((Wh * Sh))
#Actual results : 1079.368  920.632
#n1 = 1079 n2 = 921 (after approximations)

#b

#i. under Proportion allocation. 

nh_prop = n*c(Wh)

var_p_prop = sum((Wh^2)*p*(1 - p)/nh_prop)
#2.673e-05


#ii. Under Optimal allocation. 

nh_opt = c(1079, 921)

var_p_opt = sum((Wh^2)*p*(1 - p)/nh_opt)
#2.472028e-05

#iii. Under SRS. 

p_srs = sum(Wh*p)
var_p_srs = p_srs*(1 - p_srs)/n
#2.7318e-05
