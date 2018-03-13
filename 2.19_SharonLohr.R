#####Vivek Kumar Gupta . 9/11/2017 Stats 607 Homework assigment 2. 
####2.19 , Sharon Lohr.


City = c("Buckeye", "Gilbert","Gila Bend","Phoenix","Tempe")
Popn = c(4857,59338,1724,1149417,153821)
z = qnorm(1-.05/2)
n0 =  ceiling(((z^2)*(1/4))/(.04^2))
n0_all = rep(n0, length(City))
#Below is the min sample size without fpc adjustment 
#Note: each index is based on index of City. 
# 601 601 601 601 601


n_min = ceiling(n0_all*(1+ (n0_all/Popn))^-1)
#Below is the min sample size after fpc adjustment 

#535 595 446 601 599


#From the above output we can see that FPC adjustment makes a difference in City=Buckeye, "Gila Bend"