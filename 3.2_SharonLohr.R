#####Vivek Kumar Gupta . 9/20/2017 Stats 607 Homework assigment 3. 
####3.2 , Sharon Lohr.

y = c(1,2,4,8,4,7,7,7)
stratum = c(rep(1,4),rep(2,4))
n1=2
n2=2
##
##a: Write all possible SRS's of size 2 from stratum 1 
##
Sample_Str1 = combn(y[1:4],2)

#All samples,y_1j where J is element of S, from stratum 1 are given below, represented in column vectors 
#``````[,1] [,2] [,3] [,4] [,5] [,6]
#[1,]    1    1    1    2    2    4
#[2,]    2    4    8    4    8    8

#prob of each sample, P(S_i), for stratum 1 

p_S1i= 1/choose(4,2) = 1/6

#a: Write all possible SRS's of size 2 from stratum 2
Sample_Str2 = combn(y[5:8],2)

#All samples from stratum 2 are given below, represented in column vectors 
#     [,1] [,2] [,3] [,4] [,5] [,6]
#[1,]    4    4    4    7    7    7
#[2,]    7    7    7    7    7    7

#prob of each sample, P(S_i), for stratum 2
p_S2i= 1/choose(4,2) = 1/6

##
##b) Sampling distribution of est_t_str 
##

Sampling_weight_propalloc= 2

t1= Sampling_weight_propalloc*apply(Sample_Str1,2,sum)
t2= Sampling_weight_propalloc*apply(Sample_Str2,2,sum)

#prep up the outer function to give t_str for any combination of samples from stratum1 and stratum2
f <- function(x,y)
{
  z=cbind(x,y)
  z[,1] +z[,2]
}

#Sampling distribution 
z=as.vector(outer(t1,t2,f))
plot(density(z), main = "Sampling distribution of the point estimate", xlab="t,stratified")

##
##c) Mean and Variance of Sampling distribution of est_t_str 
##

mean= mean(z)
#40
variance = (sd(z))^2
#48.68571

# We noted that the mean in example 2.2 was 40 and variance in SRS was 54.86. With Startification 
#and then independent SRS in each start the variance of the estimator is reduced to 48.61
