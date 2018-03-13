#####Vivek Kumar Gupta . 9/11/2017 Stats 607 Homework assigment 2. 
####2.20 , Sharon Lohr.

#Define CI procedure as given by CI(S) = [t_hat -1.96*SE(t_hat) , t_hat + 1.96*SE(t_hat)]

# Confidence Interval, as we know is a probabilistic statement made on the
#population parameter such as mean, proportion, std deviation, quantiles etc.
#Within the probability framework it is represent by two endpoints C_L and C_U 
#such that P(C_L < popn_param < C_U) = 1 - alpha where alpha is quantity that 
#is known by size or Type I error. Essentially, we state the confidence of 100(1 -) on the process 
#by which these intervals are generated i.e. we are for eg. 95% confident in taking large number of 
#repeated sampling and constructing confidence intervals, 95% of the times the population parameter 
#will fall within the interval. A sample, however, may either contain or not contain the population paramater i.e. 
#the probability is either 1 or 0.

#Exact confidence level.
N=8
n=4
f=n/N

#Population y_i's
y_popn= c(1,2,4,4,7,7,7,8)

#Known population parameter 
t=sum(y_popn)

#Generate all possible samples i.e choose(8,4)= 70 samples and store them in a matrix
S=combn(y_popn,4)

#Apply vector functions to calculate point estimates and their standard errors with fpc correction 
Sample_sum=N*(apply(S,2,mean))
sd_est=apply(S,2,sd)
se_est= (N*(sqrt(1-f))*sd_est)/sqrt(n)

#Store all the upper confidence endpoint in a vector
UCI = Sample_sum + 1.96*se_est

#Store all the lower confidence endpoint in a vector
LCI = Sample_sum - 1.96*se_est
  
#Comapre UCI and LCI with t and sum up the booleans to get the samples that were "good"
GoodSamples=sum((UCI > t) & (LCI < t))
#60 

P(S) = 6-

