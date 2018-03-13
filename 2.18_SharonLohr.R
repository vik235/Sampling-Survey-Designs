2.18

#####Vivek Kumar Gupta . 9/11/2017 Stats 607 Homework assignment 2. 
####2.18 , Sharon Lohr.

#a
#N: It is assumed to be large given the survey in question
golfers = read.csv('C:/Users/vigupta/OneDrive/Learning/DataScience/Statistics Texas A&M University/607/TextMaterial/Dataset/golfsrs.csv', header = TRUE)

n=nrow(golfers)
#120

p_hat= (nrow(golfers[golfers$holes==18,]))/n # estimate of the proportion of golf courses that have 18 holes. 

#We see from below that min (np,n(1-p))>5 so we can use
#asymptotic theory in estimating the parameters.
#min((n*p_hat),(n*(1- p_hat))) >5

se_p=sqrt(p_hat*(1 - p_hat)/n)
#0.04149269

#95% CI of population proportion
(p_hat + qnorm(1 - .05/2)*se_p)
(p_hat - qnorm(1 - .05/2)*se_p)
#(0.6270092,0.7896575)
