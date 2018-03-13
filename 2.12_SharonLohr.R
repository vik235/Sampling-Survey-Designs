#####Vivek Kumar Gupta . 9/11/2017 Stats 607 Homework assigment 2. 
####2.12 , Sharon Lohr.

#a
#N: It is assumed to be large given the survey in question
n=580
n = ((qnorm(1 - .05/2))^2)/(4*(.1)^2)
#97

#b
n=120 
p_hat= (27)/120 # per the subquestion parameter of interest, p is patients not overdue. 

#We see from below that min (np,n(1-p))>5 so we can use
#asymptotic theory in estimating the parameters.
#min((n*p_hat),(n*(1- p_hat))) >5

se_p=sqrt(p_hat*(1 - p_hat)/n)
#0.04261122

#95% CI of 
p_hat + qnorm(1 - .05/2)*se_p
p_hat - qnorm(1 - .05/2)*se_p
#(0.1414835,0.3085165)
