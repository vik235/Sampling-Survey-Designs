#####Vivek Kumar Gupta . 9/20/2017 Stats 607 Homework assignment 4. 
####3.17 , Sharon Lohr.
#Given data points 
#a
divorce = read.csv('C:/Users/vigupta/OneDrive/Learning/DataScience/Statistics Texas A&M University/607/TextMaterial/Dataset/divorce.csv', header = TRUE)


divorce$stratumsize = divorce$numrecs/divorce$samprate

#Total number of divorces in 1987 = N1+N2+..+NH
N = sum(divorce$stratumsize)

#571185

#b 

# It is likely that during initial studies NCHS noted that the variance in number of divorces in various parts 
# of the states were very different and hence it was desired to use an optimal allocation of sample across 
# each state which are also the stratas so that an optimal variance of the estimate could be obtained. 

#c 
divorce$hsblt24 = divorce$hsblt20 + divorce$hsb20.24

est= sum((1/divorce$samprate)*divorce$hsblt24)

divorce$hsblt24ph = divorce$hsblt24/divorce$numrecs

t_hsblt24_str_est= sum(divorce$hsblt24ph * divorce$stratumsize)
#49039

var_t_hsblt24_str_est = sum((divorce$stratumsize^2)*(1 - divorce$numrecs/divorce$stratumsize)*divorce$hsblt24ph*(1 - divorce$hsblt24ph)/(divorce$numrecs - 1))
se_t_hsblt24_str_est =sqrt(var_t_hsblt24_str_est)
#448.8004
#95% CI 

t_hsblt24_str_est + c(-1,1)*qnorm(1 - .05/2)*se_t_hsblt24_str_est

#48159.37 49918.63 , approximated to 48159 49919

divorce$wflt24 = divorce$wflt20 + divorce$wf20.24



divorce$wflt24ph = divorce$wflt24/divorce$numrecs

t_wflt24_str_est= sum(divorce$wflt24ph * divorce$stratumsize)
#86619

var_t_wflt24_str_est = sum((divorce$stratumsize^2)*(1 - divorce$numrecs/divorce$stratumsize)*divorce$wflt24ph*(1 - divorce$wflt24ph)/(divorce$numrecs - 1))
se_t_wflt24_str_est =sqrt(var_t_wflt24_str_est)
#564.4915
#95% CI 

t_wflt24_str_est + c(-1,1)*qnorm(1 - .05/2)*se_t_wflt24_str_est

#85512.62 87725.38 , approximated to 85513 87725

