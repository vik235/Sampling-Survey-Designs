#####Vivek Kumar Gupta . 9/20/2017 Stats 607 Homework assigment 3. 
####3.7 , Sharon Lohr.

##a
#Total sampling fram 
N =807 

#Sampling frame withing the stratum
N_h=c(102,310,217,178)

#samples within the stratum
n_h= c(7,19,13,11)

#data model
f_b= c(1,2,0,1,0,2,0,1,0)
f_p= c(10,2,0,1,2,1,1,0,2)
f_s= c(9,0,1,0,2,0,1,0,0)
f_h= c(8,2,0,1,0,0,0,0,0)
refereed_pubs= 0:8

#t _estimate in each stratum
total_refereed_bio = (f_b*refereed_pubs)
total_refereed_phy = (f_p*refereed_pubs)
total_refereed_soc = (f_s*refereed_pubs)
total_refereed_hum = (f_h*refereed_pubs)

#Weighted average.
t_h_bio= (102/7)*sum(total_refereed_bio)
t_h_phy= (310/19)*sum(total_refereed_phy)
t_h_soc= (217/13)*sum(total_refereed_soc)
t_h_hum= (178/11)*sum(total_refereed_hum)

#Estimate of t via stratification
t_str = sum(t_h_bio + t_h_phy + t_h_soc + t_h_hum)
t_h_str = c(t_h_bio , t_h_phy , t_h_soc , t_h_hum)
#1321.189

s_h = c(sd(total_refereed_bio),sd(total_refereed_phy), sd(total_refereed_soc),sd(total_refereed_hum))

Var_t_str= sum((1 - (n_h/N_h))*(N_h^2)*(s_h^2)/n_h)
##SE of the estimate 
sqrt(Var_t_str)
#427.3931

##b Via SRS we had obtained Y_bar str= 1.78, implies that t_str is 1.78*N= 1436.46 
#which is a bit higher than that of stratified estimate
#SE via SRS is 488.8848 as shown below, thus our estimate via startification is more precise as it has less SE. 
sqrt(.367*N^2)


#c

p_hat_h= c(f_b[1]/n_h[1],f_p[1]/n_h[2],f_s[1]/n_h[3],f_h[1]/n_h[4])

p_est_str = sum((N_h/N)*p_hat_h)
#0.5668087

fpc= 1- n_h/N_h #fpc in each strata
W_h=N_h/N #sampling weight in each strata

var_p_est_str = sum(fpc*(W_h^2)*p_est_str*(1 - p_est_str)/(n_h - 1))

#se of the estimate 
sqrt(var_p_est_str)
# 0.07078133

#d We can see the outcome of a and b subparts of the question that the
#precision has definitely increased by precision. 
#Why did it increase?
#1. We can see that the s_h wthing the stratums are pretty close 
s_h
#3.678013 5.198825 3.073181 1.130388


#2 The individual stratums for the paramter t has different means than the population 
t_h_str
#320.57143 652.63158 267.07692  80.90909

#Overall estimate error of parameter if we ignore stratums is 
#higher than that of startification as shown in #b
