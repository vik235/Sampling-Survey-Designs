#####Vivek Kumar Gupta . 9/20/2017 Stats 607 Homework assigment 3. 
####3.5 , Sharon Lohr.

#b) 

N_h=c(9100,1950,5500,10850,2100,5500,900)
N=44000
n_h= c(915,633,658,855,667,833,824)
W_h=N_h/N
p_h_est =c(.37,.23,.23,.29,.19,.43,.41)

#Estimate of the percentage of persons that agree

p_str_est= sum(W_h*p_h_est)

p_str_est*100
#25.81818


var_est_pstr=sum((1 - (n_h/N_h))*((W_h)^2)*p_h_est*(1 - p_h_est)/(n_h - 1 ))
se =sqrt(var_est_pstr)

#standard error of the estimate 
se*100
#0.5627814