##
## Vivek Kumar Gupta, Question 5.4 - Sharon Lohr
##

##
##Part b solution
##
journal = read.csv('C:/Users/vigupta/OneDrive/Learning/DataScience/Statistics Texas A&M University/607/TextMaterial/Dataset/journal.csv' , header = TRUE)

N = 1285 
n = 26
M_i = journal[, 1] # first column is the total number of obs units measured = all units in the i_th cluster 
t_i = journal[, 3] # Sum of measurements/observations


#estimate of y_bar_U (proportion in this case), we will use more efficient ratio estimation 

y_bar_ratio_est = sum(t_i)/sum(M_i)
# 0.9256757

##Standard Error calculation, refer 5.17 Sharon Lohr 

y_bar_i = t_i/M_i


se_y_bar_ratio_est = sqrt((1 - n/N)*(1 / (n * mean(M_i)^2))* sum((t_i - y_bar_ratio_est*M_i)^2)/ (n - 1))
#0.03398672

