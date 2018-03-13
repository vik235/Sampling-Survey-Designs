
##
## Vivek Kumar Gupta, Question 5.12 - Sharon Lohr
##

coots = read.csv("C:/Users/vigupta/OneDrive/Learning/DataScience/Statistics Texas A&M University/607/TextMaterial/Dataset/coots.csv" , header = TRUE)
plot ( coots$clutch , coots$length  ,  type = "p", ylab = "Length of an Egg" ,xlab = "Clutch Number" , main = "Scatter Plot of Lengths vs Clutch")
coots$clutch = as.factor(coots$clutch)
n = 184 
attach(coots)
str(coots)

y_bar_i = tapply(length , clutch , mean )
M_i = tapply(csize , clutch , mean )
m_i = rep(2, 184) # as only 2 eggs are observed

var_i = tapply(length , clutch , var)


y_bar_ratio_est = sum(M_i * y_bar_i) / sum(M_i)
#48.64863 

var_r = (1/(n -1)) * sum((M_i * y_bar_i - M_i*y_bar_ratio_est)^2)

var_y_bar_ratio_est = (1/(mean(M_i)^2)) * var_r/n # assume fpc = 1 and second variance term negligible
se_y_bar_ratio_est = sqrt(var_y_bar_ratio_est)
#0.129321

