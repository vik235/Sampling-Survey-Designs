###
###Vivek Kumar Gupta, Sharon Lohr - Sampling Design and Analysis 
###
### Problem 4.8

#Prepare data
ag = read.csv("C:/Users/vigupta/OneDrive/Learning/DataScience/Statistics Texas A&M University/607/TextMaterial/Dataset/agsrs.csv", header = T)
attach(ag)
n = nrow(ag)
N = 3078
t_x =  2087759

##a, Scatter plot of No. of Acres in 1992 vs 1987. 

plot( farms87 , acres92 , type = "p" , col = 2, main = "Scatter Plot- No. of farms in 87 vs acres92" , pch = 19 )
abline(lm(acres92 ~ farms87)  , lwd = 1 ,  col = 1)

##b

B_est = mean( acres92) / mean(farms87)
t_y = B_est * t_x
#960155061

##c 

#estimate coefficients 

intcpt = rep(1, length(farms87))
X = cbind ( intcpt , farms87)

B_vector = solve(t(X)%*%X)%*%t(X)%*%acres92
B0_est = B_vector[1]
#267029.8

B1_est = B_vector[2]
#47.65325

#Estimate acres in 1992 

y_reg_est = B0_est + (B1_est * mean(farms87))
#297897
t_y_est_reg = N*y_reg_est
#916927110



#d 

#estimating precision by var ~ mse of the estimators

# i . for ratio with farms87 

B_est = mean( acres92) / mean(farms87)
t_y = B_est * t_x

resid = acres92 - B_est * farms87

var_B_est = (1 - n/N)*(var(resid))/(n * mean(farms87)*mean(farms87))

var_t_y_est = (t_x^2)*var_B_est

se = sqrt(var_t_y_est)
#68446406

# ii . for reg with farms87 

resid = acres92 - (B0_est + B1_est * farms87)

var_y_bar_reg = ((1 - n/N)*var(resid)/n)
var_t_y_reg = (N^2)*var_y_bar_reg
se_t_y_reg = sqrt(var_t_y_reg)
#58065813

var_B_est = (1 - n/N)*(var(acres92)*(1 - (cor(acres92 , farms87))^2))/n 

var_t_y_est = (N^2)*var_B_est

se = sqrt(var_t_y_est)
#58065813

# iii . for ratio estimation with acres87
t_x = 964470625

B_est = mean(acres92)/mean(acres87)

t_y_est_ratio_acres87 = B_est * t_x
#951513191 

resid = acres92 - B_est * acres87

var_B_est_est = (1 - n/N)*var(resid)/(n * mean(acres87)* mean(acres87))
var_t_y_est_est = t_x^2*var_B_est_est
se = sqrt(var_t_y_est_est)
#5546162


###
###Comparison of i. , ii. and iii. We see the the estimates from using auxilliary variable acres 87 
###has a high precision as compared to other ones. This is because of high correlation between acreage in 87 to that of 92
###As seen in the plot, the regression line passes through the origin and thus the ration estimate is highly efficient.
###
plot( acres87 , acres92 , type = "p" , col = 2, main = "Scatter Plot- acreagee in 87 vs acres92" , pch = 19 )
cor(acres87 , acres92)
#.9958 : Correlation between acres87 and acres92
abline(summary(lm(acres92 ~ acres87)))



