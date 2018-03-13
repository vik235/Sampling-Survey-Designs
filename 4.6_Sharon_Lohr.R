###
###Vivek Kumar Gupta, Sharon Lohr - Sampling Design and Analysis 
###
### Problem 4.6 

#Prepare data

golfers = read.csv("C:/Users/vigupta/OneDrive/Learning/DataScience/Statistics Texas A&M University/607/TextMaterial/Dataset/golfsrs.csv", header = TRUE)
golfers_cln = golfers[!is.na(golfers$wkend18) , ]
attach(golfers_cln)
n = nrow(golfers_cln)
#Plot the data, Weened 18 vs Backtee

plot(backtee, wkend18 , type = "p" , col = 2, main = "Scatter Plot- Weekend 18H Greens Fee vs Backtee yardage" , pch = 19 )
abline(lm(wkend18 ~ backtee) , lty = 2 , lwd = 1 ,  col = 1)
fit.0 = lm(wkend18 ~ backtee)
#estimate correlation 
cor(backtee ,wkend18 , method = c("pearson" ))
#0.3370142 Pretty weak correlation between the 2 variables

#estimate coefficients 

intcpt = rep(1, length(backtee))
X = cbind ( intcpt , backtee)

B_vector = solve(t(X)%*%X)%*%t(X)%*%wkend18
B0_est = B_vector[1]
#-37.26008

B1_est = B_vector[2]
#0.01127747

#calculate estimates by non matrix way to check. 
B1_est = sum (( backtee - mean(backtee)) * ( wkend18 - mean(wkend18)) ) / sum((( backtee - mean(backtee)))^2)

B0_est = mean(wkend18) - B1_est*mean(backtee)

#Estimate weekend 18-hole greens fee , E(Y|X)

y_bar__reg_est = B0_est + B1_est * mean(backtee)

#34.82882

#SE Estimated
sqrt(var(wkend18)*(1 - 0.3370142^2)/n)

#2.030873

se_y_bar_reg_est = sqrt((1 - n/N) * ((var(fit.0$residuals)/ (n - 2))*(n - 1))/n)
