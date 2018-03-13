###
###Vivek Kumar Gupta, question 4.13 Sharon Lohr
###
counties = read.csv('C:/Users/vigupta/OneDrive/Learning/DataScience/Statistics Texas A&M University/607/TextMaterial/Dataset/counties.csv', header = T)

head(counties)

attach(counties)
n = 100 
N = 3141 

#a Plot of veterans in the sample 

hist(veterans , nclass = 50 , freq = T)
plot(density(veterans) , xlab = 'veterans' , main = 'Density Plot of veterans')

#b 

y_bar = mean(veterans)

y_est = N*y_bar
#38476339 

var_est = (1 - n/N)*var(veterans)/n
se = sqrt(var_est)
#4681.145

#c

plot(totpop, veterans, xlab ="Population" , ylab = "Veterans" , main = "Plot of Veterans vs Population" )
abline(lm(veterans ~ totpop) , col=2)
fit.0 = lm(veterans ~ totpop) 
summary(fit.0)

#Call:
#  lm(formula = veterans ~ totpop)

#Coefficients:
#  (Intercept)       totpop  
#1534.2013       0.0904  


#Call:
#  lm(formula = veterans ~ totpop)

#Residuals:
#  Min      1Q  Median      3Q     Max 
#-8263.3 -1473.4 -1177.7    83.6 20967.6 

#Coefficients:
#  Estimate Std. Error t value Pr(>|t|)    
#(Intercept) 1.534e+03  3.808e+02   4.028 0.000111 ***
#  totpop      9.040e-02  7.114e-04 127.072  < 2e-16 ***
#  ---
  # Signif. codes:  0 '***' 0.001 '**' 0.01 '*' 0.05 '.' 0.1 ' ' 1

#Residual standard error: 3714 on 98 degrees of freedom
#Multiple R-squared:  0.994,	Adjusted R-squared:  0.9939 
#F-statistic: 1.615e+04 on 1 and 98 DF,  p-value: < 2.2e-16


r = cor(veterans, totpop)
#0.9969

##Based on the above fit line we see that we have an intercept in the model and thus we will use regression estimation procedures 
##to estimate veterans in population

#d 
t_x = 255077536

intcpt = rep(1, length(totpop))
X = cbind ( intcpt , totpop)

B_vector = solve(t(X)%*%X)%*%t(X)%*%veterans
B0_est = B_vector[1]
#1534.201

B1_est = B_vector[2]
#0.09040246

y_reg_est = B0_est + (B1_est * mean(totpop))
#12249.71
t_y_est_reg = N*y_reg_est
#38476339

resid = veterans - (B0_est + B1_est * totpop)

var_y_bar_reg = ((1 - n/N)*var(resid)/n)
var_t_y_reg = (N^2)*var_y_bar_reg
se_t_y_reg = sqrt(var_t_y_reg)
#se = 1142010
