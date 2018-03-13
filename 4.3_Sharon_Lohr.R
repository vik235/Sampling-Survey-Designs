###
###Vivek Kumar Gupta, Sharon Lohr - Sampling Design and Analysis 
###
### Problem 4.3 

N = 1132 
n = 20 
x_bar_U = 10.3

Diameter = c(12.0 , 11.4 , 7.9 , 9.0 , 10.5 , 7.9 , 7.3 , 10.2 , 11.7 ,
             11.3 , 5.7 , 8.0 , 10.3 , 12.0 , 9.2 , 8.5 , 7.0 , 10.7 , 9.3 , 8.2 )
Age = c(125 , 119 , 83 , 85, 99, 117 , 69 , 133 , 154 , 168 , 61 , 
        80 , 114 , 147 , 122 , 106 , 82 , 88 , 97 , 99)

##a, Scatter plot of Age vs Diameter. 

plot( Diameter , Age , type = "p" , col = 2, main = "Scatter Plot- Age vs Diameter" , pch = 19 )
abline(lm(Age ~ Diameter) , lty = 2 , lwd = 1 ,  col = 1)
#dev.off()

##
##b 
##

#Estimate of B

B_hat = sum(Age) / sum(Diameter)

#Estimate of mean age of trees 

y_bar_ratio = B_hat * x_bar_U
#117.6204 

segments(x_bar_U, 0 , x_bar_U , y_bar_ratio , lty = 2 , col = "blue" , lwd = 1)
segments(0 , y_bar_ratio, x_bar_U , y_bar_ratio  , lty = 2 , col = "blue" , lwd = 1)
lines(x_bar_U, y_bar_ratio, type = "p" , col = "blue" , pch = 19)
#Se calculations 

resid = Age - B_hat * Diameter

var_y_bar_ratio_est = (1 - n/N)*((x_bar_U/mean(Diameter))^2)*var(resid)/n
se_y_bar_ratio_est = sqrt(var_y_bar_ratio_est)
#4.354872

##
##c 
##

fit.0 = lm (Age ~ Diameter)
fit.0$coefficients

#Estimate of mean age of trees 

y_bar_reg = fit.0$coefficients[1] + fit.0$coefficients[2] * x_bar_U
#118.3634 

#Se calculations 
resid = fit.0$residuals

var_res  = (sum(fit.0$residuals^2))/(n - 2)

var_y_bar_reg_est = (1 - n/N)*var_res/n
se_y_bar_reg_est = sqrt(var_y_bar_reg_est)
#4.070774

segments(x_bar_U, 0 , x_bar_U , y_bar_reg , lty = 2 , col = "magenta" , lwd = 1)
segments(0 , y_bar_reg, x_bar_U , y_bar_reg  , lty = 2 , col = "magenta" , lwd = 1)
lines(x_bar_U, y_bar_reg, type = "p" , col = "magenta" , pch = 19)
##
##d Label
##

legend("topleft", 
       legend = c("y bar estimate - ratio" , "y bar estimate - regression"), 
       col = c("blue" , "magenta") , 
       lty = 2
)
       