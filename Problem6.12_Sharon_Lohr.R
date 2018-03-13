####
####Vivek Kumar Gupta, Problem 6.12 Sharon Lohr
####
####

###
###Data Setup 
###


statepop = read.csv('C:/Users/vigupta/OneDrive/Learning/DataScience/Statistics Texas A&M University/607/TextMaterial/Dataset/statepop.csv' , header = TRUE)
attach(statepop)
M_o = 255077536
n = nrow(statepop)
statepop$psi = popn / M_o
attach(statepop)

###
###Part a solution 
###
plot(statepop$psi , statepop$numfarm , col = 2, xlab = "Sampling Probability - PPS" , ylab = "# of Farms" , main = "Plot of farms vs sampling probability" )
abline(lm(statepop$numfarm ~ statepop$psi))
#We see a poor fit and a poor relation between the two plotted in the graph and thus it seems that the PPS based sampling scheme 
#may not be the best sampling scheme

cor(statepop$psi , statepop$numfarm)
#.256

#Conclusion also made stronger by a weak correlation between the two data points. 

###
###Part b solution 
###

t_psi_est = (1 / n) * sum (statepop$numfarm / statepop$psi)
#1896300

var_t_psi_est = (1 / n) * (1 / (n - 1)) *sum( ((statepop$numfarm / statepop$psi ) - t_psi_est)^2)
se_t_psi_est = sqrt(var_t_psi_est)
#367422.5 
