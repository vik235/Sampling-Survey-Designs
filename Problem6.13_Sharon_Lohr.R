####
####Vivek Kumar Gupta, Problem 6.13 Sharon Lohr
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
plot(statepop$psi , statepop$veterans , col = 2, xlab = "Sampling Probability - PPS" , ylab = "# of Veterans" , main = "Plot of veterans vs sampling probability" )
abline(lm(statepop$veterans ~ statepop$psi))
#We see a very goot linear fit and a strong correlation between the two plotted in the graph and thus it seems that the PPS based sampling scheme 
#is a good sampling scheme

cor(statepop$psi , statepop$veterans)
#0.9871941

#Conclusion also made stronger by a strong correlation between the two data variables 

###
###Part b solution 
###

t_psi_est = (1 / n) * sum (statepop$veterans / statepop$psi)
#27914180

var_t_psi_est = (1 / n) * (1 / (n - 1)) *sum( ((statepop$veterans / statepop$psi ) - t_psi_est)^2)
se_t_psi_est = sqrt(var_t_psi_est)
#1087453

###
###Part c solution 
###

#Estimates pf th epopn prop of vietnam vets.
statepop$vietvet = round(statepop$veterans * statepop$percviet / 100, 0)
M_o_est = (1/n) * sum(statepop$veterans / statepop$psi) #Note M_i's are # of veterans here as in 6.15
t_vet_psi_est = (1/n) *(sum( statepop$vietvet / statepop$psi))
#8050346
y_bar_vet_psi_est = t_vet_psi_est / M_o_est
#0.2883963

var_y_bar_vet_psi_est = (1 / M_o_est^2 )*(1/n)*(1/(n - 1)) * sum(((statepop$vietvet / statepop$psi) - (y_bar_vet_psi_est*statepop$veterans/psi))^2)
se_y_bar_vet_psi_est = sqrt(var_y_bar_vet_psi_est)
#0.01876168


#Estimates of the popn prop of vietnam vets.

statepop$vietvet = round(statepop$veterans * statepop$percviet / 100, 0)
t_vet_psi_est = (1/n) *(sum( statepop$vietvet / statepop$psi))
#8050346

var_t_vet_psi_est = (1/n) * ((1/(n - 1))) * sum( ((statepop$vietvet / statepop$psi ) - t_vet_psi_est)^2)
se_t_vet_psi_est = sqrt(var_t_vet_psi_est)
#327350.3