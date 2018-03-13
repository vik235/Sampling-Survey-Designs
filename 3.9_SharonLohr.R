#####Vivek Kumar Gupta . 9/20/2017 Stats 607 Homework assigment 3. 
####3.9 , Sharon Lohr.

agr = read.csv('C:/Users/vigupta/OneDrive/Learning/DataScience/Statistics Texas A&M University/607/TextMaterial/Dataset/agstrat.csv' ,
                 header = TRUE, sep = ",")

#a
plot(density(agr$acres87) , main = "Density plot of Y, Acres in 1987",
      ylab = "Density, f(y)", xlab = "Y, Areas in acres devoted to farm, 1987",
      col ="magenta", type = "o")

N=3078
n_h =tapply(agr$acres87, agr$region, length)
y_bar_h=tapply(agr$acres87, agr$region, mean)
weight_h = c(10.23300971,10.47619048,10.23703704,10.29268293)

N_h=weight_h*n_h
y_bar_est_str= sum(y_bar_h*N_h/N)

#298547.1

fpc_h= (1 - n_h/N_h)
Weigth_h = N_h/N #Stratum weight  
s_h= tapply(agr$acres87, agr$region, sd)

var_y_bar_est_str = sum(fpc*(Weigth_h^2)*(s_h^2)/n_h)

se_y_bar_est_str= sqrt(var_y_bar_est_str)
#16608.82

df_tstat= sum(n_h) - 4

#95% CI of mean 

y_bar_est_str + qt(1-.05/2, df_tstat)*se_y_bar_est_str
y_bar_est_str - qt(1-.05/2, df_tstat)*se_y_bar_est_str

#(265860.8,331233.4)

#######################################################################
#c
plot(density(agr$largef92) , main = "Density plot of Y, LargeFarms in 1992",
     ylab = "Density, f(y)", xlab = "Y, LargeFarms, 1992",
     col ="magenta", type = "o")

N=3078
n_h =tapply(agr$largef92, agr$region, length)
y_bar_h=tapply(agr$largef92, agr$region, mean)
weight_h = c(10.23300971,10.47619048,10.23703704,10.29268293)

N_h=weight_h*n_h
y_bar_est_str= sum(y_bar_h*N_h/N)

#56.69795

fpc_h= (1 - n_h/N_h)
Weigth_h = N_h/N #Stratum weight  
s_h= tapply(agr$largef92, agr$region, sd)

var_y_bar_est_str = sum(fpc*(Weigth_h^2)*(s_h^2)/n_h)

se_y_bar_est_str= sqrt(var_y_bar_est_str)
#3.62349

df_tstat= sum(n_h) - 4

#95% CI of mean 

y_bar_est_str + qt(1-.05/2, df_tstat)*se_y_bar_est_str
y_bar_est_str - qt(1-.05/2, df_tstat)*se_y_bar_est_str

#(49.56689,63.82902)
