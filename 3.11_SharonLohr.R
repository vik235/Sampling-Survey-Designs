#####Vivek Kumar Gupta . 9/20/2017 Stats 607 Homework assignment 4. 
####3.11 , Sharon Lohr.
#Given data points 

seals = read.csv('C:/Users/vigupta/OneDrive/Learning/DataScience/Statistics Texas A&M University/607/TextMaterial/Dataset/seals.csv', header = TRUE)
seals$zone <- as.factor(seals$zone)

y_bar_h = tapply(seals$holes, seals$zone , mean)
Nh = c(68, 84, 48)
nh = c(17 , 12 , 11)
sh = tapply(seals$holes, seals$zone , sd)
t_est_str = sum(Nh * y_bar_h)
#997.1818 

var_est_t_str = sum((1 - (nh/Nh))*(Nh^2)*(sh^2)/nh)
se = sqrt(var_est_t_str)
#118.0264

