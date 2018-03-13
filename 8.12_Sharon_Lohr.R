library("MASS")

#read the data in 
teachmi  = read.csv('F:/OneDrive/Learning/DataScience/Statistics Texas A&M University/607/TextMaterial/Dataset/teachmi.csv', header = TRUE)
teachers  = read.csv('F://OneDrive/Learning/DataScience/Statistics Texas A&M University/607/TextMaterial/Dataset/teachers.csv', header = TRUE)
teachnr  = read.csv('F:/OneDrive/Learning/DataScience/Statistics Texas A&M University/607/TextMaterial/Dataset/teachnr.csv', header = TRUE)
nrow(teachmi)
str(teachmi)
 
#convert into factors 
teachmi$dist = as.factor(teachmi$dist )
teachmi$school = as.factor(teachmi$school)

#a
resrate = sum(teachmi$ssteach) / sum(teachmi$popteach)
#0.4111406 

#b 
#Yes, I will expect non response bias in this study. The response rate is low in each psu and overall combined. This begs an analysis 
#of non resonse bias. The non response bias can go in either direction though depending upon the estimate of Y_bar in the Missing group.


#c 

#teachnr$hrwork[teachnr$hrwork == -9 ] = NA
#teachnr$size[teachnr$size == -9 ] = NA
#teachnr$preprmin[teachnr$preprmin == -9 ] = NA
#teachnr$assist[teachnr$assist == -9 ] = NA
#teachnr = na.omit(teachnr)
summary(teachnr)
n_m = nrow(teachnr)
#We will use fpc for estimating the summary statistics

y_bar_m = mean(teachnr$hrwork)
var_y_m = var(teachnr$hrwork)
var_y_bar_m = var_y_m / n_m

data.frame("Statistics" = c("y_bar" , "sample variance" , "var_y_bar_est" ), value = round(c(y_bar_m , var_y_m , var_y_bar_m) , 2))

#Respondent group

#####Statistics    value
#1           y_bar 36.46
#2 sample variance  2.61
#3   var_y_bar_est  0.10

teachers$school = as.factor(teachers$school)
teachers$dist = as.factor(teachers$dist)

#Fro cluster sample, ignore stratums and analyze it as a two stage cluster sampling

y_bar_i = tapply(teachers$hrwork , teachers$school , mean)
M_i = tapply(teachmi$popteach , teachmi$school , sum)

y_bar_r = sum (M_i * y_bar_i) /sum(M_i)

N = 46 
n = 23 
var_y_bar_r = (1 - n/N) * (1 / (n * (mean(M_i))^2)) * (sum((M_i^2 ) * (y_bar_i - y_bar_r)^2 ))/ (n - 1)

data.frame("Statistics" = c("y_bar_r" ,  "var_y_bar_r_est" ), value = round(c(y_bar_r ,  var_y_bar_r) , 2))

#Non respondent group
########Statistics value
#1         y_bar_r 33.68
#2 var_y_bar_r_est  0.51
