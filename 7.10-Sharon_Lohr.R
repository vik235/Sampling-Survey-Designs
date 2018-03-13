
#####Vivek Kumar Gupta . 11/18/2017 Stats 607 Homework assignment 7. 
####7.8 , Sharon Lohr.
#Given data points 

N_1 = 245 
N_2 = 66 
n_1 = 23 
n_2 = 8

#read the data in 
teachmi  = read.csv('C:/Users/vigupta/OneDrive/Learning/DataScience/Statistics Texas A&M University/607/TextMaterial/Dataset/teachmi.csv', header = TRUE)
teachers  = read.csv('C:/Users/vigupta/OneDrive/Learning/DataScience/Statistics Texas A&M University/607/TextMaterial/Dataset/teachers.csv', header = TRUE)

#remove the missign values
teachers = teachers[teachers$hrwork != -9, ]

#convert into factors 
teachmi$dist = as.factor(teachmi$dist )
teachmi$school = as.factor(teachmi$school)

#replicate N1 , N2 , n1 , n2 , M_i = popteach , m_i = ssteach 
teachmi$Nh = c(rep(66 , 8), rep(245 , 23))
teachmi$nh =  c(rep(8, 8) , rep(23 , 23 ))

#Calculate the weights for each psu
teachmi$wt = (teachmi$Nh * teachmi$popteach) / (teachmi$nh * teachmi$ssteach)


teachers$wt = rep(0 , nrow(teachers))
teachers$epmf = rep(0, nrow(teachers))

for (i in 1:length(teachmi$school)) {
  teachers$wt[teachers$school == teachmi$school[i]]  = teachmi$wt[i]
}


#so that I can calculate the epmf, we will have to factorize teachers$wt 
hrwork = as.factor(teachers$hrwork)
as.numeric(hrwork)
#e pmf as calculated by weights 
epmf = tapply(teachers$wt, hrwork, sum) / sum(teachers$wt)
epmf1$y = as.numeric(names(epmf))
epmf1$density = epmf

plot(as.numeric(names(epmf)) , epmf , type ='h' , col = 2, main = 'Density plot' , xlab = "hours" , ylab= "density")

y = as.numeric(names(epmf))
N = sum(N_1, N_2)

var_est_epmf = (N / (N - 1)) * (( sum((y^2) * epmf )) - ( sum(y * epmf) )^2 )

#Design effect, ignoring fpc 

var_est_srs = var(teachers$hrwork)

deff = var_est_epmf / var_est_srs
#1.243868