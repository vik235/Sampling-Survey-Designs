#####Vivek Kumar Gupta . 9/11/2017 Stats 607 Homework assigment 2. 
####2.6 , Sharon Lohr.

N=807
n=50
refereed = c(0,1,2,3,4,5,6,7,8,9,10)
nfaculty = c(28,4,3,4,4,2,1,0,2,1,1)

raw = c(rep(0,28),rep(1,4),rep(2,3),rep(3,4),rep(4,4)
        ,rep(5,2),rep(6,1),rep(7,0),rep(8,2),rep(9,1),rep(10,1))

#a)
hist(raw, nclass = 11, freq = F, xlab =" # of Refereed Publications"
     , ylab = "Density" , main = "Histogram of the X, \n The total publications refereed   ")


#b)  

(y_bar =mean(raw))
#Estimate of the mean = 1.78 
sd_est = sd(raw)
f= n/N
fpc=1-f

(se_bar = sqrt((sd_est/(n))*fpc))
#Estimate of the SE = 0.2243

#c
(((sum((raw - y_bar)^3))/(n* sd_est))^2)*25 + 28




