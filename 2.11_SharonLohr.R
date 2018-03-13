#####Vivek Kumar Gupta . 9/11/2017 Stats 607 Homework assigment 2. 
####2.11 , Sharon Lohr.

#N=807 It is assumed to be large 
n=240
age = 9:20
nChild = c(13,35,44,69,36,24,7,3,2,5,1,1)


raw = c(rep(9,13),rep(10,35),rep(11,44),rep(12,69),rep(13,36)
        ,rep(14,24),rep(15,7),rep(16,3),rep(17,2),rep(18,5),rep(19,1),rep(20,1))

#a)
hist(raw, nclass = length(age), freq = F, xlab =" # Children"
     , ylab = "Density" , main = "Histogram of the X, \n Age(months) of Child who could walk freely   ")

plot(density(raw), xlab =" # Children"
     , ylab = "Density" , main = "Kernel Density of the X, \n Age(months) of Child who could walk freely   ")
shapiro.test(raw)
#b)  

(y_bar =mean(raw))
#Estimate of the mean = 12.07917 
sd_est = sd(raw)

(se_bar = sqrt((sd_est/n)))
#Estimate of the SE = 0.2243

#95% CI usign t distribution 
(y_bar - qt(1-.05/2, 240-1)*se_bar)
(y_bar + qt(1-.05/2, 240-1)*se_bar)

#(11.90275,12.25559)


#c
n = ((1.96*sd_est)/.5)^2
#57

