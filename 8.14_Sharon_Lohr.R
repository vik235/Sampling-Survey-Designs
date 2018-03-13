#####
#####Vivek Kumar Gupta , Sharon Lohr 14 
#####
#####


agpop <- read.csv(file = "C:/Users/vigupta/OneDrive/Learning/DataScience/Statistics Texas A&M University/607/TextMaterial/Dataset/agpop.csv" , header = T)
N = nrow(agpop)
#ignore missing data in the data 
agpop <- agpop[agpop$acres87 != -99 , ]

agpopsrs <- agpop[sample(n , 400) , ]

agpopsrs$U = runif(400)

n = 400 #all data 

#add some missing obs to the acres92 , in this run it comes to be 89
agpopsrs[16*agpopsrs$U >= log(agpopsrs$acres87 + 1) , "acres92"] = NA

summary(agpopsrs$acres92)
#a
#If we ignore the missing data in agpopsrs I will expect the mean of y to be unbiased or not 
#depending upon Bias(y_bar_R) appr= (Nm/N)(Y_bar_RU - y_bar_MU) , thus if the
#mean in the missing stratum is equal to that of non missing then it will be unbiased
#otherwise not andf it can be either large or small than the true mean. 

#b
#compute the mean ignoring the added missing data for y 

n1= nrow(agpopsrs[!is.na(agpopsrs$acres92), ])
y_bar_est = sum(agpopsrs[!is.na(agpopsrs$acres92), "acres92"]) / n1 
#384505.2 

#95% CI 

y_bar_est + c(1 , -1) * qt(1 - .05/2 , n1 - 1 ) * (sd(agpopsrs[!is.na(agpopsrs$acres92), "acres92"]))/(n1)
#389702.4 379308.0

#reread agpop 
agpop <- read.csv(file = "C:/Users/vigupta/OneDrive/Learning/DataScience/Statistics Texas A&M University/607/TextMaterial/Dataset/agpop.csv" , header = T)

mean(agpop$acres92)
#306677

#Thus we see that 95% CI doesnt contain the true mean. 