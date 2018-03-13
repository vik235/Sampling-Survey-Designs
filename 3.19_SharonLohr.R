#####Vivek Kumar Gupta . 9/20/2017 Stats 607 Homework assigment 3. 
####3.19 , Sharon Lohr.

winter = read.csv('C:/Users/vigupta/OneDrive/Learning/DataScience/Statistics Texas A&M University/607/TextMaterial/Dataset/winter.csv' ,
                 header = TRUE, sep = ",")

N_h= c(1374,1960,252,95)

#a: Find the number of persons who responded in each strata
n_h=tapply(winter$breakaga[winter$breakaga != 9], winter$class[winter$breakaga != 9], length)

n_h
##  1   2   3   4 
##232 514  86  67 

#b

#Using 3.6
p_h= tapply(as.numeric(winter$breakagarecoded[winter$breakagarecoded != 'NA']),
             winter$class[winter$breakagarecoded != 'NA'],
             sum)/n_h

p_str_est= sum(p_h*N_h/sum(N_h))
#0.8262216

fpc= 1 - n_h/N_h
N= sum(N_h)
W_h= N_h/N
Var_p_str_est = sum((fpc*W_h^2)*p_h*(1-p_h)/(n_h - 1 ))

se= sqrt(Var_p_str_est)

#Estimate of Se = se
0.01201918


#c
breakaga_tf = rep(0,length(winter$breakaga))

breakaga_tf[which(winter$breakaga == 1)]=0
breakaga_tf[which(winter$breakaga == 2)]=1
breakaga_tf[which(winter$breakaga == 9)]='NA'

#new variable to recode responses
winter$breakagarecoded=(breakaga_tf)


w_h=N_h/n_h

winter$sweights = rep(0,nrow(winter))

winter$sweights[which(winter$class == 1 )]=w_h[1]
winter$sweights[which(winter$class == 2 )]=w_h[2]
winter$sweights[which(winter$class == 3 )]=w_h[3]
winter$sweights[which(winter$class == 4 )]=w_h[4]
winter$sweights[which(winter$breakagarecoded == 'NA' )] =0




#Using 3.10 

num=sum(winter$sweights[winter$breakagarecoded != 'NA'] * as.numeric(winter$breakagarecoded[winter$breakagarecoded != 'NA']))
den=sum(winter$sweights)

y_str_est= num/den

#Estimate: 0.8262216


#e

responserate=n_h/c(500,653,98,95) #Use the values from the book but replace 74 with 98

#First row= stratas and second row is the ResponseRate. We see a lowest response rate, .46 from strata 1
#    1         2         3         4 
#0.4640000 0.7871363 0.8775510 0.7052632

#The stratification treats the non respondents as lost, not observed values. 