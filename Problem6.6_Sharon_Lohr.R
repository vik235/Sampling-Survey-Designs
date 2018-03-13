####
####Vivek Kumar Gupta, Problem 6.6 Sharon Lohr
####
####

###
###Part a Solution
###

azcounties = read.csv('C:/Users/vigupta/OneDrive/Learning/DataScience/Statistics Texas A&M University/607/TextMaterial/Dataset/azcounties.csv' , header = TRUE)
attach(azcounties)
M_o = sum(population)
psi = population / M_o

#values of psi_i's for the sample size of 1 for the year 2000 is 
psi

# 0.057150642 0.096938679 0.095757353 0.042260176 0.027568931 0.007036091
# 0.016229851 0.127625980 0.080239591 0.147955483 0.031596140 0.137903925
# 0.131737158
t_psi_est = housing / psi

# 553292.1 527405.6 558108.6 667034.6 414597.1 532113.6 932417.7 627317.4
# 590892.8 548502.8 412582.0 592659.0 562787.3

##Above is all possible estimated housing units for a sample of size 1 

#Theoritical variance 

t_pop = sum (housing)

var_pop = sum(psi * (t_psi_est - t_pop)^2) #Via expectation representation of variance

#4789282131


###
###Part b Solution
###

#equal prob 

psi = rep(1/nrow(azcounties), nrow(azcounties))

t_psi_est = housing / psi

#  411073  664638  694759  366457  148590   48672  196729 1040806  616369 1055002
#  169468 1062490  963820

##Above is all possible estimated housing units for a sample of size 1 

#Theoritical variance 

t_pop = sum (housing)

var_pop = sum(psi * (t_psi_est - t_pop)^2)

#130534375140

##We see that design of part a is more efficient than of part b because sampling
#design in a takes into account the population sizes in each of the stratum
#and samples accordingly to get to an estimate which has much lower variance
#than SRS.


###
###Part c Solution
###

##Considering PPS 
n = 3
M_o = sum(population)
psi = population / M_o

s = sample(number , n , prob = psi , replace = TRUE)

t_psi_est = (1/n) * sum(housing[s] / psi[s]) #Using 6.5
#565834.7

var_t_est_psi = (1/n) * (1/(n - 1)) * sum(((housing[s] / psi[s]) - t_psi_est)^2) #Using 6.6
#164666258

