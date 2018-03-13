####
####Vivek Kumar Gupta, Problem 2, Ch 6 - Sharon Lohr
####
####

###
###Part a solution, cumulative size method  
###

#Setup the data
n = 10
dat = read.csv("F:/vigupta/OneDrive/Learning/DataScience/Statistics Texas A&M University/607/TextMaterial/Dataset/exercise0602.csv" , header = TRUE)
attach(dat)

(cumulative_sample = sample(x = psu , size = n ,  replace = TRUE , prob = psi))
#4  9 13 16  2  8  4 14  5  4



###
###Part b solution, Lahiri's method  
###

(cumulative_sample = sample(x = psu , size = 10 ,  replace = TRUE , prob = psi))
#4  9 13 16  2  8  4 14  5  4

Lahiri_sample = rep(0 , n)

#Function that takes a vector as input , see Lahiri's method ch 6 Sharon Lohr
getLahiriSample <- function(data){
success = FALSE
while (!success) {
  r = sample( x = data , 1 )  
  if (r <= max(data)) { success = TRUE ; return(r) }
}
}

for (i in 1:n) {
  Lahiri_sample[i] = which(psi == getLahiriSample(psi))
}

Lahiri_sample
#1  7 18  1  2 10 13 16 19  2

