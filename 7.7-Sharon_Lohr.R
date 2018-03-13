

#####Vivek Kumar Gupta . 11/18/2017 Stats 607 Homework assignment 7. 
####7.7 , Sharon Lohr.
#Given data points 
fish = read.csv('C:/Users/vigupta/OneDrive/Learning/DataScience/Statistics Texas A&M University/607/TextMaterial/Dataset/nybight.csv', header = TRUE)

fish$year = as.factor(fish$year)
fish$stratum = as.factor(fish$stratum)

#b - Calculations for average number 
Wh = c(.1, .1, .2, .2, .2, .2)
wh = 1/c(1, 1, 1/2, 1/2, 1/2, 1/2)
nh = tapply(fish$catchnum[fish$year=='1974'],fish$stratum[fish$year=='1974'], length )
Nh = nh*wh ##THESE ARE APPROXIMATED AND RELATIVE VALUES
Wh = Nh/sum(Nh)##THESE ARE APPROXIMATED AND RELATIVE VALUES

numsppwt = rep(0 , nrow(fish))
fish$numsppwt = numsppwt

for (i in 1:length(numsppwt)) {
  fish$numsppwt[which(fish$stratum == i & fish$year=='1974')] = wh[i]
}
fish$epmf = fish$numsppwt/sum(fish$numsppwt)

#empirical pmf is given by 
round(tapply(fish$numsppwt[fish$year=='1974'], as.factor(fish$numspp[fish$year=='1974']), sum) / sum(fish$numsppwt),4)
#1      3      4      5      6      7      8      9     10     11     12     1
#0.0328 0.0328 0.0820 0.0328 0.0656 0.0656 0.1803 0.0328 0.2295 0.0492 0.0820
#3     14     16     17     18 
#0.0328 0.0164 0.0328 0.0164 0.0164 

#Top row represents numspp and corresponding below rows represent the densities calculated by sampling weights. 



