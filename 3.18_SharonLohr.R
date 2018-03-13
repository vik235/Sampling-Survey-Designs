#####Vivek Kumar Gupta . 9/20/2017 Stats 607 Homework assignment 4. 
####3.18 , Sharon Lohr.
#Given data points 
#a
fish = read.csv('C:/Users/vigupta/OneDrive/Learning/DataScience/Statistics Texas A&M University/607/TextMaterial/Dataset/nybight.csv', header = TRUE)

fish$year = as.factor(fish$year)
fish$stratum = as.factor(fish$stratum)

#a
boxplot(fish$catchnum[fish$year=='1974'] ~ fish$stratum[fish$year=='1974'], main= "SidebySide BoxPlot", xlab='Stratum', ylab='Catch number')

#b - Calculations for average number 
Wh = c(.1, .1, .2, .2, .2, .2)
wh = 1/c(1, 1, 1/2, 1/2, 1/2, 1/2)
nh = tapply(fish$catchnum[fish$year=='1974'],fish$stratum[fish$year=='1974'], length )
Nh = nh*wh ##THESE ARE APPROXIMATED AND RELATIVE VALUES
Wh = Nh/sum(Nh)##THESE ARE APPROXIMATED AND RELATIVE VALUES

catchnum_est_mean_h = tapply(fish$catchnum[fish$year=='1974'],fish$stratum[fish$year=='1974'], mean )
catchnum_est_sd_h = tapply(fish$catchnum[fish$year=='1974'],fish$stratum[fish$year=='1974'], sd)

avg_catchnum_est_str = sum (Wh*catchnum_est_mean_h)
#180.541 
var_avg_catchnum_est_str = sum ((Wh^2)*(catchnum_est_sd_h^2)/nh) ##IGNORING FPC
se_avg_catchnumber_est= sqrt(var_avg_catchnum_est_str)
#32.48592

#b -  - Calculations for average weight 
nh = tapply(fish$catchwt[fish$year=='1974'],fish$stratum[fish$year=='1974'], length )
catchwt_est_mean_h = tapply(fish$catchwt[fish$year=='1974'],fish$stratum[fish$year=='1974'], mean )
catchwt_est_sd_h = tapply(fish$catchwt[fish$year=='1974'],fish$stratum[fish$year=='1974'], sd)

avg_catchwt_est_str = sum (Wh*catchwt_est_mean_h)
#28.96557 
var_avg_catchwt_est_str = sum ((Wh^2)*(catchwt_est_sd_h^2)/nh)##IGNORING FPC
se_avg_catchwt_est= sqrt(var_avg_catchwt_est_str)
#3.99226