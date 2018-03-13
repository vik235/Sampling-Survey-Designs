####
####Vivek Kumar Gupta, Sharon Lohr Question 8.6 Response 
####
####

head(certify)

##Read in the data 
certify <- read.csv(file = "F:/vigupta/OneDrive/Learning/DataScience/Statistics Texas A&M University/607/TextMaterial/Dataset/certify.csv" , header = T)
N = 18609 

## Part a : Response rate in various subclasses 

#Ph.D 
n_R_Phd = nrow(certify[certify$college  == "P" , ]) #Ph. D statistians who responded to the survey. 

RR_Phd = n_R_Phd / (N * .55)
#0.2956536 

#Masters
n_R_MS = nrow(certify[certify$college  == "M" , ]) #MS statistians who responded to the survey. 

RR_MS = n_R_MS / (N  * .38)
#0.2317781

#industry workers
n_R_Ind = nrow(certify[certify$workenv == "I" , ]) #Industry Workers statistians who responded to the survey. 

RR_Ind = n_R_Ind / (N  * .29)
#0.3348398

#Academic workers
n_R_Acad = nrow(certify[certify$workenv == "A" , ]) #Academic Workers statistians who responded to the survey. 

RR_Acad = n_R_Acad / (N  * .34)
#0.3502417


#Govt workers
n_R_Govt = nrow(certify[certify$workenv == "G" , ]) #Govt Workers statistians who responded to the survey. 

RR_Govt = n_R_Govt / (N  * .11)
#0.4274569

#Other workers
n_R_Other = nrow(certify[certify$workenv == "O" & certify$certify != 0, ]) #Govt Workers statistians who responded to the survey. 

RR_Other = n_R_Other / (N  * .11)
#0.4274569


data = data.frame("SubClass" = c("Ph.D" , "Masters" ,"Industry" ,"Academic", "Govt" ) , 
      "Response Rate" = round(c(RR_Phd , RR_MS , RR_Ind , RR_Acad , RR_Govt), 2) , 
      "Responses" = c(n_R_Phd , n_R_MS , n_R_Ind , n_R_Acad , n_R_Govt), 
      "NonResponses" = round(c((N * .55) - n_R_Phd ,(N  * .38) - n_R_MS , (N  * .29) - n_R_Ind ,(N  * .34) - n_R_Acad , (N  * .11) - n_R_Govt), 0) )

chisq.test(data[, c( "Responses" , "NonResponses")])

### Test of independence between Subclasses and Responses 
#Pearson's Chi-squared test

#data:  data[, c("Responses", "NonResponses")]
#X-squared = 408.43, df = 4, p-value < 2.2e-16

#We see a significant result hence we conclude that there is an evidence of association between various subclasses 
#and Response rates in them thus the data is atleast not MCAR.  

###
###Part b: Raking 
###

PhD = c(nrow(certify[certify$college  == "P" & certify$workenv  == "I", ]) ,
nrow(certify[certify$college  == "P" & certify$workenv  == "A", ]) ,
nrow(certify[certify$college  == "P" & !certify$workenv  %in% c("A" , "I"), ]) 
)


NotPhD = c(nrow(certify[certify$college  != "P" & certify$workenv  == "I", ]) ,
            nrow(certify[certify$college  != "P" & certify$workenv  == "A", ]) ,
            nrow(certify[certify$college  != "P" & !certify$workenv  %in% c("A" , "I"), ]) 
)

data = data.frame(PhD, NotPhD)
row.names(data) = c("Industry" , "Academia" , "Other")

apply(data, 2, sum)

R1 = (18609/5001) * data

popRow = c(5397 , 6327 , 6885)
popCol = c(10235 , 8374)

#Step 1 : adjust for rows , TP[1 , ] = 6731 , TP[2 , ] = 4838 
rowMultiplier = apply(R1 , 1 , sum)
R2 = rbind((popRow[1]/rowMultiplier[1])*R1[1 , ] , (popRow[2]/rowMultiplier[2])*R1[2 , ] , (popRow[3]/rowMultiplier[3])*R1[3 , ]) 
colMultiplier = apply(R2, 2, sum)

#Step 2 : adjust for columns , TP[ , 1] = 10235 , TP[ , 2] = 8374
R3 = cbind((popCol[1]/colMultiplier[1])*R2[ , 1] , (popCol[2]/colMultiplier[2])*R2[ , 2] ) 
rowMultiplier = apply(R3 , 1 , sum)

#Step 3:
R4 = rbind((popRow[1]/rowMultiplier[1])*R3[1 , ] , (popRow[2]/rowMultiplier[2])*R3[2 , ] 
           , (popRow[3]/rowMultiplier[3])*R3[3 , ]) 

colMultiplier = apply(R4, 2,sum)

#Step 4: 
R5 = cbind((popCol[1]/colMultiplier[1])*R4[ , 1] , (popCol[2]/colMultiplier[2])*R4[ , 2] ) 
rowMultiplier = apply(R5 , 1 , sum)

#step 5 

R6 = rbind((popRow[1]/rowMultiplier[1])*R5[1 , ] , (popRow[2]/rowMultiplier[2])*R5[2 , ] 
           , (popRow[3]/rowMultiplier[3])*R5[3 , ]) 
rowMultiplier = apply(R6 , 2 , sum)

round(R6,0)
#We used 6 iterations to converge for Rakign were aprox converging . Final raking adjusted weights
#Rows : Phd, NonPhd
#Cols : I , A , Others
##[,1] [,2]
#[1,] 2241 3156
#[2,] 4982 1345
#[3,] 3018 3867

#Assumption , samples in the cells are same as far as response is concerned i.e MAR data and 
#no interaction between rows and columns

###Part c
###

# The conclusion does not look correct since the non response rate is pretty high.