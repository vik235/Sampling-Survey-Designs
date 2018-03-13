###
###Vivek Kumar Gupta, question 4.9 Sharon Lohr
###
agsrs = read.csv('C:/Users/vigupta/OneDrive/Learning/DataScience/Statistics Texas A&M University/607/TextMaterial/Dataset/agsrs.csv', header = T)
attach(agsrs)

N = 3078
n = 300 
acres_lessfarms = agsrs[ agsrs$farms92 < 600 , 'acres92' ]
acres_morefarms = agsrs[ agsrs$farms92 >= 600 , 'acres92' ]

#a
t_y1 = N*sum(acres_lessfarms)/n

#497939808 

se_t_y1 = N*sqrt((1 - n/N)*var(acres_lessfarms)/n)
#67132705 


#b
t_y2 = N*sum(acres_morefarms)/n

#418987302 

se_t_y2 = N*sqrt((1 - n/N)*var(acres_morefarms)/n)
#43599317 
