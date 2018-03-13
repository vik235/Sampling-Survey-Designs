#####Vivek Kumar Gupta . 9/20/2017 Stats 607 Homework assignment 4. 
####3.8 , Sharon Lohr.
#Given data points 
#Assume 1:Telephone households, 2: Non Telephone housholds

C = 20000
c0 = 5000
c1 = 10
c2 = 30 
c3 = 40 
N1 = .9 #Proportion of population 
N2 = .1
S1 = S2 = 1
#After cancelling common terms in numerator and denominator 
n = (C - c0) *((N1/sqrt(c2)) + (N2/sqrt(c3)))/(N1*sqrt(c2) +N2*sqrt(c3))
#n = 486 (approx)

#a 
n1 = 486*(.9/sqrt(30))/((.9/sqrt(30)) + (.1/sqrt(40)))
#443 (approx)
n2 = 486*(.1/sqrt(40))/((.9/sqrt(30)) + (.1/sqrt(40)))
#43 (approx)

#b

n = (C - c0) *((N1/sqrt(c1)) + (N2/sqrt(c3)))/(N1*sqrt(c1) +N2*sqrt(c3))
#1295

n1 = 1295*(.9/sqrt(10))/((.9/sqrt(10)) + (.1/sqrt(40)))
#1227 (approx)
n2 = 1295*(.1/sqrt(40))/((.9/sqrt(10)) + (.1/sqrt(40)))
#68




