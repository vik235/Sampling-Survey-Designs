#####Vivek Kumar Gupta . 9/20/2017 Stats 607 Homework assigment 3. 
####3.6 , Sharon Lohr.

#1: Houses, 2:Apartments 3: Condos
#a) 


#Under optimal allocation 
N1 = 35000
N2 = 45000
N3 = 10000
N = N1 + N2 + N3
n1 = 900 * N1/(N1 + (N2 + N3)/2)
#504
n2 = 900 * N2/(2*N1 + N2 + N3)
#324
n3 = 900 * N3/(2*N1 + N2 + N3)
#72
n = n1 + n2 + n3


#Thus allocations are n1 = 504 , n2 = 324 and n3 = 72

#b) 
#Under the setting of #b we have 

p1 = .45
W1 = N1/N
p2 = .25
W2 = N2/N
p3 = .03
W3 = N3/N

Wh = c(W1, W2, W3)
ph = c(p1 , p2 , p3)
Nh = c(N1, N2, N3)
p_str = sum(Wh * ph)
#.3033

#Under prop Allocation 
nh = (n/N)*c(N1, N2 , N3)

p_srs = (p1*N1 + p2*N2 + p3*N3)/N

var_p_srs =  ((N - n)/(N-1))*p_srs*(1 - p_srs)/n # We will not use the estimates since we know population response
var_p_prop = sum((Wh^2)*((Nh - nh)/(Nh - 1))*ph*(1 - ph)/nh) # We will not use the estimates since we know population response

Relative_Strat_gain = var_p_prop/var_p_srs
#0.9144156 


