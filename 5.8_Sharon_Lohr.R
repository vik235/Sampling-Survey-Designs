
##
## Vivek Kumar Gupta, Question 5.8 - Sharon Lohr
##

##
##Part a solution
##
N = 44 
n = 12 
books = read.csv('C:/Users/vigupta/OneDrive/Learning/DataScience/Statistics Texas A&M University/607/TextMaterial/Dataset/books.csv' , header = TRUE)
books$shelf = as.factor(books$shelf)
attach(books)

boxplot(replace ~ shelf, col = "lightblue" , ylab = 'Replacement Cost of Book' , xlab ="Shelf Number" , main = "Side by Side Box Plot")

##
##Part b solution
##

# 2 stage cluster with clusters of unequal sizes 

y_bar_i = tapply(books$replace , books$shelf , mean)
M_i = tapply(books$Mi , books$shelf , mean)
m_i = tapply(books$booknumber , books$shelf , length)
var_i = tapply(books$replace , books$shelf , var)

t_i_est = M_i * y_bar_i

t_unb_est = (N/n) * sum(t_i_est)
# 32637.73

var_t_unb = (N^2) * (1 - n/N)*var(t_i_est)/n + (N/n) * sum((1 - m_i/M_i)*(M_i^2)*var_i/m_i)
se_t_unb_est = sqrt(var_t_unb)
#5733.527

cv_est = se_t_unb_est / t_unb_est
#0.1756717 

##
##Part c solution
##

# 2 stage cluster with clusters of unequal sizes 

y_bar_i = tapply(books$replace , books$shelf , mean)
M_i = tapply(books$Mi , books$shelf , mean)
m_i = tapply(books$booknumber , books$shelf , length)
var_i = tapply(books$replace , books$shelf , var)

y_bar_ratio_est = sum(M_i * y_bar_i) / sum(M_i)
#23.61061

var_r = (1/(n -1)) * sum((M_i * y_bar_i - M_i*y_bar_ratio_est)^2)

var_y_bar_ratio_est = (1/mean(M_i)^2) * (1- n/N)*var_r/n + (1/(n*N*(mean(M_i)^2)))* sum((M_i^2) * (1 - (m_i/M_i))*var_i/m_i)
se_y_bar_ratio_est = sqrt(var_y_bar_ratio_est)
#5.475943


cv_est = se_y_bar_ratio_est / y_bar_ratio_est
#0.2319272 



                                                           