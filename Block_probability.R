block_prob <- function(){
  
  r <- readline("Enter the rate of arrival of customers to the system in as unit per hour: ")
  r <- as.double(r)
  t <- readline("Enter the average time spent by each customer in the system in hours: ")
  t <- as.double(t)
  n <- readline("Enter the number of servers in the system: ")
  n <- as.integer(n)
  
  a <- r*t
  pi_o <- 1/(exp(n))
  print(paste("Probability of zero customers in the system is:", round(pi_o,3)))
  
  b <- (a^n)/(factorial(n)*exp(a))
  b <- b*100
  b <- round(b, digits = 2)
  print(paste("Blocking probability of the system is", b , "%"))
  
  
  j <- readline(paste("Enter a number between 0 and", n , "representing number of customers in the system: "))
  j <- as.double(j)
  pi_j <- ((a^j)*(pi_o))/factorial(j)
  print(paste("Probability of", j, "customers in the system is:", round(pi_j,3)))
  
 
  
  
}

block_prob()
