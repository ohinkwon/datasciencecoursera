## Poisson Distribution

# Alt_1
poisson <- function(x){
  m <- sum(x[,1] * x[,2]) / sum(x[,2])
  p <- ((m^x[,1]) * (exp(-m))) / factorial(x[,1]) * sum(x[,2])
  p[length(p)] <- sum(x[,2]) - sum(p[1:length(p)-1])
  return(p)
}
d1 <- data.frame(no=c(0, 1, 2, 3, 4), 
                 frequency=c(94, 63, 21, 2, 0))
poisson(d1)

# Alt_2
x <- data.frame(no=c(0, 1, 2, 3, 4), frequency=c(94, 63, 21, 2, 0))
m <- sum(x[,1] * x[,2]) / sum(x[,2])
p <- ((m^x[,1]) * (exp(-m))) / factorial(x[,1]) * sum(x[,2])
p[length(p)] <- sum(d1[,2]) - sum(p[1:length(p)-1])
p


## Binomial Distribution

# Alt_1
x <- data.frame(no=c(1,2,3,4,5,6,7,8,9,10,11,12,13), 
                frequency=c(0,0,3,0,8,10,11,10,11,9,1,1,0))
m <- sum(x[,1] * x[,2]) / sum(x[,2])
sum((x[,1]-m)^2)/length(x[,1])
sum((x[,1]-m)^2)/
length(x[,1])
sum((x[,1]-m)^2)/sum(x[,1])

p <- combn()




sum(d2[,2])
