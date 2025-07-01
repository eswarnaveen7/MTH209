library(MASS)

#1
# Generating data from bivariate unimodal distn
# Considering BVN(c(0,0), matrix(c(1,0,0,1), nrow = 2)) popln
data1 <- mvrnorm(n = 1000, mu = c(0,0),
                 Sigma = matrix(c(1,0,0,1), nrow = 2))

# Estimating the density of the popln
# by using Kernel density estimation
kde.data1 <- kde2d(data1[ ,1], data1[ ,2])

# drawing contours based on the estimated density
contour(kde.data1, main = "Density Contour Plot")

### As all the contours are concentric so the data has only one mode

#######
# Generating data from bivariate bimodal distn
# Considering 0.5 * BVN(c(0,0), matrix(c(1,0,0,1), nrow = 2)) 
#               + 0.5 * BVN(c(3,3), matrix(c(1,0,0,1), nrow = 2)) popln

# Function generates a sample of size n feom the above mentioned popln
mvrnorm.mixture <- function(n)
{
  data <- matrix(0, nrow = n, ncol = 2)
  for(i in 1:n)
  {
    u <- runif(1)
    if(u < 0.5)
    {
      data[i, ] <- mvrnorm(1, mu = c(0,0),
                           Sigma = matrix(c(1,0,0,1), nrow = 2))
    }
    else
    {
      data[i, ] <- mvrnorm(1, mu = c(3,3),
                           Sigma = matrix(c(1,0,0,1), nrow = 2))
    }
  }
  return(data)
}

data2 <- mvrnorm.mixture(n = 1000)

# Estimating the dencity of the popln
# by using Kernel dencity estimation
kde.data2 <- kde2d(data2[ ,1], data2[ ,2])

# drawing contours based on the estimated density
contour(kde.data2, main = "Density Contour Plot")

### Clearly we can see two different sets of concentric contours 
#   so the data has two modes
#######################

# Finding mode of real data set
df <- read.csv("SOCR-HeightWeight.csv")
data <- df[ ,2:3]
# Estimating the density of the popln
# by using Kernel density estimation
kde.data <- kde2d(data[ ,1], data[ ,2])
# drawing contours based on the estimated density
contour(kde.data, main = "Density Contour Plot")



#2
# We know that 355/113 is an upper bound of pi and the given integral gives
# a error in approximation of pi.
# Estimating the given integral by using Monte Carlo 
est.int <- function(n)
{
  u <- runif(n)
  rtn <- mean(u^8 * (1-u)^8 * (25+816*u^2) / 3164 / (1+u^2))
  return(rtn)
}

# Estimating pi for different size of samples drawn from U(0,1)
pi.hat <- numeric(10000)
for(i in 1:10000)
{
  pi.hat[i] <- 355/113 - est.int(i)
}

plot(pi.hat, pch = 16, 
     xlab = "Sample size", ylab = expression(hat(pi)),)
abline(h = pi, col = "red", lwd = 2)
legend("topright", col = "red", lty = 1,
       legend = expression(pi))
#--------------------------------------


# Getting some idea about the variability of the integral

# This function draws 
f <- function(n, delta)
{
  u <- runif(n)
  integrand <- u^8 * (1-u)^8 * (25+816*u^2) / 3164 / (1+u^2)
  int.hat <- mean(integrand)
  if (n > 1) {
    rtn <- n^delta * int.hat / sqrt(var(integrand))
  } else {
    rtn <- int.hat # For n = 1, we just return the mean of the integrand
  }
  return(rtn)
}

par(mfrow = c(3,3))

delta <- c(0.01, 0.02, 0.05, 0.1, 0.2, 0.5, 0.6, 0.8, 1 )
for(i in 1:length(delta))
{
  int.hat <- numeric(10000)
  for(n in 1:10000)
    int.hat[n] <- f(n, delta[i])
  
  plot.ts(int.hat, pch = 16, main = paste("Delta =", delta[i]))
  abline(h = 0, col = "red", lwd = 2)
}

