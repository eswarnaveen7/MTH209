set.seed(123)

#Q1. Estimation of e

#method 1: using U(0,1)
e.est1 <- function(n)   # n = sample size
{
  # Generate 'n' random binomial values
  dat <- runif(n)
  # Sample geometric mean
  geo.mean <- prod(dat^(1/n))
  e.hat <- 1/geo.mean
  return(e.hat)
}

#method 2: using Bin(n,1/n)
e.est2 <- function(n)   #n = sample size
{
  # Generate 'n' random binomial values
  dat <- rbinom(n, n, 1/n)
  # Count the number of zeros and calculate e
  e.hat <- n / sum(dat == 0)
  return(e.hat)
}

#method 3: By card shuffling
e.est3 <- function(n)  # n = #of cards
{
  arranged.deck <- 1:n
  n.shuffle <- 1e3
  derangements <- 0
  for(i in 1:n.shuffle)   
  {
    shuffled.deck <- sample(arranged.deck)
    if(sum(shuffled.deck == arranged.deck) == 0)
    {
      derangements <- derangements + 1
    }
  }
  e.hat <- n.shuffle / derangements
  return(e.hat)
}


# Comparison of the methods
set.seed(123)
# Initialize vectors to store estimates
m <- 1e3
e.hat1 <- numeric(m)
e.hat2 <- numeric(m)
e.hat3 <- numeric(m)

# Compute estimates for each method
for (i in 1:m) {
  e.hat1[i] <- e.est1(i)
  e.hat2[i] <- e.est2(i)
  e.hat3[i] <- e.est3(i)
}

# Plot the results
par(mfrow = c(1, 3))
methods <- list(e.hat1, e.hat2, e.hat3)
for (i in 1:3) {
  plot(1:m, methods[[i]], 
       pch = 16, ylim = c(1.5, 4.5),
       main = paste("Method", i), 
       xlab = "Sample size", ylab = expression(hat(e)))
  abline(h = exp(1), col = "red", lty = 2, lwd = 2)
  legend("topright", legend = expression(e), lty = 2, col = "red", lwd = 2)
}

# These side by side plots suggests that the rate of convergence of Method 3 is
# very much faster than the other two methods and hence Method 3 is most efficient.
# And we can say that the rate of convergence of Method 1 is slightly faster than Method 2.
## So to estimate e we can use Method 3 with number of cards 1000
set.seed(123)
estimated.e <- e.est3(1000)
estimated.e







#_________________________
#Q2 Estimation of pi

pi.est <- function(n, d)  # n = sample size, d = dimension
{
  sum <- 0
  for(i in 1:n)
  {
    point <- runif(d, -1, 1)
    if(norm(point, "2") < 1) sum <- sum + 1
  }
  p <- sum / n
  
  pi.hat <- (p * 2^d * gamma(d/2+1))^(2/d)
  return(pi.hat)
}


set.seed(123)

par(mfrow = c(1,2))
#plotting pi.hat estimated in 2D with increasing sample size
n <- 1:2e3  #vector of sample sizes
pi.hat.2D <- numeric(length = 2e3)
for(i in n)
{
  pi.hat.2D[i] <- pi.est(i, 2)
}
plot(n, pi.hat.2D, ylim = c(2,4.2),
     main = expression(paste("Estimates of ", pi," in 2D")), 
     xlab = "Sample size", ylab = expression(hat(pi)))
abline(h = pi, col = "red", lty = 2, lwd = 2)
legend("topright", legend = expression(pi), lty = 2, lwd = 2, col = "red")

#plotting pi.hat estimated in 3D with increasing sample size
n <- 1:2e3
pi.hat.3D <- numeric(length = 2e3)
for(i in n)
{
  pi.hat.3D[i] <- pi.est(i, 3)
}
plot(n, pi.hat.3D, ylim = c(2,4.2),
     main = expression(paste("Estimates of ", pi," in 3D")), 
     xlab = "Sample size", ylab = expression(hat(pi)))
abline(h = pi, col = "red", lty = 2, lwd = 2)
legend("topright", legend = expression(pi), lty = 2, lwd = 2, col = "red")


# Plotting pi.hat estimated with fixed sample size 2000 in different dimensions
set.seed(123)
d <- 2:16   #vector of dimensions
pi.hat <- numeric(length = 15)
for(i in d)
{
  pi.hat[i-1] <- pi.est(2e3, i)
}
plot(d, pi.hat, pch = 16, ylim = c(2, 4.2),
     main = "Estimates of pi", 
     xlab = "Dimension", ylab = expression(hat(pi)))
abline(h = pi, col = "red", lty = 2, lwd = 2)
legend("topright", legend = expression(pi), lty = 2, lwd = 2, col = "red")

# Clearly estimation error is getting bigger in higher dimensions.
# Also the previous plots suggests large sample size will give a better estimate.
# So we can use n = 2000 & d = 3 to get an estimate.
set.seed(238)
estimated.pi <- pi.est(2000, 2)
estimated.pi







#__________________________
#Q3. Test of randomness

library(tseries)
test.randomness <- function(data, n)    # n = #partitions of the data
{
  #splitting the data into n many parts
  l = length(data)
  subdata <- vector(mode = 'list', length = n)   #empty list of length n
  if(n > 1)
  {
    for(i in 1:(n-1))
    {
      subdata[[i]] <- data[((i-1)*floor(l/n)+1) : (i*floor(l/n))]
    }
  }
  temp <- data[(n-1)*floor(l/n)+1 : l]
  subdata[[n]] <- temp[!is.na(temp)]
  
  #applying run test on each of the part
  p.val <- numeric(length = n)
  for(i in 1:n)
  {
    dat <- subdata[[i]]
    p.val[i] <- runs.test(as.factor(dat > median(dat)))$p.value
  }
  return(p.val)
}


data <- rnorm(100)
for(i in 1:6)
{
  print(test.randomness(data, i))
}

# By looking at the p values we conclude that there is no significant evidence
# to reject H0 (:Data is random), at 5% level of significance.

