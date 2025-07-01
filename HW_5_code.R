#1
library(parallel)
library(foreach)
library(doParallel)

total.obsns <- 1e11   # number of obsns to generate
chunk.size <- 1e7    # number of ovsns to generate per chunk
n.chunks <- total.obsns / chunk.size   # number of chunks
n.cores <- detectCores() - 2    # number of cores using

cl <- makeCluster(n.cores)
registerDoParallel(cl)

# tracking the required time
start <- Sys.time()

# generating observations in parallel
foreach(i = 1:n.chunks) %dopar% {
  set.seed(i)
  x <- rnorm(chunk.size)    # generating from N(0,1)
  rm(x)                    # removing from memory
}

end <- Sys.time()
req.time <- end - start
req.time

# we have used a 10 core system here and it took 17.8742 mins to generate
# 10^11 many observations from N(0,1). Also we have not stored the 
# observations as it will require 800GB memory which is not supported by 
# our system. We are stopping the generation at 10^11 many observations also 
# do not storing them due to lac of memory and time.



#2
library(MASS)

k <- 1e20    # dimension of the multivariate normal
rho <- 0.6    # correlation
lambda_1 <- 1 + (k-1)*0.6
lambda_2 <- 0.4

m <- 500    # Monte Carlo replication number
track <- 0
for(i in 1:m)
{
  z <- rnorm(1)
  iter <- 1    # to track the #of z's generated 
  parial.sq.norm <- lambda_1 * z^2     # tracks partial norm of the k-dimensional observation
  if(parial.sq.norm > 0.75^2)
  {
    track <- track + 1
    next
  }
  
  while(parial.sq.norm <= 0.75^2 & iter <= k)
  {
    z <- rnorm(1)
    iter <- iter + 1
    parial.sq.norm <-  parial.sq.norm + lambda_2 * z^2
    if(parial.sq.norm > 0.75^2)
    {
      track <- track + 1
      break
    }
  }
}

prob.est <- track / m
prob.est

