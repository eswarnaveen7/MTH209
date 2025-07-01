# This function takes a sample and the hypothesized distribution(F_0) as
# its arguments and returns the value of the Kolmogorv-Smirnov test statistic
ks.stat <- function(data, F_0 = pnorm)  
{
  ecdf.fn <- ecdf(data)   # this function calculates empirical cdf
  rtn <- max(abs(ecdf.fn(data) - F_0(data)))    # K-S test statistic
  return(rtn)
}

# This function takes a sample and the hypothesized distribution(F_0) as
# its arguments and returns the p-value of Kolmogorov-Smirnov test
ks.test.12 <- function(data, F_0 = pnorm)
{
  obsd.ks.stat <- ks.stat(data, F_0)
  m <- 1000    # number of resamples
  sum <- 0
  for(i in 1:m)
  {
    resample <- sample(data, length(data), replace = TRUE)
    if(ks.stat(resample, F_0) > obsd.ks.stat) 
    {
      sum <- sum + 1
    }
  }
  
  p.val <- sum/m
  return(p.val)
}


#Q1
# Loading the dataset
delhi_temp <- read.csv("delhi_temp.csv")
delhi_temp$Date <- as.Date(delhi_temp$Date)
# accessing daily maximum temperature from 01.01.2015 to 31.12.2019
indx1 <- which(delhi_temp$Date == "2015-01-01")
indx2 <- which(delhi_temp$Date == "2019-12-31")
data <- delhi_temp$Temp.Max[indx1:indx2]

# normalizing the data
data <- (data - mean(data))/sd(data)
hist(data, prob = TRUE)
#It seems that the data is slightly -vely skeweed

qqnorm(data)  
lines(-5:5, -5:5, col = "red", lwd = 2)
#Here also it seems that the data is not normal

#let's see with the test
ks.test.12(data, F_0 = rnorm)
# Since the p-value is not so small, so the data favours the null hypoyhesis.


#Q2
data(iris)
data <- iris$Petal.Length
n <- length(data)

# this function finds sampel mode for a given data
sample.mode <- function(x) {
  freq.table <- table(x)  # Frequency table
  mode.val <- as.numeric(names(freq.table)[freq.table == max(freq.table)])  # Extract mode
  return(mode.val[1])  # Return first mode in case of ties
}

set.seed(124)
# generating multiple resamples and calculating mean, median and mode
m <- 1000    #number of resamples
means <- numeric(length = n)
medians <- numeric(length = n)
modes <- numeric(length = n)
for(i in 1:m)
{
  resample <- sample(data, size = n, replace = TRUE)
  means[i] <- mean(resample)
  medians[i] <- median(resample)
  modes[i] <- sample.mode(resample)
}

# Normalizing the vectors
means <- (means - mean(means)) / sd(means)
medians <- (medians - mean(medians)) / sd(medians)
modes <- (modes - mean(modes)) / sd(modes)

par(mfrow=c(1,3))
# Plot histograms
hist(means, prob = TRUE, main = "Histogram of Resampled Means", xlab = "Standardized Mean")
hist(medians, prob = TRUE, main = "Histogram of Resampled Medians", xlab = "Standardized Median")
hist(modes, prob = TRUE, main = "Histogram of Resampled Modes", xlab = "Standardized Mode")

par(mfrow=c(1,3))
# Q-Q Plots
qqnorm(means, main = "Q-Q Plot for Means")
qqline(means, col = "red")

qqnorm(medians, main = "Q-Q Plot for Medians")
qqline(medians, col = "red")

qqnorm(modes, main = "Q-Q Plot for Modes")
qqline(modes, col = "red")


# Perform Kolmogorov-Smirnov test
ks.test.12(means, F_0 = rnorm)
ks.test.12(medians, F_0 = rnorm)
ks.test.12(modes, F_0 = rnorm)

