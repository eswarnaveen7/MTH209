#Question 1............
library(copula)
library(ggplot2)

# Function to estimate the integral using the independent copula
estimate_integral_indep_copula <- function(n_values, num_samples=2000) 
{
  integral_approx=numeric(length(n_values))
  for (i in 1:length(n_values))
  {
    n=n_values[i]
    # Step 1: Generate independent copula samples (which is just uniform sampling)
    indep_cop <- indepCopula(dim = n)  # Define the independent copula
    U_sample <- rCopula(num_samples, indep_cop)  # Sample from the copula
    
    # Step 2: Compute function values
    numerator <- rowSums(U_sample^101)  # Sum of x_i^101
    denominator <- rowSums(U_sample)    # Sum of x_i
    
    # Avoid division by zero (though unlikely for U(0,1) samples)
    valid_indices <- denominator > 0
    integral_estimates <- numerator[valid_indices] / denominator[valid_indices]
    
    # Step 3: Compute Monte Carlo estimate (mean of function values)
    integral_approx[i]<- mean(integral_estimates)
  }
  
  return(integral_approx)
}

# Set parameters
n_values=seq(1,700)
num_samples <- 200  # Monte Carlo sample size

# Compute approximation
integral_estimates <- estimate_integral_indep_copula(n_values, num_samples)
ie= (sum(integral_estimates))/(length(n_values))
  
# Print result
cat("Monte Carlo estimate of the integral using independent copula:", ie, "\n")
cat("Theoretical limit (1/51):", 1/51, "\n")

# Store results in a data frame
results_df <- data.frame(n = n_values, Integral_Estimate = integral_estimates)

# Plot the results with theoretical limit 1/51
ggplot(results_df, aes(x = n, y = Integral_Estimate)) +
  geom_point(color = "blue", size = 3) +  # Plot points
  geom_line(color = "blue") +  # Connect points with a line
  geom_hline(yintercept = 1/51, linetype = "dashed", color = "red", size = 1) +  # Mark 1/51
  annotate("text", x = max(n_values) * 0.7, y = 1/51, 
           label = "1/51", color = "red", vjust = -1, size = 5) +  # Label
  scale_x_log10() +  # Use log scale for better visualization
  labs(title = "Convergence of Integral Estimate as n → ∞",
       x = "n (log scale)",
       y = "Integral Estimate") +
  theme_minimal()

#Question 2..............
data=read.csv("SeoulBikeData.csv",header=TRUE,sep=",")
head(data)
str(data)

# Load necessary libraries
library(quantreg)  # For LAD regression

# Select relevant columns
bike_data <- data[, c("Temperature", "Rented.Bike.Count")]

# Rename columns for easier use
colnames(bike_data) <- c("Temperature", "Bike_Count")

# LSE (Least Squares Estimation) using lm()
lse_model <- lm(Bike_Count ~ Temperature, data = bike_data)

# LAD (Least Absolute Deviation) using rq() from quantreg package
lad_model <- rq(Bike_Count ~ Temperature, data = bike_data, tau = 0.5)

# Summary of models
summary(lse_model)
summary(lad_model)

# Compare Performance using RMSE and MAE
library(Metrics)
pred_lse <- predict(lse_model)
pred_lad <- predict(lad_model)

rmse_lse <- rmse(bike_data$Bike_Count, pred_lse)
rmse_lad <- rmse(bike_data$Bike_Count, pred_lad)
mae_lse <- mae(bike_data$Bike_Count, pred_lse)
mae_lad <- mae(bike_data$Bike_Count, pred_lad)

cat("LSE RMSE:", rmse_lse, "  LSE MAE:", mae_lse, "\n")
cat("LAD RMSE:", rmse_lad, "  LAD MAE:", mae_lad, "\n")

# Plot Data and Regression Lines
plot(bike_data$Temperature, bike_data$Bike_Count, 
     main = "LSE vs LAD Regression",
     xlab = "Temperature", ylab = "Bike Count",
     pch = 16, col = "blue")

# Add LSE line
abline(lse_model, col = "red", lwd = 2, lty = 1)

# Add LAD line
lad_coef <- coef(lad_model)
abline(a = lad_coef[1], b = lad_coef[2], col = "green", lwd = 2, lty = 2)

legend("topright", legend = c("LSE (Red)", "LAD (Green)"), col = c("red", "green"), lty = c(1, 2), lwd = 2)


