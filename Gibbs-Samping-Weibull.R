library(coda)

# Generate parameter Weibull data
shape <- 2.0
scale <- 3.0
# Gibbs sampling function for Weibull data
gibbs_sampling <- function(weibull_data, iterations, alpha_init, beta_init, alpha_shape) {
  n <- length(weibull_data)
  alpha <- alpha_init
  beta <- beta_init
  samples <- matrix(0, nrow = iterations, ncol = 2)
  
  for (i in 1:iterations) {
    # Update beta given alpha
    beta <- 1 / sum((weibull_data / alpha)^(alpha_shape - 1))
    
    # Update alpha given beta
    alpha <- rgamma(1, alpha_shape + n, rate = 1 / (sum((data / beta)^(alpha_shape))))
    
    # Save samples
    samples[i,] <- c(alpha, beta)
  }
  
  return(samples)
}

scatter.smooth(weibull_data)

# Set initial values for alpha and beta
alpha_init <- 2.5
beta_init <- 2.5

# Set the shape for the gamma distribution used in updating alpha
alpha_shape <- 1

# Number of Gibbs sampling iterations
iterations <- 15000
keep.alpha <- rep(0,iterations)
keep.beta <- rep(0,iterations)

# Perform Gibbs sampling
gibbs_samples <- gibbs_sampling(weibull_data, iterations, alpha_init, beta_init, alpha_shape)

# Plot trace plots
matplot(gibbs_samples, type = "l", lty = 1, col = 1:2, xlab = "Iteration", ylab = "Parameter Value", main = "Gibbs Sampling Trace Plots")
legend("topright", legend = c("alpha", "beta"), col = 1:2, lty = 1)

# Plot posterior distributions
par(mfrow = c(2, 1))
hist(gibbs_samples[, 1], breaks = 30, main = "Posterior Distribution of alpha", xlab = "alpha")
hist(gibbs_samples[, 2], breaks = 30, main = "Posterior Distribution of beta", xlab = "beta")


