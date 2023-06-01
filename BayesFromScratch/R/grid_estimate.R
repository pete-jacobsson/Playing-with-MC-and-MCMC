library(dplyr)


calculate_likelihood <- function (n_s, n_t, p_s){
  # Function to calculate likelihood on Bernoulli trials (P(ns|ps))
  # n_s = number successes; n_t = total trials; p_s = prob of success on trial
  # Returns P(n_s|p_s)
  # Assumes n_s, n_t are integers; p_s is probability
  
  likelihood <- choose(n_t, n_s) * p_s ^ n_s * (1 - p_s) ^ (n_t - n_s)
  likelihood
}

approximate_binomial_grid <- function(n_s, n_t, prior_a, prior_b){
  #n_s = n_success; n_t = total Bernoulli trials; prior = params for beta distrib
  #Function returns a data frame with the grid points (theta), prior and posterior distribs
  #Function assumes all inputs are integers
  
  grid_results <- data.frame(theta = seq(from = 0, to = 1, length.out = 100)) %>%
    mutate(
      likelihood = calculate_likelihood(n_s, n_t, theta),
      prior = dbeta(theta, prior_a, prior_b),
      likelihood_times_prior = likelihood * prior
    )
  
  norm_constant <- sum(grid_results$likelihood_times_prior, na.rm = TRUE)
  
  grid_results <- grid_results %>%
    mutate(posterior = likelihood_times_prior / norm_constant) %>%
    select(theta, prior, posterior)
  
  grid_results
}