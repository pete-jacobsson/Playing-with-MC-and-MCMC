library(dplyr)

#'Likelihood calculation function
#'
#'Calculates binomial likelihood given number of trials, number of successes and success probability
#'@param n_s Number of Bernoulli successes
#'@param n_t Number of Bernoulli trials
#'@param n_p Probability of success on a Bernoulli trial
#'@examples
#'likelihood <- calculate_likelihood(n_s = 20, n_t = 60, p_s = 0.2)
#'@export
calculate_likelihood <- function (n_s, n_t, p_s){
  # Function to calculate likelihood on Bernoulli trials (P(ns|ps))
  # n_s = number successes; n_t = total trials; p_s = prob of success on trial
  # Returns P(n_s|p_s)
  # Assumes n_s, n_t are integers; p_s is probability

  likelihood <- choose(n_t, n_s) * p_s ^ n_s * (1 - p_s) ^ (n_t - n_s)
  likelihood
}


#' Grid estimation of a binomial probability
#'
#' Calculates the posterior probability of success across values from zero to one, provided number of successful trials, total number of trials and parameters for a prior distribution
#' @param n_s Number of Bernoulli successes
#' @param n_t Number of Bernoulli trials
#' @param prior_a Alpha parameter of the beta distribution underlying the priors grid
#' @param prior_b Beta parameter of the beta distribution underlying the priors grid
#' @examples
#' approximate_binomial_grid(20, 60, 3, 1)
#' @export
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
