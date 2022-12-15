
compute_new_ranef_n_model <- function(N_empirical, y, N_coef, N_intercept, min, max, sigma) {


  mle_par <- stats::optim(fn = bayes_modal_estimation_N_model,
                          par = c(alpha = 0), # start with the mean of 0
                          N_empirical = N_empirical,
                          N_coef = N_coef,
                          N_intercept = N_intercept,
                          y = y,
                          sigma = sigma,
                          method = "Brent",
                          lower = min,
                          upper = max)

  ability <- as.vector(mle_par$par)
  cat('ability is: ')
  cat(ability)
  ability
}

bayes_modal_estimation_N_model <- function(par, N_empirical, y, N_coef, N_intercept, sigma) {

  alpha <- par[1] # we only need to optimise for the participant intercept

  R <- y - alpha - N_intercept - N_coef * N_empirical

  -sum(stats::dnorm(R, mean = 0, sd = sigma, log = TRUE) + # likelihood
         stats::dnorm(alpha, mean = 0, sd = sigma, log = TRUE)) # prior

}

