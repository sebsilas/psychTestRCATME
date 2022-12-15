


#' Predict based on arrhythmic mixed-effects model
#'
#' @param model
#' @param new_data
#'
#' @return
#' @export
#'
#' @examples
predict_based_on_mixed_effects_arrhythmic_model <- function(model, new_data) {

  sigma <- lme4::VarCorr(model) %>% # get model SD
    as.data.frame() %>%
    dplyr::filter(grp == "p_id") %>%
    dplyr::pull(sdcor)

  new_data <- new_data %>%
    dplyr::filter(!is.na(opti3))

  ability_length_preds <- lme4::ranef(model)$p_id[, '(Intercept)']

  min_ability <- min(ability_length_preds)
  max_ability <- max(ability_length_preds)


  compute_new_ranef_arrhythmic_model(new_data$tmp_scores,
                                     fixed_effect_intercept = get_fixed_effect_param('(Intercept)', model),
                                     N = list(empirical = new_data$N,
                                              coef = get_fixed_effect_param('N', model)),
                                     step.cont.loc.var = list(
                                       empirical = new_data$step.cont.loc.var,
                                       coef = get_fixed_effect_param('step.cont.loc.var', model)),
                                     tonalness = list(empirical = new_data$tonalness,
                                                      coef = get_fixed_effect_param('tonalness', model)),
                                     log_freq = list(empirical = new_data$log_freq,
                                                     coef = get_fixed_effect_param('log_freq', model)),
                                     min = min_ability,
                                     max = max_ability,
                                     sigma = sigma)

}



#' Compute a participant random effect intercept, based on the scores they got on different items (with different difficulty features)
#'
#' @param y
#' @param N
#' @param step.cont.loc.var
#' @param tonalness
#' @param log_freq
#' @param min
#' @param max
#' @param fixed_effect_intercept
#' @param sigma
#'
#' @return
#' @export
#'
#' @examples
compute_new_ranef_arrhythmic_model <- function(y,
                                         N,
                                         step.cont.loc.var,
                                         tonalness,
                                         log_freq,
                                         min,
                                         max,
                                         fixed_effect_intercept,
                                         sigma) {


  mle_par <- stats::optim(fn = bayes_modal_estimation_arrhythmic_model,
                          par = c(alpha = 0), # start with the mean of 0
                          N = N,
                          step.cont.loc.var = step.cont.loc.var,
                          tonalness = tonalness,
                          log_freq = log_freq,
                          y = y,
                          fixed_effect_intercept = fixed_effect_intercept,
                          sigma = sigma,
                          method = "Brent",
                          lower = min,
                          upper = max)

  as.vector(mle_par$par)
}




bayes_modal_estimation_arrhythmic_model <- function(par,
                                                    y,
                                                    fixed_effect_intercept,
                                                    N = list(empirical = NA,
                                                             coef = NA),
                                                    step.cont.loc.var = list(empirical = NA,
                                                                             coef = NA),
                                                    tonalness = list(empirical = NA,
                                                                     coef = NA),
                                                    log_freq = list(empirical = NA,
                                                                    coef = NA),
                                                    sigma) {

  #  N + step.cont.loc.var + tonalness + log_freq

  alpha <- par[1] # we only need to optimise for the participant intercept

  R <- y - alpha - fixed_effect_intercept -
    N$coef * N$empirical -
    step.cont.loc.var$coef * step.cont.loc.var$empirical -
    tonalness$coef * tonalness$empirical -
    log_freq$coef * log_freq$empirical

  -sum(stats::dnorm(R, mean = 0, sd = sigma, log = TRUE) + # likelihood
         stats::dnorm(alpha, mean = 0, sd = sigma, log = TRUE)) # prior

}




