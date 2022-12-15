

#' Predict based on rhythmic mixed-effects model
#'
#' @param model
#' @param new_data
#'
#' @return
#' @export
#'
#' @examples
predict_based_on_mixed_effects_rhythmic_model <- function(model, new_data) {

  sigma <- lme4::VarCorr(model) %>% # get model SD
    as.data.frame() %>%
    dplyr::filter(grp == "p_id") %>%
    dplyr::pull(sdcor)

  new_data <- new_data %>%
    dplyr::filter(!is.na(opti3))

  ability_length_preds <- lme4::ranef(model)$p_id[, '(Intercept)']

  min_ability <- min(ability_length_preds)
  max_ability <- max(ability_length_preds)


  compute_new_ranef_rhythmic_model(new_data$tmp_scores,
                                   fixed_effect_intercept = get_fixed_effect_param('(Intercept)', model),

                                   N = list(empirical = new_data$N,
                                            coef = get_fixed_effect_param('N', model)),

                                   step.cont.loc.var = list(
                                     empirical = new_data$step.cont.loc.var,
                                     coef = get_fixed_effect_param('step.cont.loc.var', model)),

                                   log_freq = list(empirical = new_data$log_freq,
                                                   coef = get_fixed_effect_param('log_freq', model)),

                                   d.entropy = list(empirical = new_data$d.entropy,
                                                    coef = get_fixed_effect_param('d.entropy', model)),

                                   i.entropy = list(empirical = new_data$i.entropy,
                                                    coef = get_fixed_effect_param('i.entropy', model)),

                                   min = min_ability,
                                   max = max_ability,
                                   sigma = sigma)

}

#' Compute a participant random effect intercept, based on the scores they got on different items (with different difficulty features)
#'
#' @param y
#' @param N
#' @param step.cont.loc.var
#' @param log_freq
#' @param d.entropy
#' @param i.entropy
#' @param min
#' @param max
#' @param fixed_effect_intercept
#' @param sigma
#'
#' @return
#' @export
#'
#' @examples
compute_new_ranef_rhythmic_model <- function(y,
                                         N,
                                         step.cont.loc.var,
                                         log_freq,
                                         d.entropy,
                                         i.entropy,
                                         min,
                                         max,
                                         fixed_effect_intercept,
                                         sigma) {


  mle_par <- stats::optim(fn = bayes_modal_estimation_rhythmic_model,
                          par = c(alpha = 0), # start with the mean of 0
                          N = N,
                          step.cont.loc.var = step.cont.loc.var,
                          log_freq = log_freq,
                          d.entropy = d.entropy,
                          i.entropy = i.entropy,
                          y = y,
                          fixed_effect_intercept = fixed_effect_intercept,
                          sigma = sigma,
                          method = "Brent",
                          lower = min,
                          upper = max)

  as.vector(mle_par$par)
}




bayes_modal_estimation_rhythmic_model <- function(par,
                                                    y,
                                                    fixed_effect_intercept,
                                                    N = list(empirical = NA, coef = NA),
                                                    step.cont.loc.var = list(empirical = NA, coef = NA),
                                                    log_freq = list(empirical = NA, coef = NA),
                                                    d.entropy = list(empirical = NA, coef = NA),
                                                    i.entropy = list(empirical = NA, coef = NA),
                                                    sigma) {

  #  N + step.cont.loc.var + log_freq + d.entropy + i.entropy

  alpha <- par[1] # we only need to optimise for the participant intercept

  R <- y - alpha - fixed_effect_intercept -
    N$coef * N$empirical -
    step.cont.loc.var$coef * step.cont.loc.var$empirical -
    log_freq$coef * log_freq$empirical -
    d.entropy$coef * d.entropy$empirical -
    i.entropy$coef * i.entropy$empirical


  -sum(stats::dnorm(R, mean = 0, sd = sigma, log = TRUE) + # likelihood
         stats::dnorm(alpha, mean = 0, sd = sigma, log = TRUE)) # prior

}




