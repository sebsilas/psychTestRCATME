
save_result_mixed_effects <- function(item_bank, opt) {
  psychTestR::code_block(
    function(state, ...) {
      test_state <- psychTestR::get_local("test_state", state)

      previous_ability_estimate <- get_current_ability_estimate_mixed_effects(
        test_state = test_state, opt = opt)

      item_info <- test_state$next_item
      item_id <- item_info$item
      answer <- psychTestR::answer(state)

      score <- answer[[opt$dv_name]]

      print('opti3...')
      print(score)

      # only take length one answers
      answer  <- answer[purrr::map_lgl(answer, function(x) length(x) == 1)] %>%
        tibble::as_tibble()

      new_row <- data.frame(
        num = get_num_items_administered(test_state) + 1L,
        item_id = item_id,
        information = item_info$info,
        criterion = item_info$criterion,
        score = score
      )

      fixed_effects <- item_bank %>% dplyr::slice(item_id) %>%
        dplyr::select(opt$fixed_effects)

      new_row <- cbind(new_row,
                       fixed_effects,
                       answer = answer)

      stopifnot(nrow(new_row) == 1L)

      test_state$results.by_item <- plyr::rbind.fill(test_state$results.by_item, new_row)

      tmp_item_params <- test_state$results.by_item[, opt$fixed_effects, drop = FALSE]
      tmp_scores <- test_state$results.by_item$score


      new_data <- cbind(tmp_item_params, tmp_scores) %>%
        dplyr::mutate(p_id = psychTestR::p_id(state))

      n <- nrow(test_state$results.by_item)
      test_state$num_items_administered <- n

      print('new_data')
      print(new_data)

      tmp_ability <- predict_based_on_mixed_effects_model(opt$mixed_effects_model, new_data)
      tmp_ability_sem <- NA

      print('new ability estimate...')
      print(tmp_ability)

      test_state$results.by_item[n, "ability_ME"] <- tmp_ability
      test_state$results.by_item[n, "ability_ME_sem"] <- tmp_ability_sem


      if (psychTestR::demo(state)) {
        new_ability_estimate <- get_current_ability_estimate_mixed_effects(test_state, opt)
        ability_change <- new_ability_estimate - previous_ability_estimate
        msg <- shiny::div(
          shiny::p(shiny::strong(if (score) "Correct" else "Incorrect"),
                   " answer."),
          shiny::p(
            "Ability: ",
            shiny::strong(format(new_ability_estimate, digits = 3, nsmall = 3)),
            paste0(
              "(",
              if (ability_change > 0) "+",
              format(ability_change, digits = 3, nsmall = 3),
              ")"
            )))
        shiny::showNotification(msg, duration = opt$notify_duration,
                                type = if (score) "message" else "error")
      }
      psychTestR::set_local(key = "test_state", value = test_state, state = state)
    }
  )
}

#' Current ability via mixed effects modelling estimate
#'
#' Returns the current ability estimate.
#'
#' @param test_state A \code{test_state} object as provided to
#' stopping rules in \code{\link{new_stopping_rule}}.
#'
#' @param opt Test options as defined by \code{\link{adapt_test_options}}.
#'
#' @param estimator The estimation method to use when computing
#' the current ability estimate;
#' see the \code{next_item.estimator} argument in
#' \code{\link{adapt_test_options}}.
#'
#' @return A numeric scalar corresponding to the current ability estimate,
#' with its standard error provided as the \code{sem} attribute.
#'
#' @export
get_current_ability_estimate_mixed_effects <- function(test_state,
                                         opt,
                                         estimator = opt$next_item.estimator,
                                         mixed_effects_model = opt$mixed_effects_model) {

  df <- test_state$results.by_item


  if (is.null(df)) {

    res <- 0 # predict the mean

    attr(res, "sem") <- NA # TODO

  } else {
    n <- nrow(df)
    res <- df[n, "ability_ME"]
    attr(res, "sem") <- df[n, "ability_ME_sem"]
  }
  stopifnot(is.scalar.numeric(res))
  res
}


predict_based_on_mixed_effects_model <- function(model, new_data) {

  sigma <- lme4::VarCorr(model) %>% # get model SD
    as.data.frame() %>%
    dplyr::filter(grp == "p_id") %>%
    dplyr::pull(sdcor)

  new_data <- new_data %>%
    dplyr::filter(!is.na(opti3))

  ability_length_preds <- lme4::ranef(model)$p_id[, '(Intercept)']

  min_ability <- min(ability_length_preds)
  max_ability <- max(ability_length_preds)


  compute_new_ranef_full_model(new_data$tmp_scores,
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




compute_new_ranef <- function(N_empirical, y, N_coef, N_intercept, min, max, sigma) {


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
  print('ability is: ')
  print(ability)
  ability
}

bayes_modal_estimation_N_model <- function(par, N_empirical, y, N_coef, N_intercept, sigma) {

  alpha <- par[1] # we only need to optimise for the participant intercept

  R <- y - alpha - N_intercept - N_coef * N_empirical

  -sum(stats::dnorm(R, mean = 0, sd = sigma, log = TRUE) + # likelihood
         stats::dnorm(alpha, mean = 0, sd = sigma, log = TRUE)) # prior

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

  -sum(dnorm(R, mean = 0, sd = sigma, log = TRUE) + # likelihood
         dnorm(alpha, mean = 0, sd = sigma, log = TRUE)) # prior

}



compute_new_ranef_full_model <- function(y,
                                         N,
                                         step.cont.loc.var,
                                         tonalness,
                                         log_freq,
                                         min,
                                         max,
                                         fixed_effect_intercept,
                                         sigma) {


  mle_par <- optim(fn = bayes_modal_estimation_arrhythmic_model,
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


get_fixed_effect_param <- function(param, model) {
  as.vector(fixef(model)[param])
}
