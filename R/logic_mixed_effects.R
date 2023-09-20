#' Adaptive test
#'
#' Defines an adaptive test for use within psychTestR.
#'
#' This is the top-level function of the psychTestRCAT package.
#' It defines adaptive tests within the framework of item response theory.
#' An adptive test is one that tailors item selection to the
#' current test-taker on the basis of their performance during the test.
#' @param label (Character scalar, e.g. 'MDT')
#' A label for the test when saving the participant's results.
#' @param item_bank (Data frame, or coerceable to such)
#' Defines the item bank, the collection of items available
#' for administration during the adaptive test.
#' Each row should correspond to a different item.
#' Four columns are mandatory, corresponding to the
#' item's psychometric parameters according to item response theory:
#' - \code{discrimination} -
#' The item's discrimination parameter, with higher values
#' indicating that the item is better at discriminating between participants.
#' - \code{difficulty} -
#' The item's difficulty parameter, with higher values
#' corresponding to harder items.
#' - \code{guessing} -
#' The item's guessing parameter, corresponding to the probability
#' that a participant with no ability can nevertheless answer the
#' item correctly.
#' - \code{inattention} -
#' The item's inattention parameter, corresponding to the probability
#' that a participant with infinite ability answers the question
#' correctly.
#'
#' The data frame must also contain a column called \code{answer},
#' defining the correct answer for each item.
#' These answers will be checked for equality with
#' the answer output from \code{show_item} using \code{==},
#' which coerces to a shared class
#' (e.g. numeric answers can be coerced to character answers).
#' List-columns are not permitted.
#'
#' This argument must also provide sufficient information for presenting
#' a particular item to the participant.
#' When an item has been selected,
#' the corresponding row from the item bank is extracted and
#' passed to the \code{show_item} function,
#' another of the arguments to \code{adapt_test}.
#' The \code{show_item} function uses this row as a basis
#' to display the item to the participant.
#'
#' @param show_item (Function, list, or psychTestR timeline)
#' This function defines the logic for administering an item
#' from its definition in \code{item_bank}.
#' This function may take two forms:
#' - \strong{Single-page items} -
#' If your test item takes one psychTestR page to administer,
#' \code{show_item} should be a function of the form
#' \code{function(item, state, ...) {...}}.
#' The \code{item} parameter will correspond to
#' a row of the \code{item_bank} data frame.
#' The \code{state} parameter (advanced use only)
#' allows the \code{show_item} function to access
#' additional information about the current psychTestR session,
#' e.g. local variables (see \code{\link[psychTestR]{get_local}}).
#' The function should return a psychTestR \code{\link[psychTestR]{page}}
#' displaying the test item and storing the response in \code{answer(state)};
#' psychTestR's built-in question functions
#' (e.g. \code{\link[psychTestR]{NAFC_page}})
#' do the latter automatically.
#'
#' - \strong{Multi-page items} -
#' If your test item takes multiple psychTestR pages to administer,
#' \code{show_item} should be a list of psychTestR test elements
#' or, equivalently, a psychTestR timeline as created by
#' \code{\link[psychTestR]{new_timeline}}).
#' Mutability is achieved by using reactive pages
#' (\code{\link[psychTestR]{reactive_page}}),
#' which can access the current value of \code{item} from
#' the psychTestR session state object as follows:
#' \code{psychTestR::get_local("item", state)}.
#' When the timeline completes, \code{answer(state)} must
#' be set to the participant's answer.
#' If several pages contribute to the final answer,
#' this may involve accumulating values in a temporary variable
#' (see \code{\link[psychTestR]{get_local}}
#' and \code{\link[psychTestR]{set_local}}).
#'
#' psychTestR's built-in question functions
#' (e.g. \code{\link[psychTestR]{NAFC_page}})
#' by default save each response to the psychTestR results list.
#' This behaviour is unnecessary in \code{show_item},
#' because psychTestRCAT accumulates and saves its own store
#' of item-wise results.
#' It is therefore recommended to set \code{save_answer = FALSE}
#' in these question functions.
#'
#' @param stopping_rule (Function)
#' A stopping rule for determining when the test should terminate,
#' as created by \code{\link{new_stopping_rule}}.
#' In the common case where the test terminates after a set number of items,
#' use \code{\link{stopping_rule.num_items}},
#' passing the number of items as a function parameter.
#'
#' @param opt (List)
#' A list of further options as created by \code{\link{adapt_test_options}}.
#'
#' @return A psychTestR timeline representing the adaptive test.
#' Once the adaptive test is complete,
#' psychTestR saves three primary results into the results table:
#' - \code{ability} - The participant's final ability estimate.
#' - \code{ability_sem} - The standard error of the final ability estimate,
#' as computed from the IRT model.
#' - \code{num_items} - The number of items administered to the participant.
#'
#' Further information, in particular item-by-item results,
#' can be extracted from the \code{metadata} slot of the \code{ability} field.
#' This can be accessed by loading psychTestR RDS results files.
#' Alternatively, item-by-item results can be accessed
#' using the function \code{\link{compile_trial_by_trial_results}}.
#'
#' @note
#' By default, \code{adapt_test} displays no feedback to the
#' participant when they finish the test.
#' Use \code{\link{cat.feedback.graph}} directly after \code{adapt_test}
#' to display a feedback graph to the participant.
#'
#' @note
#' Ability estimation and item selection are performed using
#' the \code{catR} package.
#'
#' @md
#' @export
adapt_test <- function(label,
                       item_bank,
                       show_item,
                       stopping_rule = stopping_rule.num_items(n = NULL),
                       opt = adapt_test_options()) {
  item_bank <- as.data.frame(item_bank)
  check_inputs(label, item_bank, show_item, opt)
  c(
    setup(label, stopping_rule, opt, item_bank),
    psychTestR::while_loop(
      test = check_stopping_rule(stopping_rule),
      logic = c(
        if(opt$continuous_response) select_next_item_mixed_effects(item_bank, opt) else select_next_item(item_bank, opt),
        administer_next_item(item_bank, show_item),
        if(opt$continuous_response) save_result_mixed_effects(item_bank, opt) else save_result(item_bank, opt)
      )),
    finalise(opt))
}

#' Adaptive test options
#'
#' Creates a list of psychTestRCAT options for use in
#' the \code{\link{adapt_test}} function.
#'
#' @param next_item.criterion
#' (Character scalar, default = "MFI")
#' Criterion by which the next item is selected;
#' see the \code{criterion} argument of \code{\link[catR]{nextItem}}.
#'
#' @param next_item.estimator
#' (Character scalar, default = "BM")
#' Ability scoring method used for item selection;
#' see the \code{method} argument of \code{\link[catR]{nextItem}}.
#' Only \code{"BM"}, \code{"ML"}, \code{"WL"}, and \code{"EAP"}
#' are supported.
#'
#' @param next_item.prior_dist
#' (Character scalar, default = "norm")
#' Class of prior distribution used when estimating abilities
#' for item selection;
#' see the \code{priorDist} argument of \code{\link[catR]{nextItem}}.
#'
#' @param next_item.prior_par
#' (Numeric vector, length 2, default = c(0, 1))
#' Prior distribution used when estimating abilities for item selection;
#' see the \code{priorPar} argument of \code{\link[catR]{nextItem}}.
#'
#' @param final_ability.estimator
#' (Character scalar, default = "BM")
#' Ability scoring method used for final ability estimation;
#' see the \code{method} argument of \code{\link[catR]{nextItem}}.
#' The prior distribution (if relevant) remains the same as that used
#' for item selection, i.e. that specified by
#' \code{next_item.prior_dist} and \code{next_item.prior_par}.
#' Only \code{"BM"}, \code{"ML"}, \code{"WL"}, and \code{"EAP"}
#' are supported.
#'
#' @param constrain_answers
#' (Boolean; default = FALSE)
#' If \code{TRUE}, item selection actively balances
#' the available response options.
#' This only makes sense when the item bank contains only a small
#' number of unique answers.
#'
#' @param avoid_duplicates
#' (\code{NULL} or character scalar)
#' If not \code{NULL}, should specify a column of the item bank
#' defining a categorical item feature for which duplicates
#' should be avoided during item selection.
#'
#' @param cb_control
#' Content-balancing argument passed to \code{\link[catR]{nextItem}}.
#'
#' @param cb_group
#' Content-balancing argument passed to \code{\link[catR]{nextItem}}.
#'
#' @param eligible_first_items
#' (\code{NULL} or integerish vector)
#' If not \code{NULL},
#' lists the eligible items for the first item in the test,
#' where each item is identified by its 1-indexed row number
#' in \code{item_bank} (see \code{\link{adapt_test}}).
#' For example, \code{c(2, 3, 4)} means that the
#' first item will be drawn from rows 2, 3, 4 of the item bank).
#'
#' @param notify_duration
#' (Numeric scalar)
#' Defines the length of time for which item-by-item feedback messages
#' persist on screen, in seconds.
#' This is only relevant when the test is taken in admin or demo mode.
#'
#' @param mixed_effects_model
#' A mixed effects model of class lmerModLmerTest.
#'
#' @param continuous_response
#' Is response continuous?
#'
#' @param dv_name
#' Name of dependent variable.
#'
#' @param fixed_effects
#' Character vector of names of fixed effects.
#'
#' #' @param demo
#' Logical.
#'
#' #' @param predict_based_on_mixed_effects_model_function
#' A function to to predict from the mixed effects model.
#'
#' @return A list to be passed to the \code{opt} argument
#' of \code{\link{adapt_test}}.
#'
#' @export
adapt_test_options <- function(next_item.criterion = "MFI",
                               next_item.estimator = "BM",
                               next_item.prior_dist = "norm",
                               next_item.prior_par = c(0, 1),
                               final_ability.estimator = "BM",
                               constrain_answers = FALSE,
                               avoid_duplicates = NULL,
                               cb_control = NULL,
                               cb_group = NULL,
                               eligible_first_items = NULL,
                               notify_duration = 5,
                               mixed_effects_model = NULL,
                               continuous_response = FALSE,
                               dv_name = " ",
                               fixed_effects = c("fixed_effect1", "fixed_effect2"),
                               demo = FALSE,
                               predict_based_on_mixed_effects_model_function = NULL) {
  stopifnot(
    is.scalar.character(next_item.criterion),
    is.scalar.character(next_item.estimator),
    is.scalar.character(next_item.prior_dist),
    is.scalar.character(final_ability.estimator),
    is.numeric(next_item.prior_par),
    length(next_item.prior_par) == 2L,
    next_item.estimator %in% c("ML", "BM", "EAP", "WL"),
    final_ability.estimator %in% c("ML", "BM", "EAP", "WL"),
    is.null.or(avoid_duplicates, is.character),
    is.null.or(eligible_first_items,
               function(x) is.numeric(x) && !anyDuplicated(x)),
    is.null.or(notify_duration, is.scalar.numeric),
    is.scalar.logical(constrain_answers),
    is.null.or(mixed_effects_model,
               function(x) class(x) == "lmerModLmerTest"),
    is.logical(continuous_response),
    is.scalar.character(dv_name),
    is.character(fixed_effects),
    is.logical(demo),
    is.null.or(predict_based_on_mixed_effects_model_function, is.function)
  )
  local(if (!(is.null(cb_control) && is.null(cb_group))) {
      cb_test <- catR::test.cbList(cb_control, cb_group)
      if (!cb_test$test) {
        stop("problem detected with cb_control/cb_group arguments: ",
             cb_test$message)
      }})
  as.list(environment())
}

select_next_item_mixed_effects <- function(item_bank, opt) {

  model <- opt$mixed_effects_model

  sigma <- lme4::VarCorr(model) %>% # get model SD
    as.data.frame() %>%
    dplyr::filter(grp == "p_id") %>%
    dplyr::pull(sdcor)

  ability_preds <- lme4::ranef(model)$p_id[, '(Intercept)']

  min_ability <- min(ability_preds)
  max_ability <- max(ability_preds)


  psychTestR::code_block(
    function(state, ...) {
      test_state <- psychTestR::get_local("test_state", state)
      ability_estimate <- get_current_ability_estimate_mixed_effects(
        test_state, opt = opt, estimator = opt$next_item.estimator)

      allowed_items <- get_allowed_items(test_state, item_bank, opt)
      next_item <- tryCatch(catR::nextItem(
        itemBank = as.matrix(item_bank[, c("discrimination", "difficulty",
                                           "guessing", "inattention")]),
        theta = ability_estimate,
        out = test_state$results.by_item$item_id,
        x = test_state$results.by_item$score,
        criterion = opt$next_item.criterion,
        method = opt$next_item.estimator,
        nAvailable = as.numeric(allowed_items),
        maxItems = Inf,
        cbControl = opt$cb_control,
        cbGroup = opt$cb_group,
        priorPar = c(0, sigma),
        range = c(min_ability, max_ability)
      ), error = function(e) NULL)

      test_state$next_item <- next_item
      if (is.null(next_item) ||
          is.null(next_item$item) ||
          is.na(next_item$item)) {
        shiny::showNotification(
          "Failed to select new item, terminating adaptive procedure.",
          duration = opt$notify_duration)
        test_state$terminate_test <- TRUE
        psychTestR::skip_n_pages(state, n = 2L) # this is dangerous
      } else if (opt$demo) {
        msg <- shiny::p("Difficulty: ",
                        shiny::strong(format(next_item$par[2],
                                             digits = 3,
                                             nsmall = 3)))
        shiny::showNotification(msg, duration = opt$notify_duration)
      }
      psychTestR::set_local(key = "test_state", value = test_state, state = state)
    })
}



save_result <- function(item_bank, opt) {
  psychTestR::code_block(
    function(state, ...) {
      test_state <- psychTestR::get_local("test_state", state)

      previous_ability_estimate <-
        if(opt$continuous_response) get_current_ability_estimate_mixed_effects(
        test_state = test_state, opt = opt) else get_current_ability_estimate(
          test_state = test_state, opt = opt)

      item_info <- test_state$next_item
      item_id <- item_info$item
      answer <- psychTestR::answer(state)
      correct_answer <- item_bank[item_id, "answer"]
      score <- answer == correct_answer

      new_row <- data.frame(
        num = get_num_items_administered(test_state) + 1L,
        item_id = item_id,
        discrimination = item_info$par[["discrimination"]],
        difficulty = item_info$par[["difficulty"]],
        guessing = item_info$par[["guessing"]],
        inattention = item_info$par[["inattention"]],
        information = item_info$info,
        criterion = item_info$criterion,
        answer = answer,
        correct_answer = correct_answer,
        score = score
      )
      stopifnot(nrow(new_row) == 1L)

      test_state$results.by_item <- plyr::rbind.fill(test_state$results.by_item,
                                                     new_row)

      tmp_item_params <- test_state$results.by_item[, c("discrimination",
                                                        "difficulty",
                                                        "guessing",
                                                        "inattention")]
      tmp_scores <- test_state$results.by_item$score
      n <- nrow(test_state$results.by_item)
      test_state$num_items_administered <- n

      for (method in c("ML", "BM", "EAP", "WL")) {
        tmp_ability <- catR::thetaEst(tmp_item_params, tmp_scores, method = method)
        tmp_ability_sem <- catR::semTheta(thEst = tmp_ability,
                                          it = tmp_item_params,
                                          method = method)
        test_state$results.by_item[n, paste0("ability_",
                                             method)] <- tmp_ability
        test_state$results.by_item[n, paste0("ability_",
                                             method,
                                             "_sem")] <- tmp_ability_sem
      }

      if (opt$demo) {
        new_ability_estimate <-
          if(opt$continuous_response) get_current_ability_estimate_mixed_effects(test_state, opt) else get_current_ability_estimate(test_state, opt)
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



save_result_mixed_effects <- function(item_bank, opt) {
  psychTestR::code_block(
    function(state, ...) {
      test_state <- psychTestR::get_local("test_state", state)

      previous_ability_estimate <- get_current_ability_estimate_mixed_effects(
        test_state = test_state, opt = opt)

      item_info <- test_state$next_item
      item_id <- item_info$item
      answer <- psychTestR::answer(state)

      if(answer$error) {
        shiny::showNotification("Sorry, we didn't get capture a response. Predicting mean.", duration = opt$notify_duration)
        score <- 0
      } else {
        score <- answer[[opt$dv_name]]
      }

      # Only take length one answers
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


      tmp_ability <- opt$predict_based_on_mixed_effects_model_function(opt$mixed_effects_model, new_data)
      tmp_ability_sem <- NA

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




get_fixed_effect_param <- function(param, model) {
  as.vector(lme4::fixef(model)[param])
}
