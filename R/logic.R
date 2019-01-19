#' Adaptive test
#'
#' Defines an adaptive test for use within psychTestR.
#'
#' This is the top-level function of the psychTestRCAT package.
#' It defines adaptive tests within the framework of item response theory.
#' An aadptive test is one that tailors item selection to the
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
    psychTestR::loop_while(
      test = check_stopping_rule(stopping_rule),
      logic = c(
        select_next_item(item_bank, opt),
        administer_next_item(item_bank, show_item),
        save_result(item_bank, opt)
      )),
    finalise(opt))
}

#' @param eligible_first_items NULL or numeric vector listing eligible items for the first item in the test (e.g. c(2, 3, 4) means that the first item will be drawn from rows 2, 3, 4 of the item bank)
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
                               notify_duration = 5
) {
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
    is.scalar.logical(constrain_answers)
  )
  local(if (!(is.null(cb_control) && is.null(cb_group))) {
      cb_test <- catR::test.cbList(cb_control, cb_group)
      if (!cb_test$test) {
        stop("problem detected with cb_control/cb_group arguments: ",
             cb_test$message)
      }})
  as.list(environment())
}

# returns TRUE if we should stop

new_stopping_rule <- function(f, num_items_in_test = NULL) {
  stopifnot(is.function(f), is.null.or(num_items_in_test, is.scalar.numeric))
  class(f) <- c("stopping_rule", class(f))
  attr(f, "num_items_in_test") <- num_items_in_test
  f
}

#' @export
stopping_rule.num_items <- function(n) {
  if (is.null(n)) stop("number of items cannot be NULL")
  stopifnot(is.scalar.numeric(n), n > 0)
  f <- function(test_state) {
    get_num_items_administered(test_state) >= n
  }
  new_stopping_rule(f, num_items_in_test = n)
}

#' @export
get_num_items_administered <- function(test_state) {
  df <- test_state$results.by_item
  if (is.null(df)) 0L else nrow(df)
}

get_items_administered <- function(test_state) {
  df <- test_state$results.by_item
  if (is.null(df)) integer() else as.integer(df$item_id)
}

#' @export
get_item_number <- function(x) {
  UseMethod("get_item_number")
}

#' @export
get_item_number.item <- function(x) {
  attr(x, "item_number")
}

#' @export
get_num_items_in_test <- function(x) {
  UseMethod("get_num_items_in_test")
}

#' @export
get_num_items_in_test.stopping_rule <- function(x) {
  attr(x, "num_items_in_test")
}

#' @export
get_num_items_in_test.item <- function(x) {
  attr(x, "num_items_in_test")
}

#' @export
get_num_items_in_test.test_state <- function(x) {
  x$num_items_in_test
}

#' @export
get_current_ability_estimate <- function(test_state,
                                         opt,
                                         estimator = opt$next_item.estimator) {
  df <- test_state$results.by_item
  if (is.null(df)) {
    res <- opt$next_item.prior_par[1]
    attr(res, "sem") <- opt$next_item.prior_par[2]
  } else {
    n <- nrow(df)
    col <- paste0("ability_", estimator)
    col_sem <- paste0("ability_", estimator, "_sem")
    res <- df[n, col]
    attr(res, "sem") <- df[n, col_sem]
  }
  stopifnot(is.scalar.numeric(res))
  res
}

new_state <- function(num_items_in_test, constrain_answers, item_bank) {
  stopifnot(is.null.or(num_items_in_test, is.scalar.numeric),
            is.scalar.logical(constrain_answers),
            is.null.or(item_bank, is.data.frame))
  x <- list(num_items_in_test = num_items_in_test,
            results.by_item = NULL,
            terminate_test = FALSE)
  x$correct_answers <- if (constrain_answers) {
    stopifnot(!is.null(item_bank$answer))
    possible_answers <- sort(unique(item_bank$answer))
    sample(possible_answers, num_items_in_test, replace = TRUE)
  }
  class(x) <- "test_state"
  x
}

check_inputs <- function(label, item_bank, show_item, opt) {
  if (!is.function(show_item)) show_item <- psychTestR::as.timeline(show_item)
  stopifnot(
    is.scalar.character(label),
    is.data.frame(item_bank),
    is.function(show_item) || psychTestR::is.timeline(show_item)
  )
  for (col in c("discrimination", "difficulty", "guessing", "inattention")) {
    if (!col %in% names(item_bank)) {
      stop("column ", col, " not found in <item_bank>")
    }
    if (!is.numeric(item_bank[[col]])) {
      stop(col, " must be numeric")
    }
    if (anyNA(item_bank[[col]]))
      stop("NA values not permitted in column ", col)
  }
  if (!is.null(opt$avoid_duplicates)) {
    if (!all(opt$avoid_duplicates %in% names(item_bank)))
      stop("all elements of avoid_duplicates must correspond to ",
           "columns of the item bank")
  }
  if (!is.null(opt$eligible_first_items)) {
    if (!all(opt$eligible_first_items %in% seq_len(nrow(item_bank))))
      stop("eligible_first_items must be integers indexing rows of item_bank")
  }
}

setup <- function(label, stopping_rule, opt, item_bank) {
  psychTestR::code_block(function(state, ...) {
    num_items_in_test <- get_num_items_in_test(stopping_rule)
    test_state <- new_state(num_items_in_test = num_items_in_test,
                            constrain_answers = opt$constrain_answers,
                            item_bank = item_bank)
    psychTestR::set_local(key = "test_state", value = test_state, state = state)
    psychTestR::register_next_results_section(state, label)
  })
}

# Returns TRUE if we should keep going
check_stopping_rule <- function(stopping_rule) {
  function(state, ...) {
    test_state <- psychTestR::get_local(key = "test_state", state = state)
    !(stopping_rule(test_state) || test_state$terminate_test)
  }
}

#' The item must match correct_answers (if specified),
#' and it must avoid duplicates in opt$avoid_duplicates.
#' If this is not possible, the item is chosen randomly, with a warning.
get_allowed_items <- function(test_state, item_bank, opt) {
  cond1 <- is_answer_valid(test_state, item_bank)
  cond2 <- are_duplicates_avoided(test_state, item_bank, opt)
  cond3 <- check_eligible_first_items(test_state, item_bank, opt)
  for (x in list(cond1, cond2, cond3)) {
    stopifnot(is.logical(x), length(x) == nrow(item_bank))
  }
  res <- cond1 & cond2 & cond3
  stopifnot(is.null(res) || length(res) == nrow(item_bank))
  if (!any(res)) {
    res <- rep(TRUE, times = nrow(item_bank))
    warning("couldn't satisfy all constraints on item selection, ",
            "so disabling content balancing")
  }
  res
}

check_eligible_first_items <- function(test_state, item_bank, opt) {
  item_num <- get_num_items_administered(test_state) + 1L
  if (is.null(opt$eligible_first_items) || (item_num != 1)) {
    rep(TRUE, times = nrow(item_bank))
  } else {
    res <- rep(FALSE, times = nrow(item_bank))
    res[opt$eligible_first_items] <- TRUE
    res
  }
}

is_answer_valid <- function(test_state, item_bank) {
  if (is.null(test_state$correct_answers)) {
    rep(TRUE, times = nrow(item_bank))
  } else {
    item_num <- get_num_items_administered(test_state) + 1L
    correct_answer <- test_state$correct_answers[item_num]
    item_bank$answer == correct_answer
  }
}

are_duplicates_avoided <- function(test_state, item_bank, opt) {
  if (is.null(opt$avoid_duplicates)) {
    rep(TRUE, times = nrow(item_bank))
  } else {
    cols <- opt$avoid_duplicates
    items_administered <- get_items_administered(test_state)
    res <- rep(TRUE, times = nrow(item_bank))
    for (col in cols) {
      res[item_bank[[col]] %in% item_bank[items_administered, col]] <- FALSE
    }
    res
  }
}

select_next_item <- function(item_bank, opt) {
  psychTestR::code_block(
    function(state, ...) {
      test_state <- psychTestR::get_local("test_state", state)
      ability_estimate <- get_current_ability_estimate(
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
        cbGroup = opt$cb_group
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
      } else if (psychTestR::demo(state)) {
        msg <- shiny::p("Difficulty: ",
                        shiny::strong(format(next_item$par[2],
                                             digits = 3,
                                             nsmall = 3)))
        shiny::showNotification(msg, duration = opt$notify_duration)
      }
      psychTestR::set_local(key = "test_state", value = test_state, state = state)
    })
}

administer_next_item <- function(item_bank, show_item) {
  c(
    psychTestR::code_block(fun = function(state, ...) {
      test_state <- psychTestR::get_local("test_state", state)
      item_id <- test_state$next_item$item
      stopifnot(is.scalar.numeric(item_id),
                item_id > 0, item_id <= nrow(item_bank))
      item <- new_item(df = item_bank[item_id, ],
                       item_number = get_num_items_administered(test_state) + 1L,
                       num_items_in_test = get_num_items_in_test(test_state))
      psychTestR::set_local("item", item, state)
    }),
    if (is.function(show_item)) {
      psychTestR::reactive_page(function(state, ...) {
        item <- psychTestR::get_local("item", state)
        show_item(item, state, ...)
      })
    } else {
      show_item
    }
  )
}

new_item <- function(df, item_number, num_items_in_test) {
  stopifnot(is.data.frame(df), nrow(df) == 1L,
            is.scalar.numeric(item_number),
            is.null.or(num_items_in_test, is.scalar.numeric))
  attr(df, "item_number") <- item_number
  attr(df, "num_items_in_test") <- num_items_in_test
  class(df) <- c("item", class(df))
  df
}

save_result <- function(item_bank, opt) {
  psychTestR::code_block(
    function(state, ...) {
      test_state <- psychTestR::get_local("test_state", state)

      previous_ability_estimate <- get_current_ability_estimate(
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

      if (psychTestR::demo(state)) {
        new_ability_estimate <- get_current_ability_estimate(test_state, opt)
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

finalise <- function(opt) {
  psychTestR::code_block(function(state, ...) {
    test_state <- psychTestR::get_local("test_state", state)
    df <- test_state$results.by_item
    n <- nrow(df)
    final_ability <- df[n, paste0("ability_", opt$final_ability.estimator)]
    attr(final_ability, "metadata") <- list(results = df, options = opt)
    final_ability_sem <-
      df[n, paste0("ability_", opt$final_ability.estimator, "_sem")]
    psychTestR::answer(state) <- list(
      ability = final_ability, ability_sem = final_ability_sem)
    psychTestR::save_result(
      state, label = "ability", value = final_ability)
    psychTestR::save_result(
      state, label = "ability_sem", value = final_ability_sem)
    psychTestR::save_result(
      state, label = "num_items", value = n)
    psychTestR::set_local("test_state", value = NULL, state = state)
  })
}
