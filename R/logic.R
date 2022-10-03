
# returns TRUE if we should stop

#' New stopping rule
#'
#' Creates a new stopping rule for the adaptive test.
#'
#' A stopping rule defines a condition that is checked each time
#' a new item is about to be administered.
#'
#' @param f
#' Function of the form \code{function(test_state) {...}},
#' which will be called whenever a new item is about to be administered.
#' \code{test_state} is a data frame representing the current status of
#' the adaptive test (items administered, participant responses, etc.).
#' Currently the only method defined for \code{test_state} objects
#' is \code{\link{get_num_items_administered}};
#' other extractors need to be defined manually.
#'
#' @param num_items_in_test
#' (NULL or integerish scalar)
#' If the number of items in the test is known,
#' providing it in this argument allows it to be used by other test features,
#' e.g. to tell the participant their progress through the test.
#'
#' @return
#' An object of class \code{stopping_rule} which can be
#' passed to the \code{stopping_rule} argument of
#' \code{\link{adapt_test}}.
#'
#' @export
new_stopping_rule <- function(f, num_items_in_test = NULL) {
  stopifnot(is.function(f), is.null.or(num_items_in_test, is.scalar.numeric))
  class(f) <- c("stopping_rule", class(f))
  attr(f, "num_items_in_test") <- num_items_in_test
  f
}

#' Stopping rule (num items)
#'
#' Creates a stopping rule where the test finishes once a set number
#' of items have been administered.
#'
#' @param n
#' (Integerish scalar)
#' Number of items after which the test should terminate.
#'
#' @return
#' An object of class \code{stopping_rule} which can be
#' passed to the \code{stopping_rule} argument of
#' \code{\link{adapt_test}}.
#'
#' @export
stopping_rule.num_items <- function(n) {
  if (is.null(n)) stop("number of items cannot be NULL")
  stopifnot(is.scalar.numeric(n), n > 0)
  f <- function(test_state) {
    get_num_items_administered(test_state) >= n
  }
  new_stopping_rule(f, num_items_in_test = n)
}

#' Number of items administered
#'
#' Returns the number of items administered so far in the test.
#'
#' @param test_state
#' A \code{test_state} object; see \code{\link{new_stopping_rule}}.
#'
#' @return
#' (Integer scalar)
#' The number of items administered so far in the test.
#'
#' @export
get_num_items_administered <- function(test_state) {
  df <- test_state$results.by_item
  if (is.null(df)) 0L else nrow(df)
}

get_items_administered <- function(test_state) {
  df <- test_state$results.by_item
  if (is.null(df)) integer() else as.integer(df$item_id)
}

#' Get item number
#'
#' Returns an item's position in the test.
#'
#' @param x An item object as passed to the \code{show_item} function
#' (see \code{\link{adapt_test}}).
#'
#' @return
#' (Integer scalar) The item's position in the test; for example,
#' the second item in the test would yield the integer \code{2L}.
#'
#' @rdname get_item_number
#' @export
get_item_number <- function(x) {
  UseMethod("get_item_number")
}

#' @rdname get_item_number
#' @export
get_item_number.item <- function(x) {
  attr(x, "item_number")
}

#' Number of items in test
#'
#' Returns the total number of items in the test,
#' including those that have yet to be administered.
#' Methods exist for:
#' - \code{stopping_rule} (see \code{\link{new_stopping_rule}}).
#' - \code{item} (as passed to \code{show_item} in \code{\link{adapt_test}}).
#' - \code{test_state} (as passed to stopping rules,
#' see \code{\link{new_stopping_rule}}).
#'
#' @param x Object from which to extract the information.
#' @return (Integer scalar)
#' The total number of items in the test,
#' including those that have yet to be administered.
#'
#' @md
#' @rdname get_num_items_in_test
#' @export
get_num_items_in_test <- function(x) {
  UseMethod("get_num_items_in_test")
}

#' @rdname get_num_items_in_test
#' @export
get_num_items_in_test.stopping_rule <- function(x) {
  attr(x, "num_items_in_test")
}

#' @rdname get_num_items_in_test
#' @export
get_num_items_in_test.item <- function(x) {
  attr(x, "num_items_in_test")
}

#' @rdname get_num_items_in_test
#' @export
get_num_items_in_test.test_state <- function(x) {
  x$num_items_in_test
}

#' Current ability estimate
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
  if (is.null(item_bank$answer)) stop("answer column not found in item bank")
  if (is.list(item_bank$answer)) stop("answer column cannot be a list")

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

# The item must match correct_answers (if specified),
# and it must avoid duplicates in opt$avoid_duplicates.
# If this is not possible, the item is chosen randomly, with a warning.
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

# save_result <- function(item_bank, opt) {
#   psychTestR::code_block(
#     function(state, ...) {
#       test_state <- psychTestR::get_local("test_state", state)
#
#       previous_ability_estimate <- get_current_ability_estimate(
#         test_state = test_state, opt = opt)
#
#       item_info <- test_state$next_item
#       item_id <- item_info$item
#       answer <- psychTestR::answer(state)
#       correct_answer <- item_bank[item_id, "answer"]
#       score <- answer == correct_answer
#
#       new_row <- data.frame(
#         num = get_num_items_administered(test_state) + 1L,
#         item_id = item_id,
#         discrimination = item_info$par[["discrimination"]],
#         difficulty = item_info$par[["difficulty"]],
#         guessing = item_info$par[["guessing"]],
#         inattention = item_info$par[["inattention"]],
#         information = item_info$info,
#         criterion = item_info$criterion,
#         answer = answer,
#         correct_answer = correct_answer,
#         score = score
#       )
#       stopifnot(nrow(new_row) == 1L)
#
#       test_state$results.by_item <- plyr::rbind.fill(test_state$results.by_item,
#                                                      new_row)
#
#       tmp_item_params <- test_state$results.by_item[, c("discrimination",
#                                                         "difficulty",
#                                                         "guessing",
#                                                         "inattention")]
#       tmp_scores <- test_state$results.by_item$score
#       n <- nrow(test_state$results.by_item)
#       test_state$num_items_administered <- n
#
#       for (method in c("ML", "BM", "EAP", "WL")) {
#         tmp_ability <- catR::thetaEst(tmp_item_params, tmp_scores, method = method)
#         tmp_ability_sem <- catR::semTheta(thEst = tmp_ability,
#                                           it = tmp_item_params,
#                                           method = method)
#         test_state$results.by_item[n, paste0("ability_",
#                                              method)] <- tmp_ability
#         test_state$results.by_item[n, paste0("ability_",
#                                              method,
#                                              "_sem")] <- tmp_ability_sem
#       }
#
#       if (psychTestR::demo(state)) {
#         new_ability_estimate <- get_current_ability_estimate(test_state, opt)
#         ability_change <- new_ability_estimate - previous_ability_estimate
#         msg <- shiny::div(
#           shiny::p(shiny::strong(if (score) "Correct" else "Incorrect"),
#                    " answer."),
#           shiny::p(
#             "Ability: ",
#             shiny::strong(format(new_ability_estimate, digits = 3, nsmall = 3)),
#             paste0(
#               "(",
#               if (ability_change > 0) "+",
#               format(ability_change, digits = 3, nsmall = 3),
#               ")"
#             )))
#         shiny::showNotification(msg, duration = opt$notify_duration,
#                                 type = if (score) "message" else "error")
#       }
#       psychTestR::set_local(key = "test_state", value = test_state, state = state)
#     }
#   )
# }

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
    if(opt$continuous_response) psychTestR::save_result_mixed_effects(state, label = "ability", value = final_ability) else psychTestR::save_result(state, label = "ability", value = final_ability)
    if(opt$continuous_response) psychTestR::save_result_mixed_effects(state, label = "ability_sem", value = final_ability_sem) else psychTestR::save_result(state, label = "ability_sem", value = final_ability_sem)
    if(opt$continuous_response) psychTestR::save_result_mixed_effects(state, label = "num_items", value = n) else psychTestR::save_result(state, label = "num_items", value = n)
        psychTestR::set_local("test_state", value = NULL, state = state)
  })
}
