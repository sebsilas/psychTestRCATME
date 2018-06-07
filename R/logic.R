#' @export
adapt_test <- function(label,
                       item_bank,
                       show_item,
                       stopping_rule = stopping_rule.num_items(n = NULL),
                       opt = adapt_test_options()) {
  check_inputs(label, item_bank, show_item)
  c(
    setup(label, stopping_rule, opt, item_bank),
    psychTest::loop_while(
      test = check_stopping_rule(stopping_rule),
      logic = c(
        select_next_item(item_bank, opt),
        administer_next_item(item_bank, show_item),
        save_result(item_bank, opt)
      )),
    finalise(opt))
}

#' @export
adapt_test_options <- function(next_item.criterion = "MFI",
                               next_item.estimator = "BM",
                               next_item.prior_dist = "norm",
                               next_item.prior_par = c(0, 1),
                               final_ability.estimator = "BM",
                               cb_control = NULL,
                               cb_group = NULL,
                               notify_duration = 5,
                               constrain_answers = FALSE) {
  stopifnot(
    is.scalar.character(next_item.criterion),
    is.scalar.character(next_item.estimator),
    is.scalar.character(next_item.prior_dist),
    is.scalar.character(final_ability.estimator),
    is.numeric(next_item.prior_par),
    length(next_item.prior_par) == 2L,
    next_item.estimator %in% c("ML", "BM", "EAP", "WL"),
    final_ability.estimator %in% c("ML", "BM", "EAP", "WL"),
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
  f <- function(test_state) get_num_items_administered(test_state) == n
  new_stopping_rule(f, num_items_in_test = n)
}

#' @export
get_num_items_administered <- function(test_state) {
  df <- test_state$results.by_item
  if (is.null(df)) 0L else nrow(df)
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
    print(sample(possible_answers, num_items_in_test, replace = TRUE))
  }
  class(x) <- "test_state"
  x
}

check_inputs <- function(label, item_bank, show_item) {
  stopifnot(
    is.scalar.character(label),
    is.data.frame(item_bank),
    is.function(show_item)
  )
  for (col in c("discrimination", "difficulty", "guessing", "inattention")) {
    if (!col %in% names(item_bank)) {
      stop("column ", col, " not found in <item_bank>")
    }
    if (!is.numeric(item_bank[[col]])) {
      stop(col, " must be numeric")
    }
  }
}

setup <- function(label, stopping_rule, opt, item_bank) {
  psychTest::code_block(function(state, ...) {
    message("Setting up adaptive test...")
    num_items_in_test <- get_num_items_in_test(stopping_rule)
    test_state <- new_state(num_items_in_test = num_items_in_test,
                            constrain_answers = opt$constrain_answers,
                            item_bank = item_bank)
    psychTest::set_local(key = "test_state", value = test_state, state = state)
    psychTest::register_next_results_section(state, label)
  })
}

# Returns true if we should stop
check_stopping_rule <- function(stopping_rule) {
  function(state, ...) {
    test_state <- psychTest::get_local(key = "test_state", state = state)
    stopping_rule(test_state) || test_state$terminate_test
  }
}

select_next_item <- function(item_bank, opt) {
  psychTest::code_block(
    function(state, ...) {
      test_state <- psychTest::get_local("test_state", state)
      ability_estimate <- get_current_ability_estimate(
        test_state, opt = opt, estimator = opt$next_item.estimator)
      allowed_items <- if (!is.null(test_state$correct_answers)) {
        item_num <- get_num_items_administered(test_state) + 1L
        correct_answer <- test_state$correct_answers[item_num]
        as.numeric(item_bank$answer == correct_answer)
      }
      stopifnot(is.null(allowed_items) ||
                  length(allowed_items) == nrow(item_bank))
      next_item <- tryCatch(catR::nextItem(
        itemBank = item_bank[, c("discrimination", "difficulty",
                                 "guessing", "inattention")],
        theta = ability_estimate,
        out = test_state$results.by_item[, "item_id"],
        x = test_state$results.by_item[, "score"],
        criterion = opt$next_item.criterion,
        method = opt$next_item.estimator,
        nAvailable = allowed_items,
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
        psychTest::skip_n_pages(state, n = 2L) # this is dangerous
      } else if (psychTest::demo(state)) {
        msg <- shiny::p("Difficulty: ",
                        shiny::strong(format(next_item$par[2],
                                             digits = 3,
                                             nsmall = 3)))
        shiny::showNotification(msg, duration = opt$notify_duration)
      }
      psychTest::set_local(key = "test_state", value = test_state, state = state)
    })
}

administer_next_item <- function(item_bank, show_item) {
  psychTest::reactive_page(
    function(state, ...) {
      test_state <- psychTest::get_local("test_state", state)
      item_id <- test_state$next_item$item
      stopifnot(is.scalar.numeric(item_id),
                item_id > 0, item_id <= nrow(item_bank))
      item <- new_item(df = item_bank[item_id, ],
                       item_number = get_num_items_administered(test_state) + 1L,
                       num_items_in_test = get_num_items_in_test(test_state))
      show_item(item)
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
  psychTest::code_block(
    function(state, ...) {
      test_state <- psychTest::get_local("test_state", state)

      previous_ability_estimate <- get_current_ability_estimate(
        test_state = test_state, opt = opt)

      item_info <- test_state$next_item
      item_id <- item_info$item
      answer <- psychTest::answer(state)
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
      tmp_scores <- test_state$results.by_item[, "score"]
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

      if (psychTest::demo(state)) {
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
      psychTest::set_local(key = "test_state", value = test_state, state = state)
    }
  )
}

finalise <- function(opt) {
  psychTest::code_block(function(state, ...) {
    test_state <- psychTest::get_local("test_state", state)
    df <- test_state$results.by_item
    n <- nrow(df)
    final_ability <- df[n, paste0("ability_", opt$final_ability.estimator)]
    attr(final_ability, "metadata") <- list(results = df, options = opt)
    final_ability_sem <-
      df[n, paste0("ability_", opt$final_ability.estimator, "_sem")]
    psychTest::answer(state) <- list(
      ability = final_ability, ability_sem = final_ability_sem)
    psychTest::save_result(
      state, label = "ability", value = final_ability)
    psychTest::save_result(
      state, label = "ability_sem", value = final_ability_sem)
    psychTest::save_result(
      state, label = "num_items", value = n)
    psychTest::set_local("test_state", value = NULL, state = state)
  })
}
