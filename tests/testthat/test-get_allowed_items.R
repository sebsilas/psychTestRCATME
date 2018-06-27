library(magrittr)
library(testthat)

context("get_allowed_items")

test_that("example 1", {
  opt <- psychTestRCAT::adapt_test_options(constrain_answers = TRUE,
                                           avoid_duplicates = "audio")

  item_bank <- data.frame(
    audio = rep(letters, each = 2),
    discrimination = 1,
    difficulty = rnorm(26 * 2),
    guessing = 0,
    inattention = 1,
    answer = rep(c(1, 2), times = 26),
    stringsAsFactors = FALSE
  )

  test_state <- psychTestRCAT:::new_state(num_items_in_test = 30L,
                                          constrain_answers = TRUE,
                                          item_bank = item_bank)

  num_items_administered <- 25
  items_administered <- rep(NA, times = 2)
  for (i in seq_len(num_items_administered))
    items_administered[i] <- which(
      item_bank$answer == test_state$correct_answers[i] &
        !seq_len(nrow(item_bank)) %in% na.omit(items_administered) &
        !item_bank$audio %in% item_bank$audio[na.omit(items_administered)]
    ) %>% (function(x) x[sample(length(x), size = 1)])

  test_state$results.by_item <- data.frame(
    num = seq_along(items_administered),
    item_id = items_administered,
    discrimination = item_bank$discrimination[items_administered],
    difficulty = item_bank$discrimination[items_administered],
    guessing = item_bank$discrimination[items_administered],
    inattention = item_bank$discrimination[items_administered],
    information = NA,
    criterion = NA,
    answer = sample(1:2, size = num_items_administered, replace = TRUE),
    correct_answer = item_bank$answer[items_administered])
  test_state$results.by_item$score <-
    test_state$results.by_item$answer == test_state$results.by_item$correct_answer

  res <- psychTestRCAT:::get_allowed_items(test_state = test_state, item_bank = item_bank, opt = opt)
  expect_equal(sum(res), 1)
  expect_true(!item_bank$audio[res] %in% item_bank$audio[test_state$results.by_item$item_id])
  expect_true(item_bank$answer[res] == test_state$correct_answers[num_items_administered + 1L])
})

test_that("example 2 - constraints impossible", {
  opt <- psychTestRCAT::adapt_test_options(constrain_answers = TRUE,
                                           avoid_duplicates = "audio")

  item_bank <- data.frame(
    audio = rep(letters, each = 2),
    discrimination = 1,
    difficulty = rnorm(26 * 2),
    guessing = 0,
    inattention = 1,
    answer = rep(c(1, 2), times = 26),
    stringsAsFactors = FALSE
  )

  test_state <- psychTestRCAT:::new_state(num_items_in_test = 30L,
                                          constrain_answers = TRUE,
                                          item_bank = item_bank)

  num_items_administered <- 26
  items_administered <- rep(NA, times = 2)
  for (i in seq_len(num_items_administered))
    items_administered[i] <- which(
      item_bank$answer == test_state$correct_answers[i] &
        !seq_len(nrow(item_bank)) %in% na.omit(items_administered) &
        !item_bank$audio %in% item_bank$audio[na.omit(items_administered)]
    ) %>% (function(x) x[sample(length(x), size = 1)])

  test_state$results.by_item <- data.frame(
    num = seq_along(items_administered),
    item_id = items_administered,
    discrimination = item_bank$discrimination[items_administered],
    difficulty = item_bank$discrimination[items_administered],
    guessing = item_bank$discrimination[items_administered],
    inattention = item_bank$discrimination[items_administered],
    information = NA,
    criterion = NA,
    answer = sample(1:2, size = num_items_administered, replace = TRUE),
    correct_answer = item_bank$answer[items_administered])
  test_state$results.by_item$score <-
    test_state$results.by_item$answer == test_state$results.by_item$correct_answer
  expect_warning({
    res <- psychTestRCAT:::get_allowed_items(test_state = test_state, item_bank = item_bank, opt = opt)
  })
  expect_true(all(res))
})

test_that("example 3 - accumulating items administered", {
  opt <- psychTestRCAT::adapt_test_options(constrain_answers = TRUE,
                                           avoid_duplicates = "audio")
  item_bank <- data.frame(
    audio = rep(letters, each = 2),
    discrimination = 1,
    difficulty = rnorm(26 * 2),
    guessing = 0,
    inattention = 1,
    answer = rep(c(1, 2), times = 26),
    stringsAsFactors = FALSE
  )
  test_state <- psychTestRCAT:::new_state(num_items_in_test = 26L,
                                          constrain_answers = TRUE,
                                          item_bank = item_bank)
  for (i in seq_len(26)) {
    allowed_items <- psychTestRCAT:::get_allowed_items(
      test_state = test_state, item_bank = item_bank, opt = opt) %>%
      which
    chosen <- allowed_items %>% (function(x) x[sample(length(x), size = 1)])
    test_state$results.by_item <- rbind(test_state$results.by_item,
                                        data.frame(item_id = chosen))
  }
  expect_equal(item_bank$answer[test_state$results.by_item$item_id],
               test_state$correct_answers)
  expect_true(!anyDuplicated(item_bank$audio[test_state$results.by_item$item_id]))
  expect_warning(psychTestRCAT:::get_allowed_items(
    test_state = test_state, item_bank = item_bank, opt = opt))
})

test_that("example 4 - just constrain answers", {
  opt <- psychTestRCAT::adapt_test_options(constrain_answers = TRUE)
  item_bank <- data.frame(
    audio = rep(letters, each = 2),
    discrimination = 1,
    difficulty = rnorm(26 * 2),
    guessing = 0,
    inattention = 1,
    answer = rep(c(1, 2), times = 26),
    stringsAsFactors = FALSE
  )
  test_state <- psychTestRCAT:::new_state(num_items_in_test = 26L,
                                          constrain_answers = TRUE,
                                          item_bank = item_bank)
  for (i in seq_len(26)) {
    allowed_items <- psychTestRCAT:::get_allowed_items(
      test_state = test_state, item_bank = item_bank, opt = opt) %>%
      which
    expect_equal(allowed_items,
                 which(item_bank$answer == test_state$correct_answers[i]))
    chosen <- allowed_items %>% (function(x) x[sample(length(x), size = 1)])
    test_state$results.by_item <- rbind(test_state$results.by_item,
                                        data.frame(item_id = chosen))
  }
})

test_that("example 5 - just avoid duplicates", {
  opt <- psychTestRCAT::adapt_test_options(constrain_answers = FALSE,
                                           avoid_duplicates = "audio")
  item_bank <- data.frame(
    audio = rep(letters, each = 2),
    discrimination = 1,
    difficulty = rnorm(26 * 2),
    guessing = 0,
    inattention = 1,
    answer = rep(c(1, 2), times = 26),
    stringsAsFactors = FALSE
  )
  test_state <- psychTestRCAT:::new_state(num_items_in_test = 26L,
                                          constrain_answers = FALSE,
                                          item_bank = item_bank)
  for (i in seq_len(26)) {
    allowed_items <- psychTestRCAT:::get_allowed_items(
      test_state = test_state, item_bank = item_bank, opt = opt) %>%
      which
    expect_equal(allowed_items,
                 which(!item_bank$audio %in%
                         item_bank$audio[test_state$results.by_item$item_id]))
    chosen <- allowed_items %>% (function(x) x[sample(length(x), size = 1)])
    test_state$results.by_item <- rbind(test_state$results.by_item,
                                        data.frame(item_id = chosen))
  }
})

test_that("example 6 - constraining first item", {
  opt <- psychTestRCAT::adapt_test_options(constrain_answers = TRUE,
                                           eligible_first_items = c(1, 2))
  item_bank <- data.frame(
    audio = rep(letters, each = 2),
    discrimination = 1,
    difficulty = rnorm(26 * 2),
    guessing = 0,
    inattention = 1,
    answer = rep(c(1, 2), times = 26),
    stringsAsFactors = FALSE
  )
  test_state <- psychTestRCAT:::new_state(num_items_in_test = 26L,
                                          constrain_answers = TRUE,
                                          item_bank = item_bank)
  for (i in seq_len(26)) {
    allowed_items <- psychTestRCAT:::get_allowed_items(
      test_state = test_state, item_bank = item_bank, opt = opt) %>%
      which
    expect_equal(allowed_items,
                 which(item_bank$answer == test_state$correct_answers[i]) %>%
                   (function(x) if (i == 1L) intersect(x, c(1, 2)) else x))
    chosen <- allowed_items %>% (function(x) x[sample(length(x), size = 1)])
    test_state$results.by_item <- rbind(test_state$results.by_item,
                                        data.frame(item_id = chosen))
  }
})
