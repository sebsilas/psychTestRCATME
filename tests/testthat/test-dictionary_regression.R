test_that("dictionary regression tests", {
  sanitise <- function(x) {
    gsub(" </p>", "</p>", x, fixed = TRUE)
  }

  test_dict <- function(old, languages, ignore = character()) {
    filter <- function(df, ignore) {
      df[!df$key %in% ignore, ]
    }
    old_1 <- local({
      env <- new.env()
      load(system.file(old, package = "psychTestRCAT", mustWork = TRUE),
           envir = env)
      filter(as.data.frame(env$ptrcat_dict), ignore)
    })
    current <- filter(as.data.frame(psychTestRCAT::ptrcat_dict), ignore)
    for (l in c("key", languages)) {
      expect_equal(sanitise(old_1[[l]]),
                   sanitise(current[[l]]))
    }
  }

  test_dict("regression-tests/dictionaries/dict-2019.rda",
            ignore = c("feedback_iq", "feedback_irt"),
            c("DE", "EN"))
})
