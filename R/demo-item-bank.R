#' Demo item bank
#'
#' This function creates an example item bank
#' with randomly generated difficulty and discrimination parameters.
#' @param n (Integerish scalar) Number of questions to include in the item bank.
#' @param questions Vector of questions to sample from.
#' @param answers Vector of answers to sample from.
#' @return A generated item bank in the form of a data frame,
#' suitable for passing to \code{\link{adapt_test}}.
#' @export
demo_item_bank <- function(n = 20, questions = 1:4, answers = 1:2) {
  checkmate::qassert(n, "X1")
  stopifnot(
    is.vector(questions),
    is.vector(answers)
  )
  data.frame(
    id = seq_len(n),
    discrimination = stats::runif(min = 1, max = 2, n = n),
    difficulty = stats::rnorm(n),
    guessing = 0,
    inattention = 1,
    question = sample(questions, size = n, replace = TRUE),
    answer = sample(answers, size = n, replace = TRUE)
  )
}
