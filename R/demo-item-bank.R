#' @export
demo_item_bank <- function(n = 20, questions = 1:4, answers = 1:2) {
  data.frame(
    id = seq_len(n),
    discrimination = runif(min = 1, max = 2, n = n),
    difficulty = rnorm(n),
    guessing = 0,
    inattention = 1,
    question = sample(questions, size = n, replace = TRUE),
    answer = sample(answers, size = n, replace = TRUE)
  )
}
