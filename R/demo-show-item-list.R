#' Demo: show_item of class list
#'
#' This is a demonstration of an adaptive test with a variable number
#' of pages for each item.
#' This is achieved by using a \code{show_item} argument corresponding
#' to a list of test elements.
#' The crucial trick is using a \code{while_loop} function to
#' repeat a 'reactive page' until the item is complete,
#' and a local variable \code{counter} to keep track of the iteration number.
#' @export
demo_show_item_list <- function() {
  item_bank <- demo_item_bank(n = 5, questions = 1:4, answers = 1:3)
  show_item <- c(
    psychTestR::code_block(function(state, ...) {
      psychTestR::set_local("counter", 0L, state)
    }),
    psychTestR::while_loop(
      test = function(state, ...) {
        counter <- psychTestR::get_local("counter", state)
        n <- psychTestR::get_local("item", state)$question
        counter < n
      },
      logic = c(
        psychTestR::code_block(function(state, ...) {
          counter <- psychTestR::get_local("counter", state)
          counter <- 1L + counter
          psychTestR::set_local("counter", counter, state)
        }),
        psychTestR::reactive_page(function(state, ...) {
          counter <- psychTestR::get_local("counter", state)
          n <- psychTestR::get_local("item", state)$question
          psychTestR::one_button_page(paste0("Question page ", counter, "/", n))
        }))),
    psychTestR::NAFC_page("question", "What's your answer?",
                          choices = as.character(1:3),
                          arrange_vertically = FALSE)
  )
  test <- c(psychTestR::one_button_page(paste0(
    "This is a demonstration of an adaptive test with a variable number ",
    "of pages for each item.")),
            adapt_test(label = "Demo",
                       item_bank = item_bank,
                       show_item = show_item,
                       stopping_rule = stopping_rule.num_items(5)),
            psychTestR::final_page("End of demo."))
  psychTestR::make_test(test, opt = psychTestR::demo_options())
}
