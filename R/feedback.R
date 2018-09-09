cat.feedback.graph <- function(test_label,
                               prompt = "Your final score was:",
                               x_axis = "Score", y_axis = "Count",
                               caption = "Your score compared to historic participants. Your rank:",
                               next_button = NULL,
                               digits = 3L) {
  stopifnot(is.scalar.character(test_label))
  c(
    cat.feedback.graph.manage_scores(test_label = test_label),
    cat.feedback.graph.display_scores(prompt = prompt,
                                      x_axis = x_axis, y_axis = y_axis,
                                      caption = caption,
                                      next_button = next_button,
                                      digits = digits)
  )
}

cat.feedback.graph.manage_scores <- function(test_label) {
  psychTestR::code_block(function(answer, opt, state, ...)) {
    score <- answer
    stopifnot(is.scalar.numeric(score))
    file <- file.path(psychTestR::get_supplementary_results_dir(opt),
                      paste0(test_label, "_final_scores.txt"))
    write(score, file, append = TRUE)
    all_scores <- as.numeric(read.table(file))
    num_scores <- length(all_scores)
    rank <- rank(all_scores)[num_scores]
    psychTestR::set_local(key = "cat_results",
                          value = list(score = score,
                                       all_scores = all_scores,
                                       num_scores = num_scores,
                                       rank = rank),
                          state = state)
  }
}

cat.feedback.graph.display_scores <- function(x_axis, y_axis, caption,
                                              next_button, digits) {
  stopifnot(is.scalar.character(x_axis),
            is.scalar.character(y_axis),
            is.scalar.character(prompt) || is(prompt, "shiny.tag"),
            is.scalar.character(caption) || is(caption, "shiny.tag"),
            is.null(next_button) || is.scalar.character(next_button) ||
              is(next_button, "shiny.tag"),
            is.scalar.integerlike(digits))
  psychTestR::reactive_page(function(state, ...)) {
    res <- psychTestR::get_local(key = "cat_results", state = state)
    if (!is.list(res)) stop("<cat_results> was malformed, looking like this: ",
                            utils::capture.output(print(res)))
    num_bins <- ceiling(log2(res$num_scores)) + 1
    psychTestR::page(
      ui = shiny::div(
        shiny::p(prompt, shiny::strong(round(res$score, digits = digits))),

        plotly::ggplotly(ggplot2::ggplot(
          data.frame(Score = res$scores), ggplot2::aes(x = Score)
        ) + ggplot2::geom_histogram(bins = num_bins, colour = "#004d66",
                                    fill = "#00ace6") +
          ggplot2::geom_vline(xintercept = 0.5, colour = "#e60000") +
          ggplot2::scale_y_continuous("Count") +
          ggplot2::theme_bw() +
          ggplot2::theme(panel.grid = ggplot2::element_blank()))

        plotly::plot_ly(x = res$scores, type = "histogram") %>%
          plotly::layout(xaxis = list(title = "Score"),
                         yaxis = list(title = "Count"))
      )
    )
  }
}
