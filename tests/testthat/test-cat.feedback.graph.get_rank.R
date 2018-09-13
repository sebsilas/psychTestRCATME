context("cat.feedback.graph.get_rank")

test_that("examples", {
  expect_equal(cat.feedback.graph.get_rank(all_scores = c(1:5, 5)), 1)
  expect_equal(cat.feedback.graph.get_rank(all_scores = c(1:5, 6)), 1)
  expect_equal(cat.feedback.graph.get_rank(all_scores = c(1, 2, 2, 3, 4, 2)), 3)
  expect_equal(cat.feedback.graph.get_rank(all_scores = c(1:5, 0)), 6)
})
