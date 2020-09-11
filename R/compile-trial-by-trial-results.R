#' Compile trial-by-trial results
#'
#' Compiles a data frame of trial-by-trial results for each participant.
#' This function is intended to be called once data have been collected
#' for several participants; it allows the researcher to inspect
#' test results on a trial-by-trial level.
#'
#' Suppose that a participant completes 5 items in the test.
#' They will have 5 rows in the table, corresponding to their 5 items.
#' You can find the participant's rows by filtering by the "p_id" column.
#' The column "num" indexes these rows: 1 corresponds to the first item
#' they took, 2 corresponds to the second item, and so on.
#' The ability scores are given in the "ability_" columns; they tell
#' you the participant's estimated ability after having answered that
#' particular item. To get the participant's final estimated ability score,
#' look for the highest value of "num", in this case 5. Then look up the ability
#' score within that row.
#'
#' There are different columns for the different ability
#' estimation methods, make sure you are looking at the right one.
#' The correct one to look at corresponds to the \code{final_ability.estimator}
#' parameter in your adaptive test. We most commonly use weighted likelihood,
#' i.e. "WL". This is stored in the column "ability_WL".
#' If you are unsure that you are looking at the right column,
#' you can compare the results to the CSV results that you can download
#' from the psychTestR admin interface, which provide
#' solely the participant's final ability score.
#'
#' @param in_dir Results directory to process.
#' @param label Label that the test's results were saved under (e.g. "MDT").
#' @param combine Whether to combine results into one big data frame,
#' or instead to return a list of data frames, one for each participant.
#' @return A data frame, or list of data frames, of trial-by-trial results.
#' @export
compile_trial_by_trial_results <- function(in_dir = "output/results", label,
                                           combine = TRUE) {
  stopifnot(is.character(in_dir), is.scalar(in_dir),
            is.logical(combine), is.scalar(combine))
  if (!dir.exists(in_dir))
    stop("input directory '", in_dir, "' could not be found")
  files <- list.files(in_dir, pattern = "\\.rds",
                      ignore.case = TRUE, full.names = TRUE)
  n <- list(rds = length(files),
            results = 0L,
            test = 0L)
  res <- list()
  for (i in seq_along(files)) {
    file <- files[i]
    x <- readRDS(file)
    if (is(x, "results")) {
      n$results <- n$results + 1L
      x <- as.list(x)
      if (!is.null(x[[label]])) {
        if (sum(names(x) == label) > 1L)
          stop("found a results file ('", file, "')",
               " with multiple outputs with label '", label,
               "', not sure what to do here. You might have to process ",
               "this directory manually with readRDS().")
        n$test <- n$test + 1L
        df <- attr(x[[label]]$ability, "metadata")$results
        if (!is.data.frame(df)) stop("malformed ", label, " results for file ", file)
        p_id <- x$session$p_id
        df <- cbind(p_id = p_id, test = label, file = file, df)
        res[[length(res) + 1L]] <- df
      }
    }
  }
  if (combine) res <- do.call(plyr::rbind.fill, res)
  message(sprintf(paste0("processed %i RDS files, %i of which were psychTestR results files, ",
                         "%i of which contained %s results"),
                  n$rds, n$results, n$test, label))
  res
}
