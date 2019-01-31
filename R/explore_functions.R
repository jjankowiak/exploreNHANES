#' Count number of observations in selected groups.
#'
#' Dataset is grouped by \code{grouping_cols} and for each combination of values
#' number of observations in each group is calculated.
#' @param dataset survey dataset
#' @param grouping_cols column name or vector of column names used for creating groups
#' @return data frame with counts
#'
#' @examples
#' count_observations(survey, "Gender")
#' count_observations(survey, c("Gender", "Age"))
#' @importFrom dplyr group_by_ summarise n %>%
#' @importFrom assertthat assert_that
#' @export
count_observations <- function(dataset, grouping_cols) {
  assert_that(!missing(dataset), msg = "dataset argument is missing")
  assert_that(!missing(grouping_cols), msg = "grouping_cols argument is missing")
  assert_that(is.data.frame(dataset), msg = "dataset is not a data frame")
  assert_that(is.character(grouping_cols),
    msg = "grouping_cols must be a string or vector of strings"
  )
  assert_that(all(grouping_cols %in% colnames(dataset)),
    msg = "all elements of grouping_cols must be dataset's column names"
  )
  assert_that(all(sapply(grouping_cols, function(x) !is.numeric(dataset[, x]))),
    msg = "all grouping_cols must be of type factor or character"
  )
  assert_that(all(grouping_cols != "SeqID"), msg = "SeqID must not be in grouping_cols")

  dataset_grouped <- dataset %>%
    group_by_(.dots = grouping_cols) %>%
    summarise(Count = n())
  return(dataset_grouped)
}
