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
#'
#' @importFrom dplyr group_by_ summarise n %>%
#' @import assertthat
#' @export
count_observations <- function(dataset, grouping_cols) {
    assert_that(!missing(dataset), msg = "dataset argument is missing")
    assert_that(!missing(grouping_cols), msg = "grouping_cols argument is missing")
    assert_that(is.data.frame(dataset), msg = "dataset is not a data frame")
    assert_that(is.character(grouping_cols),
                msg = "grouping_cols must be a string or vector of strings")
    assert_that(all(grouping_cols %in% colnames(dataset)),
                msg = "all elements of grouping_cols must be dataset's column names")
    assert_that(all(sapply(grouping_cols, function(x) !is.numeric(dataset[, x]))),
                msg = "all grouping_cols must be of type factor or character")
    assert_that(all(grouping_cols != "SeqID"), msg = "SeqID must not be in grouping_cols")

    dataset_grouped <- dataset %>%
        group_by_(.dots = grouping_cols) %>%
        summarise(n = n())
    return(dataset_grouped)
}


#' Calculate one of the aggregating statistic.
#'
#' For selected \code{column} calculate \code{agg_fun}.
#' @param dataset survey dataset
#' @param column column name used for calculating statistic
#' @param agg_fun function used to calculate statistic - one of \code{mean}, \code{min}, \code{max} or \code{median}
#' @param as_num if \code{TRUE} returns value as numeric otherwise returns data.frame object (default \code{FALSE})
#' @return value of calculated statistic
#'
#' @examples
#' calculate_aggregated_value(survey, "NoChild", "mean")
#' calculate_aggregated_value(survey, "NoTeen", "min", TRUE)
#' calculate_aggregated_value(survey, "BMI", "max", TRUE)
#
#' @importFrom dplyr filter_ mutate_ summarise_ %>%
#' @export
calculate_aggregated_value <- function(dataset, column, agg_fun, as_num = FALSE) {
    assert_that(!missing(dataset), msg = "dataset argument is missing")
    assert_that(!missing(column), msg = "column argument is missing")
    assert_that(!missing(agg_fun), msg = "agg_fun argument is missing")
    assert_that(is.data.frame(dataset), msg = "dataset is not a data frame")
    assert_that(is.character(column), msg = "column argument must be a string")
    assert_that(column %in% colnames(dataset),
                msg = "column argument must be in dataset's column names")
    assert_that(is.numeric(dataset[, column]), msg = "column must be of type numeric")
    assert_that(all(is.character(agg_fun), length(agg_fun) == 1),
                msg = "agg_fun must be a string")
    assert_that(agg_fun %in% c("min", "max", "mean", "median"),
                msg = "agg_fun must be one of min, max, mean or median")
    assert_that(is.logical(as_num) & length(as_num) == 1,
                msg = "as_num must be a logical value")

    agg <- dataset %>%
        filter_(paste0("!is.na(", column, ")")) %>%
        summarise_(.dots = list(output = paste0("round(", agg_fun, "(", column, "), 2)")))
    if (as_num) {
        agg <- agg %>%
            as.numeric()
    }
    return(agg)
}
