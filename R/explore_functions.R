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
#' @export
count_observations <- function(dataset, grouping_cols) {
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
#' @importFrom dplyr summarise_ %>%
#' @export
calculate_aggregated_value <- function(dataset, column, agg_fun, as_num = FALSE) {
    agg <- dataset %>%
        summarise_(paste0("avg = ", agg_fun, "(", column, ", na.rm = TRUE)"))
    if (as_num) {
        agg <- agg %>%
            as.numeric()
    }
    return(agg)
}
