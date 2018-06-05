#' Draw barplot for factor variables.
#'
#' For selected \code{counted_col} draw barplot to count observations in groups specified by \code{grouping_col}.
#' @param dataset survey dataset
#' @param counted_col column name on X asix
#' @param grouping_col column name used for grouping
#' @return barplot with counts for \code{counted_col}
#'
#' @examples
#' draw_barplot(survey, "Age", "Ethnicity")
#' draw_barplot(survey, "Income", "Gender")
#
#' @importFrom dplyr filter_ %>%
#' @import ggplot2
#' @export
draw_barplot <- function(dataset, counted_col, grouping_col) {
    dataset_plot <- dataset %>%
        filter_(paste0("!is.na(", counted_col, ") & !is.na(", grouping_col, ")"))
    plt <- ggplot(dataset_plot, aes_string(x = counted_col, fill = grouping_col)) +
        geom_bar() +
        theme_minimal() +
        scale_y_continuous(name = '# of Participants') +
        scale_x_discrete(name = counted_col) +
        scale_fill_brewer(palette = "OrRd") +
        theme(axis.text.x = element_text(angle = 45, hjust = 1))
    return(plt)
}

#' Draw density of exercise time.
#'
#' Draw density of \code{exercise_type} time in groups specified by \code{grouping_col}.
#' @param dataset survey dataset
#' @param exercise_type one of the factor value in \code{ExerciseType} column
#' @param grouping_col column name used for grouping
#' @return density plot for exercise time of specific \code{exercise_type} grouped by \code{grouping_col}.
#'
#' @examples
#' draw_exercise_time_density(survey, "Vigorous Work", "Gender")
#' draw_exercise_time_density(survey, "Walking/Biking", "Age")
#
#' @importFrom dplyr filter_ %>%
#' @import ggplot2
#' @export
draw_exercise_time_density <- function(dataset, exercise_type, grouping_col) {
    dataset_plot <- dataset %>%
        filter_(paste0("ExerciseType == '", exercise_type, "'")) %>%
        filter_(paste0("!is.na(MinutesPerDay) & !is.na(", grouping_col, ")"))
    plt <- ggplot(dataset_plot, aes_(~MinutesPerDay)) +
        geom_density(aes_string(group = grouping_col, colour = grouping_col)) +
        theme_minimal() +
        scale_y_continuous(name = "Density") +
        scale_x_continuous(name = "Avg Minutes/Day") +
        scale_color_brewer(palette = "OrRd")
    return(plt)
}


#' Draw activity effect on continuous variable.
#'
#' Draw scatter plot of exercise time of specific \code{exercise_type} and \code{effect_on_col} (with regression line if \code{corr} is TRUE)
#' @param dataset survey dataset
#' @param exercise_type one of the factor value in \code{ExerciseType} column
#' @param effect_on_col column name used for measuring excercise effect
#' @param corr if \code{TRUE} regression line will be fitted to the plot
#' @return scatter plot of exercise time of specific \code{exercise_type} and \code{effect_on_col} (with regression line if \code{corr} is TRUE)
#'
#' @examples
#' draw_activity_effect(survey, "Vigorous Work", "Weight", FALSE)
#' draw_activity_effect(survey, "Moderate Work", "BMI")
#
#' @importFrom dplyr filter_ %>%
#' @import ggplot2
#' @export
draw_activity_effect <- function(dataset, exercise_type, effect_on_col, corr = TRUE) {
    dataset_plot <- dataset %>%
        filter_(paste0("ExerciseType == '", exercise_type, "'")) %>%
        filter_(paste0("!is.na(", effect_on_col, ")"))
    plt <- ggplot(dataset_plot, aes_(x = ~MinutesPerDay)) +
        geom_point(aes_string(y = effect_on_col), colour = 'dodgerblue4', alpha = 0.7) +
        theme_minimal() +
        scale_y_continuous(name = effect_on_col) +
        scale_x_continuous(name = "Avg Minutes/Day")
    if (corr) {
        plt <- plt + geom_smooth(aes_string(y = effect_on_col), method = 'lm',
                                 colour = 'darkred', fill = 'grey85', alpha = 0.7)
    }
    return(plt)
}


# TODO:
# - change colors
# - add titles
# - finish documentation
