#' Draw barplot
#'
#' Description
#' @param dataset survey dataset
#' @param counted_col a
#' @param grouping_col a
#' @return ggplot
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

#' Draw density
#'
#' Description
#' @param dataset survey dataset
#' @param exercise_type a
#' @param grouping_col a
#' @return ggplot
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


#' Draw activity effect
#'
#' Description
#' @param dataset survey dataset
#' @param exercise_type a
#' @param effect_on_col a
#' @param corr a
#' @return ggplot
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
