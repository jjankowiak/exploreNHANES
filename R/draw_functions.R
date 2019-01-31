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
#' @importFrom dplyr filter_ %>%
#' @importFrom assertthat assert_that
#' @import ggplot2
#' @export
draw_barplot <- function(dataset, counted_col, grouping_col) {
  assert_that(!missing(dataset), msg = "dataset argument is missing")
  assert_that(!missing(counted_col), msg = "counted_col argument is missing")
  assert_that(!missing(grouping_col), msg = "grouping_col argument is missing")
  assert_that(is.data.frame(dataset), msg = "dataset is not a data frame")
  assert_that(is.character(counted_col), msg = "counted_col must be a string")
  assert_that(counted_col %in% colnames(dataset),
    msg = "counted_col must be dataset's column name"
  )
  assert_that(!is.numeric(dataset[, counted_col]),
    msg = "counted_col must be of type factor or character"
  )
  assert_that(counted_col != "SeqID", msg = "counted_col must not be SeqID")
  assert_that(is.character(grouping_col), msg = "grouping_col must be a string")
  assert_that(grouping_col %in% colnames(dataset),
    msg = "grouping_col must be dataset's column name"
  )
  assert_that(!is.numeric(dataset[, grouping_col]),
    msg = "grouping_col must be of type factor or character"
  )
  assert_that(grouping_col != "SeqID", msg = "grouping_col must not be SeqID")
  assert_that(grouping_col != counted_col,
    msg = "grouping_col must differ from counted_col"
  )

  dataset_plot <- dataset %>%
    filter_(paste0("!is.na(", counted_col, ") & !is.na(", grouping_col, ")"))

  assert_that(nrow(dataset_plot) > 0, msg = "dataset used to plot is empty")

  plt <- ggplot(dataset_plot, aes_string(x = counted_col, fill = grouping_col)) +
    geom_bar() +
    theme_minimal() +
    scale_y_continuous(name = "# of Participants") +
    scale_x_discrete(name = counted_col) +
    scale_fill_brewer() +
    theme(
      axis.text.x = element_text(angle = 45, hjust = 1, size = 12),
      plot.title = element_text(hjust = 0.5)
    ) +
    ggtitle(paste0("Counts of ", counted_col, " grouped by ", grouping_col))
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
#' @importFrom dplyr filter_ %>%
#' @importFrom assertthat assert_that
#' @import ggplot2
#' @export
draw_exercise_time_density <- function(dataset, exercise_type, grouping_col) {
  assert_that(!missing(dataset), msg = "dataset argument is missing")
  assert_that(!missing(exercise_type), msg = "exercise_type argument is missing")
  assert_that(!missing(grouping_col), msg = "grouping_col argument is missing")
  assert_that(is.data.frame(dataset), msg = "dataset is not a data frame")
  assert_that(is.character(exercise_type), msg = "exercise_type must be a string")
  assert_that(exercise_type %in% c(
    "Vigorous Work", "Moderate Work", "Walking/Biking",
    "Vigorous Recreation", "Moderate Recreation"
  ),
  msg = paste(
    "exercise_type must be equal Vigorous Work, Moderate Work,",
    "Walking/Biking, Vigorous Recreation or Moderate Recreation"
  )
  )
  assert_that(is.character(grouping_col), msg = "grouping_col must be a string")
  assert_that(grouping_col %in% colnames(dataset),
    msg = "grouping_col must be dataset's column name"
  )
  assert_that(!is.numeric(dataset[, grouping_col]),
    msg = "grouping_col must be of type factor or character"
  )
  assert_that(grouping_col != "SeqID", msg = "grouping_col must not be SeqID")

  dataset_plot <- dataset %>%
    filter_(paste0("ExerciseType == '", exercise_type, "'")) %>%
    filter_(paste0("!is.na(MinutesPerDay) & !is.na(", grouping_col, ")"))

  assert_that(nrow(dataset_plot) > 0, msg = "dataset used to plot is empty")

  plt <- ggplot(dataset_plot, aes_(~MinutesPerDay)) +
    geom_density(aes_string(group = grouping_col, colour = grouping_col), size = 1) +
    theme_minimal() +
    scale_y_continuous(name = "Density") +
    scale_x_continuous(name = "Avg Minutes/Day") +
    ggtitle(paste0("Density of ", exercise_type, " time grouped by ", grouping_col)) +
    theme(plot.title = element_text(hjust = 0.5))
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
#' @importFrom dplyr filter_ %>%
#' @importFrom assertthat assert_that
#' @import ggplot2
#' @export
draw_activity_effect <- function(dataset, exercise_type, effect_on_col, corr = TRUE) {
  assert_that(!missing(dataset), msg = "dataset argument is missing")
  assert_that(!missing(exercise_type), msg = "exercise_type argument is missing")
  assert_that(!missing(effect_on_col), msg = "effect_on_col argument is missing")
  assert_that(is.data.frame(dataset), msg = "dataset is not a data frame")
  assert_that(is.character(exercise_type), msg = "exercise_type must be a string")
  assert_that(exercise_type %in% c(
    "Vigorous Work", "Moderate Work", "Walking/Biking",
    "Vigorous Recreation", "Moderate Recreation"
  ),
  msg = paste(
    "exercise_type must be equal Vigorous Work, Moderate Work,",
    "Walking/Biking, Vigorous Recreation or Moderate Recreation"
  )
  )
  assert_that(is.character(effect_on_col), msg = "effect_on_col must be a string")
  assert_that(effect_on_col %in% colnames(dataset),
    msg = "effect_on_col must be dataset's column name"
  )
  assert_that(is.numeric(dataset[, effect_on_col]),
    msg = "effect_on_col must be of type numeric"
  )
  assert_that(is.logical(corr) & length(corr) == 1,
    msg = "corr must be a logical value"
  )

  dataset_plot <- dataset %>%
    filter_(paste0("ExerciseType == '", exercise_type, "'")) %>%
    filter_(paste0("!is.na(MinutesPerDay) & !is.na(", effect_on_col, ")"))

  assert_that(nrow(dataset_plot) > 0, msg = "dataset used to plot is empty")

  plt <- ggplot(dataset_plot, aes_(x = ~MinutesPerDay)) +
    geom_point(aes_string(y = effect_on_col), colour = "dodgerblue4", alpha = 0.7) +
    theme_minimal() +
    scale_y_continuous(name = effect_on_col) +
    scale_x_continuous(name = "Avg Minutes/Day") +
    ggtitle(paste0("Effect of ", exercise_type, " on ", effect_on_col)) +
    theme(plot.title = element_text(hjust = 0.5))
  if (corr) {
    plt <- plt + geom_smooth(aes_string(y = effect_on_col),
      method = "lm",
      colour = "darkred", fill = "grey85", alpha = 0.7
    )
  }
  return(plt)
}
