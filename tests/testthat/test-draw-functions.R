context("Test draw functions")

test_that("draw_barplot throws correct errors", {
    expect_error(draw_barplot(), "dataset argument is missing")
    expect_error(draw_barplot(survey), "counted_col argument is missing")
    expect_error(draw_barplot(survey, "Age"), "grouping_col argument is missing")
    expect_error(draw_barplot("survey", "Age", "Gender"),
                 "dataset is not a data frame")
    expect_error(draw_barplot(survey, 123, "Gender"), "counted_col must be a string")
    expect_error(draw_barplot(survey, "City", "Gender"),
                 "counted_col must be dataset's column name")
    expect_error(draw_barplot(survey, "NoChild", "Gender"),
                 "counted_col must be of type factor or character")
    expect_error(draw_barplot(survey, "SeqID", "Gender"),
                 "counted_col must not be SeqID")
    expect_error(draw_barplot(survey, "Age", list()), "grouping_col must be a string")
    expect_error(draw_barplot(survey, "Age", "Country"),
                 "grouping_col must be dataset's column name")
    expect_error(draw_barplot(survey, "Age", "Pulse"),
                 "grouping_col must be of type factor or character")
    expect_error(draw_barplot(survey, "Age", "SeqID"),
                 "grouping_col must not be SeqID")
})

test_that("draw_barplot output is a correct plot", {
    example1 <- draw_barplot(survey, "Age", "Education")
    expect_true(is.ggplot(example1))
    expect_true(all(!is.na(example1$data[, "Age"])))
    expect_true(all(!is.na(example1$data[, "Education"])))
    expect_equal(length(example1$layers), 1)
    expect_equal(class(example1$layers[[1]]$geom)[1], "GeomBar")
    expect_equal(example1$labels$title, "Counts of Age grouped by Education")
    expect_equal(example1$labels$fill, "Education")
    expect_equal(example1$labels$x, "Age")
    expect_equal(example1$labels$y, "count")

    example2 <- draw_barplot(survey, "Income", "Ethnicity")
    expect_true(is.ggplot(example2))
    expect_true(all(!is.na(example2$data[, "Income"])))
    expect_true(all(!is.na(example2$data[, "Ethnicity"])))
    expect_equal(length(example2$layers), 1)
    expect_equal(class(example2$layers[[1]]$geom)[1], "GeomBar")
    expect_equal(example2$labels$title, "Counts of Income grouped by Ethnicity")
    expect_equal(example2$labels$fill, "Ethnicity")
    expect_equal(example2$labels$x, "Income")
    expect_equal(example2$labels$y, "count")
})

test_that("draw_exercise_time_density throws correct errors", {
    expect_error(draw_exercise_time_density(), "dataset argument is missing")
    expect_error(draw_exercise_time_density(survey), "exercise_type argument is missing")
    expect_error(draw_exercise_time_density(survey, "Vigorous Work"),
                 "grouping_col argument is missing")
    expect_error(draw_exercise_time_density("survey", "Vigorous Work", "Gender"),
                 "dataset is not a data frame")
    expect_error(draw_exercise_time_density(survey, 123, "Gender"),
                 "exercise_type must be a string")
    expect_error(draw_exercise_time_density(survey, "Swimming", "Gender"),
                 paste("exercise_type must be equal Vigorous Work, Moderate Work,",
                       "Walking/Biking, Vigorous Recreation or Moderate Recreation"))
    expect_error(draw_exercise_time_density(survey, "Vigorous Work", list()),
                 "grouping_col must be a string")
    expect_error(draw_exercise_time_density(survey, "Vigorous Work", "Country"),
                 "grouping_col must be dataset's column name")
    expect_error(draw_exercise_time_density(survey, "Vigorous Work", "Pulse"),
                 "grouping_col must be of type factor or character")
    expect_error(draw_exercise_time_density(survey, "Vigorous Work", "SeqID"),
                 "grouping_col must not be SeqID")
})

test_that("draw_exercise_time_density output is a correct plot", {
    example1 <- draw_exercise_time_density(survey, "Walking/Biking", "Age")
    expect_true(is.ggplot(example1))
    expect_true(all(example1$data[, "ExerciseType"] == "Walking/Biking"))
    expect_true(all(!is.na(example1$data[, "MinutesPerDay"])))
    expect_true(all(!is.na(example1$data[, "Age"])))
    expect_equal(length(example1$layers), 1)
    expect_equal(class(example1$layers[[1]]$geom)[1], "GeomDensity")
    expect_equal(example1$labels$title, "Density of Walking/Biking time grouped by Age")
    expect_equal(example1$labels$x, "MinutesPerDay")
    expect_equal(example1$labels$group, "Age")
    expect_equal(example1$labels$y, "density")

    example2 <- draw_exercise_time_density(survey, "Moderate Recreation", "Ethnicity")
    expect_true(is.ggplot(example2))
    expect_true(all(example2$data[, "ExerciseType"] == "Moderate Recreation"))
    expect_true(all(!is.na(example2$data[, "MinutesPerDay"])))
    expect_true(all(!is.na(example2$data[, "Ethnicity"])))
    expect_equal(length(example2$layers), 1)
    expect_equal(class(example2$layers[[1]]$geom)[1], "GeomDensity")
    expect_equal(example2$labels$title, "Density of Moderate Recreation time grouped by Ethnicity")
    expect_equal(example2$labels$x, "MinutesPerDay")
    expect_equal(example2$labels$group, "Ethnicity")
    expect_equal(example2$labels$y, "density")
})

test_that("draw_activity_effect throws correct errors", {
    expect_error(draw_activity_effect(), "dataset argument is missing")
    expect_error(draw_activity_effect(survey), "exercise_type argument is missing")
    expect_error(draw_activity_effect(survey, "Vigorous Work"),
                 "effect_on_col argument is missing")
    expect_error(draw_activity_effect("survey", "Vigorous Work", "BMI"),
                 "dataset is not a data frame")
    expect_error(draw_activity_effect(survey, 123, "BMI"),
                 "exercise_type must be a string")
    expect_error(draw_activity_effect(survey, "Swimming", "BMI"),
                 paste("exercise_type must be equal Vigorous Work, Moderate Work,",
                       "Walking/Biking, Vigorous Recreation or Moderate Recreation"))
    expect_error(draw_activity_effect(survey, "Vigorous Work", list()),
                 "effect_on_col must be a string")
    expect_error(draw_activity_effect(survey, "Vigorous Work", "Country"),
                 "effect_on_col must be dataset's column name")
    expect_error(draw_activity_effect(survey, "Vigorous Work", "Age"),
                 "effect_on_col must be of type numeric")
    expect_error(draw_activity_effect(survey, "Vigorous Work", "Pulse", 123),
                 "corr must be a logical value")
})

test_that("draw_activity_effect output is a correct plot", {
    example1 <- draw_activity_effect(survey, "Walking/Biking", "Pulse")
    expect_true(is.ggplot(example1))
    expect_true(all(example1$data[, "ExerciseType"] == "Walking/Biking"))
    expect_true(all(!is.na(example1$data[, "MinutesPerDay"])))
    expect_true(all(!is.na(example1$data[, "Pulse"])))
    expect_equal(length(example1$layers), 2)
    expect_equal(class(example1$layers[[1]]$geom)[1], "GeomPoint")
    expect_equal(class(example1$layers[[2]]$geom)[1], "GeomSmooth")
    expect_equal(example1$labels$title, "Effect of Walking/Biking on Pulse")
    expect_equal(example1$labels$x, "MinutesPerDay")
    expect_equal(example1$labels$y, "Pulse")

    example2 <- draw_activity_effect(survey, "Vigorous Work", "Weight", FALSE)
    expect_true(is.ggplot(example2))
    expect_true(all(example2$data[, "ExerciseType"] == "Vigorous Work"))
    expect_true(all(!is.na(example2$data[, "MinutesPerDay"])))
    expect_true(all(!is.na(example2$data[, "Weight"])))
    expect_equal(length(example2$layers), 1)
    expect_equal(class(example2$layers[[1]]$geom)[1], "GeomPoint")
    expect_equal(example2$labels$title, "Effect of Vigorous Work on Weight")
    expect_equal(example2$labels$x, "MinutesPerDay")
    expect_equal(example2$labels$y, "Weight")
})

# TODO: change pallette (too few colors)
# TODO: what if filtered data will be empty?
