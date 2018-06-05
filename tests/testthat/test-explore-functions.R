context("Test explore functions")

test_that("count_observations throws correct errors", {
    expect_error(count_observations(), "dataset argument is missing")
    expect_error(count_observations(survey), "grouping_cols argument is missing")
    expect_error(count_observations("survey", c("Age", "Gender")),
                 "dataset is not a data frame")
    expect_error(count_observations(survey, c(1, 2, 3)),
                 "grouping_cols must be a string or vector of strings")
    expect_error(count_observations(survey, list()),
                 "grouping_cols must be a string or vector of strings")
    expect_error(count_observations(survey, c("Country")),
                 "all elements of grouping_cols must be dataset's column names")
    expect_error(count_observations(survey, c("Age", "Country")),
                 "all elements of grouping_cols must be dataset's column names")
    expect_error(count_observations(survey, c("Age", "NoTeen")),
                 "all grouping_cols must be of type factor or character")
    expect_error(count_observations(survey, "BMI"),
                 "all grouping_cols must be of type factor or character")
    expect_error(count_observations(survey, "SeqID"),
                 "SeqID must not be in grouping_cols")
    expect_error(count_observations(survey, c("Gender", "SeqID")),
                 "SeqID must not be in grouping_cols")
})

test_that("count_observation works fine", {
    example1 <- count_observations(survey, "Gender")
    answer1 <- data.frame(Gender = c("Female", "Male"),
                         n = as.integer(c(4786, 5825)), stringsAsFactors = FALSE)
    expect_true(is.data.frame(example1))
    expect_equal(colnames(example1), c("Gender", "n"))
    expect_equal(example1, answer1)

    example2 <- count_observations(survey, c("ExerciseType", "Gender"))
    answer2 <- data.frame(ExerciseType = rep(levels(survey$ExerciseType), each = 2),
                         Gender = rep(c("Female", "Male"), times = 5),
                         n = as.integer(c(358, 808, 1073, 1223, 937, 1103, 840, 1212, 1578, 1479)), stringsAsFactors = FALSE)
    answer2$ExerciseType <- factor(answer2$ExerciseType, ordered = TRUE, levels = levels(survey$ExerciseType))
    expect_true(is.data.frame(example2))
    expect_equal(colnames(example2), c("ExerciseType", "Gender", "n"))
    expect_equal(example2, answer2)
})

test_that("calculate_aggregated_value throws correct errors", {
    expect_error(calculate_aggregated_value(), "dataset argument is missing")
    expect_error(calculate_aggregated_value(survey), "column argument is missing")
    expect_error(calculate_aggregated_value(survey, "BMI"), "agg_fun argument is missing")
    expect_error(calculate_aggregated_value("survey", "NoTeen", "max"),
                 "dataset is not a data frame")
    expect_error(calculate_aggregated_value(survey, list(), "min"),
                 "column argument must be a string")
    expect_error(calculate_aggregated_value(survey, "City", "min"),
                 "column argument must be in dataset's column names")
    expect_error(calculate_aggregated_value(survey, "Gender", "min"),
                 "column must be of type numeric")
    expect_error(calculate_aggregated_value(survey, "NoChild", 1),
                 "agg_fun must be a string")
    expect_error(calculate_aggregated_value(survey, "NoChild", "subtract"),
                 "agg_fun must be one of min, max, mean or median")
    expect_error(calculate_aggregated_value(survey, "NoChild", "max", 123),
                 "as_num must be a logical value")
})

test_that("calculate_aggregated_value works fine", {
    example1 <- calculate_aggregated_value(survey, "NoChild", "max")
    expect_true(is.data.frame(example1))
    expect_equal(example1, data.frame(output = 3))

    example2 <- calculate_aggregated_value(survey, "MinutesPerDay", "mean")
    expect_true(is.data.frame(example2))
    expect_equal(example2, data.frame(output = 93.25))

    example3 <- calculate_aggregated_value(survey, "BMI", "min", TRUE)
    expect_true(is.numeric(example3))
    expect_equal(example3, 13.4)

    example4 <- calculate_aggregated_value(survey, "Pulse", "median", TRUE)
    expect_true(is.numeric(example4))
    expect_equal(example4, 72)
})
