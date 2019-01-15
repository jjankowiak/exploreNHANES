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
