context("Test explore functions")

test_that("count_observations throws correct errors", {
  expect_error(count_observations(), "dataset argument is missing")
  expect_error(count_observations(survey), "grouping_cols argument is missing")
  expect_error(
    count_observations("survey", c("Age", "Gender")),
    "dataset is not a data frame"
  )
  expect_error(
    count_observations(survey, c(1, 2, 3)),
    "grouping_cols must be a string or vector of strings"
  )
  expect_error(
    count_observations(survey, list()),
    "grouping_cols must be a string or vector of strings"
  )
  expect_error(
    count_observations(survey, c("Country")),
    "all elements of grouping_cols must be dataset's column names"
  )
  expect_error(
    count_observations(survey, c("Age", "Country")),
    "all elements of grouping_cols must be dataset's column names"
  )
  expect_error(
    count_observations(survey, c("Age", "Weight")),
    "all grouping_cols must be of type factor or character"
  )
  expect_error(
    count_observations(survey, "BMI"),
    "all grouping_cols must be of type factor or character"
  )
  expect_error(
    count_observations(survey, "SeqID"),
    "SeqID must not be in grouping_cols"
  )
  expect_error(
    count_observations(survey, c("Gender", "SeqID")),
    "SeqID must not be in grouping_cols"
  )
})

test_that("count_observation works fine", {
  example1 <- count_observations(survey, "Gender")
  answer1 <- tibble::tibble(
    Gender = as.factor(c("Female", "Male")),
    Count = as.integer(c(4782, 5824))
  )
  expect_true(is.data.frame(example1))
  expect_equal(colnames(example1), c("Gender", "Count"))
  expect_equal(example1, answer1)

  example2 <- count_observations(survey, c("ExerciseType", "Gender"))
  answer2 <- data.frame(
    ExerciseType = rep(levels(survey$ExerciseType), each = 2),
    Gender = as.factor(rep(c("Female", "Male"), times = 5)),
    Count = as.integer(c(357, 808, 1072, 1223, 936, 1102, 840, 1212, 1577, 1479)), stringsAsFactors = FALSE
  )
  answer2$ExerciseType <- factor(answer2$ExerciseType, ordered = TRUE, levels = levels(survey$ExerciseType))
  expect_true(is.data.frame(example2))
  expect_equal(colnames(example2), c("ExerciseType", "Gender", "Count"))
  expect_equal(example2, answer2)
})
