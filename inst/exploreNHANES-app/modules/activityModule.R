activityUI <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Physical activity",
    fluidRow(
      sidebarLayout(
        sidebarPanel(
          selectInput(ns("activity_type"), "Type of physical activity",
            choices = c(
              "Vigorous Work", "Moderate Work",
              "Walking/Biking", "Vigorous Recreation",
              "Moderate Recreation"
            )
          ),
          radioButtons(ns("group_by"), "Grouped_by",
            choices = c(
              "Education", "Ethnicity", "Age",
              "Income", "Gender"
            )
          )
        ),
        mainPanel(
          plotOutput(ns("density"))
        )
      )
    ),
    fluidRow(
      sidebarLayout(
        sidebarPanel(
          selectInput(ns("activity_type2"), "Type of physical activity",
            choices = c(
              "Vigorous Work", "Moderate Work",
              "Walking/Biking", "Vigorous Recreation",
              "Moderate Recreation"
            )
          ),
          radioButtons(ns("health_outcome"), "Health outcome",
            choices = c(
              "Weight", "BMI", "LDLcholesterol",
              "Triglyceride", "Pulse"
            )
          ),
          checkboxInput(ns("fit_cor"), "Fit correlation line", TRUE)
        ),
        mainPanel(
          plotOutput(ns("activity_effect"))
        )
      )
    )
  )
}

activity <- function(input, output, session) {
  output$density <- renderPlot({
    draw_exercise_time_density(survey, input$activity_type, input$group_by)
  })

  output$activity_effect <- renderPlot({
    draw_activity_effect(
      survey, input$activity_type2, input$health_outcome,
      input$fit_cor
    )
  })
}
