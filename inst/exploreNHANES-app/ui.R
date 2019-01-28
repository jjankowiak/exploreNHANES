library(shiny)
library(exploreNHANES)

shinyUI(fluidPage(navbarPage(title = "Explore NHANES data",
    tabPanel("Demographics",
        sidebarLayout(
            sidebarPanel(
                tags$div("The ", tags$b("National Health and Nutrition Examination Survey (NHANES)"), "is a recurring assessment of national health metrics in the United States of America. The survey combines interviews with physical examinations and laboratory testing of ~10,000 Americans. The application allows to explore and visualize the results of the survey from 2013-2014."),
                br(),
                selectInput("count_of", "View counts of",
                            choices = c("Age", "Education", "Income")),
                radioButtons("group_by", "Grouped by",
                            choices = c("Gender", "Ethnicity")),
                checkboxInput("display_data", "Display data table?",
                             value = FALSE)
            ),
            mainPanel(
                plotOutput("barplot"),
                DT::dataTableOutput("count_table")
            )
        )
    ),
    tabPanel("Physical activity",
        fluidRow(
            sidebarLayout(
                sidebarPanel(
                    selectInput("activity_type", "Type of physical activity",
                                choices = c("Vigorous Work", "Moderate Work",
                                            "Walking/Biking", "Vigorous Recreation",
                                            "Moderate Recreation")),
                    radioButtons("group_by2", "Grouped_by",
                                choices = c("Education", "Ethnicity", "Age",
                                            "Income", "Gender"))
                ),
                mainPanel(
                    plotOutput("density")
                )
            )
        ),
        fluidRow(
            sidebarLayout(
                sidebarPanel(
                    selectInput("activity_type2", "Type of physical activity",
                                choices = c("Vigorous Work", "Moderate Work",
                                            "Walking/Biking", "Vigorous Recreation",
                                            "Moderate Recreation")),
                    radioButtons("health_outcome", "Health outcome",
                                 choices = c("Weight", "BMI", "LDLcholesterol",
                                             "Triglyceride", "Pulse")),
                    checkboxInput("fit_cor", "Fit correlation line", TRUE)
                ),
                mainPanel(
                    plotOutput("activity_effect")
                )
            )
        )
    ),
    tabPanel("Raw data",
        fluidRow(
            column(4,
                   selectInput("filter_exercise_type", "Exercise type",
                               choices = unique(survey$ExerciseType),
                               selected = unique(survey$ExerciseType),
                               multiple = TRUE),
                   sliderInput("filter_minutes_per_day", "Minutes per day",
                               min = min(survey$MinutesPerDay),
                               max = max(survey$MinutesPerDay),
                               value = c(min(survey$MinutesPerDay),
                                         max(survey$MinutesPerDay))),
                   checkboxGroupInput("filter_gender", "Gender",
                                      choices = unique(survey$Gender),
                                      selected = unique(survey$Gender),
                                      inline = TRUE)
                   ),
            column(4,
                   selectInput("filter_age", "Age",
                               choices = levels(survey$Age),
                               selected = levels(survey$Age), multiple = TRUE),
                   selectInput("filter_ethnicity", "Ethnicity",
                               choices = unique(survey$Ethnicity),
                               selected = unique(survey$Ethnicity), multiple = TRUE),
                   selectInput("filter_education", "Education",
                               choices = levels(survey$Education),
                               selected = levels(survey$Education), multiple = TRUE)
            ),
            column(4,
                   selectInput("filter_income", "Income",
                               choices = levels(survey$Income),
                               selected = levels(survey$Income), multiple = TRUE),
                   actionButton("filter_button", "Filter data"),
                   downloadButton("download_data", "Download data")
            )
        ),
        fluidRow(
            DT::dataTableOutput("raw_data")
        )
    )
 )
))
