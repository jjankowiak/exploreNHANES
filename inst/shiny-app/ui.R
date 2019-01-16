library(shiny)
library(exploreNHANES)

shinyUI(fluidPage(navbarPage(title = "",
    tabPanel("Demographics",
        sidebarLayout(
            sidebarPanel(
                selectInput("count_of", "View counts of",
                            choices = c("Age", "Education", "Income")),
                radioButtons("group_by", "Grouped_by",
                            choices = c("Gender", "Ethnicity"))
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
        downloadButton("download_data", "Download data"),
        br(),
        br(),
        DT::dataTableOutput("raw_data")
    )
 )
))
