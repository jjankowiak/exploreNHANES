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
    tabPanel("Physical activity", ""
    ),
    tabPanel("Raw data", ""
    )
 )
))
