demographicsUI <- function(id) {
  ns <- NS(id)
  tabPanel(
    "Demographics",
    sidebarLayout(
      sidebarPanel(
        tags$div("The ", tags$b("National Health and Nutrition Examination Survey (NHANES)"), "is a recurring assessment of national health metrics in the United States of America. The survey combines interviews with physical examinations and laboratory testing of ~10,000 Americans. The application allows to explore and visualize the results of the survey from 2013-2014."),
        br(),
        selectInput(ns("count_of"), "View counts of",
          choices = c("Age", "Education", "Income")
        ),
        radioButtons(ns("group_by"), "Grouped by",
          choices = c("Gender", "Ethnicity")
        ),
        checkboxInput(ns("display_data"), "Display data table?",
          value = FALSE
        )
      ),
      mainPanel(
        plotOutput(ns("barplot")),
        DT::dataTableOutput(ns("count_table"))
      )
    )
  )
}

demographics <- function(input, output, session) {
  output$barplot <- renderPlot({
    draw_barplot(survey, input$count_of, input$group_by)
  })

  output$count_table <- DT::renderDataTable(
    if (input$display_data) {
      DT::datatable({
        count_observations(survey, c(input$count_of, input$group_by))
      }, filter = "top")
    }
  )
}
