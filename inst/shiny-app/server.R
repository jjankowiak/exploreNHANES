library(shiny)

shinyServer(function(input, output) {
    output$barplot <- renderPlot({
        draw_barplot(survey, input$count_of, input$group_by)
    })

    output$count_table <- DT::renderDataTable(DT::datatable({
        count_observations(survey, c(input$count_of, input$group_by))
    }, filter = "top"))
})
