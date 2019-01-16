library(shiny)

shinyServer(function(input, output) {
    output$barplot <- renderPlot({
        draw_barplot(survey, input$count_of, input$group_by)
    })

    output$count_table <- DT::renderDataTable(DT::datatable({
        count_observations(survey, c(input$count_of, input$group_by))
    }, filter = "top"))

    output$density <- renderPlot({
        draw_exercise_time_density(survey, input$activity_type, input$group_by2)
    })

    output$activity_effect <- renderPlot({
        draw_activity_effect(survey, input$activity_type2, input$health_outcome,
                             input$fit_cor)
    })

    output$download_data <- downloadHandler(
        filename = function() {
            "survey_data.csv"
        },
        content = function(file) {
            write.csv(survey, file, row.names = FALSE)
        }
    )

    output$raw_data <- DT::renderDataTable(DT::datatable({
        survey
    }))
})
