library(shiny)
library(dplyr)

shinyServer(function(input, output) {
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

    output$density <- renderPlot({
        draw_exercise_time_density(survey, input$activity_type, input$group_by2)
    })

    output$activity_effect <- renderPlot({
        draw_activity_effect(survey, input$activity_type2, input$health_outcome,
                             input$fit_cor)
    })

    survey_filtered <- eventReactive(input$filter_button, {
        filter_education <- ifelse(input$filter_education == "NA",
                                   NA, input$filter_education)
        filter_income <- ifelse(input$filter_income == "NA",
                                   NA, input$filter_income)
        survey %>% dplyr::filter(
            ExerciseType %in% input$filter_exercise_type,
            MinutesPerDay >= input$filter_minutes_per_day[1],
            MinutesPerDay <= input$filter_minutes_per_day[2],
            Gender %in% input$filter_gender,
            Age %in% input$filter_age,
            Ethnicity %in% input$filter_ethnicity,
            Education %in% filter_education,
            Income %in% filter_income
        )
    })

    output$download_data <- downloadHandler(
        filename = "survey_data.csv",
        content = function(file) {
            write.csv(survey_filtered(), file, row.names = FALSE)
        },
        contentType = "text/csv"
    )

    output$raw_data <- DT::renderDataTable(DT::datatable({
        if (input$filter_button == 0) {
            survey
        } else {
            survey_filtered()
        }
    }))
})
