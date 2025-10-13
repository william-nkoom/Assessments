# Server
server <- function(input, output) {
  # Reactive data set based on widget input
  filtered_data <- reactive({
    NHANES %>%
      filter(NHANES$wave_id %in% input$wave_id) %>%
      filter(age >= input$age_range[1] & age <= input$age_range[2] & 
               gender %in% input$gender)  
    
  })
  
  # Render data table
  output$dataTable <- renderDT({
    datatable(
      filtered_data()[, input$columns, drop = FALSE],
      options = list(
        pageLength = 10,
        scrollX = TRUE,
        autoWidth = TRUE
      )
    )
  })
  
  output$selected_age_range <- renderText({
    paste0(
      "You have selected ages between ",
      input$age_range[1],
      " and ",
      input$age_range[2],
      "."
    )
  })
  
  # filter by gender
  output$selected_gender <- renderText({
    if (length(input$gender) == 1) {
      paste0("You have selected gender: ", input$gender, ".")
    } else if (length(input$gender) == 2) {
      paste0(
        "You have selected gender: ",
        paste(input$gender, collapse = " and "), "."
      )
    } else {
      "No gender selected."
    }
  })
  
  # Download filtered data with selected columns
  output$downloadData <- downloadHandler(
    filename = function() {
      paste("filtered_data", Sys.Date(), ".csv", sep = "")
    },
    content = function(file) {
      selected_data <- filtered_data() %>% select(all_of(input$columns))
      export(selected_data, here(file))
    }
  )
  
  # Render scatter plot with Plotly
  output$scatterPlot <- renderPlotly({
    plotly::ggplotly(
      ggplot(filtered_data(), aes(x = age, y = average_systolic)) +
        geom_point(color = input$color, size = 1) +
        labs(
          x = "Age (years)",
          y = "Average systolic bood pressure (mmHg)",
          title = "Scatter Plot of Age versus 
            Average systolic blood pressure"
        ) +
        theme_bw() +
        theme(
          plot.title = element_text(size = 12, face = "bold"),
          axis.title = element_text(size = 10),
          axis.text = element_text(size = 8)
        )
    ) %>%
      config(
        modeBarButtonsToRemove = c(
          "autoScale2d", "lasso2d", "hoverCompareCartesian", "toggleSpikelines"
        ),
        toImageButtonOptions = list(
          format = "png",  # download format
          filename = "scatterplot",  # file name
          height = 800,  # image height
          width = 1200  # image width
        ),
        displaylogo = FALSE  # hide Plotly logo
      )
  })
  
  # Render summary statistics
  output$summaryStatistics <- renderTable({
    NHANES <- filtered_data() %>%
      group_by(Systolic_BP_category) %>%
      summarise(
        `Sample Size` = n(),
        `Mean (SD)` = paste0(round(mean(average_systolic, na.rm = TRUE), 1), " (", 
                             round(sd(average_systolic, na.rm = TRUE), 1), ")"),
        Median = round(median(average_systolic, na.rm = TRUE), 1),
        `Min - Max` = paste(round(min(average_systolic, na.rm = TRUE), 1), 
                            round(max(average_systolic, na.rm = TRUE), 1), sep = " - ")
      )
    
  })
  
}