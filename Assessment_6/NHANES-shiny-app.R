# load essential packages
pacman::p_load(shiny,
               tidyverse,
               here,
               rio,
               DT,
               plotly,
               viridis
               )

# import data set
NHANES <- import(here("week_6", "cleaned_NHANES.csv"))

# Create average systolic blood pressure variable
NHANES <- NHANES %>%
  mutate(average_systolic =
           rowMeans(across(
             c(systolic_bp_1, systolic_bp_2, systolic_bp_3, systolic_bp_4)
           ), 
           na.rm = TRUE)
  )

# Create a new categorical variable of average systolic pressure  class
NHANES <- NHANES %>%
  mutate(Systolic_BP_category = 
           case_when(
             average_systolic < 120 ~ "Normal BP",
             average_systolic >= 120 & average_systolic < 129 ~ "Elevated BP",
             average_systolic >= 130 & average_systolic <= 139 ~ "Stage 1 hypertension",
             TRUE                            ~ "Stage 2 hypertension"
           ))

# Step 2: create new data set of filtered data 
NHANES <- NHANES %>%
  filter(!is.na(age),
         !is.na(gender),
         !is.na(average_systolic),
         !is.na(education),
         !is.na(marital_status)
  )

# round off average systolic pressure to 1 decimal place
NHANES$average_systolic <-round(NHANES$average_systolic, 1)

  
# Define constants for UI components
min_age <- floor(min(NHANES$age))
max_age <- ceiling(max(NHANES$age))
gender_choice <- levels(factor(NHANES$gender))
default_columns <- c("patient_id", "wave_id", "age", "gender", "education",
                     "ethnicity_2", "family_income", "marital_status",
                     "average_systolic", "Systolic_BP_category")
blood_pressure_choice <- levels(factor(NHANES$Systolic_BP_category))

                  

# UI
ui <- navbarPage(
  title = "NHANES Shiny App",
  
  # Tab 1: Data viewer
  tabPanel(
    title = "Dater viewer",
    fluidPage(
      sidebarLayout(
        sidebarPanel(
          h4("Customize Table"),
          checkboxGroupInput("columns", "Select Columns to Display:",
                             choices = colnames(NHANES),
                             selected = default_columns
          ),
          # wave selection by checkbox
          checkboxGroupInput(
            inputId = "wave_id",
            label = "Select wave (s):",
            choices = sort(unique(NHANES$wave_id)),
            selected = unique((NHANES$wave_id))
          ),
          
          # age slider
          sliderInput(
            inputId = "age_range",
            label = "Select age range:",
                      min = min_age, max = max_age, value = c(min_age, max_age)
          ),
          
          # gender selection
          selectInput(
            inputId = "gender",
            label = "Select gender:",
            choices = gender_choice, 
            selected = gender_choice[2],
            multiple = TRUE
          )
        ),
        
        mainPanel(
          h3("Filtered Data Table"),
          DTOutput("dataTable"),
          br(),
          p("Summary of your current selection:"),
          tags$ul(
            tags$li(textOutput("selected_age_range")),
            tags$li(textOutput("selected_gender")),
            tags$li("Only the selected column will be included in the download file")
          ),
          downloadButton("downloadData", "Download Filtered Data")
        )
      )
    )
  ),

# Tab 2: Interactive Scatter Plot
tabPanel(
  title = "Scatter Plot",
  fluidPage(
    sidebarLayout(
      sidebarPanel(
        h4("Filter Options"),
        sliderInput("age_range", "Age range selection:",
                    min = min_age, max = max_age, value = c(min_age, max_age)
        ),
        br(),
        selectInput("gender", "Gender selection:",
                    choices = gender_choice, selected = gender_choice[2],
                    multiple = TRUE
        ),
        textInput("color", "Enter point color:", value = "#ED1B2F"),
        br(),
        h5("Instructions:"),
        p("Use the filters above to refine the data displayed in the scatter plot.
            Enter a valid color name or hex code to customize the plot points.")
      ),
      mainPanel(
        plotlyOutput("scatterPlot"),
        br(),
        h5("Summary of Filtered Data:"),
        tableOutput("summaryStatistics")
        )
      )
    )
  )
)
  
 
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
  
  # Run the application
  shinyApp(ui = ui, server = server)
    
   
  
