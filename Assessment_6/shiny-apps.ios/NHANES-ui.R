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