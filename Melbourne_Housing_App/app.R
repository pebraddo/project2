library(bslib)
library(ggplot2)
library(shiny)
library(shinydashboard)
library(tidyverse)
library(DT)
tib <- read_csv('../MELBOURNE_HOUSE_PRICES_LESS.csv') |>
  mutate(Full_Date = as.Date(Date, format = "%m/%d/%Y"))

# Define UI for application
ui <- dashboardPage(
  dashboardHeader(title='Melbourne Housing Application'),
  dashboardSidebar(
    sidebarMenu(
      menuItem('About', tabName = 'about_tab', icon=icon('archive')),
      menuItem('Data Download', tabName='download_dat', icon=icon('download')),
      menuItem('Data Exploration', tabName = 'explore_tab', icon=icon('chart-bar'))
      )),


  dashboardBody(
    tabItems(
      tabItem(tabName = 'about_tab',
              titlePanel('About the Data')
              ),
      tabItem(tabName = 'download_dat',
              titlePanel('Data Download'),
              DTOutput('data_table'),
              downloadButton('download_filtered_data', 'Download Filtered Data Here')
              ),
      tabItem(tabName = 'explore_tab',
              titlePanel('Data Exploration'),
              card(layout_sidebar(sidebar=sidebar(
                selectInput('suburb',
                            label='Choose Suburb(s)',
                            choices = unique(tib$Suburb), 
                            multiple=TRUE),
                selectInput('type',
                            label='Choose Type(s) of Homes',
                            choices = unique(tib$Type), 
                            multiple=TRUE),
                selectInput('num_var1',
                            label='Choose a numeric variable',
                            choices= c('Price', 'Propertycount')),
                uiOutput('range1'),

                selectInput('num_var2',
                            label='Choose another numeric variable',
                            choices= c('Rooms','Distance')),
                uiOutput('range2'),

                actionButton('plot_it',label='Plot it!'),
                selectInput('cat_or_num', 
                            label='Choose whether you want to display the Categorical Data Summaries or the Numeric Variable summaries', 
                            choices=c(
                              'Categorical',
                              'Numerical'),
                            multiple=FALSE)
                            ))),
                
                
                
              mainPanel(plotOutput(outputId = 'scatterplot'),
                        tableOutput('var_type')))
    )
  ))





server <- function(input, output) {
  output$range1 <- renderUI({
    req(input$num_var1)
    var <- tib[[input$num_var1]]
    sliderInput('range1', paste('Range of', input$num_var1),
                min=min(var, na.rm = TRUE),
                max=max(var, na.rm = TRUE),
                value=c(min(var, na.rm = TRUE), max(var, na.rm = TRUE)))
  })
  output$range2 <- renderUI({
    req(input$num_var2)
    var <- tib[[input$num_var2]]
    sliderInput('range2', paste('Range of', input$num_var2),
                min=min(var, na.rm = TRUE),
                max=max(var, na.rm = TRUE),
                value=c(min(var, na.rm = TRUE), max(var, na.rm = TRUE)))
  })

  filtered_data <-eventReactive(input$plot_it, {
    data <- tib
    if (length(input$suburb) >0 ){
      data <- data |> filter(Suburb %in% input$suburb)
      
    }
    if (length(input$type) >0 ){
      data <-data |> filter(Type %in% input$type)
      
    }
    data <- data |>
      filter(between(.data[[input$num_var1]], input$range1[1], input$range1[2]),
             between(.data[[input$num_var2]], input$range2[1], input$range2[2]))
    data
  })
  var_type <- eventReactive(output$cat_or_num, {
    melbourne_house_data |>
      group_by(input$num_var1) |>
      summarize(count = n())
  })
  output$scatterplot <- renderPlot({
    req(filtered_data())
    ggplot(filtered_data(), aes_string(x=input$num_var2, 
                                   y=input$num_var1, color = 'Type')) +
      geom_point(alpha=0.6) +
      theme_dark() +
      labs(title = 'Your Filtered Housing Data')
    })
  output$data_table <- renderDT({
    filtered_data()
  })
  output$download_filtered_data <-downloadHandler(
    filename=function() {paste0('filtered_housing_data_', Sys.Date(), '.csv')},
    content=function(file) {
      write_csv(filtered_data(),file)
    }
  )
    
}

# Run the application 
shinyApp(ui = ui, server = server)
