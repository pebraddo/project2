library(bslib)
library(ggplot2)
library(shiny)
library(shinydashboard)

# Define UI for application
ui <- dashboardPage(
  dashboardHeader(title='Melbourne Housing Application'),
  dashboardSidebar(
    sidebarMenu(
      menuItem('About', tabName = 'about_tab', icon=icon('archive')),
      menuItem('Data Download', tabName='download_dat', icon=icon('laptop')),
      menuItem('Data Exploration', tabName = 'explore_tab', icon=icon('laptop'))
      ), 
    conditionalPanel(selectInput('cat_var', 
                                 label='Choose categorical variables', 
                                 c('Suburb', 'Method', 'Type', 'SellerG', 'Postcode', 'Regionname', 'CouncilArea'),
                                 selected = c('Suburb', 'Method'), 
                                 multiple=TRUE),
                     selectInput('num_var1',
                                 label='Choose a numeric variable',
                                 choices= c('Price', 'Propertycount'),
                                 selected = 'Price'),
                     sliderInput('range1',
                                 label= 'Range of interest for Price or Propertycount:', 
                                 min=0,max=1000000,value=c(2000, 10000)),
                     selectInput('num_var2',
                                 label='Choose another numeric variable',
                                 choices= c('Rooms','Propertycount'),
                                 selected = 'Rooms'),
                     sliderInput('range2',
                                 label='Range of Interest for Rooms or Distance:',
                                 min = 1,max=10,value=c(1,4)),
                     actionButton('plot_it',label='Plot it!')),
    card(plotOutput(outputId = 'scatterplot'))),
  dashboardBody(
    tabItems(
      tabItem(tabName = 'about_tab',
              titlePanel('About the Data')
              ),
      tabItem(tabName = 'download_dat',
              titlePanel('Data Download')
              ),
      tabItem(tabName = 'explore_tab',
              titlePanel('Data Exploration'))
    )
  )
)




server <- function(input, output) {
  plot_reactive <-eventReactive(input$plot_it, 
                                {melbourne_house_data})
  output$scatterplot <- renderPlot({
    scatterplot <- plot_reactive()
    
    ggplot(scatterplot, aes_string(x=input$cat_var[1], 
                                   y=input$num_var1)) +
      geom_point() +
      ylim(input$range1)
    })
    
}

# Run the application 
shinyApp(ui = ui, server = server)
