library(bslib)
library(ggplot2)
library(shiny)

# Define UI for application
ui <- fluidPage(

    # Application title
    titlePanel("Melbourne Housing Application"),

    
    sidebarLayout(
        sidebarPanel(
            selectInput('var1', 
                        label='Choose categorical variables', 
                        c('Suburb', 'Method', 'Type', 'SellerG', 'Postcode', 'Regionname', 'CouncilArea'),
                        selected = c('Suburb', 'Method'), 
                        multiple=TRUE
                        ),
            selectInput('num_var',
                        label='Choose a numeric variable',
                        choices= c('Rooms', 'Price', 'Propertycount', 'Distance'),
                        selected = 'Price'
                        ),
            sliderInput(
              'range',
              label= 'Range of interest:',
              min=0,
              max=6000000,
              value=c(40000, 80000)
            ),
            actionButton('plot_it',
                         label='Plot it!')
        ),
        

        # Show a plot of the generated distribution
        card(
           plotOutput(outputId = 'scatterplot')
        )
    )
)


server <- function(input, output) {
  #re<- eventReactive(input$plot_it,
    
    output$scatterplot <- renderPlot({
      ggplot(data=melbourne_house_data, aes_string(x=input$var1[1], y=input$num_var, fill=input$var1[2])) +
        geom_point() +ylim(input$range)
    })#)
}

# Run the application 
shinyApp(ui = ui, server = server)
