#load necessary libraries
library(bslib)
library(ggplot2)
library(shiny)
library(shinyWidgets)
library(bs4Dash)
library(shinydashboard)
library(tidyverse)
library(DT)
library(gapminder)
library(gganimate)
library(gifski)
#read in data from .csv in project 2 repo
tib <- read_csv('../MELBOURNE_HOUSE_PRICES_LESS.csv') |>
  #get date data in the correct format
  mutate(Full_Date = as.Date(Date, format = "%m/%d/%Y"))

# Define UI for application
ui <- dashboardPage(
  dashboardHeader(title='Melbourne Housing Application'),
  dashboardSidebar(
    sidebarMenu(
      #define tabs
      menuItem('About', tabName = 'about_tab', icon=icon('archive')),
      menuItem('Data Download', tabName='download_dat', icon=icon('download')),
      menuItem('Data Exploration', tabName = 'explore_tab', icon=icon('chart-bar'))
      )),

#elements within the tabs
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
                
                
                
              mainPanel(card(card_header('Scatterplot of Subsetted Data'),
                             plotOutput(outputId = 'scatterplot'),
                        card(card_header('Summaries'),
                             div(style = 'background-color:white; padding:10px; border-radius:8px;',
                                 tableOutput('one_way'),
                                 tableOutput('two_way'))),
                        card(card_header('Measures of Association between two numeric variables selected using pairwise complete observations'),
                             div(style='background-color:white; padding:10px; border-radius:8px;',
                                 tableOutput('measures_of_association'))),
                        card(card_header('Graphical Summaries for Investigating the Data'),
                            plotOutput(outputId = 'scat_dist_price'),
                            plotOutput(outputId = 'hist_price_type'),
                            plotOutput(outputId = 'bar_method_region'), #explain the methods in the about tab
                            plotOutput(outputId = 'box_prop_type'),
                            plotOutput(outputId = 'facet_box_prop_type'),
                            imageOutput('anim_facet')
                            ),
                        tabBox(id='plot_tabs',
                               tabPanel(title='Scatterplot',
                                        plotOutput(outputId = 'scat_dist_price')))
                        ))))))





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
#filter the data to react elements to it
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
    data <- data |> 
      filter(Distance>0) |>
      mutate(Full_Date = as.Date(Full_Date),
             Method= factor(Method))|>
      filter(Method %in% c('PI', 'S', 'SA', 'SP', 'VB'))
    
  })

  output$one_way <- renderTable({
    data <- filtered_data()
    req(nrow(data)>0)
    if(input$cat_or_num =='Numerical') {
      data |>
        group_by(Regionname, Type) |>
        drop_na(Regionname, Type) |>
        summarise(
          mean_price = mean(Price, na.rm = TRUE), 
          med_price = median(Price, na.rm = TRUE),
          mean_rooms = mean(Rooms, na.rm = TRUE), 
          med_rooms = median(Rooms, na.rm = TRUE))
          } else {
      data |>
        group_by(Regionname) |>
        summarise(count=n())
    }})
  output$two_way <- renderTable({
    data <- filtered_data()
    req(nrow(data)>0)
    if(input$cat_or_num =='Numerical') {
      data |>
        group_by(Region = as.factor(Regionname)) |>
        summarize(mean_price = mean(Price, na.rm = TRUE), 
                  med_price = median(Price, na.rm = TRUE),
                  sd_price = sd(Price, na.rm = TRUE))
    } else {
      data |>
        group_by(Regionname, Type) |>
        summarise(Count=n())|>
        pivot_wider(names_from = Type, values_from = Count)
    }})
  
  output$scatterplot <- renderPlot({
    req(filtered_data())
    ggplot(filtered_data(), aes_string(x=input$num_var2, 
                                   y=input$num_var1, color = 'Type')) +
      geom_point(alpha=0.6) +
      theme_minimal() +
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
  output$measures_of_association <- renderTable({
    data <- filtered_data()
    req(nrow(data)>0)
    tibble(correlation = cor(data[input$num_var1], data[input$num_var2], use = 'pairwise.complete.obs'))
  }
  )
  
  output$scat_dist_price <- renderPlot({
    req(filtered_data())
    ggplot(filtered_data(), aes(x = Price, y = Distance)) + 
      geom_point(alpha=0.6) +
      ggtitle('Scatter Plot of Distance from Central Business District versus Price')+
      theme_minimal()
  })
  
  output$hist_price_type <- renderPlot({
    req(filtered_data())
    ggplot(filtered_data(), aes(x=Price)) +
      geom_density(alpha=0.5, aes(fill=Type)) +
      theme_minimal() +
      ggtitle('Histogram of Frequency of Price for Each House Type')
  })
  
  output$bar_method_region <- renderPlot({
    req(filtered_data())
    ggplot(filtered_data() |> drop_na(Regionname, Method), aes(x = Regionname, fill = Method))+
    geom_bar() +
    ggtitle('Bar Plot of Count of Houses Sold by Region over Method of Sale') +
    theme_minimal() + coord_flip()
  })
  
  output$box_prop_type <- renderPlot({
    req(filtered_data())
    ggplot(filtered_data()|> drop_na(Type, Propertycount), aes(x = Type, y=Propertycount, fill=Type)) +
    geom_boxplot() +
    ggtitle('Boxplots of Property Count versus Type') +
    theme_minimal()
  })
  
  output$facet_box_prop_type <-renderPlot({
    req(filtered_data())
    ggplot(filtered_data()|> drop_na(Type, Propertycount, Method, Regionname), aes(x = Type, y=Propertycount,fill=Method))+
      geom_boxplot() +
      ggtitle('Facet of Boxplots of Property Count versus Type across Sale Method and Regions') +
      facet_wrap(~ Regionname) +
      theme_minimal()
  })
  output$anim_facet <- renderImage({

    req(filtered_data())
    p <- ggplot(filtered_data(), aes(Distance, Price, size = Propertycount, colour = Regionname)) +
      geom_point(alpha = 0.7, show.legend = TRUE) +
      scale_colour_discrete() +
      scale_size(range = c(2, 12)) +
      scale_x_log10() +
      facet_wrap(~Method) +
    # Here comes the gganimate specific bits
      labs(title = 'Year: {closest_state}, Price versus Distancce from CBD over Time by Method of Sale', x = 'Distance from Central Business District', y = 'Price') +
      transition_states(Full_Date, transition_length = 3, state_length = 3) +
      enter_fade()+exit_fade()
    
    temp_gif_file <- tempfile(fileext = '.gif')
    anim_save(temp_gif_file, animation = animate(p,fps=10, width=800, height=600, renderer=gifski_renderer()))
    
    list(src=temp_gif_file,
         contentType='image/gif',
         width='100%',
         height='auto',
         alt='Animated Facet Plot')
  
  }, deleteFile=TRUE)
}

# Run the application 
shinyApp(ui = ui, server = server)
