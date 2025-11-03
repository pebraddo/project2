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
library(shinycssloaders)

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
  dashboardBody(fluidRow(
    tabItems(
      tabItem(tabName = 'about_tab',
              titlePanel('About the Data'),
              mainPanel(card(
                #about section blurb
                
                card_body(p(markdown("Within this application, you will discover methods of investigating the *Melbourne Housing Data*.
                             This data comes from [Kaggle](https://www.kaggle.com/datasets/anthonypino/melbourne-housing-market) and explores
                             the housing bubble of Melbourne, including attributes of
                             
                             * Suburb
                             * Address
                             * Rooms (the number of rooms)
                             * Type (housing type) 
                                  + H (House)
                                  + U (Unit)
                                  + T (Townhouse)
                             * Price
                             * Method (method of sale)
                                  + S - property sold
                                  + SP - property sold prior
                                  + PI - property passed in
                                  + VB - vendor bid
                                  + SA - sold after auction
                             * SellerG (seller's name)
                             * Date (date sold)
                             * Postcode (Zipcode)
                             * Regionname
                             
                             On the left side of this application, you can find three tabs (About, Data Download, and Data Exploration).  
                             
                             You are currently viewing the About tab.  
                             
                             Within the Data Download tab, you can download a .csv of filtered data from the Data Exploration tab.
                             
                             Within the Data Exploration tab, you will find options for subsetting the housing data by numerical variables.  Below this, you can indicate whether you want to show numeric or categorical summaries of the subsetted data. Finally, you can choose a main variable to plot across in the following graphs, which are inside named tabs!
                             "
                          ))),
                          card_image('../Melbourne_pic.jpg')))
              ),
      #data download of subsetted data
      tabItem(tabName = 'download_dat',
              titlePanel('Data Download'),
              DTOutput('data_table'),
              downloadButton('download_filtered_data', 'Download Filtered Data Here')
              ),
      #exploration of prep_app shenanigans
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
                
                
                
              mainPanel(card(
                             card_header(h2('Scatterplot of Subsetted Data')),
                             plotOutput(outputId = 'scatterplot'))),
                        card(card_header(h2(textOutput('summary_title'))),
                             div(style = 'background-color:white; padding:10px; border-radius:8px;',
                                 tableOutput('one_way'),
                                 tableOutput('two_way'))),
                        card( card_header(h2('Measures of Association between two numeric variables selected using pairwise complete observations')),
                             div(style='background-color:white; padding:10px; border-radius:8px;',
                                 tableOutput('measures_of_association'))),
                        
                        card(card_header(h2('Graphical Summaries for Investigating the Data')),
                             selectInput('var_across', 
                                         label='Select the main variable you would like to Summarize Across',
                                         choices = c('Method', 'Regionname', 'Suburb', 'Postcode', 'SellerG',
                                                     'CouncilArea', 'Type'),
                                         multiple = FALSE),
                          tabBox(id='plot_tabs',
                               
                              
                               
              
                               tabPanel(title='Scatterplot',
                                        withSpinner(plotOutput(outputId = 'scat_dist_price'))),
                               tabPanel(title='Histogram',
                                        withSpinner(plotOutput(outputId = 'hist_price_type'))),
                               tabPanel(title='Bar Plot',
                                        withSpinner(plotOutput(outputId = 'bar_method_region'))),
                               tabPanel(title='Box Plots', 
                                        withSpinner(plotOutput(outputId = 'box_prop_type')),
                                        withSpinner(plotOutput(outputId = 'facet_box_prop_type'))),
                               tabPanel(title='Animation Plot',
                                        withSpinner(
                                          imageOutput('anim_facet')))
                        ))
                        
                        )))))




#make those things happen
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
  
  output$summary_title <-renderText({
    paste(input$cat_or_num, 'Summaries')
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
                                   y=input$num_var1)) +
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
    scat_title <- paste0('Scatterplot of ', input$num_var1, ' vs. ', input$num_var2)
    ggplot(filtered_data(),
           aes_string(x = input$num_var1, y = input$num_var2, color=input$var_across)) +
      geom_point(alpha = 0.6) +
      labs(title = scat_title) +
      theme_minimal()
  })
  
  output$hist_price_type <- renderPlot({
    req(filtered_data())
    hist_title <- paste0('Histogram of ', input$num_var1)
    ggplot(filtered_data(), 
           aes_string(x=input$num_var1, fill=input$var_across)) +
      geom_density(alpha=0.5) +
      labs(title=hist_title)+
      theme_minimal()
  })
  
  output$bar_method_region <- renderPlot({
    req(filtered_data())
    bar_title <- paste0('Barplot of Count of Houses Sold by Region over ', input$var_across)
    ggplot(filtered_data() |> drop_na('Regionname', input$var_across), aes_string(
      x = 'Regionname', fill = input$var_across))+
    geom_bar() +
    ggtitle(bar_title) +
    theme_minimal() + coord_flip()
  })
  
  output$box_prop_type <- renderPlot({
    req(filtered_data())
    box_title <- paste0('Boxplot of ', input$num_var1, ' versus ', input$var_across)
    ggplot(filtered_data()|> drop_na(input$var_across, input$num_var1), 
           aes_string(x = input$var_across, y=input$num_var1, fill=input$var_across)) +
      geom_boxplot() +
      labs(title=box_title) +
      theme_minimal()
  })
  
  output$facet_box_prop_type <-renderPlot({
    req(filtered_data())
    facet_box_title <- paste0('Facet of Boxplots of ', input$num_var1, ' versus Type across Sale Method and Regions')
    ggplot(filtered_data()|> drop_na(Type, Propertycount, Method, Regionname), 
           aes_string(x = 'Type', y=input$num_var1))+
      geom_boxplot(aes(fill=Method)) +
      labs(title=facet_box_title) +
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
