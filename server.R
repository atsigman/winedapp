

library(shiny)
library(shinydashboard)
library(jpeg)
library(DT)
library(dplyr)
library(plotly)

red_wine_color <- "#800020"
white_wine_color <- "#EADB9F" 



shinyServer(function(input, output, session){
  
  wine_rec_reactive <- reactive({
  wines_df %>% filter(color == input$color) %>% filter (country %in% input$country) %>% filter(variety %in% input$variety) %>% 
    filter(input$price[1] < price & price < input$price[2])
  }) 
    
  wine_descr_reactive <- reactive({ 
    wines_df %>% filter(grepl(input$keyword1, description) & grepl(input$keyword2, description))
    })
  
  
  
  #wine explorer outputs 
  output$wine_rec_table <- DT::renderDataTable(
  DT::datatable( 
    data = wine_rec_reactive() %>% 
      select(title, winery, variety, country, price, points) %>% arrange(desc(points)), 
    options = list(scrollX = T)
  ) 
)
  output$wine_descr_table <- DT:: renderDataTable(
      DT::datatable( 
      data = wine_descr_reactive() %>% 
        select(title, winery, variety, country, price, points) %>% arrange(desc(points)), 
      options = list(scrollX = T)
    ))  

  output$interactive_price_point_plot <- renderGvis(
    wine_rec_reactive() %>% gvisBubbleChart(xvar = 'price', yvar = 'points', colorvar = 'country', idvar = 'winery', options = list(title = "Price (in USD) vs. Points: Is There Any Correlation?", backgroundColor="#D3D3D3",
                            hAxis="{title:'Price (in USD)', titleTextStyle:{color:'red'}}", vAxis = "{title: 'Points', titleTextStyle: {color: 'white'}}", legend="bottom", height = 400, width = 800))                                                                                                                               
  )                                                                         
    #observe functions for each input 
  
  observe({ 
    
variety_observe = sort(unique(wines_df[wines_df$color == input$color & wines_df$country == input$country, 'variety']))

updateSelectizeInput(
    session, "variety", 
    choices = variety_observe
  )

  }) 
  
}) 

 





