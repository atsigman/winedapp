

library(shiny)
library(shinydashboard)
library(jpeg)
library(DT)
library(dplyr)
library(plotly)


red_wine_color <- "#800020"
white_wine_color <- "#EADB9F" 

shinyServer(function(input, output, session){
  
  #wine explorer reactives: 
  
  wine_descr_reactive <- reactive({ 
    wines_df %>% filter(color == input$color) %>% filter(input$price[1] < price & price < input$price[2]) %>% filter(grepl(input$keyword1, description) | grepl(input$keyword2, description))
  })
  
  wine_rec_reactive <- reactive({
  wines_df %>% filter(color == input$color) %>% filter (country %in% input$country) %>% filter(variety %in% input$variety) %>% 
    filter(input$price[1] < price & price < input$price[2])
  }) 

  #wine explorer outputs 
  
  output$wine_descr_table <- DT::renderDataTable(
    DT::datatable( 
      data = wine_descr_reactive() %>% 
        select(title, winery, variety, country, price, points, description) %>% arrange(desc(points)), 
      options = list(scrollX = T)
    ) 
  )

  output$wine_rec_table <- DT::renderDataTable(
  DT::datatable( 
    data = wine_rec_reactive() %>% 
      select(title, winery, variety, country, price, points, description) %>% arrange(desc(points)), 
    options = list(scrollX = T)
  ) 
)
  
#interactive bubble chart for wine explorer
  
output$interactive_price_point_plot <- renderGvis(
wine_rec_reactive() %>% gvisBubbleChart(xvar = 'price', yvar = 'points', colorvar = 'country', idvar = 'winery', options = list(title = "Price (in USD) vs. Points: Is There Any Correlation?", backgroundColor="#D3D3D3",
                            hAxis="{title:'Price (in USD)', titleTextStyle:{color:'red'}}", vAxis = "{title: 'Points', titleTextStyle: {color: 'white'}}", legend="bottom", height = 400, width = 1200))                                                                                                                               
  ) 
  
  #info boxes for Graphs menu item panels 
    
   #by country: 
 output$maxWorldPrice <- renderInfoBox({ 
   max_price <- wines_df_country_graph %>% filter(country == input$graph_country) %>% summarise(max_price = max(price)) %>% select(max_price)
   infoBox('Max Price', max_price, icon = icon('hand-o-up'), color = 'green') 
   }) 
 
 output$meanWorldPrice <- renderInfoBox({ 
   mean_price <- wines_df_country_graph %>% filter(country == input$graph_country) %>% summarise(mean_price = round(mean(price))) %>% select(mean_price)
   infoBox('Mean Price', mean_price, icon = icon('arrows-alt-h'), color = 'blue') 
 }) 
 
 output$minWorldPrice <- renderInfoBox({ 
   min_price <- wines_df_country_graph %>% filter(country == input$graph_country) %>% summarise(min_price = min(price)) %>% select(min_price)
   infoBox('Min Price', min_price, icon = icon('hand-o-down'), color = 'red') 
 }) 

 output$maxWorldPoints <- renderInfoBox({ 
   max_points <- wines_df_country_graph %>% filter(country == input$graph_country) %>% summarise(max_points = max(points)) %>% select(max_points)
   infoBox('Max Points', max_points, icon = icon('hand-o-up'), color = 'green') 
 }) 
 
 output$meanWorldPoints <- renderInfoBox({ 
   mean_points <- wines_df_country_graph %>% filter(country == input$graph_country) %>% summarise(mean_points = round(mean(points))) %>% select(mean_points)
   infoBox('Mean Points', mean_points, icon = icon('arrows-alt-h'), color = 'blue') 
 }) 
 
 output$minWorldPoints <- renderInfoBox({ 
   min_points <- wines_df_country_graph %>% filter(country == input$graph_country) %>% summarise(min_points = min(points)) %>% select(min_points)
   infoBox('Min Points', min_points, icon = icon('hand-o-down'), color = 'red') 
 }) 
 
 output$worldPricePointsCorr <- renderInfoBox({ 
   price_points_corr <- wines_df_country_graph %>% filter(country == input$graph_country) %>% summarise(corr = round(cor(price, points, method = 'pearson'), digits = 2)) %>% select(corr)
   infoBox('Correlation', price_points_corr, icon = icon('handshake'), color = 'teal') 
 }) 
 
 output$worldRedPerc <- renderInfoBox({ 
   red_perc <-  wines_df_country_graph %>% filter(country == input$graph_country) %>% summarise(red_perc = round((sum(color == 'red')/n()) * 100)) %>% select(red_perc) 
   infoBox('% Red Wine', red_perc, icon = icon('wine_glass'), color = 'red')
 })
 
 output$worldWhitePerc <- renderInfoBox({ 
   white_perc <- wines_df_country_graph %>% filter(country == input$graph_country) %>% summarise(white_perc = round((sum(color == 'white')/n()) * 100)) %>% select(white_perc) 
   infoBox('% White Wine', white_perc, icon = icon('wine_glass'), color = 'yellow')
 })
 
 #by US State: 
 output$maxUSPrice <- renderInfoBox({ 
   max_price <- wines_df_country_graph %>% filter(country == 'US', province == input$graph_state) %>% summarise(max_price = max(price)) %>% select(max_price)
   infoBox('Max Price', max_price, icon = icon('hand-o-up'), color = 'green') 
 }) 
 
 output$meanUSPrice <- renderInfoBox({ 
   mean_price <- wines_df_country_graph %>% filter(country == 'US', province == input$graph_state) %>% summarise(mean_price = round(mean(price))) %>% select(mean_price)
   infoBox('Mean Price', mean_price, icon = icon('arrows-alt-h'), color = 'blue') 
 }) 
 
 output$minUSPrice <- renderInfoBox({ 
   min_price <- wines_df_country_graph %>% filter(country == 'US', province == input$graph_state) %>% summarise(min_price = min(price)) %>% select(min_price)
   infoBox('Min Price', min_price, icon = icon('hand-o-down'), color = 'red') 
 }) 
 
 output$maxUSPoints <- renderInfoBox({ 
   max_points <- wines_df_country_graph %>% filter(country == 'US', province == input$graph_state) %>% summarise(max_points = max(points)) %>% select(max_points)
   infoBox('Max Points', max_points, icon = icon('hand-o-up'), color = 'green') 
 }) 
 
 output$meanUSPoints <- renderInfoBox({ 
   mean_points <- wines_df_country_graph %>% filter(country == 'US', province == input$graph_state) %>% summarise(mean_points = round(mean(points))) %>% select(mean_points)
   infoBox('Mean Points', mean_points, icon = icon('arrows-alt-h'), color = 'blue') 
 }) 
 
 output$minUSPoints <- renderInfoBox({ 
   min_points <- wines_df_country_graph %>% filter(country == 'US', province == input$graph_state) %>% summarise(min_points = min(points)) %>% select(min_points)
   infoBox('Min Points', min_points, icon = icon('hand-o-down'), color = 'red') 
 }) 
 
 output$USPricePointsCorr <- renderInfoBox({ 
   price_points_corr <- wines_df_country_graph %>% filter(country == 'US', province == input$graph_state) %>% summarise(corr = round(cor(price, points, method = 'pearson'), digits = 2)) %>% select(corr)
   infoBox('Correlation', price_points_corr, icon = icon('handshake'), color = 'teal') 
 }) 
 
 output$USRedPerc <- renderInfoBox({ 
   red_perc <-  wines_df_country_graph %>% filter(country == 'US', province == input$graph_state) %>% summarise(red_perc = round((sum(color == 'red')/n()) * 100)) %>% select(red_perc) 
   infoBox('% Red Wine', red_perc, icon = icon('wine_glass'), color = 'red')
 })
 
 output$USWhitePerc <- renderInfoBox({ 
   white_perc <- wines_df_country_graph %>% filter(country == 'US', province == input$graph_state) %>% summarise(white_perc = round((sum(color == 'white')/n()) * 100)) %>% select(white_perc) 
   infoBox('% White Wine', white_perc, icon = icon('wine_glass'), color = 'yellow')
 })
 
 #by variety 
 
 output$maxVarPrice <- renderInfoBox({ 
   max_price <- wines_df_country_graph %>% filter(variety == input$graph_variety) %>% summarise(max_price = max(price)) %>% select(max_price)
   infoBox('Max Price', max_price, icon = icon('hand-o-up'), color = 'green') 
 }) 
 
 output$meanVarPrice <- renderInfoBox({ 
   mean_price <- wines_df_country_graph %>% filter(variety == input$graph_variety) %>% summarise(mean_price = round(mean(price))) %>% select(mean_price)
   infoBox('Mean Price', mean_price, icon = icon('arrows-alt-h'), color = 'blue') 
 }) 
 
 output$minVarPrice <- renderInfoBox({ 
   min_price <- wines_df_country_graph %>% filter(variety == input$graph_variety) %>% summarise(min_price = min(price)) %>% select(min_price)
   infoBox('Min Price', min_price, icon = icon('hand-o-down'), color = 'red') 
 }) 
 
 output$maxVarPoints <- renderInfoBox({ 
   max_points <- wines_df_country_graph %>% filter(variety == input$graph_variety) %>% summarise(max_points = max(points)) %>% select(max_points)
   infoBox('Max Points', max_points, icon = icon('hand-o-up'), color = 'green') 
 }) 
 
 output$meanVarPoints <- renderInfoBox({ 
   mean_points <- wines_df_country_graph %>% filter(variety == input$graph_variety) %>% summarise(mean_points = round(mean(points))) %>% select(mean_points)
   infoBox('Mean Points', mean_points, icon = icon('arrows-alt-h'), color = 'blue') 
 }) 
 
 output$minVarPoints <- renderInfoBox({ 
   min_points <- wines_df_country_graph %>% filter(variety == input$graph_variety) %>% summarise(min_points = min(points)) %>% select(min_points)
   infoBox('Min Points', min_points, icon = icon('hand-o-down'), color = 'red') 
 }) 
 
 output$VarPricePointsCorr <- renderInfoBox({ 
   price_points_corr <- wines_df_country_graph %>% filter(variety == input$graph_variety) %>% summarise(corr = round(cor(price, points, method = 'pearson'), digits = 2)) %>% select(corr)
   infoBox('Correlation', price_points_corr, icon = icon('handshake'), color = 'teal') 
 }) 
 
 
 
 #for the entire dataset: 
 
 output$maxDSPrice <- renderInfoBox({ 
   max_price <- wines_df_world_stats %>%  select(max_price)
   infoBox('Max Price', max_price, icon = icon('hand-o-up'), color = 'green') 
 }) 
 
 output$meanDSPrice <- renderInfoBox({ 
   mean_price <- wines_df_world_stats %>%  select(mean_price)
   infoBox('Mean Price', mean_price, icon = icon('arrows-alt-h'), color = 'blue') 
 }) 
 
 output$minDSPrice <- renderInfoBox({ 
   min_price <- wines_df_world_stats %>% select(min_price)
   infoBox('Min Price', min_price, icon = icon('hand-o-down'), color = 'red') 
 }) 
 
 output$maxDSPoints <- renderInfoBox({ 
   max_points <- wines_df_world_stats %>%  select(max_points)
   infoBox('Max Points', max_points, icon = icon('hand-o-up'), color = 'green') 
 }) 
 
 output$meanDSPoints <- renderInfoBox({ 
   mean_points <- wines_df_world_stats %>% select(mean_points)
   infoBox('Mean Points', mean_points, icon = icon('arrows-alt-h'), color = 'blue') 
 }) 
 
 output$minDSPoints <- renderInfoBox({ 
   min_points <- wines_df_world_stats %>%  select(min_points)
   infoBox('Min Points', min_points, icon = icon('hand-o-down'), color = 'red') 
 }) 
 
 output$DSPricePointsCorr <- renderInfoBox({ 
   price_points_corr <- wines_df_world_stats %>%  select(corr)
   infoBox('Correlation', price_points_corr, icon = icon('handshake'), color = 'teal') 
 }) 
 
 output$DSRedPerc <- renderInfoBox({ 
   red_perc <-wines_df_world_stats %>%  select(red_perc) 
   infoBox('% Red Wine', red_perc, icon = icon('wine_glass'), color = 'red')
   })
 
 output$DSWhitePerc <- renderInfoBox({ 
   white_perc <-wines_df_world_stats %>%  select(white_perc) 
   infoBox('% White Wine', white_perc, icon = icon('wine_glass'), color = 'yellow')
 })
 
 
 #price-point plots by country, US state, variety 
 
 output$scatterByCountry <- renderPlotly({
  scatter_graph_country <- wines_df_country_graph %>% filter(country == input$graph_country) %>% arrange(desc(points)) %>% head(100) %>% ggplot(aes(x = price, y = points)) + geom_point(aes(color = color, shape = winery), alpha = 0.4)+ scale_color_manual(values = c("red" = "#800020", "white" = "#EADB9F")) + xlab('Price per Bottle (in USD)')+ ylab('Points') + ggtitle('Price per Bottle vs. Points for Top 100 Wines') 
 })

 output$scatterByState <- renderPlotly({
  scatter_graph_state <- wines_df_country_graph %>% filter(country == 'US', province == input$graph_state) %>% arrange(desc(points)) %>% head(100) %>% ggplot(aes(x = price, y = points)) + geom_point(aes(color = color, shape = winery), alpha = 0.4)+ scale_color_manual(values = c("red" = "#800020", "white" = "#EADB9F")) + xlab('Price per Bottle (in USD)')+ ylab('Points') + ggtitle('Price per Bottle vs. Points for Top 100 Wines') 
})

 output$scatterByVariety <- renderPlotly({
  scatter_graph_Variety <- wines_df_country_graph %>% filter(variety == input$graph_variety) %>% arrange(desc(points)) %>% head(100) %>% ggplot(aes(x = price, y = points)) + geom_point(aes(shape = country), alpha = 0.4) + xlab('Price per Bottle (in USD)')+ ylab('Points') + ggtitle('Price per Bottle vs. Points for Top 100 Wines') 
})
 

    #OBSERVE FUNCTIONS for relevant inputs 
  
observe({ 
    
variety_observe = sort(unique(wines_df[wines_df$color == input$color & wines_df$country == input$country, 'variety']))

updateSelectizeInput(
    session, "variety", 
    choices = variety_observe
  )

#observe color choice for GRAPHS menu item 
graph_variety_observe = sort(unique(wines_df[wines_df$color == input$graph_color, 'variety']))


updateSelectizeInput(
  session, "graph_variety", 
  choices = graph_variety_observe
) 

#MAPS menu item 

#world
if (input$world_map_type == 'wpc'){
 
  output$world_map <- renderGvis({
  world_map <- gvisGeoChart(wines_df_wpc, 'country', 'cnt', options = list(width = 600, height = 400,displayMode = 'regions', colorAxis = "{colors:[ '#EADB9F', 'blue', 'purple', '#800020']}", title = 'Number of Wines per Country (Excluding the US)'))
    })
}

else if (input$world_map_type == 'nv') {
  output$world_map <- renderGvis({
    world_map <- gvisGeoChart(wines_df_nv, 'country', 'cnt', options = list(displayMode="regions", width = 600, height = 400, colorAxis = "{colors:[ '#EADB9F', 'blue', 'purple', '#800020']}"))
  })
}  

else if (input$world_map_type == 'apr') {
  output$world_map <- renderGvis({
    world_map <- gvisGeoChart(wines_df_apr, 'country', 'avg_pr', options = list(displayMode="regions", width = 600, height = 400, colorAxis = "{colors:[ '#EADB9F', 'blue', 'purple', '#800020']}"))
  })
}  

else if (input$world_map_type == 'apt') {
  output$world_map <- renderGvis({
    world_map <- gvisGeoChart(wines_df_apt, 'country', 'avg_pt', options = list(displayMode="regions", width = 600, height = 400, colorAxis = "{colors:[ '#EADB9F', 'blue', 'purple', '#800020']}"))
  })
}  

#by US state: 
if (input$us_map_type == 'wps'){
  
  output$us_map <- renderGvis({
    us_map <- gvisGeoChart(wines_df_wps, 'province', 'cnt', options = list(region = 'US', displayMode="regions", resolution="provinces", width = 600, height = 400, colorAxis = "{colors:[ '#EADB9F', 'blue', 'purple', '#800020']}"))
  })
}
  
else if (input$us_map_type == 'nvs'){
  
  output$us_map <- renderGvis({
    us_map <- gvisGeoChart(wines_df_nvs, 'province', 'cnt', options = list(region = 'US', displayMode="regions", resolution="provinces", width = 600, height = 400, colorAxis = "{colors:[ '#EADB9F', 'blue', 'purple', '#800020']}"))
  })
}

else if (input$us_map_type == 'aps'){
  
  output$us_map <- renderGvis({
    us_map <- gvisGeoChart(wines_df_aps, 'province', 'avg_pr', options = list(region = 'US', displayMode="regions", resolution="provinces", width = 600, height = 400, colorAxis = "{colors:[ '#EADB9F', 'blue', 'purple', '#800020']}"))
  })
}

else if (input$us_map_type == 'apts'){
  
  output$us_map <- renderGvis({
    us_map <- gvisGeoChart(wines_df_apts, 'province', 'avg_pt', options = list(region = 'US', displayMode="regions", resolution="provinces", width = 600, height = 400, colorAxis = "{colors:[ '#EADB9F', 'blue', 'purple', '#800020']}"))
     })
    }

  }) #observe
  
}) #shinyServer 







