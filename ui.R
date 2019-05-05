

library(googleVis)
library(leaflet)
library(shiny)
library(shinydashboard)
library(maps)
library(jpeg)
library(plotly)


shinyUI(dashboardPage(
#header + skin  
    skin = 'purple', 
    dashboardHeader(title = 'winedApp'),
    
#sidebar panel 
    dashboardSidebar(
    sidebarMenu(
    menuItem("Introduction", tabName ='intro', icon = icon('list-alt')),
    menuItem("Wine Explorer", tabName = 'winerec', icon = icon('wine-glass-alt')),
    menuItem("Global Insights", tabName = 'maps', icon = icon('globe-americas')),
    menuItem("Graphs and Charts", tabName = 'graphs', icon = icon('signal'))
    )
  ), 

# body 
    
   dashboardBody(
     tabItems(
       tabItem(tabName = 'intro',  #intro 
               fluidRow(
                   box(
                       h3("wineApp Interactive Wine Recommendations and Analysis"),
                       tags$p("Don't wind up with the wrong wine at the wrong time: unwind with the world's best!"), 
                       tags$p("This app provides insights into prices, ratings, descriptions, and geographic distribution of the world's most esteemed wines. Novice or connoisseur, consumer or seller, this app will meet your oenophile needs."), 
                       tags$p("In the Wine Explorer, you can enter your location, varietal, aroma, taste, and price range preferences, and retrieve information on compatible vintages."),
                       tags$p("The Global Insights feature offers map visualizations of international wine trends."), 
                       tags$p ("Graphs and Charts provides additional lenses into relationships amongst countries of origin, varietals, prices per bottle, and ratings."),
                       tags$p("The data was sourced from 36,000 wine reviews on the"),  
                       tags$a(href = "https://www.winemag.com/", "WineEnthusiast site."),
                       tags$p(), 
                       tags$p("Further information was extracted from the"),
                       tags$a(href = 'https://en.wikipedia.org/wiki/List_of_grape_varieties', "Wikipedia list of grape varieties."),
                       tags$p(), 
                       tags$p("Enjoy!"),
                       width = 48)),  
                  fluidRow(
                       box(
                          HTML('<center><img src="wine_glasses.jpg" width="400"></center>'), 
                          width = 48))
               
                  ), 
       
      
       tabItem(tabName ='winerec', #wine explorer 
       tabsetPanel(type = 'tabs',
       tabPanel('Input Fields', 
       radioButtons(
           "color",
           label = ("Pick Your Poison:"),
           choices = list("red" = "red", "white" = "white"),
           selected = "white"),
        
       selectizeInput(
           "country",
           'Enter or select a country or countries:', 
           choices = sort(unique(wines_df$country)), 
           multiple = T, 
           selected = 'US'
       ),
       
        selectizeInput(
           "variety", 
           'Enter or select a wine variety or varieties (e.g., merlot, chardonnay...)
            [Note: some very common varieties not found in dataset.]', 
           choices = sort(unique(wines_df$variety)), 
           multiple = T, 
           selected = 'Chardonnay'
           ),
       
       selectizeInput( 
         "keyword1", 
         "Enter or select a descriptive term for preferred aroma or taste (e.g., full-bodied, smooth, dry, sweet, fruity...)",
         choices = sort(common_words), 
         multiple = F,
         selected = sample(common_words, 1),
       ),
       
       selectizeInput( 
         "keyword2", 
         "Enter or select a second descriptive term for preferred aroma or taste (e.g., full-bodied, smooth, dry, sweet, fruity...)",
         choices = sort(common_words), 
         multiple = F,
         selected = sample(common_words, 1),
       ),
      
       sliderInput(
           "price", 
           "Select a price range (in USD):", 
           min = min(wines_df$price),
           max = 200, #due to outliers
           value = c(50,150))), 
       
       tabPanel("Wine List", 
       fluidRow(box(width = 16, height = "80%", dataTableOutput("wine_rec_table")))),
       
       tabPanel("Wines (Purely) by Description", 
                fluidRow(box(width = 16, height = "80%", dataTableOutput("wine_descr_table")))),
       
       tabPanel("Price vs. Point Relationships for Selected Wines", 
                fluidRow(box(width = 16, height = "80%", htmlOutput("interactive_price_point_plot"))))
             )
           )
         )
        )
       )
      ) 
    
    



       
    
                       
                   
  
        
    
    

    
    
    
    
    
    

    
    





