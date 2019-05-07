

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
                       h3("winedApp Interactive Wine Recommendations and Analysis"),
                       tags$p("Don't wind up with the wrong wine at the wrong time: unwind with the world's best!"), 
                       tags$p("This app provides insights into prices, ratings, descriptions, and geographic distribution of the world's most esteemed wines. Novice or connoisseur, consumer or seller, this app will meet your oenophile needs."), 
                       tags$p("In the Wine Explorer, you can enter your location, varietal, aroma, taste, and price range preferences, and retrieve information on compatible vintages."),
                       tags$p("The Global Insights feature offers map visualizations of international wine trends."), 
                       tags$p ("Graphs and Charts provides additional lenses into relationships amongst countries of origin, varietals, prices per bottle, and ratings."),
                       tags$p("The data was sourced from ca. 36,000 wine reviews on the"), 
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
       navlistPanel(
      'Input and Tables',  
       tabPanel('Input Fields', 
       radioButtons(
           "color",
           label = ("Pick Your Poison:"),
           choices = list("red" = "red", "white" = "white"),
           selected = "red"),
       
        selectizeInput(
           "country",
           'Select a country:', 
           choices = sort(unique(wines_df$country)), 
           selected = 'France',
           multiple = F
          ),
       
        selectizeInput(
           "variety", 
           'Select a wine variety:', 
           choices = sort(unique(wines_df$variety)), 
           multiple = F
        ), 
       
       selectizeInput( 
         "keyword1", 
         "Select a descriptive term for preferred aroma or taste:",
         choices = sort(unique(wines_df['keywords' != '', 'keywords'])),
         multiple = F,
         selected = sample(common_words, 1)
       ),
       
       selectizeInput( 
         "keyword2", 
         "Select a second descriptive term:",
         choices = sort(unique(wines_df['keywords' != '', 'keywords'])), 
         multiple = F,
         selected = sample(common_words, 1)
       ),
       
       sliderInput(
           "price", 
           "Select a price range (in USD):", 
           min = min(wines_df$price),
           max = 300, #due to outliers
           value = c(min,120))
       
          ), #input tabPanel 
       
       tabPanel("Wine List by Color, Description, and Price", 
                fluidRow(box(width = 16, height = "80%", DT::dataTableOutput("wine_descr_table")))),
                
       tabPanel("Wine List by Country, Color, Variety, and Price", 
       fluidRow(box(width = 16, height = "80%", DT::dataTableOutput("wine_rec_table")))
       
    ),
       'Graphs', 
    
    tabPanel('Price vs. Points Graph: Wines by Description', 
    fluidRow(box(width = 16, height = "80%", plotlyOutput("interactive_price_point_descr_plot")))
    ),
    tabPanel('Price vs. Points Graph: Wines by Country and Variety', 
             fluidRow(box(width = 16, height = "80%", plotlyOutput("interactive_price_point_rec_plot")))
         )
       ) #navlistpanel 
      ), #tabitem 
    
      
  
      tabItem(tabName ='maps',
          tabsetPanel(type = 'tabs',
            tabPanel('World Maps',     
            radioButtons(
              "world_map_type",
              label = ("Select map content:"),
              choices = list("Number of wines per country (excluding the US, Italy, and France)" = "wpc","Number of varieties per country" = "nv", "Average price per country" = 'apr', "Average points per country" = "apt"), 
              selected = "wpc"),     
              fluidRow(box(width = 16, height = "80%", htmlOutput("world_map")))),
  
              tabPanel('US Maps',     
                     radioButtons(
                       "us_map_type",
                       label = ("Select map content:"),
                       choices = list("Number of wines per US State" = "wps","Number of varieties per state" = "nvs", "Average price per state" = 'aps', "Average points per state" = "apts"), 
                       selected = "wps"),     
                     fluidRow(box(width = 16, height = "80%", htmlOutput("us_map"))))
             ) #tabsetpanel
           ), #tabitem
  
    tabItem(tabName ='graphs',
            navlistPanel('Graphs and Charts', 
                         'Interactive: Geographic', 
                      tabPanel('By Country',  
                      
                               selectizeInput(
                                 "graph_country",
                                 'Select a country:', 
                                 choices = sort(unique(wines_df_country_graph$country)), 
                                 multiple = F, 
                                 selected = 'Argentina'
                               ),
                               
                               fluidRow(
                                 infoBoxOutput("maxWorldPrice"),
                                 infoBoxOutput("meanWorldPrice"),
                                 infoBoxOutput("minWorldPrice")
                               ),
                               
                               fluidRow(
                                 infoBoxOutput("maxWorldPoints"),
                                 infoBoxOutput("meanWorldPoints"),
                                 infoBoxOutput("minWorldPoints")
                               ), 
                               
                               fluidRow(
                                 infoBoxOutput("worldPricePointsCorr"),
                                 infoBoxOutput("worldRedPerc"),
                                 infoBoxOutput("worldWhitePerc")
                               ),
                               
                               fluidRow(
                                 plotlyOutput("scatterByCountry")
                               )
                             ), 
                               
                            tabPanel('By US State',  
                                        
                                  selectizeInput(
                                    "graph_state",
                                     'Select a US State:', 
                                      choices = sort(unique(wines_df_us_graph$province)), 
                                      multiple = F, 
                                      selected = 'California'
                                      ),
                                        
                                      fluidRow(
                                      infoBoxOutput("maxUSPrice"),
                                      infoBoxOutput("meanUSPrice"),
                                      infoBoxOutput("minUSPrice")
                                      ),
                                        
                                     fluidRow(
                                      infoBoxOutput("maxUSPoints"),
                                      infoBoxOutput("meanUSPoints"),
                                      infoBoxOutput("minUSPoints")
                                      ), 
                                        
                                      fluidRow(
                                      infoBoxOutput("USPricePointsCorr"),
                                      infoBoxOutput("USRedPerc"),
                                      infoBoxOutput("USWhitePerc")
                                        ),
                                  
                                      fluidRow(
                                      plotlyOutput("scatterByState")
                                      )
                                    ),
                            'Interactive: Wine Varieties',
                            tabPanel('By Color and Variety',  
                               
                                radioButtons(
                                 "graph_color",
                                 label = ("Pick Your Poison:"),
                                 choices = list("red" = "red", "white" = "white"),
                                 selected = "white"),
                               
                               
                               selectizeInput(
                                 "graph_variety",
                                 'Select a Variety:', 
                                 choices = sort(unique(wines_df_country_graph[wines_df_country_graph$country == 'US', 'province'])), 
                                 multiple = F 
                                ),
                               
                               fluidRow(
                                 infoBoxOutput("maxVarPrice"),
                                 infoBoxOutput("meanVarPrice"),
                                 infoBoxOutput("minVarPrice")
                               ),
                               
                               fluidRow(
                                 infoBoxOutput("maxVarPoints"),
                                 infoBoxOutput("meanVarPoints"),
                                 infoBoxOutput("minVarPoints")
                               ), 
                               
                               fluidRow(
                                 infoBoxOutput("VarPricePointsCorr")
                                
                               ),
                               
                               fluidRow(
                                 plotlyOutput("scatterByVariety")
                               )
                              ),
                          '-------------------------',
                      tabPanel('Dataset Statistics',  
                               
                               fluidRow(
                                 infoBoxOutput("maxDSPrice"),
                                 infoBoxOutput("meanDSPrice"),
                                 infoBoxOutput("minDSPrice")
                               ),
                               
                               fluidRow(
                                 infoBoxOutput("maxDSPoints"),
                                 infoBoxOutput("meanDSPoints"),
                                 infoBoxOutput("minDSPoints")
                               ), 
                               
                               fluidRow(
                                 infoBoxOutput("DSPricePointsCorr"),
                                 infoBoxOutput("DSRedPerc"),
                                 infoBoxOutput("DSWhitePerc") 
                               ),
                               fluidRow(
                                 plotlyOutput('DSPriceDensity'),
                                 plotlyOutput('DSPointsDensity')
                                 ) #fluidRow
                               ) #tabpanel 
                             )#tabsetpanel
                           ) #tabitem
                        ) #tabitems 
                      ) ##dashboardbody
                    ) #dashboard page 
                  ) #shinyUI


       
    
                       
                   
  
        
    
    

    
    
    
    
    
    

    
    





