
library(shiny)

shinyUI(fluidPage(
  img(src="reddit.png", height = 50),
  titlePanel(textOutput("cover"), tags$head(tags$style("#cover{font-family: Arial Rounded MT Bold;}"))),
  
  sidebarLayout(
    
    sidebarPanel(
      ##################################
      #control panel for choosing data
      ##################################
      
      
      fluidRow(
        column(8, selectInput('board', label = h3('Reddit board'),
                    choices = c('select board' = "none", 
                                LifeProTips = "LPT", 
                                Science = "science",
                                Gaming = "gaming",
                                FoodPorn = "FoodPorn",
                                politics = "politics"
                      ), selected = "none")
        ),

        column(4, imageOutput("pic", height = "auto"))
      ),
      
      
      uiOutput("filter"),

      uiOutput("dates"),
              
      tags$hr(),
      fluidRow(
        column(6, uiOutput("authorUI")
        ),

        column(6,  uiOutput("keywordUI"), 
          uiOutput("union")
        )
      ),
          
      #tags$hr(),
      
      uiOutput("point"),
      
      uiOutput("comment"),

      #tags$hr(),
      ##################################
      #control panel for plots
      ##################################
      selectInput('plot', label = h3('Plot type'),
        choices = c('Frequent words barplot' = "bar", 'Word cloud (tf)' = "tfCloud", 
          'Word cloud (tf-idf)' = "tfidfCloud", 'Sentiment word cloud' = "sCloud", 
          'Bigrams cloud' = "bCloud", 'Topic word Cloud' = "tCloud"), selected = "bar"),
      
      checkboxInput("removeFreq", "Remove frequent terms", value = F),
      
      uiOutput("removeFreqPercent"),
      
      uiOutput("opt"),
      fluidRow(
        column(4, downloadButton("saveData", "Data")),
        
        column(4, downloadButton("savePlot", "Plot")),
        
        column(4, actionButton("draw", "Make plot", icon("paper-plane"), 
        style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
        
      )
      
      
  ),
    mainPanel(
      tabsetPanel(id = 'mainTab', 
        
        tabPanel("Summary", value = 'sumTab',
          fluidRow( 
            column(6, 

              textOutput("size"),
              textOutput("sumAuthor"), 
              textOutput("sumKeyword"),
              textOutput("time"),
              tags$head(tags$style("#size, #sumAuthor, #sumKeyword, #time{
                                 font-size: 30px;
                                 font-family: Arial Rounded MT Bold;
                                 line-height: 200%;
                                 }"
                         )
              )
            ),
            column(6,
              plotOutput("boxplot")
            ), inline = F
          ),
    
          tags$hr(),
          tableOutput("head")
        ),
        
        tabPanel("Plot", value = 'plotTab',
          
          textOutput("removed"),
          
          plotOutput("fig", height = "800px")
        )
        
      )
      
      

    )
  )
)

)
