
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
                    choices = c(none = 'none'), selected = "none")
        ),

        column(4, imageOutput("pic", height = "auto"))
      ),
      
      
      uiOutput("filter"),

      uiOutput("dates"),
              
      tags$hr(),
      fluidRow(
        column(3, uiOutput("authorUI")
        ),

        column(9,  
          
          fluidRow(column(10, uiOutput("keywordUI")), column(2, uiOutput("keywordCase")), inlune = F),

          uiOutput("union"),

          fluidRow(column(10, uiOutput("excludeUI")), column(2, uiOutput("excludeCase")), inlune = F),
          tags$head(tags$style("#keywordCase, #excludeCase{
                         font-size: 10px;
                         }"
                         )
          )
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
        choices = list(
                  'Bar chart' = c(
                    'Term frequency barchart' = "bar", 
                    'Author-post barchart' = "authorBar", 
                    'Bigrams barchart' = "bigramBar"
                  ),
                    
                  'Word cloud' = c(
                    'Word cloud (tf)' = "tfCloud", 
                    'Word cloud (tf-idf)' = "tfidfCloud", 
                    'Sentiment word cloud' = "sCloud", 
                    'Bigrams cloud' = "bCloud", 
                    'Topic word cloud' = "tCloud"),
                  'Table' = c(
                    'Term dependence' = "termDep"
                  )
          
          ), selected = "bar"
                ),
      
      
      checkboxInput("removeFreq", "Remove frequent terms", value = F),
      
      uiOutput("removeFreqPercent"),
      
      textInput("removeKeyword", "Remove keywords from plotting"),
      
      uiOutput("opt"),
      fluidRow(
        column(4, downloadButton("saveData", "Download data")),
        
        column(4, actionButton("reload", "Reload data", icon("refresh"))),
        
        column(4, actionButton("draw", "Make plot", icon("paper-plane"), 
        style="color: #fff; background-color: #337ab7; border-color: #2e6da4"))
        
      )
      
      
  ),
    mainPanel(
      tabsetPanel(id = 'mainTab', 
        
        tabPanel("Summary", value = 'sumTab',
          textOutput("size"),
          textOutput("sumAuthor"), 
          textOutput("sumKeyword"),
          textOutput("sumExclude"),
          textOutput("time"),
          tags$head(tags$style("#size, #sumAuthor, #sumKeyword, #sumExclude, #time{
                             font-size: 30px;
                             font-family: Arial Rounded MT Bold;
                             line-height: 120%;
                             }"
                     )
          ),
          
          fluidRow( 
            column(4, 
              tableOutput("sumTable")
            ),
            column(4,
              plotOutput("boxplot")
            ),
            
            column(4,
              plotOutput("histogram")
            ),inline = T
          ),
    
          tags$hr(),
          tableOutput("head")
        ),
        
        tabPanel("Plot", value = 'plotTab',
          
          textOutput("removed"),
          
          plotOutput("fig", height = "800px")
        ),
        
        tabPanel("Table", value = 'tableTab',

          tableOutput("termDepTable")
          
        )
        
        
        
      )
      
      

    )
  )
)

)
