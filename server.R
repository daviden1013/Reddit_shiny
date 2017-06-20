
mydata = NULL
curdata = NULL
first = TRUE
out = NULL

library(shiny)
source("getData.R")
source("model.R")


####################################################################
shinyServer(function(input, output, session) {
  
  output$cover = renderText({"Reddit text mining and visualization tool"})
  
  # load data
  observeEvent(input$board, {
    if(input$board == "none"){
      return()
    }
    
    first <<- TRUE
    
    isolate({
      withProgress({
        setProgress(message = "Processing...")
        mydata <<- getData(input$board)
        curdata <<- mydata
      })
    })

    updateSummary()
    updateDate()
    updateKeyword()
  }) 
  
  output$filter = renderUI({
    if(input$board != "none")
      checkboxInput('filter', "Filter", value = F)
  })
  
  # update curdata
  readCond = function(){
    curdata <<- mydata
    
    if(length(input$keyword) !=0 && input$keyword != ""){
      keywordVec = strsplit(input$keyword, ";")[[1]]
      
      if(input$keywordUnion == "u"){
        vec = FALSE
        for(i in 1:length(keywordVec))
          vec = vec | grepl(keywordVec[i], curdata$title)
        
        curdata <<- subset(curdata, vec)
      }
      else{
        for(i in 1:length(keywordVec))
          curdata <<- subset(curdata, grepl(keywordVec[i], title))
      }

    }
    
    if(length(input$dates) != 0){
      date = as.integer(gsub("-", "", input$dates, perl = F))
      curdata <<- subset(curdata, (input$author == "All" | input$author == author) &
                    (time >= date[1] & time <= date[2]) &
                    (point >= input$point[1] & point <= input$point[2]) & 
                    (comment >= input$comment[1] & comment <= input$comment[2])
                  )
    }
    else
      curdata <<- subset(curdata, (input$author == "All" | input$author == author))
    
    updateSummary()
  }
  
  # activate readCond
  observeEvent(c(input$author, input$keyword, input$dates, input$point, input$comment, input$keywordUnion), {
    if(first)
      first <<- FALSE
    else
      readCond()
  })
  
  
  observeEvent(input$filter,{
    if(input$filter){
      updateKeyword()
      updateDate()
    }
    else{
      output$dates = renderUI({NULL})
      output$authorUI = renderUI({NULL})
      output$keywordUI = renderUI({NULL})
      output$union = renderUI({NULL})
      output$point = renderUI({NULL})
      output$comment = renderUI({NULL})
    }

      
  })
  
  updateDate = function(){
    output$dates = renderUI({

      if(nrow(curdata)==0 || length(input$filter) == 0||!(input$filter))
        return ()
      start = getDate(as.character(min(curdata$time)))
      end = getDate(as.character(max(curdata$time)))
      
      dateRangeInput("dates", label = "Date range", start = start, end = end,
        min = getDate(as.character(min(mydata$time))), max = getDate(as.character(max(mydata$time))))
      
    })
    

      output$point = renderUI({
        min = min(curdata$point)
        max = max(curdata$point)
        
        sliderInput("point", label = " Points", min = min, 
            max = max, value = c(min, max))
      })
    
      output$comment = renderUI({
        min = min(curdata$comment)
        max = max(curdata$comment)
        
        sliderInput("comment", label = " # of comments", min = min, 
            max = max, value = c(min, max))
      })
    
  }
  
  updateKeyword = function(){
    output$authorUI = renderUI({
      textInput("author", label = "Search author", value = "All")
    })
    
    output$keywordUI = renderUI({
      textInput("keyword", label = "Search keyword", value = "")
    })
    
    output$union = renderUI({
      radioButtons('keywordUnion', NULL,
                   c(Union='u', intersect='i'), selected = 'u', inline = TRUE)
    })
    
  }
  

  updateSummary = function(){
    isolate({
      withProgress({
        setProgress(message = "Processing...")
        output$head = renderTable({
          if(nrow(curdata) != 0)
            curdata[1:min(10, nrow(curdata)),c(-1,-7)]
        })
        
        output$boxplot = renderPlot({
          if(nrow(curdata) != 0)
            boxplot(curdata$point, curdata$comment, names = c("points", "comments"), main = "# of points & comments")
        })
        
        if(first)
          output$pic = renderImage({
            filename <- normalizePath(file.path('./www',
                                    paste(input$board, ".png", sep = "")))
          
            list(src=filename, height = 100, width = 100)
          }, deleteFile = FALSE)
        
        output$size = renderText({
          return (paste("Total # of posts:", nrow(curdata)))
        })
        
        output$sumAuthor = renderText({
          if(length(input$keyword) != 0)
           return (paste("Author(s):", ifelse(input$author == "All", "not specified", input$author)))
          else
            return (return ("Author(s): not specified"))
        })
        
        output$sumKeyword = renderText({
          if(length(input$keyword) != 0)
            return (paste("Keyword(s):", ifelse(input$keyword == "", "not specified", input$keyword)))
          else
            return ("Keyword(s): not specified")
        })
        
        output$time = renderText({
          if(length(input$dates) != 0)
            return (paste("Date range:", input$dates[1], "~", input$dates[2]))
          else
            return ("Date range: not specified")
        })
        
      })
    })
    

  }
  
  
  
    
  output$opt = renderUI({

    if(input$plot == "tfCloud" || input$plot == "tfidfCloud" || input$plot == "sCloud" || input$plot == "bar" )
      sliderInput("wordNum", label = "# of words to plot", min = 10, 
        max = 100, value = 20)
      
    else if(input$plot == "bCloud")
      sliderInput("wordNum", label = "Minimum frequency of word-pairs frequency", min = 1, 
        max = 100, value = 10)
    
    else if(input$plot == "tCloud")
      sliderInput("wordNum", label = "# of topics to cluster", min = 1, 
        max = 10, value = 4)
    
    else
      return (NULL)
      
  })
  
  observeEvent(input$removeFreq, {
    output$removeFreqPercent = renderUI({
      if(input$removeFreq)
        sliderInput("freqPercent", "Remove terms that are in at least ? portion of the posts",
          min = 0, max = 1, value = 0.01)
    })
  })
  
  observeEvent(input$draw, {

    if(is.null(curdata)|| nrow(curdata) ==0)
      return()
    
    isolate({
      withProgress({
        setProgress(message = "Processing...")
        if(input$removeFreq)
          out <<- getFig(title = curdata$title, plotType = input$plot, param = input$wordNum, remove = input$freqPercent)
        else
          out <<- getFig(title = curdata$title, plotType =input$plot, param = input$wordNum)
        
        output$fig = out$figure
        
        output$removed = renderText({paste("Words removed:", paste(out$removed, collapse = ", "), sep = " ")})
        
        updateTabsetPanel(session, "mainTab", "plotTab")
      })
    })
    

  })
  

  # download results
  
  output$saveData <- downloadHandler(
    
    filename = function() {
      paste(input$board, "/_", Sys.Date(), '.csv', sep='')
    },
    content = function(file) {
      write.csv(curdata, file)
    }
  )
  
  output$savePlot <- downloadHandler(
    
    filename = function() {
      paste(input$board, "/_", input$plot, Sys.Date(), '.png', sep='')
    },
    content = function(file) {
      png(file)
      out$figure
      dev.off()
    }
  )
  
  



})
