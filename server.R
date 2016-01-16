
# This is the server logic for a Shiny web application.
# You can find out more about building applications with Shiny here:
# 
# http://www.rstudio.com/shiny/
#

library(shiny)

require(quanteda)
require(stringi)
require(data.table)
require(dplyr)
require(Matrix)
require(wordcloud)

source("prediction.R")


shinyServer(function(input, output) {
        
        output$plot <- renderPlot({
                wordcloud(satzanfang$pred, freq = satzanfang$score,
                          colors = brewer.pal(8, "Dark2"))
        })
        
        table <- reactive({
                # Change when the "update" button is pressed...
                input$update
                isolate(wortwahl(input$phrase, show = 20))
        })
   
        wordcloud_rep <- repeatable(wordcloud)
        
        output$topword <- renderText({
                
                head(table()$pred, 1)
        })
        
        output$plot <- renderPlot({

                wordcloud_rep(table()$pred, freq = table()$score,
                              colors=brewer.pal(8, "Dark2"))
        })
        
        
  
})
