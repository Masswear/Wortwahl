library(shiny)

shinyUI(fluidPage(
        
        title = "Wortwahl",
        
        fluidRow(
                column(6, offset = 3,
                       #instructions
                       #textbox to enter text
                       h2("Wortwahl"),
                       h4("Enter a text and hit \"Predict\""),
                       textInput("phrase", "Input phrase:", width = "600px")

                )
                ),
        fluidRow(
                column(1, offset = 3,
                actionButton("update", "Predict")),
                
                column(2,
                       h4("The next word will be:")
                ),
                
                column(3, 
                       h4(textOutput("topword"), style = "color:green")
                       )
        ),
                
                
        hr(),        
        
        fluidRow(
                column(6, offset = 3,
                       
                       h4("This is a wordcloud with the top predictions:", align = "center"),
                       #wordcloud
                       plotOutput("plot")
                )
                ),
        
        
        
        
        
        hr(),
        
        fluidRow(
                column(4,
                #instructions
                h4("Instructions")
                ),
                
                column(4, align = "center",
                #wordcloud
                h4("v0.2")
                ),
                
                column(4,
                #credits
                h4("Credits")
                )
        )
))
