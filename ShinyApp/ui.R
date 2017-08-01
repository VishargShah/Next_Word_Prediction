###Loading libraries:
library(shiny)
library(shinythemes)

###############-----DEFINING THE APP-----###############
shinyUI(fluidPage(
  
  #Theme
  theme = shinytheme("flatly"),
  
  #Application title
  titlePanel("Next Word Predictor"),
  
  #Sidebar     
  sidebarLayout(
    
    sidebarPanel(
      
      #Text input
      textInput("text", label = ('Please enter some text'), value = ''),
      
      #Number of words slider input
      sliderInput('slider',
                  'Maximum number of words',
                  min = 0,  max = 500,  value = 5
      ),
      
      #Table output
      dataTableOutput('table')),
    
      #Mainpanel
    
      mainPanel(
      
      wellPanel(
        
        #Wordcloud output
        plotOutput('wordcloud')
      )
    ) 
  )
)
)
