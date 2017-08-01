###Setting the main Working directory:
setwd("C:/Users/Visharg Shah/Desktop/Next Word Prediction")

###Loading libraries and file: 
source('C:/Users/Visharg Shah/Desktop/Next Word Prediction/ShinyApp/NextWordPredictionS.R')
library(shiny)

###############-----DEFINING APPLICATION-----###############

shinyServer(function(input, output) {
  
###Prediction when user enters input
prediction =  reactive( {
  
  #Get input
  input_text = input$text
  input1 =  corpus_input(input_text)[1, ]
  input2 =  corpus_input(input_text)[2, ]
  n_words = input$slider
  
  #Predict
  prediction = corpus_predict(input1, input2, n = n_words)
})

###Output data table
output$table = renderDataTable(prediction(),
                               option = list(pageLength = 5,
                                             lengthMenu = list(c(5, 10, 100), c('5', '10', '100')),
                                             columnDefs = list(list(visible = F, targets = 1))
                               )
)

###Output word cloud
wordcloud_rep = repeatable(wordcloud)

output$wordcloud = renderPlot(
  wordcloud_rep(
    prediction()$tag,
    prediction()$freq,
    colors = brewer.pal(8, 'Dark2'),
    scale=c(4, 0.5),
    max.words = 300
  )
)


})

