###############-----SETTING DIR, IMPORT LIBRARY, READ DATA, DATA SUMMARY-----################

###Setting the main Working directory:
setwd("C:/Users/Visharg Shah/Desktop/Next Word Prediction")

###Importing the Required libraries:

Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jdk1.8.0_121') #setting java env for qdap as sometimes is fail to load

library(dplyr) #Data Manipulation
library(readr) #reading the txt file data 
library(caTools) #Splitting of train and test set
library(tidyr) #An R package for data tidying
library(stringi) #An R package for fast, correct, consistent, and convenient string/text manipulation.
library(quanteda) #For managing and analyzing text, n - grams , cleaning, tokenizing
library(wordcloud) #Creation of wordcloud
library(RColorBrewer) #color effect manipulation
library(qdap) #frequency visualizing
library(ggplot2)   #Visualizing 
library(parallel)  #Parallel processing 

###Read the data and combine it in one corpus:
blogs_data = read_lines('en_US.blogs.txt')
news_data = read_lines('en_US.news.txt')
twitter_data = read_lines('en_US.twitter.txt') 

corpus_data = c(blogs_data, news_data, twitter_data)


################-----SAMPLE DATA, SPLIT INTO TRAIN TEST, PREPROCESSING, DATA CLEANING-----###############

###Sample of corpus data:  
set.seed(999)
n = 5/100 #change this to take different % of corpus data, 5/100 = 5%
corpus_sample = sample(corpus_data, length(corpus_data) * n)

###Split into train and validation sets i.e. test 
split = sample.split(corpus_sample, 0.8) #80% train and 20% test
train_set = subset(corpus_sample, split == T)
test_set = subset(corpus_sample, split == F)

###Transfer corpus to quanteda corpus format and segment into sentences:
#Defining the function for converting into quanteda format
corpus_quanteda = function(x) {
  corpus(unlist(segment(x, 'sentences')))  
}

#train set to quanteda format
train_set = corpus_quanteda(train_set)

### Cleaning, Tokenizing:
corpus_tokenize = function(x, ngramSize = 1, simplify = T) {
  #by default 1 Gram
  #Simplify true will return a character vector of tokens rather than a list 
  char_tolower(
    quanteda::tokenize(x,
                       remove_numbers = T,
                       remove_punct = T,
                       remove_symbols = T,
                       remove_twitter = T,
                       remove_hyphens = T,
                       remove_url = T,
                       ngrams = ngramSize,
                       concatenator = " ",
                       simplify = simplify))
}

#Tokenizing the train set into 1, 2, 3 gram
train_set_1gram = corpus_tokenize(train_set)      #1Gram Approach - 1 word
train_set_2gram = corpus_tokenize(train_set, 2)   #2Gram Approach - 2 word
train_set_3gram = corpus_tokenize(train_set, 3)   #3Gram Approach - 3 word

###Converting the list of words into dataframe: 
df_train_set_1gram = data.frame(tag = train_set_1gram)
df_train_set_2gram = data.frame(tag = train_set_2gram)
df_train_set_3gram = data.frame(tag = train_set_3gram)

###calculating Frequency table for corpus:
corpus_frequency = function(x, minCount = 1) {
  x = x %>%
    group_by(tag) %>%
    summarize(count = n()) %>%
    filter(count >= minCount)
  x = x %>% 
    mutate(freq = count / sum(x$count)) %>% 
    select(-count) %>%
    arrange(desc(freq))
}

df_train_set_1gram = corpus_frequency(df_train_set_1gram)            #for 1 gram

df_train_set_2gram = corpus_frequency(df_train_set_2gram) %>%
  separate(tag, c('word1', 'tag'), " ")                              #for 2 gram

df_train_set_3gram = corpus_frequency(df_train_set_3gram) %>%
  separate(tag, c('word1', 'word2', 'tag'), " ")                     #for 3 gram

###Filtering out bad words from corpus:
dirty_words = c('shit', 'piss', 'fuck', 'cunt', 'cocksucker', 'motherfucker', 'tits')

df_train_set_1gram = filter(df_train_set_1gram, !tag %in% dirty_words)
df_train_set_2gram = filter(df_train_set_2gram, !word1 %in% dirty_words & !tag %in% dirty_words)
df_train_set_3gram = filter(df_train_set_3gram, !word1 %in% dirty_words & !word2 %in% dirty_words & !tag %in% dirty_words)

###############-----PREDICTION-----###############

###Parse tokens from input text:
corpus_input = function(x) {
  
  #If empty input, put both words empty
  if(x == "") {
    input1 = data.frame(word = "")
    input2 = data.frame(word = "")
  }
  
  #Tokenize with same functions as training data
  if(length(x) ==1) {
    y = data.frame(word = corpus_tokenize(corpus(x)))
  }
  
  #If only one word, put first word empty
  if (nrow(y) == 1) {
    input1 = data.frame(word = "")
    input2 = y
    #Get last 2 words    
  }else if (nrow(y) >= 1) {
    input1 = tail(y, 2)
    input2 = tail(y, 1)
  }
  #Return data frame of inputs 
  inputs = data.frame(words = unlist(rbind(input1,input2)), stringsAsFactors=FALSE)
  return(inputs)
}

###Predict using stupid backoff algorithm:
corpus_predict = function(x, y, n = 5) {
  
  #Predict giving just the top 1-gram words, if no input given
  if(x == "" & y == "") {
    prediction = df_train_set_1gram %>%
      select(tag, freq)
    #Predict using 3-gram model
  }else if(x %in% df_train_set_3gram$word1 & y %in% df_train_set_3gram$word2) {
    prediction = df_train_set_3gram %>%
      filter(word1 %in% x & word2 %in% y) %>%
      select(tag, freq)
    
    #Predict using 2-gram model
  }else if(y %in% df_train_set_2gram$word1) {
    prediction = df_train_set_2gram %>%
      filter(word1 %in% y) %>%
      select(tag, freq)
    
    #If no prediction found before, predict giving just the top 1-gram words
  }else{
    prediction = df_train_set_1gram %>%
      select(tag, freq)
  }
  
  # Return predicted word in a data frame
  return(prediction[1:n, ])
}
