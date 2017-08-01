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

###Calculating file size in MB:
blogs_file_size = round(file.info("en_US.blogs.txt")$size/(1024^2),2)
news_file_size = round(file.info("en_US.news.txt")$size/(1024^2),2)
twitter_file_size = round(file.info("en_US.twitter.txt")$size/(1024^2),2)

###Number of lines of text in each of the files:
blogs_lines = length(blogs_data)
news_lines = length(news_data)
twitter_lines = length(twitter_data)

###Number of words in each of the files:
blogs_total_words = sum(stri_count_words(blogs_data))
news_total_words = sum(stri_count_words(news_data))
twitter_total_words = sum(stri_count_words(twitter_data))

###Summary of Corpus:
corpus_summary <- data.frame(source = c("blogs","news","twitter"),
                             size_in_Mb = c(blogs_file_size,news_file_size,twitter_file_size),
                             lines = c(blogs_lines,news_lines,twitter_lines),
                             words = c(blogs_total_words,news_total_words,twitter_total_words))

corpus_summary

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

###############-----VISUALISING-----###############

###Find most occurring word frequencies with qdap:

Sys.setenv(JAVA_HOME='C:\\Program Files\\Java\\jdk1.8.0_121') #setting java env for qdap as sometimes is fail to load

frequency_1 <- freq_terms(df_train_set_1gram$tag,top=10,at.least = 3,stopwords = "Top200Words")
plot(frequency_1)

frequency_2 <- freq_terms(df_train_set_2gram$tag,top=10,at.least = 3,stopwords = "Top200Words")
plot(frequency_2)

frequency_3 <- freq_terms(df_train_set_3gram$tag,top=10,at.least = 3,stopwords = "Top200Words")
plot(frequency_3)

###Word Cloud:
#create purple orange
purpule_orange <- brewer.pal(10,"PuOr")
#Drop 2 faintest colours
purpule_orange <- purpule_orange[-(1:2)]
#create wordcloud for 1-gram with purpule_orange palette
wordcloud(df_train_set_1gram$tag,df_train_set_1gram$freq,max.words = 100,colors = purpule_orange)
#create wordcloud for 2-gram with purpule_orange palette
wordcloud(df_train_set_2gram$tag,df_train_set_2gram$freq,max.words = 100,colors = purpule_orange)
#create wordcloud for 3-gram with purpule_orange palette
wordcloud(df_train_set_3gram$tag,df_train_set_3gram$freq,max.words = 100,colors = purpule_orange)

###Point plot of most common sequencies:
plot_max = 20
df_freq = rbind(df_train_set_1gram[1:plot_max, ], 
                unite(df_train_set_2gram[1:plot_max, ], 'tag', word1, tag, sep = ' '),
                unite(df_train_set_3gram[1:plot_max, ], 'tag', word1, word2, tag, sep = ' ')) %>%
  mutate(ngram = rep(c('unigram', 'bigram', 'trigram'), each = plot_max))

df_freq$tag = factor(df_freq$tag, levels = df_freq$tag[order(df_freq$freq, decreasing = T)])
df_freq$ngram = factor(df_freq$ngram, levels = c('unigram', 'bigram', 'trigram'))

#taking top 20 words from each gram and plotting the frequency and word chart
ggplot(df_freq, aes(x = tag, y = freq)) +
  geom_point(aes(color = ngram)) +
  theme(axis.text.x = element_text(angle = 90, vjust = 0.5, hjust=1)) +
  scale_y_continuous(breaks = seq(0.000, 0.1, 0.01)) +
  labs(title = 'Top 20 Sequencies within Ngram Groups', x = 'tag', y = 'Frequency')

###Create word network:
#Create a cloud of word which as 'well' into it
word_associate(df_train_set_1gram$tag,match.string = c("well"),network.plot = TRUE,cloud.colors = c("gray85","darkred"))

###############-----PREDICTION-----###############

###Input sample: 
input_text = 'according to'

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

###Get inputs as separate strings:
input1 = corpus_input(input_text)[1, ]
input2 = corpus_input(input_text)[2, ]

input1
input2

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

###Predict:
corpus_predict(input1, input2)

###############-----VALIDATION-----###############

###Transform validation set to quanteda format
test_set = corpus_quanteda(test_set)

###Get 2-gram tokens: 
test_set_2gram = corpus_tokenize(test_set, 2, T)
test_set_2gram = data.frame(tag = test_set_2gram) %>%
  separate(tag, c('word2', 'tag'), ' ')
#Put empty string as word1
test_set_2gram = mutate(test_set_2gram, word1 = rep("", nrow(test_set_2gram))) %>%
  select(word1, word2, tag)

###Get 3-gram tokens:
test_set_3gram = corpus_tokenize(test_set, 3, T)
test_set_3gram = data.frame(tag = test_set_3gram) %>%
  separate(tag, c('word1', 'word2', 'tag'), ' ')


###############-----RESULTS-----###############

###Accuracy:
corpus_accuracy = function(x) {
  y = mcmapply(corpus_predict, x$word1, x$word2, mc.cores = 1)
  cor_accuracy = sum(ifelse(x$tag %in% unlist(y), 1, 0) / length(y))
  
  return(cor_accuracy)
}

###Accuracy for 1 previous word with 5 suggestion us all cores of system: 
accuracy_1_word = round(corpus_accuracy(test_set_2gram), 2) 

###Accuracy for 2 previous word with 5 suggestion use all cores of system:
accuracy_2_word = round(corpus_accuracy(test_set_3gram), 2) 