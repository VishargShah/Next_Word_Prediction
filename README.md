# Next_Word_Prediction
The project is about predicting the next word while you are typing for whatever purpose.

Language: R 

Data:- The link for the source data is in data folder. Data is around 550 MB from SwiftKey source.
Data is about US News, Blogs, Twitter. 

The file NextWordPrediction.R contains full code from loading, cleaning, visualizing, predicting everything.

All the output images are in the output folder.

data summary.txt: short summary of the data

Frequency plots shows the 10 highest tag occurring with count.
1) frequency_1: frequency plot with 1-gram
2) frequency_2: frequency plot with 2-gram
3) frequency_3: frequency plot with 3-gram

Wordcloud images shows the wordcloud for 1-gram, 2-gram, 3-gram.

top_20_sequence.png: Shows the chart of top 20 words w.r.t their frequency combined all together which shows the fall in the frequency
as the grams increases.

word_association.jpeg: the image shows all the associate word w.r.t some particular word e.g. image shows all the words contains 'well'.

ShinyApp has all source code require for server, ui and NextWordPredictionS.R contains the only code require to run on the server
side and other extra code was removed. Also contains the sample image of the output of the Shinny App.

About the Code:

First all the corpus data was load and from whole data only 5% as sample were used which can be changed by changing the n variable.

Basic information of corpus was created which is as in data summary.txt

Data was divided into train and test set into 8:2 ratio for validation.

I used quanteda library for cleaning the corpus, tokenizing and apply grams.

Started with 1-gram then moved to 2-gram and then 3-gram.
1) 1-gram means 1 word which is just a tag word.
2) 2- gram means 2 word which is 1 word and tag word.
3) 3-gram means 3 word which is 2 word and tag word.

Then a dataframe is created for each gram with their respected frequency. So the tag word will be predicted based on the frequency.

The predicted word which is tag based on the previous 2 word like in 3-gram will be shown up based on the highest frequency.
This is basically a stupid backoff algorithm.

Shinny App contains basic GUI which allows user to type and below it gives most 5 occuring predicted word which can be change using slider and also displays the wordcloud for the same. 
