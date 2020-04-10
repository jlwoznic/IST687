# 
# Course: IST687
# Name: Joyce Woznica
# Homework 10 - Text Mining 
# Due Date: 3/19/2019
# Date Submitted:
#
#specify the packages of interest
packages=c("tm","wordcloud", "XML", "tidytext", "syuzhet")

#use this function to check if each package is on the local machine
#if a package is installed, it will be loaded
#if any are not, the missing package(s) will be installed and loaded
package.check <- lapply(packages, FUN = function(x) {
  if (!require(x, character.only = TRUE)) {
    install.packages(x, dependencies = TRUE)
    library(x, character.only = TRUE)
  }
})

#verify they are loaded
search()

# MLK speech
mlk<-readLines("MLK.txt")
mlk <- mlk[which(mlk != "")] #remove blank lines
mlk[1:3]

#	Create a term matrix
# interprets each element of the "mlk" as a document and create a vector source
words.vec <- VectorSource(mlk)
# create a Corpus, a "Bag of Words"
words.corpus <- Corpus(words.vec)
# first step transformation: make all of the letters in "words.corpus" lowercase
words.corpus <- tm_map(words.corpus, content_transformer(tolower))
# second step transformation: remove the punctuation in "words.corpus"
words.corpus <- tm_map(words.corpus, removePunctuation)
# third step transformation: remove numbers in "words.corpus"
words.corpus <- tm_map(words.corpus, removeNumbers)
# final step transformation: take out the "stop" words, such as "the", "a" and "at"
words.corpus <- tm_map(words.corpus, removeWords, stopwords("english"))

# create a term-document matrix "tdm"
tdm <- TermDocumentMatrix(words.corpus)
# view term-document matrix "tdm"
tdm
#

# continue to manipulate the MLK speech, words, terms
#	Create a list of counts for each word
# convert tdm into a matrix called "m"
m <- as.matrix(tdm)
m[1:10,]

# create a list of counts for each word named "wordCounts"
wordCounts <- rowSums(m)
wordCounts[1:10]

# sum the total number of words and store the value to "totalWords"
totalWords <- sum(wordCounts)
totalWords
# create a vector "words" that contains all the words in "wordCounts"
words <- names(wordCounts)
head(words)

# sort words in "wordCounts" by frequency
wordCounts <- sort(wordCounts, decreasing=TRUE)
# check the first several items in "wordCounts" to see if it is built correctly
head(wordCounts)


# 1) First read in the AFINN word list. Note that each line is a word and a score
# create two vectors: words and score
sents <- get_sentiments("afinn")
sents <- as.data.frame(sents)
colnames(sents)<- c("Word", "Score")

# sort words in "wordCounts" by frequency
wordCounts <- sort(wordCounts, decreasing=TRUE)
# check the first several items in "wordCounts" to see if it is built correctly
head(wordCounts)

# Create a df
cloudFrame<-data.frame(word=names(wordCounts),freq=wordCounts)
cloudFrame[1:10,]

# 2) Compute an overall score for the MLK speech 
# first merge the mlk df with the AFINN sentiments to create a word, freq and score
mergedTable<-merge(cloudFrame,sents,by.x="word",by.y="Word")
mergedTable[1:10,]
str(mergedTable)
# Calculate the overall score based on summing up the frequency*score 
overallScore<-sum(mergedTable$freq*mergedTable$Score)
# overallScore
overallScore
sprintf("%1.2f%%", 100*overallScore/totalWords)

# this gives a different number - just tried for fun. 
get_sentiment(mlk,method="afinn")
Score.MLK <- sum(get_sentiment(mlk,method="afinn"))

# 3) Compute the senitment score for each quarter (25%)
# create a function to cut the words in chunks in order of the speech
# wc - word corpus
# num.chunks - number of chunks
chunk.up <- function (wc, num.chunks)
{
  index <- 1
  cutoff <- round(length(wc)/num.chunks)
  start <- index
  end <- cutoff
  r.scores<-NULL
    # build a new vector of the chunks
  while (index <= num.chunks)
  {
    # grab the chunk
    chunked.up <- wc[start:end]
    # create term document matrix
    tdm <- TermDocumentMatrix(chunked.up)
    # make it a matrix
    m <- as.matrix(tdm)
    word.counts <- rowSums(m)
    total.words <- sum(word.counts)
    # Create a df
    cf<-data.frame(word=names(word.counts),freq=word.counts)
    # Compute an score for current chunk 
    # first merge the mlk df with the AFINN sentiments to create a word, freq and score
    mt<-merge(cf,sents,by.x="word",by.y="Word")
    #  Calculate the overall score based on summing up the frequency*score 
    r.scores <- cbind (r.scores,overallScore<-sum(mt$freq*mt$Score))
    # increment up
    start <- end + 1
    index <- index + 1
    end <-  end+cutoff+1
  }
  r.scores
}

quarter.scores <- chunk.up(words.corpus, 4)

barplot(quarter.scores, names.arg = c("1st 25%","2nd 25%","3rd 25%","4th 25%"), main = "AFFIN Sentiment Score for Martin Luther King Speech")



