# 1. Loading data and exploratory analysis
# Loading data and packages
# Data is loaded and read by readLines function

en_blogs = readLines("en_US.blogs.txt")
en_news = readLines("en_US.news.txt")
en_twitter = readLines("en_US.twitter.txt")

# I used quanteda package for natural language process (building corpus, 
# cleaning and tokenizing the corpus as well as building ngram). Some other packages 
# like tidyr, dplyr, data.table are used to clean the data and build the table for ngram.
library(quanteda)
library(tm)
library(tidyr)
library(dplyr)
library(ngram)
library(data.table)
library(stringr)
library(wordcloud)
library(ggplot2)

# Exploratory analysis
# What is the length of the longest line seen in any of the three en_US data sets? 
nchar_blogs <- sapply(en_blogs, nchar)
nchar_news <- sapply(en_news, nchar)
nchar_twitter <- sapply(en_twitter, nchar)
max_nchar <- max(nchar_blogs, nchar_news, nchar_twitter)


# In the en_US twitter data set, if you divide the number of lines where the word "love" (all lowercase) occurs by the number of lines the word "hate" (all lowercase) occurs, about what do you get?
length(grep("love", en_twitter))/length(grep("hate", en_twitter))

# The one tweet in the en_US twitter data set that matches the word "biostats" says what?
grep("biostats", en_twitter, value=TRUE)

# How many tweets have the exact characters "A computer once beat me at chess, 
# but it was no match for me at kickboxing". (I.e. the line matches those characters 
# exactly.)
length(grep("A computer once beat me at chess, but it was no match for me at kickboxing", en_twitter, value=TRUE))

# Sampling the data
# The total data is subset by 30% to do cleaning and further analysis
samples_blogs <- en_blogs[rbinom(length(en_blogs), 1, 0.4) ==1] 
samples_news <- en_news[rbinom(length(en_blogs), 1, 0.4) ==1]
samples_twitter <- en_twitter[rbinom(length(en_blogs), 1, 0.4) ==1]
stri_stats_general(samples_blogs) # số dòng, số ký tự
stri_stats_latex (samples_blogs) # số từ số ký tự
stri_count_words(samples_blogs) # tính số từ trong mỗi dòng
data <- c(samples_blogs, samples_news, samples_twitter)

# Tokenize and cleaning the texts with quanteda package:
#- Transfer all text to lowercase
#- Split hyphens
#- Remove seperators
#- Removing punctuation 
#- Removing url
#- Remove symbols
#- Removing number
#- Remove the most common words of the language (stop words)
#- Remove all other characters that are not letters and all the words with 2 or more consecutive repeated characters

tokendata <- function(data) {
  data <- tokens(data)
  data <- tokens_tolower(data)
  data <- tokens(data, split_hyphens = TRUE)
  data <- tokens(data, remove_separators = TRUE)
  data <- tokens(data, remove_punct = TRUE)
  data <- tokens(data, remove_url = TRUE)
  data <- tokens(data, remove_symbols = TRUE)
  data <- tokens(data, remove_numbers = TRUE)
  data <- tokens_remove(data, stopwords("en"))
  data <- tokens_select(data, c("[^a-zA-Z]", "([a-zA-Z0-9])\\1\\1+"), selection = "remove", valuetype = "regex")
  return(data)
}

# 2. Building ngram:
# a) Unigram
# We shall use tokens_grams function with n = 1 to build unigram. Then building 
# the document feature matrix. The matrix shows the count of each word by line of 
# text (coresponding to each column). Therefore, we have to summarize by column to obtain the total count of each word. Afterward turning document features matrix to a table for further process.
dt <- corpus(data)
dt <- tokendata(data)
ngram <- tokens_ngrams(dt, n=1)
dfm <- dfm(ngram)
dfm <- dfm_trim(dfm, min_termfreq = 10)
dfmS <- colSums(dfm)
unigram <- data.table(word1=names(dfmS), freq=dfmS)
unigram <- arrange(unigram, -freq)
setkey(unigram, word1)
head(unigram)
# b) Bigram and Trigram
# Simmilarly, we get bigram and trigram, just have to change n to 2 and 3 
# respectively in tokens_gram function. The difference is to split the 2-word 
# strings to 2 words on 2 columns as well as split 3-word strings to 3 words into 
# 3 columns in the table.
ngram2 <- tokens_ngrams(dt, n=2)
dfm2 <- dfm(ngram2)
dfm2 <- dfm_trim(dfm2, min_termfreq = 5)
dfm2S <- colSums(dfm2)
bigram <- data.table(
  word1 = sapply(strsplit(names(dfm2S), "_", fixed = TRUE), "[[", 1),
  word2 = sapply(strsplit(names(dfm2S), "_", fixed = TRUE), "[[", 2),
  freq = dfm2S
)
bigram <- arrange(bigram, -freq)
setkey(bigram, word1, word2)
head(bigram)

discount_value <- 0.75

# Assigning total times word1 occured to bigram: Cn1 is the count of the first word in bigram 
mergeb <- merge(bigram, unigram, by="word1", all.x = TRUE)
bigram <- bigram[, Cn1:= mergeb$freq.y]

# Finding the number of possible unique word2 followed by each word1
n1wi <- bigram[, .(N = .N), by = word1]   
setkey(n1wi, word1)
bigram <- bigram[, Cfw:= n1wi[word1, N]]

# Finding number of unique bigram 
NoBigram <- nrow(bigram)

# Finding the continuation probabilities, which is the number of unique preceding the word for each word2 divided by the number of unique bigram
# In another words, dividing number of times word 2 occurs as second part of bigram, by total number of bigrams.  
Pcont2 <- bigram[, .(Prob = ((.N) /NoBigram)), by = word2]
setkey(Pcont2, word2)
bigram <- bigram[, Pcont:= Pcont2[word2, Prob]]


# Kneser Kney Algorithm
bigram[, Prob := ((freq - discount_value) / Cn1  + (discount_value / Cn1) * Cfw * Pcont)] 
#where:
#(freq - discount_value) / Cn1: first term(final word)
#discount_value / Cn1 * n1wi[word1, N]: lamda (string) 
#unigram[word2, Prob]: Pcontinuation (final word)

# It is similar to calculate the probability of the third word in trigram 
trigram <- tokens_ngrams(dt, n=3)
dfm3 <- dfm(trigram)
dfm3 <- dfm_trim(dfm3, min_termfreq = 5)
dfm3S <- colSums(dfm3)
trigram <- data.table(
  word1 = sapply(strsplit(names(dfm3S), "_", fixed = TRUE), "[[", 1),
  word2 = sapply(strsplit(names(dfm3S), "_", fixed = TRUE), "[[", 2),
  word3 = sapply(strsplit(names(dfm3S), "_", fixed = TRUE), "[[", 3),
  freq = dfm3S
)
trigram <- arrange(trigram, -freq)
setkey(trigram, word1, word2, word3)
head(trigram)

# Count of string:
merge <- merge(trigram, bigram, by=c("word1", "word2"), all.x=TRUE)
trigram <- trigram[, Cn2:= merge$freq.y]

merge$freq.y <- sapply(merge$freq.y, function (x) {if(is.na(x)) x <- 0})

# The number of unique third word given the string
n2wi <- trigram[, .(N=.N), by=.(word1, word2)]
setkey(n2wi, word1, word2)
trigram <- trigram[,Cfw:=merge(trigram, n2wi, by=c("word1", "word2"), all.x=TRUE)[,"N"]]

# Count of trigram: 
NoTrigram <- nrow(trigram)

# Coninuation probabilities: 
Pcont3 <- trigram[, .(Pcont = (.N) /NoTrigram), by = word3]
setkey(Pcont3, word3)
trigram <- trigram[,Pcont:= Pcont3[word3, Pcont]]
 
# Kneser Kney Algorithm
trigram[, Prob := (freq - discount_value) / Cn2 + discount_value / Cn2 * Cfw *
          Pcont]

# Function to return randomly the most frequent words from unigrams
uniWord <- unigram[sample(order(unigram$freq, decreasing = TRUE)[1:5], size=1), 1]

# Function to return the most frequent word given a preceding word
biWords <- function(w1) {
  pwords <- bigram[w1][order(-Prob)]
  if (any(is.na(pwords))) 
    return (uniWord) else return(pwords[1, word2])
}

# Function to return highly probable previous word given two successive words
triWords <- function(w1, w2) {
  pwords <- trigram[.(word1= w1, word2 = w2)][order(-Prob)]
  if (any(is.na(pwords))) return(biWords(w2)) else return(pwords[1, word3])
}

# The prediction model

getWords <- function(str){
  tokens <- tokens(str, split_hyphens = TRUE, remove_separators = TRUE,
                   remove_punct = TRUE,remove_symbols = TRUE, remove_numbers = TRUE) 
  tokens <- tokens_remove(tokens, stopwords("en"))
  tokens <- tokens_select(tokens, c("[^a-zA-Z]", "([a-zA-Z0-9])\\1\\1+"), selection = "remove", valuetype = "regex")
  tokens <- rev(rev(tokens[[1]])[1:2])
  words <- triWords(tokens[1], tokens[2])
  str = paste(str, words, sep = " ")
  return(str)
}


# Visualizing with word cloud
set.seed(1234)
unicloud <-  wordcloud(words = unigram$word1, freq = unigram$freq, min.freq = 1,
            max.words=200, random.order=FALSE, rot.per=0.35, 
            colors=brewer.pal(8, "Dark2"))  
bicloud <-  wordcloud(words = paste(bigram$word1, bigram$word2), freq = bigram$freq, min.freq = 1,
                       max.words=200, random.order=FALSE, rot.per=0.35, 
                       colors=brewer.pal(8, "Dark2")) 
triCoud <- wordcloud(words = paste(trigram$word1, trigram$word2, trigram$word3), freq = trigram$freq, min.freq = 1,
                     max.words=200, random.order=FALSE, rot.per=0.35, 
                     colors=brewer.pal(8, "Dark2")) 

# Visualizing with plot
plot_unigram <- ggplot(unigram[1:20,], aes(x=reorder(word1,freq), y=freq)) + geom_bar(stat="Identity", fill="darkgreen") 
plot_bigram <- ggplot(bigram[1:20,], aes(x= paste(word1, word2)), freq) + geom_bar(stat="Identity", fill="darkblue")
plot_trigram <- ggplot(bigram[1:20,], aes(x=paste(word1, word2, word3), freq)) + geom_bar(stat="Identity", fill="darkblue")

# Write n-gram tables into the csv file
write.table(unigram,file= "unigram.csv",sep= ",",row.names=F)
write.table(bigram,file= "bigram.csv",sep= ",",row.names=F)
write.table(trigram,file= "trigram.csv",sep= ",",row.names=F)

