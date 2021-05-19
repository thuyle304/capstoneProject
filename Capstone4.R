unigram <- data.table(read.csv(file= "unigram.csv"))
bigram <- data.table(read.csv(file = "bigram.csv"))
trigram <- data.table(read.csv(file = "trigram.csv"))

setkey(unigram, word1)
setkey(bigram, word1, word2)
setkey(trigram, word1, word2, word3)

# function to return random words from unigrams
uniWord <- unigram[sample(order(unigram$freq, decreasing = TRUE)[1:5], size=1), 1]

# function to return highly probable previous word given a word
biWords <- function(w1) {
  pwords <- bigram[w1][order(-Prob)]
  if (any(is.na(pwords))) 
    return (uniWord) else return(pwords[1, word2])
}

# function to return highly probable previous word given two successive words
triWords <- function(w1, w2) {
  pwords <- trigram[.(word1=w1, word2=w2)][order(-Prob)]
  if (any(is.na(pwords))) return(biWords(w2)) else return(pwords[1, word3])
}

# The prediction app
getWords <- function(str){
  tokens <- tokens_tolower(x = tokens(str))
  tokens <- rev(rev(tokens[[1]])[1:2])
  words <- triWords(tokens[1], tokens[2])
  return(words)
}

