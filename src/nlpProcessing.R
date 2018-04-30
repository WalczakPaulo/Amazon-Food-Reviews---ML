library(tokenizers)
library(tm)


nlpProcessing <- function(docs) {
  docs <- Corpus(VectorSource(docs))
  docs <- tm_map(docs, content_transformer(tolower))
  docs <- tm_map(docs, removeNumbers)
  docs <- tm_map(docs, removeWords, stopwords("english"))
  docs <- tm_map(docs, removePunctuation)
  docs <- tm_map(docs, stripWhitespace)
  docs <- tm_map(docs, stemDocument, language = "english")
  return(docs)
  
  
}


divideSetNumerically <- function(data) {
  result <- ""
  if(data < 3)
    result <- 1
  else 
    result <- 5
  return(result)
}
