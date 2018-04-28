library(tokenizers)
library(tm)


nlpProcessing <- function(documents) {
  corpus = Corpus(VectorSource(tolower(documents)))
  corpus = tm_map(corpus, removePunctuation)
  eStopWords <- stopwords("english")
  eStopWords <- eStopWords[eStopWords != "not"]
  corpus = tm_map(corpus, removeWords, eStopWords)
  corpus = tm_map(corpus, stemDocument, language="english")
  return(corpus)
}


