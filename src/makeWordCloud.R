library(wordcloud)
library(tm)


make_word_cloud <- function(documents, howToColor) {
  corpus = Corpus(VectorSource(tolower(documents)))
  corpus = tm_map(corpus, removePunctuation)
  eStopWords <- stopwords("english")
  eStopWords <- eStopWords[eStopWords != "not"]
  corpus = tm_map(corpus, removeWords, eStopWords)
  
  frequencies = DocumentTermMatrix(corpus)
  removeSparse <- removeSparseTerms(frequencies, 0.995)
  word_frequencies = as.data.frame(as.matrix(removeSparse))
  
  words <- colnames(word_frequencies)
  freq <- colSums(word_frequencies)
  wordcloud(words, freq,
            min.freq=sort(freq, decreasing=TRUE)[[100]],
            colors=brewer.pal(8, "Dark2"),
            random.color = howToColor
            )  
}