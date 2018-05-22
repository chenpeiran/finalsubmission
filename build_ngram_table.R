setwd('~/Downloads/coursera/final/en_US/')

library(stringi)
library(NLP)
library(tm)

# laod files
#blogs <- readLines("en_US.blogs.txt",encoding = "UTF-8", skipNul = TRUE)
news <- readLines("en_US.news.txt",encoding = "UTF-8", skipNul = TRUE)
twitter <- readLines("en_US.twitter.txt",encoding = "UTF-8", skipNul = TRUE)

news <- iconv(news,"latin1","ASCII",sub = "")
twitter <- iconv(twitter,"latin1","ASCII",sub = "")

# get sample 5%
factor <- 0.2
news1 <- sample(news,round(factor*length(news)))
twitter1 <- sample(twitter,round(factor*length(twitter)))
matrix(c(NROW(news1),NROW(twitter1)),byrow = TRUE,nrow=2,ncol=1,dimnames = list(c("news1","twitter1"),"No.Of Rows"))

set.seed(1234)

trainingcorpus <- VCorpus(VectorSource(news1))
preprocess <- function(document){
  document <- tm_map(document, removePunctuation)
  document <- tm_map(document, removeNumbers)
  document <- tm_map(document, stripWhitespace)
  document <- tm_map(document, content_transformer(tolower))
  document <- tm_map(document, PlainTextDocument)
  return(document)
}
trainingcorpus <- preprocess(trainingcorpus)

Unigramtokenizer <- function(x)
  unlist(lapply(ngrams(words(x), 1), paste, collapse = " "), use.names = FALSE)
Bigramtokenizer <- function(x)
  unlist(lapply(ngrams(words(x), 2), paste, collapse = " "), use.names = FALSE)
Trigramtokenizer <-function(x)
  unlist(lapply(ngrams(words(x), 3), paste, collapse = " "), use.names = FALSE)
Fourgramtokenizer <-function(x)
  unlist(lapply(ngrams(words(x), 4), paste, collapse = " "), use.names = FALSE)

# Create document matrix
unigramdocumentmatrix <- TermDocumentMatrix(trainingcorpus,control = list(tokenize = Unigramtokenizer))
bigramdocumentmatrix <- TermDocumentMatrix(trainingcorpus,control = list(tokenize = Bigramtokenizer))
trigramdocumentmatrix <- TermDocumentMatrix(trainingcorpus,control = list(tokenize = Trigramtokenizer))
Fourgramdocumentmatrix <- TermDocumentMatrix(trainingcorpus,control = list(tokenize = Fourgramtokenizer))

unigramf <- findFreqTerms(unigramdocumentmatrix,lowfreq = 1000)
Unigramfreq <- rowSums(as.matrix(unigramdocumentmatrix[unigramf,]))
Unigramfreq <- data.frame(word=names(Unigramfreq),frequency=Unigramfreq)

bigramf <- findFreqTerms(bigramdocumentmatrix,lowfreq = 150)
Bigramfreq <- rowSums(as.matrix(bigramdocumentmatrix[bigramf,]))
Bigramfreq <- data.frame(word=names(Bigramfreq),frequency=Bigramfreq)

trigramf <- findFreqTerms(trigramdocumentmatrix,lowfreq = 40)
Trigramfreq <- rowSums(as.matrix(trigramdocumentmatrix[trigramf,]))
Trigramfreq <- data.frame(word=names(Trigramfreq),frequency=Trigramfreq)

fourgramf <- findFreqTerms(Fourgramdocumentmatrix,lowfreq = 15)
Fourgramfreq <- rowSums(as.matrix(Fourgramdocumentmatrix[fourgramf,]))
Fourgramfreq <- data.frame(word=names(Fourgramfreq),frequency=Fourgramfreq)

library(tidyr)
newsngramtable <- rbind(
  tidyr::separate(Unigramfreq, word, c("word1", "word2","word3","word4"), " ", fill = "right"),
  tidyr::separate(Bigramfreq, word, c("word1", "word2","word3","word4"), " ", fill = "right"),
  tidyr::separate(Trigramfreq, word, c("word1", "word2","word3","word4"), " ", fill = "right"),
  tidyr::separate(Fourgramfreq, word, c("word1", "word2","word3","word4"), " ", fill = "right") )


trainingcorpus <- VCorpus(VectorSource(twitter1))
trainingcorpus <- preprocess(trainingcorpus)

# Create document matrix
unigramdocumentmatrix <- TermDocumentMatrix(trainingcorpus,control = list(tokenize = Unigramtokenizer))
bigramdocumentmatrix <- TermDocumentMatrix(trainingcorpus,control = list(tokenize = Bigramtokenizer))
trigramdocumentmatrix <- TermDocumentMatrix(trainingcorpus,control = list(tokenize = Trigramtokenizer))
Fourgramdocumentmatrix <- TermDocumentMatrix(trainingcorpus,control = list(tokenize = Fourgramtokenizer))

unigramf <- findFreqTerms(unigramdocumentmatrix,lowfreq = 1000)
Unigramfreq <- rowSums(as.matrix(unigramdocumentmatrix[unigramf,]))
Unigramfreq <- data.frame(word=names(Unigramfreq),frequency=Unigramfreq)

bigramf <- findFreqTerms(bigramdocumentmatrix,lowfreq = 250)
Bigramfreq <- rowSums(as.matrix(bigramdocumentmatrix[bigramf,]))
Bigramfreq <- data.frame(word=names(Bigramfreq),frequency=Bigramfreq)

trigramf <- findFreqTerms(trigramdocumentmatrix,lowfreq = 80)
Trigramfreq <- rowSums(as.matrix(trigramdocumentmatrix[trigramf,]))
Trigramfreq <- data.frame(word=names(Trigramfreq),frequency=Trigramfreq)

fourgramf <- findFreqTerms(Fourgramdocumentmatrix,lowfreq = 25)
Fourgramfreq <- rowSums(as.matrix(Fourgramdocumentmatrix[fourgramf,]))
Fourgramfreq <- data.frame(word=names(Fourgramfreq),frequency=Fourgramfreq)

twitterngramtable <- rbind(
  tidyr::separate(Unigramfreq, word, c("word1", "word2","word3","word4"), " ", fill = "right"),
  tidyr::separate(Bigramfreq, word, c("word1", "word2","word3","word4"), " ", fill = "right"),
  tidyr::separate(Trigramfreq, word, c("word1", "word2","word3","word4"), " ", fill = "right"),
  tidyr::separate(Fourgramfreq, word, c("word1", "word2","word3","word4"), " ", fill = "right") )

ngramtable <- rbind(twitterngramtable,newsngramtable)

write.csv(ngramtable, file = "ngram.csv",row.names=FALSE)
