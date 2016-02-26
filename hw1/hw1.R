# Text-as-Data 2016 Homework 1
# Alex Pine (akp258@nyu.edu)


#TODO clear environment: rm(list = ls())

library(quanteda)
library(quantedaData)

# Question 1

# Loads the SOTUCorpus global corpus variable.
data("SOTUCorpus")

obamaSOTUCorpus <- subset(SOTUCorpus, Date == "2009-02-24" | Date == "2016-01-12")
#summary(obamaSpeeches)
obamaSpeeches <- data.frame(obamaSOTUCorpus$documents)

# 1.1

# TTR: type-to-token ratio. #types/#tokens
# NOTE: , removePunct=FALSE by default
computeTTR <- function(text) {
  # TODO WTF is this [[]] syntax
  tokens <- tokenize(text)[[1]]
  return(length(unique(tokens)) / length(tokens))
}

first_text <- obamaSpeeches['Obama-2009','texts']
computeTTR(first_text)  # Returns 0.233
second_text <- obamaSpeeches['Obama-2016','texts']
computeTTR(second_text)  # Returns 0.247

# 1.2 - 1.4

ttrAndCosineDist <- function(dfm) {
  ttrs = lexdiv(dfm, measure=c("TTR"))
  cosine = similarity(dfm, c('Obama-2009', 'Obama-2016'), 
                      n=NULL, margin='documents', 
                      method='cosine', normalize=FALSE)[1]
  paste(c('2009 TTR: ', ttrs['Obama-2009'], ', 2016 TTR: ', ttrs['Obama-2016'],
          ", Cosine similarity: ", cosine), collapse="")
}

# Remove Puncutation only
obamaSofuDfm_base <- dfm(obamaSOTUCorpus, removePunct=TRUE, 
                         removeNumbers=FALSE, toLower=FALSE)
ttrAndCosineDist(obamaSofuDfm_base)
# "2009 TTR: 0.259845114516395, 2016 TTR: 0.278916060806345, Cosine similarity: 0.958206490749019"

# Stemming
obamaSofuDfm_stem <- dfm(obamaSOTUCorpus, removePunct=TRUE, 
                         removeNumbers=FALSE, toLower=FALSE, stem=TRUE)
ttrAndCosineDist(obamaSofuDfm_stem)
# "TTR:  0.323110079575597 , Cosine similarity:  0.957595732036153"

# Stop words
obamaSofuDfm_stop <- dfm(obamaSOTUCorpus, removePunct=TRUE,
                         removeNumbers=FALSE, toLower=FALSE,
                         ignoredFeatures=stopwords("english"))
ttrAndCosineDist(obamaSofuDfm_stop)
# "2009 TTR: 0.425381903642773, 2016 TTR: 0.455208637292092, Cosine similarity: 0.728778742112988"

# Lowercase
obamaSofuDfm_lower <- dfm(obamaSOTUCorpus, removePunct=TRUE, 
                          removeNumbers=FALSE, toLower=TRUE)
ttrAndCosineDist(obamaSofuDfm_lower)
# "2009 TTR: 0.248475860932608, 2016 TTR: 0.263549239920687, Cosine similarity: 0.964343983977504"

# tfidf
obamaSofuDfm_tfidf <- tfidf(dfm(obamaSOTUCorpus, removePunct=TRUE, 
                                removeNumbers=FALSE, toLower=FALSE))
ttrAndCosineDist(obamaSofuDfm_tfidf)
# "2009 TTR: 2.36446551569311, 2016 TTR: 2.40905179164666, Cosine similarity: 0"

# 1.5 MLTD

computeMLTD <- function(text) {
  tokens <- tokenize(toLower(text), simplify=TRUE, removePunct=TRUE)
  
  sequence_lengths = numeric(0)
  i <- 1
  while (i < length(tokens)) {
    j <- 0
    types <- character(0)
    num_types <- 0
    num_tokens <- 0
    # This inner loop adds new words to the current list, and stops 
    # when TTR gets too low.
    while (i + j <= length(tokens)) {
      num_tokens = num_tokens + 1
      if (!(tokens[i+j] %in% types)) {
        types <- c(types, tokens[i + j])
        num_types <- num_types + 1
      }
      ttr <- num_types / num_tokens
      if (ttr <= 0.72) {
        break
      }
      j <- j + 1
    }
    sequence_lengths = c(sequence_lengths, j)
    i <- i + j + 1
  }
  return(mean(sequence_lengths))
}

computeMLTD(first_text)
# 88.26471
computeMLTD(second_text)
# 95.07937
