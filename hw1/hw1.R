# Text-as-Data 2016 Homework 1
# Alex Pine (akp258@nyu.edu)


#TODO clear environment: rm(list = ls())

library(quanteda)
library(quantedaData)

# Loads the SOTUCorpus global corpus variable.
data("SOTUCorpus")

obamaSOTUCorpus <- subset(SOTUCorpus, Date == "2009-02-24" | Date == "2016-01-12")
#summary(obamaSpeeches)
obamaSpeeches <- data.frame(obamaSOTUCorpus$documents)

# TTR: type-to-token ratio. #types/#tokens
# NOTE: , removePunct=FALSE by default
computeTTR <- function(text) {
  # TODO WTF is this [[]] syntax
  tokens <- tokenize(text)[[1]]
  return(length(unique(tokens)) / length(tokens))
}

first_text <- obamaSpeeches['Obama-2009','texts']
computeTTR(first_text)  # Returns 0.223

second_text <- obamaSpeeches['Obama-2016','texts']
computeTTR(second_text)  # Returns 0.247
