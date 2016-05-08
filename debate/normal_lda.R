
rm(list=ls())
setwd("~/texas/debate/")
require(quanteda)
require(stm)
require(plyr)
require(dplyr)
require(ggplot2)
require(reshape2)

source("./parse_debate.R")

debate.df <- parseDebateText()

df_to_corpus_dfm <- function(df, lower.thresh = 3) {
  # remove twitter handles from tweets and hyperlinks
  df$text <- gsub("@\\w+ *", "", df$text)
  t_corpus <- corpus(df$text, docvars = df[,!names(df) %in% "text"])
  # DFM: remove # symbol, @ symbol, RT, SMART English stopwords, numbers, punctuation
  # convert all words to lowercase
  add_stopwords <- c("applause", "laughter", "booing", "commercial", 
                     "mr", "governor", "senator", "crosstalk", "buzzer", 
                     "end", "video", "clip", "begin")
  t_dfm <- dfm(texts(t_corpus), removeTwitter = T,
               ignoredFeatures = c(add_stopwords, stopwords("SMART")))
  # After removing stopwords, some texts are blank. Remove docs from df and corpus
  t_dfm <- t_dfm[which(ntoken(t_dfm) != 0),]
  t_corpus$documents <- t_corpus$documents[which(ntoken(t_dfm) != 0),]

  # Now use the STM code to remove rare words
  # convert dfm for use with STM package functions
  out <- readCorpus(t_dfm, type = "Matrix")
  out$vocab <- features(t_dfm)
  # remove terms that occur in less than 3 tweets in entire corpus
  prep <- prepDocuments(out$documents, out$vocab, 
                        meta = t_corpus$documents, lower.thresh = lower.thresh)  
  # Redo the DFM with the smaller vocab
  t_dfm <- dfm(texts(t_corpus), keptFeatures=prep$vocab)
  t_dfm <- t_dfm[which(ntoken(t_dfm) != 0),]
  t_corpus$documents <- t_corpus$documents[which(ntoken(t_dfm) != 0),]  
  
  return(list(corpus = t_corpus, dfm = t_dfm))
}

debates <- df_to_corpus_dfm(debate.df, lower.thresh = 5)
debate_corpus <- debates[["corpus"]]
debate_dfm <- debates[["dfm"]]

debate_LDA_10 <- LDA(debate_dfm, k = 10, method='Gibbs', control=list(seed=1, burnin=100, thin=10, iter=5000))
debate_LDA_14 <- LDA(debate_dfm, k = 14, method='Gibbs', control=list(seed=1, burnin=100, thin=10, iter=5000))
debate_LDA_24 <- LDA(debate_dfm, k = 24, method='Gibbs', control=list(seed=1, burnin=100, thin=10, iter=5000))

print('10 LDA')
get_terms(debate_LDA_10, 10)
print('14 LDA')
get_terms(debate_LDA_14, 10)
print('24 LDA')
get_terms(debate_LDA_24, 10)

# 14 is best again.
# but still not as good as stm


#  trying merged rows
merged.debate.df <- parseMergedDebate()
# Since there are fewer documents, i'm lowering this to 2
merged_debates <- df_to_corpus_dfm(merged.debate.df, lower.thresh = 2)
merged_debate_corpus <- merged_debates[["corpus"]]
merged_debate_dfm <- merged_debates[["dfm"]]

merged_debate_LDA_14 <- LDA(merged_debate_dfm, k = 14, method='Gibbs', control=list(seed=1, burnin=100, thin=10, iter=5000))
get_terms(merged_debate_LDA_14, 10)

#doesn't look much different...

