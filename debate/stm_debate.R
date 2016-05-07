# TODO(alex)
# TODO(project): Find the best STM for the debate.
# DONE Identify which topics make sense, and which ones are noise.
# DONE Pick titles for the sensical topics, like we did in the homework
# TODO Identify the top two topics for every debate snippet, along with their confidence scores. We'll 
#      use this later for sure.
# TODO Make the graphs that we made in the last homework, comparing topics for different candidates,
#      and topic prevalence over time (if it's not too hard, not clear we'll use this)
# TODO associate these with the sentiment scores in the text.

# TODO Pick the best policy terms from the debate STM. 
# TODO find the tweets with these terms. Make a DFM from those tweets with the vocab being the terms from the STM.
# TODO fit a logistic regression from the DFM rows to the sentiment score.

# TODO concat all the docs from the same speaker together?

rm(list=ls())
setwd("~/texas/debate/")
require(quanteda)
require(stm)
require(plyr)
require(dplyr)
require(ggplot2)
require(reshape2)

source("./parse_debate.R")

df_to_corpus_dfm <- function(df, add_stopwords) {
  # remove twitter handles from tweets and hyperlinks
  df$text <- gsub("@\\w+ *", "", df$text)
  t_corpus <- corpus(df$text, docvars = df[,!names(df) %in% "text"])
  # DFM: remove # symbol, @ symbol, RT, SMART English stopwords, numbers, punctuation
  # convert all words to lowercase
  t_dfm <- dfm(texts(t_corpus), removeTwitter = T,
               ignoredFeatures = c(add_stopwords, stopwords("SMART")))
  # After removing stopwords, some texts are blank. Remove docs from df and corpus
  t_dfm <- t_dfm[which(ntoken(t_dfm) != 0),]
  t_corpus$documents <- t_corpus$documents[which(ntoken(t_dfm) != 0),]
  return(list(corpus = t_corpus, dfm = t_dfm))
}

dfm_to_stm <- function(corpus, dfm, lower.thresh = 5) {
  # convert dfm for use with STM package functions
  out <- readCorpus(dfm, type = "Matrix")
  out$vocab <- features(dfm)
  # remove terms that occur in less than 5 tweets in entire corpus
  out_stm <- prepDocuments(out$documents, out$vocab, 
                           meta = corpus$documents,
                           lower.thresh = lower.thresh)
}

debate.df <- parseDebateText()
debate.df$is.candidate <- ifelse(debate.df$speaker == "MODERATOR" | debate.df$speaker == "OTHER", 0, 1)
# Stand-in for timestamp. Number from 0 to 1 -- normalized rank of snippet from start to finish
debate.df$rough.order <- as.numeric(rownames(debate.df))/nrow(debate.df)

debates <- df_to_corpus_dfm(debate.df, c("applause", "laughter", "booing", "commercial", 
                                         "mr", "governor", "senator"))
debate_corpus <- debates[["corpus"]]
debate_dfm <- debates[["dfm"]]

features(debate_dfm)

# Train STM

out_debate_stm <- dfm_to_stm(debate_corpus, debate_dfm, lower.thresh = 3) # Removes 1810 of 2216 terms from vocab
out_debate_stm$vocab

# STM 1: Speaker = Prevalence, no content covariate

# How many topics to use for debate data? Search over possible values of K
possible_k_debate <- c(10, 12, 14, 16, 18, 20, 22, 24, 26, 28, 30)

# Note: content covariate doesn't work here for some reason...
model_debate <- searchK(out_debate_stm$documents, out_debate_stm$vocab, K = possible_k_debate,
                        prevalence = ~ speaker,
                        data = out_debate_stm$meta, init.type = "Spectral", emtol=5e-5)
plot(model_debate) # no idea how to interpret and choose K

# Results:
# liklihood and coherence are jagged
# highest likelihood at 24
# residuals and lower bound increase with the number of topics

fit_debate_topic <- function(out_stm, k) {
  stm(out_stm$documents, out_stm$vocab, K = k, init.type="Spectral", 
      content = ~ speaker, # only one content covariate allowed
      prevalence = ~ speaker ,
      max.em.its=30, emtol=5e-5, data=out_stm$meta, seed=1100)
}

# Fit topic models with different values of K
debate_10 <- fit_debate_topic(out_debate_stm, 10)
debate_14 <- fit_debate_topic(out_debate_stm, 14)
debate_24 <- fit_debate_topic(out_debate_stm, 24)

# Examine topic words
topics_describe_debate_10 <- t(labelTopics(debate_10, n=15)$topics)
topics_describe_debate_10
# 10 topics: None are totally coherent
topics_describe_debate_14 <- t(labelTopics(debate_14, n=15)$topics)
topics_describe_debate_14
# 14 topics: These are better. Clear topics for obama, immigration, foreign affiars, elecion rhetoric, abortion, social security, etc
topics_describe_debate_24 <- t(labelTopics(debate_24, n=15)$topics)
topics_describe_debate_24
# 24 topics: Similar to 14 topics, but with more noise. 
# 14 topics is best

# This shows topics per candidate, and topic-candidate pair words.
labelTopics(debate_14, n=15)
# not very informative

# debate_14 topic names
# 1) Obama's economic mismanagement
# 2) Immigration
# 3) Mexican Border law
# 4) ?
# 5) America's Enemies
# 6) Moderator lead-ins
# 7) Democracy
# 8) Israel ally
# 9) Pro-life
# 10) Foreign relations with the middle east
# 11) Social Security reform
# 12) The general election
# 13) Taxes and business?
# 14) Iran deal

# 2: Regular LDA

debate_LDA_10 <- LDA(debate_dfm, k = 10, method = "Gibbs", 
                     control=list(seed=1500, burnin=500, thin=10, iter=10000))
debate_LDA_14 <- LDA(debate_dfm, k = 14, method = "Gibbs", 
                    control=list(seed=1500, burnin=500, thin=10, iter=10000))
debate_LDA_24 <- LDA(debate_dfm, k = 24, method = "Gibbs", 
                    control=list(seed=1500, burnin=500, thin=10, iter=10000))
get_terms(debate_LDA_10, k = 15)
# seems less clear than stm. more people mentioned than stm.
get_terms(debate_LDA_14, k = 15)
# A little better. 
get_terms(debate_LDA_24, k = 15)
# Lots of topics still coflated. e.g. 24 has 'common core' and 'isis'

# 14 is bets again.
# more words per topic than stm, but more muddled topics. More people's names than STM.

# Topics:
# 1) Iran deal
# 2) Debate procedure
# 3) Chris's Cristie's econ
# 4) ?
# 5) social security
# 6) foreign policy-ish?
# 7) polician names?
# 8) pro-life
# 10) obamacare's effects on the economy
# 11) ?
# 12) Immigration
# 13) ?
# 14) ?

# Going with the 14-topic STM
topics_describe_debate_14

final_topics <- c("Obama's economy", "Immigration", "Mexican immigration", "?", "America's Enemies",
                  "Moderator lead-ins", "Democracy", "Israeli relationship?", "Pro-Life",
                  "US and the middle east", "Social security reform", "the general election",
                  "?", "Iran deal")


out_debate_stm$meta$speaker <- as.factor(out_debate_stm$meta$speaker)
##pick specifcation
prep<-estimateEffect(1:length(final_topics) ~ speaker , debate_14, meta=out_debate_stm$meta)
##plot effects
topic_labels <- c("Obama", "Immigration", "Mexico", "?", "Enemies",
                  "Moderators", "Democracy", "Israel", "Pro-Life",
                  "mideast", "Social security", "general",
                  "?", "Iran")
plot.estimateEffect(prep, covariate="speaker", topics=1:length(final_topics), model=out_debate_stm, method="difference", 
                    cov.value1 = "Donald Trump", cov.value2 = "Jeb Bush",
                    xlab = "More Trump......More Bush", xlim=c(-.1, .1),
                    labeltype='custom', verboseLabels=F, custom.labels=topic_labels)
# shows which person talks about which thing more.


