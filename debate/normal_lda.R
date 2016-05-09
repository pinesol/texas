
rm(list=ls())
setwd("~/texas/debate/")
require(quanteda)
require(stm)
require(plyr)
require(dplyr)
require(ggplot2)
require(reshape2)
require(topicmodels)

source("./parse_debate.R")

debate.df <- parseDebateText()

df_to_corpus_dfm <- function(df, add_stopwords, lower.thresh = 3) {
  # remove twitter handles from tweets and hyperlinks
  df$text <- gsub("@\\w+ *", "", df$text)
  t_corpus <- corpus(df$text, docvars = df[,!names(df) %in% "text"])
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

# DFM: remove # symbol, @ symbol, RT, SMART English stopwords, numbers, punctuation
# convert all words to lowercase
debate_stopwords <- c("applause", "laughter", "booing", "commercial", 
                      "mr", "governor", "senator", "crosstalk", "buzzer", 
                      "end", "video", "clip", "begin", "thing", "things")
debates <- df_to_corpus_dfm(debate.df, debate_stopwords, lower.thresh = 5)
debate_corpus <- debates[["corpus"]]
debate_dfm <- debates[["dfm"]]

# 11 topics, because that's how many labeled topics there are
debate_LDA_11 <- LDA(debate_dfm, k = 11, method='Gibbs', control=list(alpha=0.1, seed=1, burnin=1000, thin=20, iter=30000))
# trying 15 and 20 for good measure
debate_LDA_15 <- LDA(debate_dfm, k = 15, method='Gibbs', control=list(alpha=0.1, seed=1, burnin=1000, thin=20, iter=30000))
debate_LDA_20 <- LDA(debate_dfm, k = 20, method='Gibbs', control=list(alpha=0.1, seed=1, burnin=1000, thin=20, iter=30000))

# examine the topics
print('11 LDA')
get_terms(debate_LDA_11, 10)
print('15 LDA')
get_terms(debate_LDA_15, 10)
print('20 LDA')
get_terms(debate_LDA_20, 10)

debate_LDA_15_names <- c('common core', 'mods1', 'foreign pol', 'social sec', 
                         'mods1', 'immigration', 'economics', 'border', 'budget', 
                         'Paul Ryan', 'military', 'mods2', 'gen election',
                         'iran', 'marriage')
debate_LDA_20_names <- c('mods1', '?', '?', '?', 'mods2', 'common core', 'mods3', '?', 'budget', 'marriage', 'social sec',
                         'gen election', 'military', '?', 'war', '?', 'iran', 'immigration', 'border')
# 15 is best again. 20 is not so bad tho. Just has more fluff. The war/isis category is better.

# Trying out a debate data frame where all the text for a single candidate is merged into one doc. 
merged.debate.df <- parseMergedDebate()
# Since there are fewer documents, i'm lowering this to 2
merged_debates <- df_to_corpus_dfm(merged.debate.df, debate_stopwords, lower.thresh = 2)
merged_debate_corpus <- merged_debates[["corpus"]]
merged_debate_dfm <- merged_debates[["dfm"]]

merged_debate_LDA_15 <- LDA(merged_debate_dfm, k = 15, 
                            method='Gibbs', 
                            control=list(alpha=0.1, seed=1, burnin=1000, thin=20, iter=30000))
get_terms(merged_debate_LDA_15, 10)

# This isn't great. the topics seem more muddled to gether.
# Using debate_LDA_15 from above.


# use the posterior for this LDA to create features on the twitter data.
# Using the non-merged LDA with 15 topics.

twitter.df <- parseTwitterData()
# No need to remove stop words, since the LDA only know about a small set of words.
# Just remove twitter handle names and HTML link in case it matches the debate text.
twitter.df$text <- gsub("@\\w+ *", "", twitter.df$text)
twitter_corpus <- corpus(twitter.df$text, docvars = twitter.df[,!names(twitter.df) %in% "text"])
# restricting to tweets with only the debate LDA words
debate_vocab <- as.character(debate_dfm@Dimnames[[2]])
twitter_dfm <- dfm(texts(twitter_corpus), removeTwitter = T, keptFeatures=debate_vocab)
# removing tweets with no words left in them
twitter_dfm <- twitter_dfm[which(ntoken(twitter_dfm) != 0),]
# only 10929 tweets left after filtering out those without the LDA words

# Use the poterior distribution of the debate LDA to get topics for the tweets
twitter.topics <- posterior(debate_LDA_15, twitter_dfm)
# twitter.topics$topics has the topic distribution for each tweet
top.twitter.topics <- apply(twitter.topics$topics, 1, which.max)
# these have names like "text 12745" corresponding to the tweet they belong to

tweet_indices <- as.numeric(substring(names(top.twitter.topics), 5))

# Adding the LDA 15 topic number to the twitter df. 
# Entries with a zero have no topic.
twitter.df$debate_topic <- integer(nrow(twitter.df))
twitter.df$debate_topic[tweet_indices] <- top.twitter.topics
# Adding the names of the topics to twitter.df
twitter.df$debate_topic_name <- character(nrow(twitter.df))
twitter.df$debate_topic_name[tweet_indices] <- debate_LDA_15_names[top.twitter.topics]

# Examine the distribution of debate topics. no topic is most popular, followed by moderator stuff.
hist(twitter.df$debate_topic)

save.image('~/texas/debate/debate_lda.RData')

# TODO Do the same logistic regression jackie did with that dfm, see how well it works in comparison.



