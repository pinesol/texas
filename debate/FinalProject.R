# Jacqueline Gutman
# Alex Pine
# Text as Data Final Project

rm(list=ls())
setwd("~/Desktop/Text as Data/texas/debate/")
require(quanteda)
require(stm)
require(plyr)
require(dplyr)
require(ggplot2)

# Read in the Twitter data from disk
twitter_full <- read.csv("GOP_REL_ONLY.csv", stringsAsFactors = F)
View(twitter_full)
# Let's discard the columns we won't need to use for simplicity
twitter <- twitter_full[, c("candidate", "sentiment", "subject_matter", 
                            "retweet_count", "text", "tweet_created")]
# Change column data formats
twitter$candidate <- as.factor(twitter$candidate)
twitter$sentiment <- as.factor(twitter$sentiment)
twitter$subject_matter <- as.factor(twitter$subject_matter)
# In creating these timestamps, we discarded timezone data. Worth it to include?
twitter$tweet_created <- strptime(twitter$tweet_created, "%m/%d/%y %H:%M")
twitter$candidate <- mapvalues(twitter$candidate, from=c("", "No candidate mentioned"), to=c("OTHER", "OTHER"))
twitter$subject_matter <- mapvalues(twitter$subject_matter, from=c(""), to=c("None of the above"))
View(twitter)

# Create quanteda corpus object for twitter data with appropriate metadata
text_col <- which(colnames(twitter) == "text")
twitter_corpus <- corpus(twitter$text, docvars = twitter[,-text_col])
colnames(twitter_corpus$documents)
# DFM: remove # symbol, @ symbol, RT, English stopwords, numbers, punctuation
# convert all words to lowercase
twitter_dfm <- dfm(texts(twitter_corpus), removeTwitter = T,
                   ignoredFeatures = c("rt", stopwords("english")))
# convert dfm for use with STM package functions
out <- readCorpus(twitter_dfm, type = "Matrix")
out$vocab <- features(twitter_dfm)
# remove terms that occur in less than 5 tweets in entire corpus
out_stm <- prepDocuments(out$documents, out$vocab, 
              meta = twitter_corpus$documents,
              lower.thresh = 5) # Removes 15684 of 18303 terms from vocab

# How many topics to use for Twitter data? Search over possible values of K
# Warning: this takes a long time to run!
possible_k <- seq(10, 50, by=8)
model <- searchK(out_stm$documents, out_stm$vocab, K = possible_k,
                 prevalence = ~ candidate + subject_matter + as.numeric(tweet_created), 
                 max.em.its=30, emtol=5e-5, data=out_stm$meta, init.type = "Spectral")
plot(model) # best k seems to be 34 topics
best_k <- possible_k[which.min(model$results$residual)]

# Now let's fit the STM with 34 topics using candidate, subject matter, timestamp, and # retweets
# Warning: this takes a REALLY long time to run!
twitter_34 <- stm(out_stm$documents, out_stm$vocab, K = best_k, init.type="Spectral", 
                 content = ~ subject_matter, # only one content covariate allowed
                 prevalence = ~ candidate + subject_matter + as.numeric(tweet_created), 
                 max.em.its=30, emtol=5e-5, data=out_stm$meta, seed=1100)
# Model Terminated Before Convergence Reached 

# Save the current workspace to disk
save.image("~/Desktop/Text as Data/texas/debate/project_jackie.RData")
# Load the saved workspace from disk
load("~/Desktop/Text as Data/texas/debate/project_jackie.RData")

source("./parse_debate.R")
debate.df$snippets <- as.character(debate.df$snippets)
levels(debate.df$speakers)
levels(twitter$candidate)

# Map the debate speakers to names that correspond to Twitter candidate values
debate.df$speakers <- revalue(debate.df$speakers, c("(UNKNOWN)"="OTHER", "BAIER"="MODERATOR", "BUSH"="Jeb Bush", 
            "CARSON"="Ben Carson", "MEGAN"="MODERATOR", "PAUL"="Rand Paul",
            "CHRISTIE"="Chris Christie", "COMMERCIAL"="OTHER", "CRUZ"="Ted Cruz", "FIORINA"="OTHER", 
            "HUCKABEE"="Mike Huckabee", "KASICH"="John Kasich", "KELLY"="MODERATOR", "MALE"="OTHER",
            "PERRY"="OTHER", "QUESTION"="OTHER", "RUBIO"="Marco Rubio", "TRUMP"="Donald Trump",
            "UNIDENTIFIED FEMALE"="OTHER", "UNIDENTIFIED MALE"="OTHER", "WALKER"="Scott Walker",
            "WALKRE"="Scott Walker", "WALLACE"="MODERATOR", "WALLCE"="MODERATOR"))

# 0 if speaker is MODERATOR or OTHER, 1 if speaker is one of the 10 candidates
debate.df$is.candidate <- ifelse((debate.df$speakers == "MODERATOR" || 
                                    debate.df$speakers == "OTHER"), 0, 1)
# Stand-in for timestamp. Number from 0 to 1 -- normalized rank of snippet from start to finish
debate.df$rough.order <- as.numeric(rownames(debate.df))/nrow(debate.df)
# Some text cleaning: Add spaces before and after periods, strip parentheses and add spaces
debate.df$snippets <- gsub("\\.", " . ", debate.df$snippets)
debate.df$snippets <- gsub("[()]", " ", debate.df$snippets)

# Create quanteda corpus object for debate data with appropriate metadata
text_col2 <- which(colnames(debate.df) == "snippets")
debate_corpus <- corpus(debate.df$snippets, docvars = debate.df[,-text_col2])
colnames(debate_corpus$documents)
# DFM: remove APPLAUSE, LAUGHTER, BOOING, English stopwords, numbers, punctuation
# convert all words to lowercase
debate_dfm <- dfm(texts(debate_corpus),
              ignoredFeatures = c("applause", "laughter", "booing", stopwords("english")))
features(debate_dfm)
# after removing stopwords, some snippets are blank. remove these from dfm and corpus
drop.rows <- which(ntoken(debate_dfm) == 0)
debate_dfm <- debate_dfm[-drop.rows,]
debate_corpus$documents <- debate_corpus$documents[-drop.rows,]

# convert dfm for use with STM package functions
out_debate <- readCorpus(debate_dfm, type = "Matrix")
out_debate$vocab <- features(debate_dfm)
# remove terms that occur in less than 5 tweets in entire corpus
out_debate_stm <- prepDocuments(out_debate$documents, out_debate$vocab, 
                         meta = debate_corpus$documents,
                         lower.thresh = 2) # Removing 1720 of 2443 terms
# using lower threshold of 2 leaves 5 empty documents
drop.docs <- out_debate_stm$docs.removed

# How many topics to use for debate data? Search over possible values of K
# Warning: this takes a long time to run!
possible_k_debate <- 5:15
model_debate <- searchK(out_debate_stm$documents, out_debate_stm$vocab, K = possible_k_debate,
                  prevalence = ~ speakers + is.candidate + s(rough.order),
                  data = out_debate_stm$meta, init.type = "Spectral", emtol=5e-5)
plot(model_debate) 
best_k_debate <- 10