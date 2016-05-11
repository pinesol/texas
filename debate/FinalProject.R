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
require(reshape2)

source("./parse_debate.R")

# Read in the Twitter data from disk
twitter <- parseTwitterData()
View(twitter)

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

tweets <- df_to_corpus_dfm(twitter, c("rt", "gopdebate", "http", "https", "gopdebates", "t.co"))
twitter_corpus <- tweets[["corpus"]]
twitter_dfm <- tweets[["dfm"]]

dfm_to_stm <- function(corpus, dfm, lower.thresh = 5) {
  # convert dfm for use with STM package functions
  out <- readCorpus(dfm, type = "Matrix")
  out$vocab <- features(dfm)
  # remove terms that occur in less than 5 tweets in entire corpus
  out_stm <- prepDocuments(out$documents, out$vocab, 
                           meta = corpus$documents,
                           lower.thresh = lower.thresh)
}

out_stm <- dfm_to_stm(twitter_corpus, twitter_dfm) # Removes 12384 of 14516 terms from vocab

# How many topics to use for Twitter data? Search over possible values of K
# Warning: this takes a long time to run!
possible_k <- seq(10, 50, by=8)
model <- searchK(out_stm$documents, out_stm$vocab, K = possible_k,
                 prevalence = ~ candidate + subject_matter, 
                 max.em.its=30, emtol=5e-5, data = out_stm$meta, init.type = "Spectral")
plot(model) # best k seems to be 34 topics
best_k <- 34

fit_twitter_topic <- function(out_stm, k) {
  stm(out_stm$documents, out_stm$vocab, K = k, init.type="Spectral", 
      content = ~ candidate, # only one content covariate allowed
      prevalence = ~ candidate + subject_matter ,
      max.em.its=50, emtol=5e-5, data=out_stm$meta, seed=1100)
}

# Now let's fit the STM with 34 topics using candidate, subject matter, timestamp, and # retweets
# Warning: this takes a REALLY long time to run!
twitter_34 <- fit_twitter_topic(out_stm, 34)
twitter_42 <- fit_twitter_topic(out_stm, 42)
twitter_50 <- fit_twitter_topic(out_stm, 50)

# Save the current workspace to disk
save.image("~/Desktop/Text as Data/texas/debate/project_jackie.RData")
# Load the saved workspace from disk
load("~/Desktop/Text as Data/texas/debate/project_jackie.RData")
load("debate_lda.RData")

require(glmnet)

glm_stm_model <- function(stm_model, out_stm = out_stm) {
  pos.neg <- select(out_stm$meta, -tweet_created)
  pos.neg <- filter(pos.neg, sentiment != "Neutral")
  pos.neg <- droplevels(pos.neg)
  t50_reduced <- stm_model$theta[which(out_stm$meta$sentiment != "Neutral"),]
  t50_reduced <- as.data.frame(t50_reduced)
  t50_plus_candidate <- cbind(t50_reduced, candidate = pos.neg$candidate, 
                              sentiment = pos.neg$sentiment)
  t50_plus_candidate <- select(t50_plus_candidate, -V1)
  theta_sentiment <- glm(sentiment ~ . -candidate, data = t50_plus_candidate,
                         family = "binomial")
  theta_candidate_sentiment <- glm(sentiment ~ . , data = t50_plus_candidate,
                                   family = "binomial")
  print(anova(theta_sentiment, theta_candidate_sentiment, test="Chisq"))
  summary(theta_sentiment)
}
################################################################################

# LDA model for debate: debate_LDA_15
# topic names for debate: debate_LDA_15_names
# data frame for twitter: twitter.df
# dfm for twitter: twitter_dfm
# posterior topic distribution (LDA) = @gamma
# LDA model for twitter: use simple_lda_20, simple_lda_15, simple_lda_10
# LDA posterior for twitter using debate topics: twitter.topics$topics
all(nrow(twitter_dfm) == sum(twitter.df$debate_topic != 0),
    nrow(twitter_dfm)  == nrow(twitter.topics$topics))
table(twitter.df$debate_topic)
debate_LDA_15_names

pos.neg <- select(twitter.df[tweet_indices,], -tweet_created)
pos.neg <- filter(pos.neg, sentiment != "Neutral")
# pos.neg$candidate[pos.neg$candidate == "OTHER"] <- NA
# pos.neg$subject_matter[pos.neg$subject_matter == "None of the above"] <- NA
pos.neg <- droplevels(pos.neg)
levels(pos.neg$sentiment)
levels(pos.neg$candidate)
levels(pos.neg$subject_matter)
dropped.rows <- which(twitter.df[tweet_indices, "sentiment"] == "Neutral")
nrow(pos.neg) + length(dropped.rows) == nrow(twitter.topics$topics)
all(dim(simple_lda_15@gamma) == dim(twitter.topics$topics), 
    class(simple_lda_15@gamma) == class(twitter.topics$topics))
dim(simple_lda_25@gamma[-dropped.rows,])
dim(twitter.topics$topics[-dropped.rows,])
all(abs(rowSums(simple_lda_25@gamma) - 1) < 1e-10) 
all(abs(rowSums(twitter.topics$topics) - 1) < 1e-10) 

glm_lda_model <- function(lda_model_post, modified_data, 
                          predictors = c("candidate", "subject_matter")) {
    x <- lda_model_post[,-2] # need to drop one of the topics, I drop #2
    colnames(x) <- paste("topic", 1:(ncol(x)+1), sep=".")[-2]
    data <- cbind(modified_data, x)
    formula <- paste("sentiment ~ ", 
                    paste(c(colnames(x), predictors), collapse = " + "))
    fit <- glm(as.formula(formula) , data = data, family = "binomial")
    print(summary(fit))
    fit
}  
require(MASS)

stepwise_twitter <- function(lda_model_post, modified_data, 
                        predictors = c("candidate", "subject_matter")) {
  x <- lda_model_post # don't drop any topics
  colnames(x) <- paste("topic", 1:(ncol(x)), sep=".")
  data <- cbind(modified_data, x)
  formula <- paste("sentiment ~ ", 
                   paste(c(colnames(x), predictors), collapse = " + "))
  fit <- glm(as.formula(formula) , data = data, family = "binomial")
  stepAIC(fit, trace = FALSE)
}

step_25_candidate_subject <- stepwise_twitter(simple_lda_25@gamma[-dropped.rows,], 
                        pos.neg.sub, predictors = c("candidate", "subject_matter"))
step_25_candidate_subject$anova
summary(step_25_candidate_subject)

step_debate_topics <- stepwise_twitter(twitter.topics$topics[-dropped.rows,],  
                        pos.neg.sub, predictors = c("candidate", "subject_matter"))
step_debate_topics$anova
summary(step_debate_topics) # summarize the logistic regression model chosen by stepwise with AIC criterion

# compare AIC for model given 25 topics trained on twitter data and 15 topics trained on debate data
AIC(step_25_candidate_subject); AIC(step_debate_topics)
# likelihood ratio test based on the deviance statistic, model trained on twitter topics definitely better
anova(step_debate_topics, step_25_candidate_subject, test = "Chisq")

pos.neg.sub <- pos.neg[c("sentiment", "candidate", "subject_matter")]
levels(pos.neg.sub$candidate) <- c(levels(pos.neg.sub$candidate), "other")
pos.neg.sub$candidate <- relevel(pos.neg.sub$candidate, ref = "other")
pos.neg.sub$candidate[is.na(pos.neg.sub$candidate)] <- "other"
levels(pos.neg.sub$subject_matter) <- c(levels(pos.neg.sub$subject_matter), "other")
pos.neg.sub$subject_matter[is.na(pos.neg.sub$subject_matter)] <- "other"
pos.neg.sub$subject_matter <- relevel(pos.neg.sub$subject_matter, ref = "other")

dummy_candidate <- dummy(pos.neg.sub$candidate, 
                         levels(pos.neg.sub$candidate)[-1])
dummy_subject_matter <- dummy(pos.neg.sub$subject_matter, 
                         levels(pos.neg.sub$subject_matter)[-1])
candidate_only <- cv.glmnet(x = dummy_candidate, y = pos.neg.sub$sentiment, 
          family = "binomial", alpha = 1, nfolds = 10)
candidate_subject_only <- cv.glmnet(x = cbind(dummy_candidate, dummy_subject_matter), 
          y = pos.neg.sub$sentiment, family = "binomial", alpha = 1, nfolds = 10)
min(candidate_only$cvm)
min(candidate_subject_only$cvm)
coef(candidate_only, s="lambda.min")
coef(candidate_subject_only, s="lambda.min")

# yes, subject matter significantly improves the model compared to sentiment alone
# subjects that significantly predict sentiment are Immigration, Racial Issues, 
# Religion, Women's Issues, and other
require(arm)
require(coefplot)
candidate_only2 <- glm(sentiment ~ candidate, data = pos.neg.sub, family = "binomial")
candidate_subject_only2 <- glm(sentiment ~ candidate + subject_matter, data = pos.neg.sub, 
                               family = "binomial")
summary(candidate_only2)
rename_candidate <- c("other", gsub("candidate*", "", names(coef(candidate_only2))[2:11]))
names(rename_candidate) <- names(coef(candidate_only2))
arm::coefplot(candidate_only2, varnames = rename_candidate)
coefplot::coefplot(candidate_only2, sort="magnitude", newNames = rename_candidate)

summary(candidate_subject_only2)
rename_subject <- c("other", gsub("subject_matter*", "", names(coef(candidate_subject_only2))[12:22]))
names(rename_subject) <- names(coef(candidate_subject_only2))[c(1,12:22)]
#coefplot::coefplot(candidate_subject_only2, predictors = "subject_matter", sort="magnitude", newNames = rename_subject)
anova(candidate_only2, candidate_subject_only2, test = "Chisq")



candidate_glm <- as.data.frame(summary(candidate_only2)$coef)
p <- ggplot(candidate_glm, aes(y=Estimate)) 
p + geom_bar(position="dodge", stat="identity")

sentiment_twitter_candidate_10 <- glm_lda_model(simple_lda_10@gamma[-dropped.rows,] ,
                              modified_data = pos.neg.sub, predictors = "candidate")
sentiment_twitter_candidate_15 <- glm_lda_model(simple_lda_15@gamma[-dropped.rows,] ,
                              modified_data = pos.neg.sub, predictors = "candidate")
sentiment_twitter_candidate_20 <- glm_lda_model(simple_lda_20@gamma[-dropped.rows,] ,
                              modified_data = pos.neg.sub, predictors = "candidate")
sentiment_twitter_candidate_25 <- glm_lda_model(simple_lda_25@gamma[-dropped.rows,] ,
                              modified_data = pos.neg.sub, predictors = "candidate")
sentiment_twitter_candidate_30 <- glm_lda_model(simple_lda_30@gamma[-dropped.rows,] ,
                              modified_data = pos.neg.sub, predictors = "candidate")
sentiment_twitter_candidate_50 <- glm_lda_model(simple_lda_50@gamma[-dropped.rows,] ,
                              modified_data = pos.neg.sub, predictors = "candidate")
sort((c(k10 = simple_lda_10@loglikelihood, k15 = simple_lda_15@loglikelihood, 
            k20 = simple_lda_20@loglikelihood, k25 = simple_lda_25@loglikelihood, 
            k30 = simple_lda_30@loglikelihood, k50 = simple_lda_50@loglikelihood)), 
     decreasing = TRUE)

sort((c(k10 = AIC(sentiment_twitter_candidate_10), k15 = AIC(sentiment_twitter_candidate_15),
            k20 = AIC(sentiment_twitter_candidate_20), k25 = AIC(sentiment_twitter_candidate_25),
            k30 = AIC(sentiment_twitter_candidate_30), k50 = AIC(sentiment_twitter_candidate_50))))
sort(c(k10 = BIC(sentiment_twitter_candidate_10), k15 = BIC(sentiment_twitter_candidate_15),
       k20 = BIC(sentiment_twitter_candidate_20), k25 = BIC(sentiment_twitter_candidate_25),
       k30 = BIC(sentiment_twitter_candidate_30), k50 = BIC(sentiment_twitter_candidate_50)))

# choose 25 LDA topics from twitter based on AIC, BIC , log likelihood, Chi-square test ?

summary(sentiment_twitter_candidate_25)
anova(sentiment_twitter_candidate_20, sentiment_twitter_candidate_25, test="Chisq")
anova(sentiment_twitter_candidate_25, sentiment_twitter_candidate_50, test="Chisq")

sentiment_debate_candidate <- glm_lda_model(twitter.topics$topics[-dropped.rows,] ,
                                modified_data = pos.neg.sub, predictors = "candidate")
c(AIC(sentiment_twitter_candidate_25), BIC(sentiment_twitter_candidate_25))
c(AIC(sentiment_debate_candidate), BIC(sentiment_debate_candidate)) # debate is very similar!

################################################################################

#theta_sentiment <- cv.glmnet(x = t50_reduced, y = pos.neg$sentiment, 
#                          family = "binomial", alpha = 1, nfolds = 10)

# train a normal topic model (not stm)
simple_lda_50 <- LDA(twitter_dfm, 50, method='Gibbs', control=list(seed=1, burnin=100, thin=10, iter=5000))
print('50 LDA')
get_terms(simple_lda_50, 10)

simple_lda_20 <- LDA(twitter_dfm, 20, method='Gibbs', control=list(seed=1, burnin=100, thin=10, iter=5000))
print('20 LDA')
get_terms(simple_lda_20, 10)

simple_lda_25 <- LDA(twitter_dfm, 25, method='Gibbs', control=list(seed=1, burnin=100, thin=10, iter=5000))

simple_lda_30 <- LDA(twitter_dfm, 30, method='Gibbs', control=list(seed=1, burnin=100, thin=10, iter=5000))

simple_lda_15 <- LDA(twitter_dfm, 15, method='Gibbs', control=list(seed=1, burnin=100, thin=10, iter=5000))
print('15 LDA')
get_terms(simple_lda_15, 10)

simple_lda_10 <- LDA(twitter_dfm, 10, method='Gibbs', control=list(seed=1, burnin=100, thin=10, iter=5000))
print('10 LDA')
get_terms(simple_lda_10, 10)


# Debate STM


debate.df <- parseDebateText()
levels(debate.df$speaker)
levels(twitter$candidate)
debate.df$is.candidate <- ifelse(debate.df$speaker == "MODERATOR" | debate.df$speaker == "OTHER", 0, 1)
# Stand-in for timestamp. Number from 0 to 1 -- normalized rank of snippet from start to finish
debate.df$rough.order <- as.numeric(rownames(debate.df))/nrow(debate.df)

debates <- df_to_corpus_dfm(debate.df, c("applause", "laughter", "booing", "commercial", 
                      "mr", "governor", "senator"))
debate_corpus <- debates[["corpus"]]
debate_dfm <- debates[["dfm"]]

features(debate_dfm)

out_debate_stm <- dfm_to_stm(debate_corpus, debate_dfm, lower.thresh = 3) # Removes 1810 of 2216 terms from vocab
out_debate_stm$vocab

# How many topics to use for debate data? Search over possible values of K
# Warning: this takes a long time to run!
possible_k_debate <- 5:15
# TODO re-run this, is.candidate wasn't populated correctly before!
model_debate <- searchK(out_debate_stm$documents, out_debate_stm$vocab, K = possible_k_debate,
                  prevalence = ~ speaker,
                  data = out_debate_stm$meta, init.type = "Spectral", emtol=5e-5)
plot(model_debate) # no idea how to interpret and choose K
best_k_debate <- 8 

fit_debate_topic <- function(out_stm, k) {
  stm(out_stm$documents, out_stm$vocab, K = k, init.type="Spectral", 
      content = ~ speaker, # only one content covariate allowed
      prevalence = ~ speaker ,
      max.em.its=30, emtol=5e-5, data=out_stm$meta, seed=1100)
}

debate_8 <- fit_debate_topic(out_debate_stm, 8)
debate_10 <- fit_debate_topic(out_debate_stm, 10)
debate_15 <- fit_debate_topic(out_debate_stm, 15)

topics_describe_debate <- t(labelTopics(debate_8, n=15)$topics)
topics_describe_debate <- t(labelTopics(debate_10, n=15)$topics)
topics_describe_debate
labelTopics(debate_8, n=15)
labelTopics(debate_15, n=15)

debate_LDA_6 <- LDA(debate_dfm, k = 6, method = "Gibbs", 
                    control=list(seed=1500, burnin=500, thin=10, iter=10000))

debate_LDA_8 <- LDA(debate_dfm, k = 8, method = "Gibbs", 
                     control=list(seed=1500, burnin=500, thin=10, iter=10000))
debate_LDA_10 <- LDA(debate_dfm, k = 10, method = "Gibbs", 
                     control=list(seed=1500, burnin=500, thin=10, iter=10000))
get_terms(debate_LDA_8, k = 15)
get_terms(debate_LDA_10, k = 15)
get_terms(debate_LDA_6, k = 15)
beta_terms <- debate_LDA_10@beta
names(beta_terms) <- features(debate_dfm)



debate_LDA_15_names[2] <- "mods3"
topic_theta_by_speaker <- data.frame(debate_LDA_15@gamma, speaker = debate_corpus$documents$speaker)
# come up with descriptive names for topics
colnames(topic_theta_by_speaker) <- c(debate_LDA_15_names, "speaker")
grouped <- group_by(topic_theta_by_speaker, speaker)
topic_means_by_speaker <- as.data.frame(grouped %>% summarize_each(funs(mean)))
melted <- melt(topic_means_by_speaker, id.vars = "speaker")
melted.candidate <- filter(melted, speaker != "OTHER" & speaker != "MODERATOR")
p <- ggplot(melted.candidate, aes(x = speaker, y = value, fill = variable)) 
p <- p + geom_bar(stat="identity") 
p <- p + theme(axis.text.x=element_text(angle = 90, vjust = 0.5))
p <- p + labs(fill = "Topic", x = "Candidate", y = "Mean Theta by Topic")
p

# TODO(jackie)
# TODO choose a twitter topic model you think is best
# TODO Use the gamma vector for each doc as a predictor of sentiment, do a logistic regression
# TODO make a pretty graph
# TODO Report the best predictor topics/words for sentiment. It will probably be candidate names, e.g.
#     "trump" is a good predictor of negative sentiment.
# TODO do this two more times, once on the 'live' tweets, and once one the 'reaction' tweets.
# TODO report if there are any differences. Do the reaction tweets talk about policy more?



# TODO(alex)
# TODO(project): Find the best STM for the debate.
# TODO Identify which topics make sense, and which ones are noise.
# TODO Pick titles for the sensical topics, like we did in the homework
# TODO if very few make sense, we should try fewer topics, or decide topic models don't work well here.
# TODO Identify the top two topics for every debate snippet, along with their confidence scores. We'll 
#      use this later for sure.
# TODO Make the graphs that we made in the last homework, comparing topics for different candidates,
#      and topic prevalence over time (if it's not too hard, not clear we'll use this)
# TODO associate these with the sentiment scores in the text.

# TODO Pick the best policy terms from the debate STM. 
# TODO find the tweets with these terms. Make a DFM from those tweets with the vocab being the terms from the STM.
# TODO fit a logistic regression from the DRM rows to the sentiment score.

sent.topic3 <- summarize(group_by(topic3, candidate), mean(sentiment0))
sent.topic4 <- summarize(group_by(topic4, candidate), mean(sentiment0))
sent.topic9 <- summarize(group_by(topic9, candidate), mean(sentiment0))
colnames(sent.topic3) <- c("candidate", "sentiment")
colnames(sent.topic4) <- c("candidate", "sentiment")
colnames(sent.topic9) <- c("candidate", "sentiment")
t3 <- ggplot(sent.topic3, aes(x=candidate, y=sentiment)) + geom_bar(stat="identity") + 
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5)) + labs(title = "Foreign Policy")
t4 <- ggplot(sent.topic4, aes(x=candidate, y=sentiment)) + geom_bar(stat="identity") + 
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5)) + labs(title = "Social Security")
t9 <- ggplot(sent.topic9, aes(x=candidate, y=sentiment)) + geom_bar(stat="identity") + 
  theme(axis.text.x=element_text(angle = 90, vjust = 0.5))+ labs(title = "Budget")

filter(topic3, candidate == "Jeb Bush" & sentiment == "Negative")$text[6]
filter(topic4, candidate == "Jeb Bush" & sentiment == "Negative")$text[c(8, 2)]
filter(topic9, candidate == "Chris Christie" & sentiment == "Negative")$text[2]


t3
t4
t9

