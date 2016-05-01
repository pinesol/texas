#install.packages("lda")
#install.packages("topicmodels")
#install.packages("stm")
#install.packages("LDAvis")

require(quanteda, warn.conflicts = FALSE, quietly = TRUE)
library(topicmodels)

data(immigNewsCorpus, package = "quantedaData")

summary(immigNewsCorpus, 5)
# Get 4 most common newspapers
topPapers <- sort(table(immigNewsCorpus[["paperName"]]), decreasing = TRUE)
reducedCorpus <- subset(immigNewsCorpus, paperName %in% names(topPapers)[1:4])
table(reducedCorpus[["paperName"]])

# Creates custom_stopwords vector
load('~/texas/hw3/custom_stopwords.RData')

news_dfm <- dfm(reducedCorpus, ignoredFeatures = custom_stopwords)
news_dfm <- trim(news_dfm, minCount=30, minDoc=20)

NUM_TOPICS <- 30
#iterations? seed?
# TODO good values for list(seed = SEED, burnin = 3,thin = 30, iter = 30))?
# TODO why thin?
news_lda <- LDA(news_dfm, NUM_TOPICS, method='Gibbs', control=list(seed=2, burnin=100, thin=10, iter=1000))
get_terms(news_lda, 10)

# Topic 1   Topic 2     Topic 3      Topic 4  Topic 5   
# "war"     "ukip"      "eu"         "family" "english" 
# "world"   "farage"    "european"   "life"   "language"
# "prince"  "party"     "europe"     "back"   "care"    
# "british" "racist"    "britain"    "father" "home"    
# "history" "leader"    "cameron"    "mother" "health"  
# "charles" "nigel"     "referendum" "wife"   "test"    
# "royal"   "campaign"  "union"      "years"  "speak"   
# "tour"    "comments"  "british"    "day"    "nhs"
# "time"    "european"  "merkel"     "home"   "years"
# "great"   "candidate" "countries"  "couple" "british"

# Topic 1: British Royalty
# Topic 2: UK Independence Party
# Topic 3: Brexit
# Topic 4: Family
# Topic 5: Domestic Policy

# Problem 1f

##Store the results of the distribution of topics over documents

# subset the dfm to guardian and daily mail
papers <- reducedCorpus$documents$paperName

# function that finds the second max
which.max2 <- function(x) {
  max(which(x == sort(x, partial=(NUM_TOPICS-1))[NUM_TOPICS-1]))
}

all_dates <- as.Date(as.numeric(reducedCorpus$documents$day) - 1, origin = "2015-01-01")

guardian_topic_dist <- news_lda@gamma[which(papers == 'guardian'),]
top_guardian_topics <- apply(guardian_topic_dist, 1, which.max)
second_top_guardian_topics <- apply(guardian_topic_dist, 1, which.max2)
guardian_dates <- as.Date(as.numeric(reducedCorpus$documents$day[which(papers == 'guardian')]) - 1, 
                          origin = "2015-01-01")
guardian_topics <- data.frame(top_guardian_topics, second_top_guardian_topics, guardian_dates)

mail_topic_dist <- news_lda@gamma[which(papers == 'mail'),]
top_mail_topics <- apply(mail_topic_dist, 1, which.max)
second_top_mail_topics <- apply(mail_topic_dist, 1, which.max2)
mail_dates <- as.Date(as.numeric(reducedCorpus$documents$day[which(papers == 'mail')]) - 1, 
                          origin = "2015-01-01")
mail_topics <- data.frame(top_mail_topics, second_top_mail_topics, mail_dates)

# plot

# TODO WTF is max.1.162??? max222.163.324.???

# ####plot
# z<-ggplot(gov2, aes(x=index, y=max.1.162., pch="First")) 
# 
# z + geom_point(aes(x=index, y=max222.1.162., pch="Second") ) +theme_bw() + ylab("Topic Number")  + ggtitle("Government")  + 
#   xlab(NULL) + theme(axis.ticks = element_blank(), axis.text.x = element_blank()) + geom_point() + 
#   geom_vline(xintercept=57) +
#   geom_vline(xintercept=143)  +
#   geom_vline(xintercept=114, linetype=2) +
#   scale_shape_manual(values=c(18, 1), name = "Topic Rank") 
# 
# z<-ggplot(opp2, aes(x=index, y=max.163.324., pch="First")) 
# 
# z + geom_point(aes(x=index, y=max222.163.324., pch="Second") ) + ylab("Topic Number")+theme_bw()   + ggtitle("Opposition")  +
#   xlab(NULL) + theme(axis.ticks = element_blank(), axis.text.x = element_blank()) + geom_point() + 
#   geom_vline(xintercept=57) +
#   geom_vline(xintercept=143)  +
#   geom_vline(xintercept=114, linetype=2) +  scale_shape_manual(values=c(18, 1), name = "Topic Rank") 

# TODO Problem 1g
# Average contribution by a topic to a newspaper.

# docs by topics matrix. each row is a histogram over the topics.
first5_lda <- news_lda@gamma[,1:5]

avgTopicProportions <- function(paper) {
  papers <- reducedCorpus$documents$paperName
  paper_first5_lda <- first5_lda[which(papers == paper),]
  ave_topic_proportions <- colSums(paper_first5_lda) / length(papers)
  names(ave_topic_proportions) <- c('British Royalty', 'UKIP', 'Brexit', 'Family', 'Domestic Policy')
  return(ave_topic_proportions)
}
avgTopicProportions('guardian')
# British Royalty            UKIP          Brexit          Family Domestic Policy 
# 0.006119630     0.010324730     0.008053529     0.008207289     0.006613840  
# liberal paper, UKIP largest, Royalty lowest

avgTopicProportions('mail')
# British Royalty            UKIP          Brexit          Family Domestic Policy 
# 0.007216055     0.011378225     0.007308273     0.012038282     0.008992935
# conservative paper, Family largest, Royalty lowest

avgTopicProportions('telegraph')
# British Royalty            UKIP          Brexit          Family Domestic Policy 
# 0.008811142     0.012429343     0.011735386     0.008636098     0.007014645 
# UKIP Largest, Domestic Policy Lowest

avgTopicProportions('times')
# British Royalty            UKIP          Brexit          Family Domestic Policy 
# 0.005364085     0.009009731     0.008006753     0.007744099     0.005294511
# UKIP Largest, Domestic Policy Lowest 

# TODO problem 1h

install.packages("LDAvis")
library(LDAvis)

# TODO the example given in topicmodelExamples2.R uses the lda library, not the topic models library
# could you give us an example of how to use the LDAvis library with the topicsmodels library?


# Using this library is not obvious at all
# example: http://cpsievert.github.io/LDAvis/reviews/reviews.html

# Problem 2 Topic Stability
news_lda_2 <- LDA(news_dfm, NUM_TOPICS, method='Gibbs', control=list(seed=3, burnin=100, thin=10, iter=1000))

# Problem 2b

library(lsa) # For the cosine function

closestTopicMap <- function(lda_1, lda_2) {
  # take the distribution of words for each topic from the first model, and exponentiate it to get probabilities.
  words_dist_1 <- exp(lda_1@beta)
  words_dist_2 <- exp(lda_2@beta)
  
  # given a single topics's word distribution, find all topic from the second lda that most closely matches it.
  closestTopic <- function(words_dist_row) {
    # bind the first arg of cosine similarity to the input word distribution.
    cosine_fn <- function(y) { 
      cosine(words_dist_row, y) 
    }
    # Get the cosine similarity to each, and return the index of the maximum element.
    which.max(apply(words_dist_2, 1, cosine_fn))    
  }
  apply(words_dist_1, 1, closestTopic)
}
lda_2_closest_topics <- closestTopicMap(news_lda_2, news_lda)
for (i in 1:length(lda_2_closest_topics)) {
  print(paste('Topic', i, '->', lda_2_closest_topics[i]))
}

# Topic 1 -> 19
# Topic 2 -> 18
# Topic 3 -> 12
# Topic 4 -> 10
# Topic 5 -> 15
# Topic 6 -> 11
# Topic 7 -> 4
# Topic 8 -> 2
# Topic 9 -> 25
# Topic 10 -> 1
# Topic 11 -> 9
# Topic 12 -> 21
# Topic 13 -> 7
# Topic 14 -> 20
# Topic 15 -> 17
# Topic 16 -> 9
# Topic 17 -> 25
# Topic 18 -> 29
# Topic 19 -> 4
# Topic 20 -> 22
# Topic 21 -> 8
# Topic 22 -> 23
# Topic 23 -> 26
# Topic 24 -> 5
# Topic 25 -> 28
# Topic 26 -> 27
# Topic 27 -> 16
# Topic 28 -> 3
# Topic 29 -> 13
# Topic 30 -> 30

# Problem 2c

# closest_topics maps topics from lda_1 to the most similar topics in lda_2
avg_terms_in_common <- function(lda_1, lda_2, closest_topics) {
  num_shared <- sapply(1:length(closest_topics),
                       function(i) {
                         length(intersect(terms(lda_1, 10)[,i], 
                                          terms(lda_2, 10)[,closest_topics[i]])) })
  mean(num_shared)
}

avg_terms_in_common(news_lda_2, news_lda, lda_2_closest_topics)
# returns 6.666667 

# Problem 2d

small_lda_1 <- LDA(news_dfm, 10, method='Gibbs', control=list(seed=4, burnin=100, thin=10, iter=1000))
small_lda_2 <- LDA(news_dfm, 10, method='Gibbs', control=list(seed=5, burnin=100, thin=10, iter=1000))

small_closest_topics <- closestTopicMap(small_lda_1, small_lda_2)
avg_terms_in_common(small_lda_1, small_lda_2, small_closest_topics)
# Returns 5.6

# 10 topics are less stable than 30

# Problem 3

# subset the dfm to guardian and daily mail
mailGuardCorpus <- subset(immigNewsCorpus, paperName %in% c('mail', 'guardian'))
table(mailGuardCorpus[["paperName"]])

paper <- mailGuardCorpus$documents$paperName
text <- mailGuardCorpus$documents$texts
# TODO chose 2015 arbitrarily.
date <- as.Date(as.numeric(mailGuardCorpus$documents$day) - 1, origin = "2015-01-01")
mailGuard.df <- data.frame(paper, text, date)

processed_corpus <- textProcessor(mailGuard.df$text, metadata=mailGuard.df, 
                                  customstopwords=custom_stopwords)

##remove some words for speed purposes
# TODO wtf
# TODO TODO ERROR 
# "Error in prepDocuments(processed$documents, processed_corpus$vocab, processed_corpus$meta,  : 
# Your documents object has more unique features than your vocabulary file has entries."
out_20 <- prepDocuments(processed$documents, processed_corpus$vocab, processed_corpus$meta, lower.thresh=20)

# TODO wtf is the "spline of the date variable"? This is a guess.

# NOTE reduce max EM iterations to 5 from 30 to make it faster.

fitSpec50 <- stm(out_20$documents, out_20$vocab, K=0, init.type="Spectral", 
                 content=~paper, prevalence = ~paper + smooth.spline(date), 
                 max.em.its=1, data=out_20$meta, seed=5926696)

# top 5 topics?
# visualization

# Problem 4
sotu_dfm <- dfm(subset(inaugCorpus, Year>1900))

nixon_index <- which(sotu_dfm@Dimnames$docs == "1969-Nixon")
obama_index <- which(sotu_dfm@Dimnames$docs == "2009-Obama")

# are these indices correct?
df_fit <- textmodel_wordfish(sotu_dfm, c(nixon_index, obama_index))

#TODO what are these magic numbers???


#plot(year[1:23], df_fit@theta[1:23])

#points(year[24:46], df_fit@theta[24:46], pch=8)
#?plot

#plot(as.factor(party), df_fit@theta)


##most important features--word fixed effects


#words<-df_fit@psi
#names(words) <- df_fit@features

#sort(words)[1:50]

#sort(words, decreasing=T)[1:50]

##guitar plot


#weights<-df_fit@beta

#plot(weights, words)







