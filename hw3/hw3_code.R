#install.packages("lda")
#install.packages("topicmodels")
#install.packages("stm")
#install.packages("LDAvis")

# TODO save worksave so you can load it in the markdown file

require(quanteda, warn.conflicts = FALSE, quietly = TRUE)
library(topicmodels)

data(immigNewsCorpus, package = "quantedaData")

# Get 4 most common newspapers
topPapers <- sort(table(immigNewsCorpus[["paperName"]]), decreasing = TRUE)
reducedCorpus <- subset(immigNewsCorpus, paperName %in% names(topPapers)[1:4])

# Creates custom_stopwords vector
load('~/texas/hw3/custom_stopwords.RData')

news_dfm <- dfm(reducedCorpus, ignoredFeatures = custom_stopwords)
news_dfm <- trim(news_dfm, minCount=30, minDoc=20)

NUM_TOPICS <- 30
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

guardian_topic_dist <- news_lda@gamma[which(papers == 'guardian'),]
top_guardian_topics <- apply(guardian_topic_dist, 1, which.max)
second_top_guardian_topics <- apply(guardian_topic_dist, 1, which.max2)
# TODO use paper names instead of dates
guardian_topics <- data.frame(first=top_guardian_topics, second=second_top_guardian_topics)

mail_topic_dist <- news_lda@gamma[which(papers == 'mail'),]
top_mail_topics <- apply(mail_topic_dist, 1, which.max)
second_top_mail_topics <- apply(mail_topic_dist, 1, which.max2)
mail_topics <- data.frame(first=top_mail_topics, second=second_top_mail_topics)

gmail_topics <- rbind(guardian_topics, mail_topics)
gmail_topics$paper <- rep(c('guardian', "mail"), times=c(nrow(guardian_topics), nrow(mail_topics)))

# plot
z <- ggplot(gmail_topics, aes(x=1:nrow(gmail_topics), y=first, pch="First", color=paper)) + 
    geom_point()
z + geom_point(aes(x=1:nrow(gmail_topics), y=second, pch="Second", color=paper)) +
  theme_bw() + xlab(NULL)+ ylab("Topic Number") +
  theme(axis.ticks = element_blank(), axis.text.x = element_blank()) + 
  labs(pch="Topic Order", color='Paper') + ggtitle("Paper Topics")


# Problem 1g
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
#news_lda

jsonLDA <- createJSON(phi=exp(news_lda@beta), theta=news_lda@gamma, 
                      doc.length=ntoken(news_dfm), vocab=news_lda@terms,
                      term.frequency=colSums(news_dfm))
#install.packages('servr')

#serVis(jsonLDA, out.dir = "visCollLDA", open.browser = TRUE)
serVis(jsonLDA, open.browser = TRUE)

# TODO require(png)
# TODO grid.raster(readPNG())


# Using this library is not obvious at all
# example: http://cpsievert.github.io/LDAvis/reviews/reviews.html

# Problem 2

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
library(stm)
install.packages("Rtsne")
install.packages("geometry")

# subset the dfm to guardian and daily mail
mailGuardCorpus <- subset(immigNewsCorpus, paperName %in% c('mail', 'guardian'))

paper <- mailGuardCorpus$documents$paperName
text <- mailGuardCorpus$documents$texts
# TODO chose 2015 arbitrarily.
#date <- as.Date(as.numeric(mailGuardCorpus$documents$day) - 1, origin = "2014-01-01") # TODO numeric?
days <- as.numeric(mailGuardCorpus$documents$day)
mailGuard.df <- data.frame(paper, text, days)#TODO, date)

processed_corpus <- textProcessor(mailGuard.df$text, metadata=mailGuard.df, 
                                  customstopwords=custom_stopwords)

##remove some words for speed purposes
# TODO wtf
# TODO TODO ERROR 
# "Error in prepDocuments(processed$documents, processed_corpus$vocab, processed_corpus$meta,  : 
# Your documents object has more unique features than your vocabulary file has entries."
out_25 <- prepDocuments(processed_corpus$documents, processed_corpus$vocab, processed_corpus$meta, lower.thresh=25)

# TODO wtf is the "spline of the date variable"? I'm ignoring it and just using the index.

# NOTE reduce max EM iterations to 5 from 25 to make it faster.

fitSpec25 <- stm(out_25$documents, out_25$vocab, K=0, init.type="Spectral", 
                 content=~paper, prevalence = ~paper + days, #smooth.spline(date), # TODO maybe just use numeric vector instead of date
                 max.em.its=30, data=out_25$meta, seed=5926696)

# Top 5 topics
labelTopics(fitSpec25, 1:5)


# Topic Words:
#  Topic 1: church, sex, christian, sexual, oppos, savil, spring 
#  Topic 2: beauti, pop, cloth, cook, hate, knew, writer 
#  Topic 3: birmingham, inspector, inquiri, gay, knew, friday, discuss 
#  Topic 4: bnp, farag, ukip, confer, euro, extremist, nigel 
#  Topic 5: bbcs, savil, leftw, broadcast, corpor, bbc, fee 

#  Covariate Words:
#  Group guardian: grdn, newspaperid, caption, page, overwhelm, form, address 
#  Group mail: newspapermailid, damonl, daim, newspap, mail, onlin, spark 

#  Topic-Covariate Interactions:
#  Topic 1, Group guardian: anim, muslim, islam, religion, food, scare, recommend 
#  Topic 1, Group mail: novemb, stress, briton, wintour, survey, betray, minimum 

#  Topic 2, Group guardian: itali, sea, island, euro, rent, northern, coupl 
#  Topic 2, Group mail: water, sea, murder, rape, road, wealthi, network 

#  Topic 3, Group guardian: homosexu, asian, citizenship, teacher, guest, islam, facebook 
#  Topic 3, Group mail: rowena, mason, brown, gordon, field, stanc, backbench 

#  Topic 4, Group guardian: manifesto, surg, legitim, revolt, euroscept, poll, referendum 
#  Topic 4, Group mail: ship, engin, human, eye, dream, smile, artist 

#  Topic 5, Group guardian: terribl, corrupt, academ, ian, style, salari, bloodi 
#  Topic 5, Group mail: offend, drug, releas, search, illeg, oper, approv

# Names for the topics:
# 
# 1: Christian sex
# 2: Lifestyle
# 3: Birmingham investigation?
# 4: Right-wing politics
# 5: Public broadcasting
topic_names <- c('Christian sex', 'Lifestyle', 'Birmingham investigation', 
                 'Right-wing politics', 'Public broadcasting')

# TODO in the recitation code
# TODO use labelTopics?
# TODO estimateEffect method=difference
# TODO use verbose_labels = F, label_type=custom, custom.albels = blah
# visualization
# TODO use estimateEffect method=continutuous

##change data types
out_25$meta$paper<-as.factor(out_25$meta$paper)
##pick specifcation
prep<-estimateEffect(topic_names ~ paper , fitSpec25, meta=out_25$meta)
##plot effects
plot.estimateEffect(prep, covariate="paper", topics=1:5, model=out_25, method="difference", 
                    cov.value1 = "guardian", cov.value2 = "mail",
                    xlab = "More Guardian......More Mail", xlim=c(-.1, .1),
                    labeltype='custom', verboseLabels=F, custom.labels=topic_names)

##pick specifcation--over time
prep <- estimateEffect(1:5 ~ s(days) , fitSpec25, meta=out_25$meta)
##plot effects
plot.estimateEffect(prep, covariate="days", topics=1:5, model=out_25, method="continuous",
                    labeltype='custom', verboseLabels=F, custom.labels=topic_names)


##### Problem 4
sotu_dfm <- dfm(subset(inaugCorpus, Year>1900))

nixon_index <- which(sotu_dfm@Dimnames$docs == "1969-Nixon")
obama_index <- which(sotu_dfm@Dimnames$docs == "2009-Obama")

# are these indices correct?
df_fit <- textmodel_wordfish(sotu_dfm, c(obama_index, nixon_index))

# get index of left and rightmost things in df_fit@theta, then get their names
# @theta: scores of each doc

# Leftmost
sotu_dfm@Dimnames$docs[which.min(df_fit@theta)]
# "1993-Clinton"

# Rightmost
sotu_dfm@Dimnames$docs[which.max(df_fit@theta)]
# "1909-Taft"

# fascism
df_fit@beta[which(df_fit@features == 'fascism')]
# -4.291355

# Left????


#df_fit@features: words
#df_fit@beta: scores for words. how much it discriminates on left v right. negative is left.
# psi: fixed effects: how frequent or "stop wordy" a word is.

##most important features--word fixed effects

##guitar plot
# plotting distriminating effect (beta) vs fixed effect (psi)
words<-df_fit@psi
names(words) <- df_fit@features
sort(words)[1:50]
sort(words, decreasing=T)[1:50]

weights<-df_fit@beta
plot(weights, words)







