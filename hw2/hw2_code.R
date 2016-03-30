library(quanteda)

# read in data csv into dataframe
df.reviews <- read.csv("~/Text_as_Data/HW2/p4k_reviews.csv", stringsAsFactors = FALSE)

# a
# find the median score
median_score <- median(df.reviews[['score']]) # 7.2
# make a new column for 'positive' vs 'negative'
assigned_labels <- ifelse(df.reviews$score >= median_score, 'positive', 'negative')
# find the scores below 10th percentile
anchor_negative_cutoff <- quantile(df.reviews$score, c(0.1))[[1]] # 5.4
# find the score above the 90th percentile
anchor_positive_cutoff <- quantile(df.reviews$score, c(0.9))[[1]] # 8.2
# make a new column "anchor", labeled some "anchor positive" or "anchor negative"
anchor_fn <- function(score) {
  if (score < anchor_negative_cutoff) {
    return("anchor negative")
  } else if (score > anchor_positive_cutoff) {
    return("anchor positive")
  } else {
    return("")
  }
}
anchor_labels <- sapply(df.reviews$score, anchor_fn)

# b
# NOTE: he changed the dictionary after you did this. so your results may differ.
# load in the positive dictionary file. remove header. split by line, create a quanteda dictionary
f <- file("~/Text_as_Data/HW2/positive-words.txt", open="r")
positive_words <- readLines(f)
close(f)
positive_words <- positive_words[-1:-35] # cut off header
# load in the negative dictionary file. remove header. split by line into a list, create a quanteda dictionary
f <- file("~/Text_as_Data/HW2/negative-words.txt", open="r")
negative_words <- readLines(f)
close(f)
negative_words <- negative_words[-1:-35] # cut off header
# create dictionary
review_dict <- dictionary(list(positive = positive_words, negative = negative_words))

# Create a dfm of all reviews narrowed down to the dictionary words
reviews_dfm <- as.data.frame(dfm(df.reviews$text,
                             toLower=TRUE, removeNumbers=TRUE, removePunct=TRUE, 
                             dictionary = review_dict))
# Compute predicted scores
reviews_diff <- reviews_dfm$positive - reviews_dfm$negative
reviews_total_words = reviews_dfm$positive + reviews_dfm$negative 
reviews_dict_score <- reviews_diff / reviews_total_words
# NOTE some reviews have no dict words. Give them a score of zero.
reviews_dict_score[is.na(reviews_dict_score)] <- 0.0
reviews_dict_labels <- ifelse(reviews_dict_score >= 0, 'positive', 'negative')

median_reviews_dict_score <- median(reviews_dict_score, na.rm = TRUE) # 0.3428571
perc_reviews_dict_positive <- sum(reviews_dict_labels == 'positive', na.rm = TRUE)/length(reviews_dict_labels) # 0.9622

# accuracy precision recall
compute_accuracy_stats <- function(real_labels, predicted_labels) {
  tp <- tn <- fp <- fn <- 0
  for (i in 1:length(real_labels)) {
    if (predicted_labels[[i]] == real_labels[[i]]) {
      if (predicted_labels[[i]] == 'positive') {
        tp <- tp + 1
      } else {
        tn <- tn + 1
      }
    } else {
      if (predicted_labels[[i]] == 'positive') {
        fp <- fp + 1
      } else {
        fn <- fn + 1
      }      
    }
  }
  print(paste('true positives', tp))
  print(paste('true negatives', tn))
  print(paste('false positives', fp))
  print(paste('false negatives', fn))
  print(paste('true positive rate', tp / (tp+fp)))
  print(paste('false positive rate', fp / (tp+fp)))
  print(paste('accuracy', (tp+tn) / (tp+tn+fp+fn)))
}

compute_accuracy_stats(assigned_labels, reviews_dict_labels)
# "true positives 4801"
# "true negatives 165"
# "false positives 4830"
# "false negatives 204"
# "true positive rate 0.498494445021285"
# "false positive rate 0.501505554978715"
# "accuracy 0.4966"

# Compute ranking difference score. Smaller is better.
rank_diff_score <- function(real_scores, predicted_scores) {
  sorted_real_scores_ix <- sort(real_scores, decreasing=TRUE, index.return=TRUE)$ix
  sorted_predicted_scores_ix <- sort(predicted_scores, decreasing=TRUE, index.return=TRUE)$ix
  rank_diff_sum <- sum(abs(sorted_real_scores_ix - sorted_predicted_scores_ix))
  print(paste('rank diff score', rank_diff_sum))
}
rank_diff_score(df.reviews$score, reviews_dict_score) # rank diff score 33,409,190"


# 2c Naive Bayes

# TODO doesn't work! I think quanteda sucks.
reviews_dfm <- dfm(df.reviews$text, toLower=TRUE, removeNumbers=TRUE, removePunct=TRUE, 
                   ignoredFeatures = stopwords("english"))
num_training_docs <- floor(0.8*ndoc(reviews_dfm))
training_reviews_dfm <- reviews_dfm[1:num_training_docs] #TODO check 0.8 via email
training_reviews_labels <- factor(assigned_labels[1:num_training_docs], ordered=TRUE)
test_reviews_dfm <- reviews_dfm[(num_training_docs+1):ndoc(reviews_dfm)]
test_reviews_labels <- factor(assigned_labels[(num_training_docs+1):ndoc(reviews_dfm)], ordered=TRUE)
# TODO try it without smoothing too.
nb.p4k <- textmodel_NB(x=training_reviews_dfm, y=training_reviews_labels, smooth=1.0, prior='uniform') 
nb.p4k.predictions <- predict(nb.p4k, newdata=test_reviews_dfm)
# aha 263 is empty!
#Error in colnames(PcGw)[apply(PcGw, 1, which.max)] : 
#  invalid subscript type 'list'

# 2d Word scores by hand
reviews_dfm <- dfm(df.reviews$text, toLower=TRUE, removeNumbers=TRUE, removePunct=TRUE, 
                   ignoredFeatures = stopwords("english"))

calculate_wordscores <- function(anchor_labels, reviews_dfm) {
  positive_dfm <- reviews_dfm[anchor_labels == 'anchor positive']
  negative_dfm <- reviews_dfm[anchor_labels == 'anchor negative']
  # treat the anchor reviews as two big reviews
  positive_counts <- colSums(positive_dfm)
  negative_counts <- colSums(negative_dfm)
  # normalize counts
  positive_scores <- positive_counts / sum(positive_counts)
  negative_scores <- negative_counts / sum(negative_counts)  
  # final score calculation
  scores <- (positive_scores - negative_scores) / (positive_scores + negative_scores)
  # Words that are not in the anchor docs get a score of zero.
  scores[is.na(scores)] <- 0
  print("Most positive review words:")
  print(sort(scores, decreasing=TRUE)[1:10])
  print("Most negative review words:")
  print(sort(scores)[1:10])
  return(scores)
}
word_scores <- calculate_wordscores(anchor_labels, reviews_dfm)
# The most positive words are ones that only appear in positive docs, and the most negative words are the ones that only appear in negative docs.
#[1] "Most positive review words:"
#noisemakers configurations high-intensity          vomit     pristinely unintelligible         flirts           balk      extremity    unremitting 
#1              1              1              1              1              1              1              1              1              1 
#[1] "Most negative review words:"
#corralled       dawson stadium-size    souleyman   motionless        dabke out-of-place       khaled   professing     imitates 
#-1           -1           -1           -1           -1           -1           -1           -1           -1           -1 

# TODO write a function that takes in an unscored doc and the scores list, and uses it to score the doc.
calc_avg_wordscores <- function(reviews_dfm, word_scores) {
  words_per_doc <- rowSums(reviews_dfm)
  # filter out words with a word score of zero, for speed.
  filtered_word_scores <- word_scores[word_scores != 0]
  filtered_reviews_dfm <- selectFeatures(reviews_dfm, names(filtered_word_scores))
  scores <- (filtered_reviews_dfm / words_per_doc) %*% filtered_word_scores
  all_word_scores[is.na(all_word_scores)] <- 0
  # Too slow
  #words_per_doc <- rowSums(reviews_dfm)  
  #scores <- (reviews_dfm / words_per_doc) %*% word_scores
  return(scores)
}
all_word_scores <- calc_avg_wordscores(reviews_dfm, word_scores)

word_score_labels <- ifelse(all_word_scores >= 0, 'positive', 'negative')
compute_accuracy_stats(assigned_labels, word_score_labels)
#[1] "true positives 4250"
#[1] "true negatives 2683"
#[1] "false positives 2312"
#[1] "false negatives 755"
#[1] "true positive rate 0.647668393782383"
#[1] "false positive rate 0.352331606217617"
#[1] "accuracy 0.6933"

# calculate the diff rank
rank_diff_score(df.reviews$score, as.vector(all_word_scores))  # rank diff score 33,042,402

# rank diff smaller, and thus better for word scores than it is for the dictionary method.

# 2e SVM
library(RTextTools)

reviews_dtm <- create_matrix(df.reviews$text, language="english",
                             removeStopwords=TRUE, 
                             stemWords=FALSE, 
                             stripWhitespace=TRUE, 
                             toLower=TRUE)
training_break = floor(0.9*nrow(reviews_dtm))
container <- create_container(reviews_dtm, assigned_labels, 
                              trainSize=1:training_break, 
                              testSize=(training_break+1):nrow(reviews_dtm),
                              virgin=FALSE)
# TODO 5 folds is waaaay too slow to run. 
#cv.svm <- cross_validate(container, nfold=5, algorithm = 'SVM', kernel = 'linear')

cv.svm.2.folds <- cross_validate(container, nfold=2, algorithm = 'SVM', kernel = 'linear')

# Figure out how to use cross_validate correctly
# try training vs test from 10 to 90, report the best one.

# TODO are radial or linear kernels better?

# Problem 3 - Human Intelligence Tasks (HITs)
df.hit <- read.csv("~/Text_as_Data/HW2/CF_rate_trustworthiness.csv", stringsAsFactors = FALSE)
df.hit <- df.hit[c('rating', 'image_name', 'X_country')]
df.hit$demographic <- gsub("\n", "", df.hit$image_name)
df.hit$demographic <- gsub("[0-9]+", "", df.hit$image_name)

chisq.test(df.hit$X_country, df.hit$rating)

#data:  df.hit$X_country and df.hit$rating
#X-squared = 445.41, df = 306, p-value = 3.19e-07

# statistically significant

chisq.test(df.hit$demographic , df.hit$rating)

#data:  df.hit$demographic and df.hit$rating
#X-squared = 22.708, df = 18, p-value = 0.202

# not statistically significant

