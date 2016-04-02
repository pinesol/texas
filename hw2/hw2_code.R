library(quanteda)

# read in data csv into dataframe
df.reviews <- read.csv("~/Text_as_Data/HW2/p4k_reviews.csv", stringsAsFactors = FALSE)

# a
# find the median score
median_score <- median(df.reviews[['score']]) # 7.2
print(paste('Median review score:', median_score))
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
# load in the positive dictionary file. remove header. split by line, create a quanteda dictionary
f <- file("~/Text_as_Data/HW2/positive-words.txt", open="r")
positive_words <- readLines(f)
close(f)
positive_words <- positive_words[-1:-2] # cut off blank lines
# load in the negative dictionary file. remove header. split by line into a list, create a quanteda dictionary
f <- file("~/Text_as_Data/HW2/negative-words.txt", open="r")
negative_words <- readLines(f)
close(f)
negative_words <- negative_words[-1:-2] # cut off header
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

median_reviews_dict_score <- median(reviews_dict_score, na.rm = TRUE) # 0.342857142857143
print(paste("The median sentiment score is", median_reviews_dict_score))
perc_reviews_dict_positive <- sum(reviews_dict_labels == 'positive', na.rm = TRUE)/length(reviews_dict_labels) # 0.9632
print(paste("The percentange of positive reviews is", perc_reviews_dict_positive))

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
  print(paste('precision', tp / (tp+fp)))
  print(paste('recall', tp / (tp+fn)))
  print(paste('accuracy', (tp+tn) / (tp+tn+fp+fn)))
}

compute_accuracy_stats(assigned_labels, reviews_dict_labels)
#[1] "true positives 4801"
#[1] "true negatives 164"
#[1] "false positives 4831"
#[1] "false negatives 204"
#[1] "precision 0.4984426910299"
#[1] "recall 0.959240759240759"
#[1] "accuracy 0.4965"

# Compute ranking difference score. Smaller is better.
rank_diff_score <- function(real_scores, predicted_scores) {
  sorted_real_scores_ix <- sort(real_scores, decreasing=TRUE, index.return=TRUE)$ix
  sorted_predicted_scores_ix <- sort(predicted_scores, decreasing=TRUE, index.return=TRUE)$ix
  rank_diff_sum <- sum(abs(sorted_real_scores_ix - sorted_predicted_scores_ix))
  print(paste('rank diff score', rank_diff_sum))
}
rank_diff_score(df.reviews$score, reviews_dict_score) # rank diff score 33,409,190"


# 2c Naive Bayes

reviews_dfm <- dfm(df.reviews$text[1:250], toLower=TRUE, removeNumbers=TRUE, removePunct=TRUE, 
                   ignoredFeatures = stopwords("english"))
num_training_docs <- floor(0.2*ndoc(reviews_dfm))
training_reviews_dfm <- reviews_dfm[1:num_training_docs]
training_reviews_labels <- factor(assigned_labels[1:num_training_docs], ordered=TRUE)
test_reviews_dfm <- reviews_dfm[(num_training_docs+1):ndoc(reviews_dfm)]
# train the model
nb.p4k <- textmodel_NB(x=training_reviews_dfm, y=training_reviews_labels, smooth=1, prior='uniform') 
nb.p4k.predictions <- predict(nb.p4k, newdata=test_reviews_dfm)
# evaluate it on the test set
test_reviews_labels <- assigned_labels[(num_training_docs+1):ndoc(reviews_dfm)]
compute_accuracy_stats(test_reviews_labels, nb.p4k.predictions$docs$ws.predicted)
#[1] "true positives 98"
#[1] "true negatives 1"
#[1] "false positives 100"
#[1] "false negatives 1"
#[1] "precision 0.494949494949495"
#[1] "recall 0.98989898989899"
#[1] "accuracy 0.495"


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

# write a function that takes in an unscored doc and the scores list, and uses it to score the doc.
calc_avg_wordscores <- function(reviews_dfm, word_scores) {
  words_per_doc <- rowSums(reviews_dfm)
  # filter out words with a word score of zero, for speed.
  filtered_word_scores <- word_scores[word_scores != 0]
  filtered_reviews_dfm <- selectFeatures(reviews_dfm, names(filtered_word_scores))
  scores <- (filtered_reviews_dfm / words_per_doc) %*% filtered_word_scores
  scores[is.na(scores)] <- 0
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
# NOTE: I considered scoring only the non-anchor documents, but I decided not to because it
# would make the rank scores uncomparable.

# 2e SVM
# only use first 1000 documents.
library(RTextTools)

reviews_dtm <- create_matrix(df.reviews$text[1:1000], language="english",
                             removeStopwords=TRUE, 
                             stemWords=FALSE, 
                             stripWhitespace=TRUE, 
                             toLower=TRUE)
# try training vs test from 10 to 90, report the best one.
evaluate_svm <- function(reviews_dtm, kernel) {
  best_accuracy <- 0
  best_training_size <- 0
  for (training_size in seq(.1, .9, .1)) {
    print(paste('Testing training size:', training_size))
    training_break = floor(training_size*nrow(reviews_dtm))
    container <- create_container(reviews_dtm, assigned_labels, 
                                  trainSize=1:training_break, 
                                  testSize=(training_break+1):nrow(reviews_dtm),
                                  virgin=FALSE)    
    cv.svm <- cross_validate(container, nfold=5, algorithm='SVM', kernel=kernel)
    print(paste('Accuracy:', cv.svm$meanAccuracy))
    if (cv.svm$meanAccuracy > best_accuracy) {
      best_accuracy <- cv.svm$meanAccuracy
      best_training_size <- training_size
    }
  }
  print(paste('The highest accuracy was', best_accuracy, ', when the training size was', best_training_size))
}
evaluate_svm(reviews_dtm, 'linear') 
#[1] "Accuracy: 0.670487694322781"
#[1] "The highest accuracy was  0.670487694322781 , when the training size was 0.9"

evaluate_svm(reviews_dtm, 'radial') 
# "The highest accuracy was  0.681702171991192 , when the training size was 0.7"

# It appears the radial basis is a little better.


# Problem 3 - Human Intelligence Tasks (HITs)
df.hit <- read.csv("~/Text_as_Data/HW2/CF_rate_trustworthiness.csv", stringsAsFactors = FALSE)

df.country <- df.hit[c('rating', 'X_country')]

country_anova <- aov(rating ~ X_country, data=df.country)
summary(country_anova)

#              Df Sum Sq Mean Sq F value   Pr(>F)    
# X_country    34  359.6  10.576   2.927 2.04e-06 ***
#  Residuals   185  668.4   3.613                     
---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1


# statistically significant
# 1 way anova

df.demo <- df.hit[c('rating', 'image_name')]
df.demo$demographic <- gsub("\n", "", df.demo$image_name)
df.demo$demographic <- gsub("[0-9]+", "", df.demo$image_name)

demo_anova <- aov(rating ~ image_name, data=df.demo)
summary(demo_anova)
  
#Df Sum Sq Mean Sq F value  Pr(>F)   
#image_name   43  310.4   7.218    1.77 0.00541 **
#  Residuals   176  717.6   4.077                   
#---
#  Signif. codes:  0 ‘***’ 0.001 ‘**’ 0.01 ‘*’ 0.05 ‘.’ 0.1 ‘ ’ 1  
  

