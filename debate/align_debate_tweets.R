
# TODO try niave ordering scheme: 
# 1) assign the first several tweets to the first speaker. only switch to the next speaker when he starts speaking.
# 2) If the tweet is labeled with a candidate, assign it to the last snippet the candidate said, otherwise, the current speaker.
# 3) Filter out the moderators and see if it's better or worse.

require(ggplot2)

rm(list=ls())
setwd("~/texas/debate/")

source("./parse_debate.R")

twitter.df <- parseTwitterData()

# TODO removing retweets
#twitter.df <-twitter.df[substr(twitter.df$text, 1, 3) != 'RT ',]
# Removing the retweets turns 12733 tweets into 6156 tweets

# reorder by time
twitter.df <- twitter.df[order(twitter.df$tweet_created),]

# The main debate aired from 9pm to 10:30pm EDT
# http://www.politico.com/story/2015/08/2016-presidential-debate-schedule-dates-times-moderators-and-topics-by-213684

# 6 pm to 8 pm PDT, 9 pm to 11 pm EDT
# TODO doing 7 to 9 here bc that's when all the tweets happen
debate_start <- strptime('2015-08-06 19:00:00', "%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles")
debate_end <- strptime('2015-08-06 21:00:00', "%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles")

sum(twitter.df$tweet_created >= debate_start)
sum(twitter.df$tweet_created < debate_end)
sum(twitter.df$tweet_created >= debate_start & twitter.df$tweet_created < debate_end)
# Returns 3570

binned_dates <- cut(twitter.df$tweet_created, breaks = "hour")
levels(binned_dates) <- sapply(strsplit(levels(binned_dates), " "), "[", 2)
qplot(binned_dates) + theme(axis.text.x=element_text(angle = 90, vjust = 0.5))

twitter.df$tweet_hour <- binned_dates

# let's look at the tweets
live_tweets <-  twitter.df[twitter.df$tweet_hour == '18:00:00' | 
                           twitter.df$tweet_hour == '19:00:00' | 
                           twitter.df$tweet_hour == '20:00:00' | 
                           twitter.df$tweet_hour == '21:00:00',]
# 3731 live tweets

summary_tweets <- twitter.df[twitter.df$tweet_hour == '07:00:00' | 
                             twitter.df$tweet_hour == '08:00:00' | 
                             twitter.df$tweet_hour == '09:00:00',]
# 7965 summary tweets

# recode sentiment as -1, 0, 1, then weight by confidence scores
twitter.df$sentiment <- as.numeric(ifelse(twitter.df$sentiment == 'Positive', 1, 
                                   ifelse(twitter.df$sentiment == 'Negative', -1, 0)))
twitter.df$sentiment <- twitter.df$sentiment * as.numeric(twitter.df$sentiment.confidence)

# NOTE: keeping retweets for sentiment scores for now. the number of times each one was retweeted should be
# reflected by the extra rows for that tweet.
# TODO get sentiment score about a particular candidate
trump_sentiment_scores <- twitter.df$sentiment[which(twitter.df$candidate == 'Donald Trump')]

trump_sentiment_west_scores <- twitter.df$sentiment[which(twitter.df$candidate == 'Donald Trump' & twitter.df$region == 'West')]

# TODO make a heat map of average sentiment score of candidates vs region 


