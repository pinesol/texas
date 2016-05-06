
# TODO try niave ordering scheme: 
# 1) assign the first several tweets to the first speaker. only switch to the next speaker when he starts speaking.
# 2) If the tweet is labeled with a candidate, assign it to the last snippet the candidate said, otherwise, the current speaker.
# 3) Filter out the moderators and see if it's better or worse.

require(ggplot2)

rm(list=ls())
setwd("~/texas/debate/")

source("./parse_debate.R")

twitter.df <- parseTwitterData()
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
sum(is.na(binned_dates))
levels(binned_dates) <- sapply(strsplit(levels(binned_dates), " "), "[", 2)
qplot(binned_dates) + theme(axis.text.x=element_text(angle = 90, vjust = 0.5))







