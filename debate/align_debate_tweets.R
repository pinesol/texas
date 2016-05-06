
# TODO try niave ordering scheme: 
# 1) assign the first several tweets to the first speaker. only switch to the next speaker when he starts speaking.
# 2) If the tweet is labeled with a candidate, assign it to the last snippet the candidate said, otherwise, the current speaker.
# 3) Filter out the moderators and see if it's better or worse.

rm(list=ls())
setwd("~/texas/debate/")

source("./parse_debate.R")

twitter.df <- parseTwitterData()

# The main debate aired from 9pm to 10:30pm EDT
# http://www.politico.com/story/2015/08/2016-presidential-debate-schedule-dates-times-moderators-and-topics-by-213684
# That was 1am to 2:30am August 7th UTC.
# 6 pm to 8 pm PDT, 9 pm to 11 pm EDT
debate_start <- strptime('2015-08-06 19:00:00', "%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles")
debate_end <- strptime('2015-08-06 21:00:00', "%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles")

# Stackoverflow showed me how to reorder matrices. Makes no damn sense.
twitter.df <- twitter.df[order(twitter.df$tweet_created),]

#tweets are in Pacific time maybe?
# There are a few really early ones, but most start at 18:55 and go well into the next day! 
# Therefore, only a tiny fraction actually occured during the debate???
sum(twitter.df$tweet_created >= debate_start)
sum(twitter.df$tweet_created < debate_end)
sum(twitter.df$tweet_created >= debate_start & twitter.df$tweet_created < debate_end)
# Returns 3680????? I don't believe it. that's waaaaay too few.

# I think I need to look at the tweet data itself...I don't know if I can really trust the timestamps.

# TODO maybe they're not really in UTC

binned_dates <- cut(twitter.df$tweet_created, breaks = "hour")
sum(is.na(binned_dates))
levels(binned_dates) <- sapply(strsplit(levels(binned_dates), " "), "[", 2)
qplot(binned_dates) + theme(axis.text.x=element_text(angle = 90, vjust = 0.5))

# I don't understand the output of binned dates...all the starting ones are NAs...

# There are two modes in this histogram. They might correspond to the two debates that night
# but why are they so far apart? The undercard debate was at 5pm EDT, and the primetime debat
# was at 9pm EDT the same day.

# TODO extract the dates in the two hour time when there are tweets, and thow out everything else
# THen try some method of aligning the debate with the tweets


