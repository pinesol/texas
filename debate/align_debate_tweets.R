
# TODO try niave ordering scheme: 
# 1) assign the first several tweets to the first speaker. only switch to the next speaker when he starts speaking.
# 2) If the tweet is labeled with a candidate, assign it to the last snippet the candidate said, otherwise, the current speaker.
# 3) Filter out the moderators and see if it's better or worse.

rm(list=ls())
setwd("~/texas/debate/")

source("./parse_debate.R")

twitter.df <- parseTwitterData()
# reorder by time
twitter.df <- twitter.df[order(twitter.df$tweet_created),]

# The main debate aired from 9pm to 10:30pm EDT
# http://www.politico.com/story/2015/08/2016-presidential-debate-schedule-dates-times-moderators-and-topics-by-213684
# That was 6pm to 7:30pm PDT
debate_start <- strptime('2015-08-06 18:00:00', "%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles")
debate_end <- strptime('2015-08-06 20:00:00', "%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles")

sum(twitter.df$tweet_created >= debate_start & twitter.df$tweet_created < debate_end)
# Returns 3680

binned_dates <- cut(as.POSIXct(twitter.df$tweet_created, "%Y-%m-%d %H:%M:%S", tz="America/Los_Angeles"), 
                    breaks='hour')
plot(binned_dates)

# TODO extract the dates in the two hour time when there are tweets, and thow out everything else
# THen try some method of aligning the debate with the tweets

# Maybe the timezones of the tweets are PST? Then the story is: the spike of tweets starts during the 
# debate, and the next one comes the next day at 7 am?

# That makes the most sense to me. THe debate ends at 10:30, which is lines up with the fact that there
# is a dropoff in tweets in the second hour.

# TODO the tweets seemed to be off by an hour....






