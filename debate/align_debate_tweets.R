
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
debate_start <- strptime('2015-08-07 01:00:00', "%Y-%m-%d %H:%M:%S", tz='UTC')
debate_end <- strptime('2015-08-07 02:30:00', "%Y-%m-%d %H:%M:%S", tz='UTC')

# Stackoverflow showed me how to reorder matrices. Makes no damn sense.
twitter.df <- twitter.df[with(twitter.df, order(tweet_created)),]


#tweets are in UTC (thank god)
# There are a few really early ones, but most start at 18:55 UTC and go well into the next day! 
# Therefore, only a tiny fraction actually occured during the debate???
sum(twitter.df$tweet_created >= debate_start & twitter.df$tweet_created < debate_end)
# Returns 66????? I don't believe it. that's waaaaay too few.

# I think I need to look at the tweet data itself...I don't know if I can really trust the timestamps.

# TODO maybe they're not really in UTC


#twitter.df$tweet_created <- as.character(twitter.df$tweet_created)
hours <- c('2015-08-06 17:00:00 UTC', 
           '2015-08-06 18:00:00 UTC',
           '2015-08-06 19:00:00 UTC',
           '2015-08-06 20:00:00 UTC',
           '2015-08-06 21:00:00 UTC',
           '2015-08-06 22:00:00 UTC',
           '2015-08-06 23:00:00 UTC',
           '2015-08-07 00:00:00 UTC',
           '2015-08-07 01:00:00 UTC',
           '2015-08-07 02:00:00 UTC',
           '2015-08-07 03:00:00 UTC',
           '2015-08-07 04:00:00 UTC',
           '2015-08-07 05:00:00 UTC',
           '2015-08-07 06:00:00 UTC',
           '2015-08-07 07:00:00 UTC',
           '2015-08-07 08:00:00 UTC',
           '2015-08-07 09:00:00 UTC',
           '2015-08-07 10:00:00 UTC',
           '2015-08-07 11:00:00 UTC')

binned_dates <- cut(as.POSIXct(twitter.df$tweet_created), 
                    breaks=as.POSIXct(strptime(hours, "%Y-%m-%d %H:%M:%S", tz='UTC')))

plot(binned_dates)

# I don't understand the output of binned dates...all the starting ones are NAs...

# There are two modes in this histogram. They might correspond to the two debates that night
# but why are they so far apart? The undercard debate was at 5pm EDT, and the primetime debat
# was at 9pm EDT the same day.

# TODO extract the dates in the two hour time when there are tweets, and thow out everything else
# THen try some method of aligning the debate with the tweets


