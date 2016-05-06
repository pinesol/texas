
# Note: you have to set your working directory the 'debate' github directory in
# order for these functions to find the files.
# e.g.
# setwd('~/texas/debate/')

require(plyr)

# Parses 'main_debate_cleaned.html' and returns a data frame with the name of the
# speaker, and what they said.
parseDebateText <- function() {
  # Read in the file
  filename <- 'main_debate_cleaned.html'
  full_debate_string <- readChar(filename, file.info(filename)$size)
  # split by paragraph tags
  ptag_split <- trimws(unlist(strsplit(full_debate_string, '<p>')))
  # remove ending paragraph tags
  ptag_split <- trimws(unlist(gsub('</p>', '', ptag_split)))
  # Remove empty strings
  ptag_split <- ptag_split[ptag_split != '']
  
  # Look for the different speakers
  # NOTE: There are unknown speakers like (UNKNOWN):
  SPEAKER_REGEX <- '^(\\(UNKNOWN\\)|[A-Z ]+): '
  new_speaker_indices <- grep(SPEAKER_REGEX, ptag_split)
  stopifnot(new_speaker_indices[1] == 1) # The first entry should be a speaker
  
  # Parse out the speakers and what they said.
  speakers <- character(length(new_speaker_indices))
  snippets <- character(length(new_speaker_indices))
  speaker_count <- 1
  i <- 1
  for (speaker_index in new_speaker_indices[1:length(new_speaker_indices)]) {
    # extract the speaker's name
    result <- regexpr(SPEAKER_REGEX, ptag_split[speaker_index])
    stopifnot(result[1] == 1) # The speaker's name should be first
    speaker_name_length <- attr(result, "match.length")
    # Extract the speaker name, leave out the colon and trailing space
    speaker <- substr(ptag_split[speaker_index], 1, speaker_name_length-2)
    speakers[i] <- speaker
    
    # extract the speaker's text snippet
    next_speaker_count <- min(speaker_count + 1, length(new_speaker_indices))
    next_speaker_index <- new_speaker_indices[next_speaker_count]
    snippet <- paste(ptag_split[speaker_index:(next_speaker_index-1)], collapse='')
    snippet <- substr(snippet, speaker_name_length+1, nchar(snippet))
    snippets[i] <- snippet
    
    speaker_count <- speaker_count + 1
    i <- i + 1
    
    # Look if there is a commercial, and add it as an extra 'speaker' if there is.
    commercial_index <- grep('\\(COMMERCIAL BREAK\\)', snippet)
    if (length(commercial_index) > 0) {
      # Remove the 'commerical break' text from the snippet
      snippets[i-1] <- gsub("\\(COMMERCIAL BREAK\\)", '', 
                            snippets[speaker_count])
      # Add an extra 'commercial' speaker
      speakers[i] <- 'COMMERCIAL'
      snippets[i] <- 'COMMERCIAL'
      i <- i + 1
    }
  }
  debate.df <- data.frame(speaker=speakers, text=snippets)
}

#debate.df <- parseDebateText()

# reads in closed captioning file 'timestamp_transcript_republican_debate' as a data frame.
# New speakers are denoted by ">>" or ">>>"
# The closed captioning text doesn't always match the transcript.
# I think we'll use the closed captions as guideposts, and lerp between them.
parse_captions <- function() {
  cc_lines <- readLines('timestamp_transcript_republican_debate')
  line_index <- 1
  caption_start_times <- character()
  caption_text <- character()
  while (line_index < length(cc_lines)) {
    text <- trimws(tolower(cc_lines[line_index+2]))
    if (substr(text, 0, 2) == '>>') {
      # Skip the caption index, you don't need it
      caption_index <- cc_lines[line_index]
      # Only extract the first hh:mm:ss of the time interval  
      caption_start_times[caption_index] <- substr(cc_lines[line_index+1], 0, 8)
      # Removing leading '>>' or '>>>'
      caption_text[caption_index] <- gsub('^[>]+ ', '', text)
    }
    line_index <- line_index + 4 # Skipping one more for the blank line  
  }
  data.frame(start_time=caption_start_times, text=caption_text)
}

#captions.df <- parse_captions()

# TODO create timestamps based on the total time of the debate, commercial breaks, and the number of characters in each snippet.
# TODO go through snippets and captions together, assigning a caption to a snippet index if the caption and the snippet start the same.
# TODO come up with a timestamp for each snippet, using the closed caption time, the starting time, and the commericials.
#      Move the closed caption time by the starting time of the debate (eg 8pm EST), and add time for the commercial break
# TODO this doesn't quite work. The puncutation doesn't match. Sometimes it's correct, sometimes not.
# A better strategy might be to concatenate all the captions together like you did with the debate text.
# You can tell where each new speaker is by looking at the '>>'.
# Then you can check for a high proportion substring match.
assign_caption_times <- function(debate.df, caption.df) {
  DEBATE_DATE <- '2015-08-06'
  
  debate_index <- 1
  captions_last_match <- 1
  debate_snippet_times <- character()
  
  while (debate_index < nrow(debate.df)) {
    debate_text <- tolower(debate.df$text[debate_index])
    
    captions_index <- captions_last_match
    while (captions_index < nrow(captions.df)) {
      caption_text <- tolower(captions.df$text[captions_index])
      if (grepl(paste('^', caption_text, sep=''), debate_text)) {
        date_str <- paste(DEBATE_DATE, captions.df$start_time[captions_index])
        debate_snippet_times[debate_index] <- strftime(date_str)
        print('DEBATE')
        print(head(debate_text))
        print('CAPTION')
        print(caption_text)
        print(as.character(captions.df$start_time[captions_index]))
        captions_last_match <- captions_index + 1
        break
      }    
      captions_index <- captions_index + 1  
    }
    debate_index <- debate_index + 1
  }
  debate_snippet_times
}

#caption_start_times <- assign_caption_times(debate.df, caption.df)

# Parses twitter data into data frame from 'GOP_REL_ONLY.csv'.
parseTwitterData <- function() {
  # Read in the Twitter data from disk
  twitter_full <- read.csv("GOP_REL_ONLY.csv", stringsAsFactors = F)
  #View(twitter_full)
  # Let's discard the columns we won't need to use for simplicity
  twitter <- twitter_full[, c("candidate", "sentiment", "subject_matter", 
                              "retweet_count", "text", "tweet_created")]
  # Change column data formats
  twitter$candidate <- as.factor(twitter$candidate)
  twitter$sentiment <- as.factor(twitter$sentiment)
  twitter$subject_matter <- as.factor(twitter$subject_matter)
  # In creating these timestamps, we discarded timezone data. Worth it to include?
  twitter$tweet_created <- strptime(twitter$tweet_created, "%m/%d/%y %H:%M")
  twitter$candidate <- mapvalues(twitter$candidate, from=c("", "No candidate mentioned"), to=c("OTHER", "OTHER"))
  twitter$subject_matter <- mapvalues(twitter$subject_matter, from=c(""), to=c("None of the above"))
  return(twitter)
}

#twitter <- parseTwitterData()
#View(twitter)





