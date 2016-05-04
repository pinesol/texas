
# Given the filename of the 'cleaned' debate file, this parses the text and returns a
# data frame with the name of the speaker, and what they said.
# TODO add timestamps
createDebateSnippets <- function(cleaned_debate_filename) {
  # Read in the file
  full_debate_string <- readChar(file_name, file.info(cleaned_debate_filename)$size)
  # split by paragraph tags
  ptag_split <- trimws(unlist(strsplit(full_debate_string, '<p>')))
  # remove ending paragraph tags
  ptag_split <- trimws(unlist(gsub('</p>', '', ptag_split)))
  # Remove empty strings
  ptag_split <- ptag_split[ptag_split != '']
  
  # Look for the different speakers
  # TODO There are unknown speakers like (UNKNOWN):
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
  debate.df <- data.frame(speakers, snippets)
}

file_name <- '~/texas/debate/main_debate_cleaned.html'
debate.df <- createDebateSnippets(file_name)

# TODO create timestamps based on the total time of the debate, commercial breaks, and the number of characters in each snippet.

# TODO read in closed captioning in a data frame

# New speakers are denoted by ">>" or ">>>"
# ">>>" nearly always denotes coming back from a commerical, but not always.
# The closed captioning text doesn't always match the transcript.
# I think we'll use the closed captions as guideposts, and lerp between them.




