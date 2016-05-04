
# Given the filename of the 'cleaned' debate file, this parses the text and returns a
# data frame with the name of the speaker, and what they said.
# TODO add timestamps
createDebateSnippets <- function(cleaned_debate_filename) {
  # Read in the file
  full_debate_string <- readChar(file_name, file.info(cleaned_debate_filename)$size)
  # split by paragraph tags
  ptag_split <- trimws(unlist(strsplit(full_debate_string, '<p>')))
  ptag_split <- ptag_split[ptag_split != '']
  
  # Look for the different speakers
  new_speaker_indices <- grep('^[A-Z]+: ', ptag_split)
  # Look for the commericals
  commercial_indices <- grep('(COMMERCIAL BREAK)', ptag_split)
  # merge together entries of the same speaker
  # remove everything in parens except (COMMERCIAL BREAK)
  stopifnot(new_speaker_indices[1] == 1) # The first entry should be a speaker
  speakers <- character(length(new_speaker_indices) + length(commercial_indices))
  snippets <- character(length(new_speaker_indices) + length(commercial_indices))
  
  speaker_count <- 1
  commercial_count <- 1
  # TODO commercials?
  for (speaker_index in new_speaker_indices[1:length(new_speaker_indices)]) {
    # extract the speaker's name
    result <- regexpr('^[A-Z]+: ', ptag_split[speaker_index])
    stopifnot(result[1] == 1) # The speaker's name should be first
    speaker_name_length <- attr(result, "match.length")
    # Extract the speaker name, leave out the colon and trailing space
    speaker <- substr(ptag_split[speaker_index], 1, speaker_name_length-2)
    speakers[speaker_count] <- speaker
    
    # extract the speaker's text snippet
    next_speaker_count <- min(speaker_count + 1, length(new_speaker_indices))
    next_speaker_index <- new_speaker_indices[next_speaker_count]
    snippet <- paste(ptag_split[speaker_index:(next_speaker_index-1)], collapse='')
    snippet <- substr(snippet, speaker_name_length+1, nchar(snippet))
    snippets[speaker_count] <- snippet
    
    speaker_count <- speaker_count + 1
  }
  debate.df <- data.frame(speakers, snippets)
}

file_name <- '~/texas/debate/main_debate_cleaned.html'
debate.df <- createDebateSnippets(file_name)


