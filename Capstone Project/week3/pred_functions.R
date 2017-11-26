#'-----------------------------------------------------------------------------
#'                          pred_functions.R
#' - Load files with ngrams tables
#' - Load functions for predictions
#' - "predict_next_word" function will be our predictive algorithm      
#'-----------------------------------------------------------------------------



dt_1 <- readRDS(file="./dt_1.rds")
dt_2_search <- readRDS(file="dt_2_search.rds")
dt_3_search <- readRDS(file="dt_3_search.rds")
dt_4_search <- readRDS(file="dt_4_search.rds")
dt_5_search <- readRDS(file="dt_5_search.rds")


# Functions for prediction

## Function to search & find match in a selected gram
## Can use any length of sentence, just need to set start/end indexes
gram_match <- function (sentence, start_index, end_index, gram_to_search) {
  to_search <- word(sentence, start = start_index, end = end_index, 
                    sep = fixed(" "))
  match <- gram_to_search[gram_to_search$input_word==to_search, ]["next_word"][1,1]
  return(match)
  
}


## Create predict function that look into different ngrams
## using a simple backoff model

predict_next_word <- function(input_user) {
  
  input_user <- str_to_lower(input_user)
  n_words <- str_count(input_user, "\\S+")
  fivegram_start <- n_words - 3
  fourgram_start <- n_words - 2
  trigram_start <- n_words - 1
  bigram_start <- n_words
  top_unigrams <- dt_1[1:3,1]
  
  # Start from fivegrams in this scenario 
  
  if(n_words >=4) {
    
    match <- gram_match (input_user, fivegram_start,n_words, dt_5_search)
    if (!is.na(match)) {
      
      print("found in fivegram")
      return(match)
    }
    else { match <- gram_match (input_user, fourgram_start,n_words, dt_4_search)
    if (!is.na(match)) {
      print("found in fourgram")
      return(match)
    }
    else { match <- gram_match (input_user, trigram_start,n_words, dt_3_search)
    if (!is.na(match)) {
      print("found in trigram")
      return(match)
    }
    else { match <- gram_match (input_user, bigram_start,n_words, dt_2_search)
    if (!is.na(match)) {
      print("found in bigram")
      return(match)
    }
    else {return (top_unigrams)}
    }
    }
    }
    
  } 
  # else if (n_words == 3) 
  #   {print ("3 words")} else 
  #   {print ("no word typed yet!")}
  # if sentence has 3 words search from fourgram to unigram
  else if (n_words ==3)
  { match <- gram_match (input_user, fourgram_start,n_words, dt_4_search)
  if (!is.na(match)) {
    print("found in fourgram")
    return(match)
  }
  else { match <- gram_match (input_user, trigram_start,n_words, dt_3_search)
  if (!is.na(match)) {
    print("found in trigram")
    return(match)
  }
  else { match <- gram_match (input_user, bigram_start,n_words, dt_2_search)
  if (!is.na(match)) {
    print("found in bigram")
    return(match)
  }
  else {return (top_unigrams)}
  }
  }
  }
  
  # # if sentence has 2 words search from trigram to unigram
  else if (n_words == 2)
  { match <- gram_match (input_user, trigram_start,n_words, dt_3_search)
  if (!is.na(match)) {
    print("found in trigram")
    return(match)
  }
  else { match <- gram_match (input_user, bigram_start, n_words, dt_2_search)
  if (!is.na(match)) {
    print("found in bigram")
    return(match)
  }
  else {return (top_unigrams)}
  }
  }
  
  # # if sentence has 1 word search from bigram to unigram
  else if (n_words == 1)
  { match <- gram_match (input_user, bigram_start,n_words, dt_2_search)
  if (!is.na(match)) {
    print("found in bigram")
    return(match)
  }
  else {return (top_unigrams)}
  }
  
  # of no word was typed
  else {print("please type a word!")}
  
}
