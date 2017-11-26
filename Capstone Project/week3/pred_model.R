#'-----------------------------------------------------------------------------
#'                          pred_model.R
#' Contain the whole process from getting the corpora to build and test prediction model
#' - Load corpora
#' - Sample
#' - Cleaning
#' - NLP processing with quanteda library
#' - Create ngram tables
#' - Build prediction model
#' - Test prediction model
#'-----------------------------------------------------------------------------


# Read english texts ----------------------------------------------------------
path <- ("C:/Users/Marco/Documents/ds_specialization_github/datasciencecoursera/Capstone Project/swiftkey_data/en_US/")
blogs <- readLines(paste0(path,"en_US.blogs.txt"), encoding = "UTF-8")
news <- readLines(paste0(path,"en_US.news.txt"), encoding = "UTF-8")
twitter <- readLines(paste0(path,"en_US.twitter.txt"), encoding = "UTF-8")

# Sampling --------------------------------------------------------------------
sample_10_per <- function (x) {sample(x , length(x)*0.10, replace = F)}
set.seed(15)
blogs_s <- sample_10_per(blogs)
news_s <- sample_10_per(news)
twitter_s <- sample_10_per(twitter)

# We can remove original text files loaded into R to free up memory
library(pryr)
mem_used()
rm(blogs, news, twitter)
mem_used() 

# Initial cleaning ------------------------------------------------------------
#
# Create a function to convert to ascii
conv_to_ascii <- function(x) {
  to_ascii <- iconv(x, "latin1", "ASCII", sub="")
  return(to_ascii)
}

blogs_s <- conv_to_ascii(blogs_s)
news_s <- conv_to_ascii(news_s)
twitter_s <- conv_to_ascii(twitter_s)

# Create a function to remove twitter like strings starting with # or @
# also remove hashtags and mentions from twitter text
remove_twitter <- function(x) {
  removed_tweets <- gsub("(@|#)\\w+ *", "", twitter_s)
  return(removed_tweets)
}

blogs_s <- remove_twitter(blogs_s)
news_s <- remove_twitter(news_s)
twitter_s <- remove_twitter(twitter_s)

## There are still words with @ or # but that's ok they are in the middle.
# twitter_s[grep("@", twitter_s)]

# Create a function to remove profanity words

# Create a function to remove emoticons e.g. :) ??

# Remove numbers? there are still quiete a few apparently after quanteda



# Combine the 3 samples in one dataset ----------------------------------------
all_samples <- c(blogs_s, news_s, twitter_s)
rm(blogs_s, news_s, twitter_s)

# NLP processing (using quanteda) ---------------------------------------------
library(quanteda)
library(tidytext)

# Create function to generate ngrams
create_gram <- function (x, n) {
  gram <- dfm(x, ngrams=n, concatenator=" ", tolower = T, 
              stem = F,  remove_punct=T,  remove_numbers=T, # not stemming because I am seeing is cutting too many words 
              remove = stopwords("english"), dictionary = NULL, 
              verbose=TRUE)
  return(gram)
}

unigrams <- create_gram(all_samples, 1) #51.2 15per 83.3 seconds
bigrams <- create_gram(all_samples, 2) #55.2 15 per 92.7
trigrams <- create_gram(all_samples, 3) #69.6 15 per 101
fourgrams <- create_gram(all_samples, 4) #81.7 15 per 123
fivegrams <- create_gram(all_samples, 5) #15 per 121

# # check results
# topfeatures(fourgrams, 100)
# head(featnames(unigrams), 10)


# Convert ngrams to data frame ------------------------------------------------
library(data.table) # faster and using less memory
library(dplyr)

convert_dt <- function (x) {
  dt <- data.table(ngram = featnames(x), 
                   count = colSums(x), 
                   key = "ngram")
  dt <- arrange(dt, desc(count))
}

dt_1 <- convert_dt(unigrams)
dt_2 <- convert_dt(bigrams)
dt_3 <- convert_dt(trigrams)
dt_4 <- convert_dt(fourgrams)
dt_5 <- convert_dt(fivegrams)

# # check size
# format(object.size(dt_3), "Mb") # 15 per 134 Mb 194 Mb
# format(object.size(dt_4), "Mb") #168 Mb 249 Mb
# format(object.size(dt_5), "Mb") # 250 Mb

rm(unigrams, bigrams, trigrams, fourgrams, fivegrams)


# Create a function to prepare dt for next word searching (plit string in input_word vs next_word)
library(stringr)
to_next_word_search <- function (dt, input_end=2) {
  dt$input_word <- word(dt$ngram, start = 1, end = input_end, sep = fixed(" "))
  dt$next_word <- word(dt$ngram,-1)
  return(dt)
}

dt_2_search <- to_next_word_search(dt_2, input_end = 1)
dt_3_search <- to_next_word_search(dt_3, input_end = 2)
dt_4_search <- to_next_word_search(dt_4, input_end = 3)
dt_5_search <- to_next_word_search(dt_5, input_end = 4)

format(object.size(dt_3_search), "Mb") # 15 per 299 Mb
format(object.size(dt_5_search), "Mb") # 15 per 483 Mb

rm(dt_2, dt_3, dt_4, dt_5)


# Search next word in a certain ngram -----------------------------------------

## Create a function to return next_word in a certain ngram given a user input
return_next_word <- function (to_search, dt_to_search) {
  to_search <- tolower(to_search)
  dt_to_search[dt_to_search$input_word==to_search, ]["next_word"][1,1]
}

return_next_word("please do", dt_3_search)


## Function to search & find match in a selected gram
## can use any length of sentence, just need to set start/end indexes
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



# Test prediction function ----------------------------------------------------
input_user <- "that awkward moment when"
str_count(to_search, "\\S+")
str_to_lower(to_search)

predict_next_word("I have a feeling")

# Improvements
## See 4_evaluation.Rmd
