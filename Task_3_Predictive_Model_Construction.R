library(tidyverse)
library(tidytext)

# -------------------------------------------------------------------------

# Read in data on counts of 1-grams, 2-grams, 3-grams and 4-grams
count_unigram <- readRDS("count_unigram.rds")
count_bigram <- readRDS("count_bigram.rds")
count_trigram <- readRDS("count_trigram.rds")
count_quadgram <- readRDS("count_quadgram.rds")

# Create a function to predict the next term of the provided text using
# n-gram
# In the case of 4-gram
quadgram_term <- function(phrase) {
  phrase <- phrase %>% str_extract("([:alpha:]+ [:alpha:]+ [:alpha:]+)$") %>% str_trim()
  rec_term <- count_quadgram %>%
    filter(str_detect(quadgram, str_c("^", phrase, "\\b"))) %>% 
    slice_max(n, n = 3) %>% 
    mutate(term = str_remove(quadgram, str_c("^", phrase, "\\b "))) %>% 
    pull(term)
  return(rec_term)
}
saveRDS(quadgram_term, "quadgram_term.rds")
# In the case of 3-gram
trigram_term <- function(phrase) {
  phrase <- phrase %>% str_extract("([:alpha:]+ [:alpha:]+)$") %>% str_trim()
  rec_term <- count_trigram %>%
    filter(str_detect(trigram, str_c("^", phrase, "\\b"))) %>% 
    slice_max(n, n = 3) %>% 
    mutate(term = str_remove(trigram, str_c("^", phrase, "\\b "))) %>% 
    pull(term)
  return(rec_term)
}
saveRDS(trigram_term, "trigram_term.rds")
# In the case of 2-gram
bigram_term <- function(phrase) {
  phrase <- phrase %>% str_extract("([:alpha:]+)$") %>% str_trim()
  rec_term <- count_bigram %>%
    filter(str_detect(bigram, str_c("^", phrase, "\\b"))) %>% 
    slice_max(n, n = 3) %>% 
    mutate(term = str_remove(bigram, str_c("^", phrase, "\\b "))) %>% 
    pull(term)
  return(rec_term)
}
saveRDS(bigram_term, "bigram_term.rds")
# In the case of 1-gram
unigram_term <- function(phrase) {
  rec_term <- count_unigram %>%
    slice_max(n, n = 3) %>% 
    pull(word)
  return(rec_term)
}
saveRDS(unigram_term, "unigram_term.rds")

# Create a predictive model predicting the next term from a provided text
# using n-gram with higher priority put on higher-ordered-gram
ngram_term <- function(phrase) {
  rec_term <- quadgram_term(phrase)
  if (length(rec_term) == 0) {rec_term <- trigram_term(phrase)}
  if (length(rec_term) == 0) {rec_term <- bigram_term(phrase)}
  if (length(rec_term) == 0) {rec_term <- unigram_term(phrase)}
  return(rec_term)
}
saveRDS(ngram_term, "ngram_term.rds")
# The input `phrase` of the model must be a preprocessed text

# Create a function to preprocess the text input of the model
clean_my_text <- function(my_text) {
  my_text %>% 
    str_remove_all("http[^[:space:]]*") %>% 
    str_remove_all("[^[:space:]|[:alpha:]]*") %>% 
    str_to_lower() %>% 
    str_trim() %>% 
    str_squish()
}
saveRDS(clean_my_text, "clean_my_text.rds")

# Test the model with some different inputs 
# my_text <- character(0)
# my_text <- ""
# my_text <- "this"
# my_text <- "this will"
# my_text <- "this will go"
# my_text <- "i would like to thank you forlkj"
# my_text <- "I'm here at 10 p.m in Room 404E (https://www.cbc.ca).\n Where are you?"
my_text <- clean_my_text(my_text)
my_text
last_term <- my_text %>% str_extract("([:alpha:]+)$")
last_term
length(last_term)
if (length(last_term) == 0 || is.na(last_term)) {
  rec_term <- unigram_term(my_text)
} else {
  rec_term <- ngram_term(my_text)
  if (!last_term %in% count_unigram$word) {
    rec_term <- c("MISPELLED WORD?", rec_term)
  }
}
rec_term

# Create a final model
ngram_model <- function(my_text) {
  my_text <- clean_my_text(my_text)
  last_term <- my_text %>% str_extract("([:alpha:]+)$")
  length(last_term)
  
  if (length(last_term) == 0 || is.na(last_term)) {
    # Check if the text input is "" or NULL
    rec_term <- unigram_term(my_text)
  } else {
    rec_term <- ngram_term(my_text)
    if (!last_term %in% count_unigram$word) {
      # Check if the last word of the text input is misspelled 
      rec_term <- c("MISPELLED WORD?", rec_term)
    }
  }
  
  rec_term <- tibble(`Option` = seq_along(rec_term), 
                     `Recommended Word` = rec_term)
  return(rec_term)
}
saveRDS(ngram_model, "ngram_model.rds")
