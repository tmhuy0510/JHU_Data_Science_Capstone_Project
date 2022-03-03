library(shiny)
library(tidyverse)

count_unigram <- readRDS("count_unigram.rds")
count_bigram <- readRDS("count_bigram.rds")
count_trigram <- readRDS("count_trigram.rds")
count_quadgram <- readRDS("count_quadgram.rds")

shinyServer(function(input, output) {
    
    quadgram_term <- function(phrase) {
        phrase <- phrase %>% str_extract("([:alpha:]+ [:alpha:]+ [:alpha:]+)$") %>% str_trim()
        rec_term <- count_quadgram %>%
            filter(str_detect(quadgram, str_c("^", phrase, "\\b"))) %>% 
            slice_max(n, n = 3) %>% 
            mutate(term = str_remove(quadgram, str_c("^", phrase, "\\b "))) %>% 
            pull(term)
        return(rec_term)
    }
    
    trigram_term <- function(phrase) {
        phrase <- phrase %>% str_extract("([:alpha:]+ [:alpha:]+)$") %>% str_trim()
        rec_term <- count_trigram %>%
            filter(str_detect(trigram, str_c("^", phrase, "\\b"))) %>% 
            slice_max(n, n = 3) %>% 
            mutate(term = str_remove(trigram, str_c("^", phrase, "\\b "))) %>% 
            pull(term)
        return(rec_term)
    }
    
    bigram_term <- function(phrase) {
        phrase <- phrase %>% str_extract("([:alpha:]+)$") %>% str_trim()
        rec_term <- count_bigram %>%
            filter(str_detect(bigram, str_c("^", phrase, "\\b"))) %>% 
            slice_max(n, n = 3) %>% 
            mutate(term = str_remove(bigram, str_c("^", phrase, "\\b "))) %>% 
            pull(term)
        return(rec_term)
    }
    
    unigram_term <- function(phrase) {
        rec_term <- count_unigram %>%
            slice_max(n, n = 3) %>% 
            pull(word)
        return(rec_term)
    }
    
    ngram_term <- function(phrase) {
        rec_term <- quadgram_term(phrase)
        if (length(rec_term) == 0) {rec_term <- trigram_term(phrase)}
        if (length(rec_term) == 0) {rec_term <- bigram_term(phrase)}
        if (length(rec_term) == 0) {rec_term <- unigram_term(phrase)}
        return(rec_term)
    }
    
    clean_my_text <- function(my_text) {
        my_text %>% 
            str_remove_all("http[^[:space:]]*") %>% 
            str_remove_all("[^[:space:]|[:alpha:]]*") %>% 
            str_to_lower() %>% 
            str_trim() %>% 
            str_squish()
    }
    
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
        
        rec_term <- tibble(`Option ID` = seq_along(rec_term), 
                           `Recommended Word` = rec_term)
        return(rec_term)
    }
    
    output$table <- renderTable(
        {ngram_model(input$text)}, 
        align = "c", hover = T, striped = F,
        bordered = T, spacing = "l", width = "100%"
    )
})
