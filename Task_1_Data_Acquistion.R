library(tm)
library(tidyverse)

# -------------------------------------------------------------------------

if (!file.exists("data")) {
  dir.create("data")
}

url <- "https://d396qusza40orc.cloudfront.net/dsscapstone/dataset/Coursera-SwiftKey.zip"
download.file(url = url, destfile = "dataset.zip", mode = "wb")
unzip(zipfile = "dataset.zip", exdir = "./data")

# -------------------------------------------------------------------------

blogs_file <- "./data/final/en_US/en_US.blogs.txt"
news_file <- "./data/final/en_US/en_US.news.txt"
twitter_file <- "./data/final/en_US/en_US.twitter.txt"

# Get each file's size
blogs_size <- file.size(blogs_file)/2^20
news_size <- file.size(news_file)/2^20
twitter_size <- file.size(twitter_file)/2^20

# Read in each file   
blogs <- read_lines(blogs_file)
news <- read_lines(news_file)
twitter <- read_lines(twitter_file) 

# Get the number of lines in each file
blogs_lines <- length(blogs)
news_lines <- length(news)
twitter_lines <- length(twitter)

# Get the number of characters per line in each file
blogs_nchar <- nchar(blogs)
news_nchar <- nchar(news)
twitter_nchar <- nchar(twitter)

# Create a tibble with 2 columns which are `type` and `num_char`
tibble_nchar <- tibble(type = rep("blogs", blogs_lines),
                       num_char = blogs_nchar) %>% 
  bind_rows(tibble(type = rep("news", news_lines),
                   num_char = news_nchar)) %>% 
  bind_rows(tibble(type = rep("twitter", twitter_lines),
                   num_char = twitter_nchar))
# Plot a boxplot comparing the distribution of the number of characters 
# per line among 3 kinds of text collections
tibble_nchar %>% 
  ggplot(aes(y = num_char, x = type, fill = type)) +
  geom_boxplot(show.legend = FALSE) +
  labs(x = NULL, y = "Number of characters per line") +
  scale_y_log10()

# Get the total number of characters in each whole file
blogs_nchar_sum <- sum(blogs_nchar)
news_nchar_sum <- sum(news_nchar)
twitter_nchar_sum <- sum(twitter_nchar)

# Get the number of words per line in each file
blogs_word <- blogs %>% 
  str_trim() %>% 
  str_squish() %>% 
  str_split(" ") %>% 
  purrr::map_int(length)
news_word <- news %>%
  str_trim() %>% 
  str_squish() %>% 
  str_split(" ") %>% 
  purrr::map_int(length)
twitter_word <- twitter %>%
  str_trim() %>% 
  str_squish() %>% 
  str_split(" ") %>% 
  purrr::map_int(length)

# Create a tibble with 2 columns which are `type` and `num_word`
tibble_word <- tibble(type = rep("blogs", blogs_lines),
                     num_word = blogs_word) %>% 
  bind_rows(tibble(type = rep("news", news_lines),
                   num_word = news_word)) %>% 
  bind_rows(tibble(type = rep("twitter", twitter_lines),
                   num_word = twitter_word))
# Plot a boxplot comparing the distribution of the number of words per
# line among 3 kinds of text collections
tibble_word %>% 
  ggplot(aes(y = num_word, x = type, fill = type)) +
  geom_boxplot(show.legend = FALSE) +
  labs(x = NULL, y = "Number of words per line") +
  scale_y_log10()

# Get the total number of words in each whole file
blogs_word_sum <- sum(blogs_word)
news_word_sum <- sum(news_word)
twitter_word_sum <- sum(twitter_word)

# Create a summary table
repo_summary <- tibble(file = c("blogs", "news", "twitter"),
                       size = c(blogs_size, news_size, twitter_size),
                       num_lines = c(blogs_lines, news_lines, twitter_lines),
                       num_chars = c(blogs_nchar_sum, news_nchar_sum, twitter_nchar_sum),
                       num_words = c(blogs_word_sum, news_word_sum, twitter_word_sum)) %>%
  mutate(pct_num_lines = num_lines/sum(num_lines),
         pct_num_chars = num_chars/sum(num_chars),
         pct_num_words = num_words/sum(num_words))
repo_summary %>% glimpse()

# Set a sample size for each file
sample_pct <- 0.1
blogs_sample_size <- round(blogs_lines*sample_pct)
news_sample_size <- round(news_lines*sample_pct)
twitter_sample_size <- round(twitter_lines*sample_pct)

# Take a subset of the original file as a sample
set.seed(12345)
blogs_sample <- sample(blogs, blogs_sample_size)
news_sample <- sample(news, news_sample_size)
twitter_sample <- sample(twitter, twitter_sample_size)

# Combine all the samples into a complete text dataset
repo_sample <- tibble(
  text = c(blogs_sample, news_sample, twitter_sample),
  type = rep(c("blogs", "news", "twitter"), 
             times = c(blogs_sample_size, news_sample_size, twitter_sample_size))
  )

# Save the complete text dataset
saveRDS(repo_sample, "repo_sample.rds")