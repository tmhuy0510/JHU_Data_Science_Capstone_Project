library(tidyverse)
library(tidytext)

# -------------------------------------------------------------------------

# Load the sample of data
repo_sample <- readRDS("repo_sample.rds")

# Get a list of words which need removing
# Get a list of stop words
stop_words_snowball <- stop_words %>% 
  filter(lexicon == "snowball")
stop_words_snowball
# Get a list of profanity words
if (!file.exists("./data/final/en_US/profanity_words.txt")) {
  profanity_url <- "https://raw.githubusercontent.com/LDNOOBW/List-of-Dirty-Naughty-Obscene-and-Otherwise-Bad-Words/master/en"
  download.file(url = profanity_url, destfile = "./data/final/en_US/profanity_words.txt")
}
profanity_words <- read_csv("./data/final/en_US/profanity_words.txt", 
                            col_names = FALSE) %>% 
  rename("word" = "X1") %>% 
  mutate(word = word %>% str_to_lower())
profanity_words

# Create some functions for cleaning
# Create a function to remove all URLs
removeUrls <- function(x) {
  str_remove_all(x, "http[^[:space:]]*")
}
# Create a function to remove all non-alphabetic or non-space characters
removeNumPunct <- function(x) {
  str_remove_all(x, "[^[:space:]|[:alpha:]]*")
}

# Prepare the data sample for tokenization
repo_sample_clean <- repo_sample %>% 
  mutate(text = removeUrls(text)) %>% 
  mutate(text = removeNumPunct(text))

# Tokenize the text using 1-gram
repo_sample_unigram <- repo_sample_clean %>% 
  unnest_tokens(word, text)
# Count the occurrences of 1-gram
count_unigram <- repo_sample_unigram %>% 
  count(word, sort = TRUE)
# Remove 1-gram with stopwords
repo_sample_filtered_unigram <- repo_sample_unigram %>%
  anti_join(profanity_words) %>% 
  anti_join(stop_words_snowball)
# Count the occurrences of 1-gram without certain words
count_filtered_unigram <- repo_sample_filtered_unigram %>% 
  count(word, sort = TRUE)
# Save the counts of 1-grams and filtered 1-grams
saveRDS(count_unigram, "count_unigram.rds")
saveRDS(count_filtered_unigram, "count_filtered_unigram.rds")
# Create a word cloud of top 30 common filtered words
wordcloud::wordcloud(count_filtered_unigram$word, 
                     count_filtered_unigram$n,
                     max.words = 50,
                     random.order = FALSE,
                     colors = RColorBrewer::brewer.pal(6,"Dark2"))

# Make a comparison among 3 sources of texts
count_filtered_unigram_by_type <- repo_sample_filtered_unigram %>% 
  count(type, word, sort = TRUE)
saveRDS(count_filtered_unigram_by_type, "count_filtered_unigram_by_type.rds")
# Get term frequency
count_filtered_unigram_by_type %>% 
  group_by(type) %>% 
  slice_max(n, n = 15) %>%
  ungroup() %>% 
  ggplot(aes(n, reorder_within(word, n, type), fill = type)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(vars(type), scales = "free") +
  scale_y_reordered() +
  labs(x = "Frequency", y = NULL)
# Get tf-idf
type_tf_idf <- count_filtered_unigram_by_type %>% 
  bind_tf_idf(word, type, n)
type_tf_idf %>% 
  group_by(type) %>% 
  slice_max(tf_idf, n = 15) %>% 
  ungroup() %>% 
  ggplot(aes(tf_idf, reorder_within(word, tf_idf, type), fill = type)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(vars(type), scales = "free") +
  scale_y_reordered() +
  labs(x = "tf-idf", y = NULL)

# Tokenize the text using 2-gram
repo_sample_bigram <- repo_sample_clean %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2)
# Count the occurrences of 2-grams
count_bigram <- repo_sample_bigram %>% 
  count(bigram, sort = TRUE)
count_bigram <- count_bigram %>% 
  filter(!is.na(bigram),
         n >= 10)
# Remove 2-grams with certain words
repo_sample_filtered_bigram <- repo_sample_bigram %>% 
  separate(bigram, c("item1", "item2"), sep = " ", remove = FALSE) %>% 
  filter(!item1 %in% c(stop_words_snowball$word,
                       profanity_words$word),
         !item2 %in% c(stop_words_snowball$word,
                       profanity_words$word)) %>% 
  select(-item1, -item2)
# Count the occurrences of 2-grams without certain words
count_filtered_bigram <- repo_sample_filtered_bigram %>% 
  count(bigram, sort = TRUE)
count_filtered_bigram <- count_filtered_bigram %>% 
  filter(!is.na(bigram),
         n >= 10)
# Save the counts of 2-grams and filtered 2-grams
saveRDS(count_bigram, "count_bigram.rds")
saveRDS(count_filtered_bigram, "count_filtered_bigram.rds")
# Create a plot for top 20 2-grams with and without certain words
count_bigram %>% 
  slice_max(n, n = 20) %>% 
  ggplot(aes(n, reorder(bigram, n))) +
  geom_col(fill = "midnightblue") +
  labs(x = "Frequency", y = NULL)
count_filtered_bigram %>% 
  slice_max(n, n = 20) %>% 
  ggplot(aes(n, reorder(bigram, n))) +
  geom_col(fill = "seagreen") +
  labs(x = "Frequency", y = NULL)

# Tokenize the text using 3-gram
repo_sample_trigram <- repo_sample_clean %>% 
  unnest_tokens(trigram, text, token = "ngrams", n = 3)
# Count the occurrences of 3-grams
count_trigram <- repo_sample_trigram %>% 
  count(trigram, sort = TRUE)
count_trigram <- count_trigram %>% 
  filter(!is.na(trigram),
         n >= 10)
# Remove 3-grams with certain words
repo_sample_filtered_trigram <- repo_sample_trigram %>%
  semi_join(count_trigram) %>% 
  separate(trigram, c("item1", "item2", "item3"), sep = " ", 
           remove = FALSE) %>% 
  filter(!item1 %in% c(stop_words_snowball$word,
                       profanity_words$word),
         !item2 %in% c(stop_words_snowball$word,
                       profanity_words$word),
         !item3 %in% c(stop_words_snowball$word,
                       profanity_words$word)) %>% 
  select(-item1, -item2, -item3)
# Count the occurrences of 3-grams without certain words
count_filtered_trigram <- repo_sample_filtered_trigram %>% 
  count(trigram, sort = TRUE)
count_filtered_trigram <- count_filtered_trigram %>% 
  filter(!is.na(trigram),
         n >= 10)
# Save the counts of 3-grams and filtered 3-grams
saveRDS(count_trigram, "count_trigram.rds")
saveRDS(count_filtered_trigram, "count_filtered_trigram.rds")
# Create a plot for top 20 3-grams with and without certain words
count_trigram %>% 
  slice_max(n, n = 20) %>% 
  ggplot(aes(n, reorder(trigram, n))) +
  geom_col(fill = "midnightblue") +
  labs(x = "Frequency", y = NULL)
count_filtered_trigram %>% 
  slice_max(n, n = 20) %>% 
  ggplot(aes(n, reorder(trigram, n))) +
  geom_col(fill = "seagreen") +
  labs(x = "Frequency", y = NULL)

# Tokenize the text using 4-gram
repo_sample_quadgram <- repo_sample_clean %>% 
  unnest_tokens(quadgram, text, token = "ngrams", n = 4)
# Count the occurrences of 4-grams
count_quadgram <- repo_sample_quadgram %>% 
  count(quadgram, sort = TRUE)
count_quadgram <- count_quadgram %>% 
  filter(!is.na(quadgram),
         n >= 10)
# Remove 4-grams with certain words
repo_sample_filtered_quadgram <- repo_sample_quadgram %>% 
  semi_join(count_quadgram) %>% 
  separate(quadgram, c("item1", "item2", "item3", "item4"), sep = " ", 
           remove = FALSE) %>% 
  filter(!item1 %in% c(stop_words_snowball$word,
                       profanity_words$word),
         !item2 %in% c(stop_words_snowball$word,
                       profanity_words$word),
         !item3 %in% c(stop_words_snowball$word,
                       profanity_words$word),
         !item4 %in% c(stop_words_snowball$word,
                       profanity_words$word)) %>% 
  select(-item1, -item2, -item3, -item4)
# Count the occurrences of 4-grams without certain words
count_filtered_quadgram <- repo_sample_filtered_quadgram %>% 
  count(quadgram, sort = TRUE)
count_filtered_quadgram <- count_filtered_quadgram %>% 
  filter(!is.na(quadgram),
         n >= 10)
# Save the counts of 3-grams and filtered 3-grams
saveRDS(count_quadgram, "count_quadgram.rds")
saveRDS(count_filtered_quadgram, "count_filtered_quadgram.rds")
# Create a plot for top 20 4-grams with and without certain words
count_quadgram %>% 
  slice_max(n, n = 20) %>% 
  ggplot(aes(n, reorder(quadgram, n))) +
  geom_col(fill = "midnightblue") +
  labs(x = "Frequency", y = NULL)
count_filtered_quadgram %>% 
  slice_max(n, n = 20) %>% 
  ggplot(aes(n, reorder(quadgram, n))) +
  geom_col(fill = "seagreen") +
  labs(x = "Frequency", y = NULL)

# Do experiment on term prediction using 1-gram, 2-gram, 3-gram and 
# 4-gram models
# Input
my_text <- "thank you for the follow and"
# Output of the 4-gram model
my_phrase <- my_text %>% 
  str_extract("( [:alpha:]+){3}$") %>% 
  str_trim()
count_quadgram %>% 
  filter(str_detect(quadgram, paste0("^", my_phrase, "\\b"))) %>% 
  slice_max(n, n = 3)
# No result
# Output of the 3-gram model
my_phrase <- my_phrase %>% 
  str_extract("( [:alpha:]+){2}$") %>% 
  str_trim()
count_trigram %>% 
  filter(str_detect(trigram, paste0("^", my_phrase, "\\b"))) %>% 
  slice_max(n, n = 3)
# No result
# Output of the 2-gram model
my_phrase <- my_phrase %>% 
  str_extract("( [:alpha:]+){1}$") %>% 
  str_trim()  
count_bigram %>% 
  filter(str_detect(bigram, paste0("^", my_phrase, "\\b"))) %>% 
  slice_max(n, n = 3)
# Output of the 1-gram model
count_unigram %>%
  slice_max(n, n = 3)