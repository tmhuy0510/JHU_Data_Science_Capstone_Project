library(tm)

# -------------------------------------------------------------------------

# Find the sample corpus
txt <- system.file("texts", "txt", package = "tm")

# Load the corpus
ovid <- Corpus(DirSource(txt, encoding = "UTF-8"),
               readerControl = list(reader = readPlain,
                                    language = "lat",
                                    load = TRUE))
ovid

# Examine the corpus
ovid[1]
ovid[[1]]
## Content
ovid[[1]]$content
content(ovid[[1]])
## Metadata
ovid[[1]]$meta
meta(ovid[[1]])
## Number of documents
length(ovid)
## Structure
summary(ovid)
summary(ovid[1])
summary(ovid[[1]])
## Detailed information
inspect(ovid)
inspect(ovid[1])
inspect(ovid[[1]])

# Show predefined transformations
getTransformations()
## Remove punctuation
ovid_removePunctuation <- tm_map(ovid, removePunctuation)
inspect(ovid_removePunctuation[[1]])
## Remove numbers
ovid_removeNumbers <- tm_map(ovid, removeNumbers)
inspect(ovid_removeNumbers[[1]])
## Change to all lower case
ovid_toLower <- tm_map(ovid, content_transformer(str_to_lower))
inspect(ovid_toLower[[1]])
## Remove words
words <- c("doctus", "aptus")
ovid_removeWords <- tm_map(ovid, removeWords, words)
inspect(ovid_removeWords[[1]])
## Remove whitespace
ovid_stripWhitespace <- tm_map(ovid, stripWhitespace)
inspect(ovid_stripWhitespace[[1]])
## Stem the corpus
ovid_stemDocument <- tm_map(ovid, stemDocument)
inspect(ovid_stemDocument[[1]])

# -------------------------------------------------------------------------

# Find the sample corpus
reut21578 <- system.file("texts", "crude", package = "tm")

# Load the corpus
reuters <- VCorpus(DirSource(reut21578),
                   readerControl = list(reader = readReut21578XMLasPlain))

# Examine the corpus
reuters
reuters[1]
reuters[[1]]
## Content
reuters[[1]]$content
content(reuters[[1]])
## Metadata
reuters[[1]]$meta
meta(reuters[[1]])
## Number of documents
length(reuters)
## Structure
summary(reuters)
summary(reuters[1])
summary(reuters[[1]])

## Detailed information of document 1
inspect(reuters[[1]])
## Remove whitespace
tm_map(reuters[1], stripWhitespace) %>% .[[1]] %>% inspect()
## Convert to lower case
tm_map(reuters[1], content_transformer(str_to_lower)) %>% .[[1]] %>% inspect()
## Stemming
tm_map(reuters[1], stemDocument) %>% .[[1]] %>% inspect()
## Remove stop words
tm_map(reuters[1], removeWords, stopwords("en")) %>% .[[1]] %>% inspect()
## Remove punctuation
tm_map(reuters[1], removePunctuation) %>% .[[1]] %>% inspect()
## Remove numbers
tm_map(reuters, removeNumbers) %>% .[[1]] %>% inspect()

# Create a document-term matrix
dtm <- DocumentTermMatrix(reuters)
dtm
## Inspect a subset of the document-term matrix
inspect(dtm[5:10, 740:743])

# Operations on Document-Term Matrices
## Find terms whose term frequency is 5
findFreqTerms(dtm, 5)
## Find terms correlated with a certain term with a certain correlation
findAssocs(dtm, "opec", 0.8)
## Remove sparse terms out of the document-term matrix to get a certain
## sparsity
removeSparseTerms(dtm, sparse = 0.4)
## Specify certain terms included in the document-term matrix
DocumentTermMatrix(reuters,
                   control = list(dictionary = c("prices", "crude", "oil"))) %>% 
  inspect()
