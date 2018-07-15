# Text Mining on Cancer Survivor Stories

# Source: https://www.cancercenter.com/community/survivors/
# About 125 stories
# No titles but the stories from the survivors themselves. SOme pages have two stories instead of one.
# Different types of cancers represented from survivors.
# Cancer survivor stories are copied and pasted from the cancercenter website.

# 1) Wordclouds 
# 2) Word Counts (Check For themes)
# 3) Bigrams (Two Word Phrases)
# 4) Sentiment Analysis - Are the stories positive or negative?



#----------------------------------

# Load libraries into R:
# Install packages with install.packages("pkg_name")

library(dplyr)
library(tidyr)
library(ggplot2)
library(tidytext)
library(wordcloud)
library(stringr)
library(tm)


#-------------------------------------------

# 1) Wordclouds
# Reference: http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know
# Your file name may vary.


#survivor_stories <- readLines("cancer_survivor_stories.txt", encoding = "Latin-1")


# Source: https://stackoverflow.com/questions/12626637/reading-a-text-file-in-r-line-by-line

fileName <- "cancer_survivor_stories.txt"
conn <- file(fileName, open = "r")
linn <- readLines(conn)

# Preview text:
for (i in 1:length(linn)){
  print(linn[i])
}
close(conn)


cancerStories_text <- Corpus(VectorSource(linn))



survivor_stories_clean <- tm_map(cancerStories_text, content_transformer(tolower))
survivor_stories_clean <- tm_map(survivor_stories_clean, removeNumbers)
survivor_stories_clean <- tm_map(survivor_stories_clean, stripWhitespace)

# Remove English stopwords such as: the, and or, over, under, and so on:

head(stopwords("english"), n = 15) # Sample of English stopwords

survivor_stories_clean <- tm_map(survivor_stories_clean, removeWords, stopwords('english'))

# Source: https://rstudio-pubs-static.s3.amazonaws.com/265713_cbef910aee7642dc8b62996e38d2825d.html


# Convert to Term Document Matrix (TDM):

dtm <- TermDocumentMatrix(survivor_stories_clean)
m <- as.matrix(dtm)
v <- sort(rowSums(m),decreasing=TRUE)
d <- data_frame(word = names(v),freq=v)




#Preview data:
head(d, n = 20)

# Check structure of data frame d:

str(d)

# Convert word column to string column:

d$word <- as.character(d$word)

# Removing this character: â€™
# Reference: https://stackoverflow.com/questions/24576075/gsub-apostrophe-in-data-frame-r
# http://stat545.com/block022_regular-expression.html

# Check word column
#word_col <- d$word
#word_col

# Remove the â€™ character with the lapply function of the data frame d:

#d$word <- lapply(d$word, gsub, pattern = "â€™", replacement = "'")

# Check each row and remove â€™ in the word if there is any.
for (j in (1:nrow(d))){
  d[j, "word"] <- gsub(d[j, "word"],pattern = "â€™", replacement = "'")
}



# Wordcloud with colours:

set.seed(1234)
wordcloud(words = d$word, freq = d$freq, min.freq = 100,
          max.words = 200, random.order=FALSE, rot.per=0.35, 
          colors = rainbow(30))

# Wordcloud with colours with lower max words and raise minimum frequency:

wordcloud(words = d$word, freq = d$freq, min.freq = 150,
          max.words=120, random.order=FALSE, rot.per=0.35, 
          colors = rainbow(30))

# ggplot2 bar plot Of Top 30 Most Common Words 
# In Cancer Survivor Stories (TDM approach):

d[1:30, ] %>% 
  mutate(word = reorder(word, freq)) %>% 
  ggplot(aes(word, freq)) + 
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(x = "Word \n", y = "\n Count ", title = "Top 30 Most Common Words In \n Cancer Survivor Stories \n") +
  geom_text(aes(label = freq), hjust = 1.2, colour = "white", fontface = "bold", size = 3.7) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="darkblue", size = 12),
        axis.title.y = element_text(face="bold", colour="darkblue", size = 12))

#-------------------------------------------

### 2) Word Counts In Survivor Stories - A tidytext approach

survivor_stories <- readLines("cancer_survivor_stories.txt")

# Preview the stories:

survivor_stories_df <- data_frame(Text = survivor_stories) # tibble aka neater data frame

head(survivor_stories_df , n = 20) 

# Unnest tokens: each word in the stories in a row:

survivor_words <- survivor_stories_df %>% 
  unnest_tokens(output = word, input = Text) 

# Preview with head() function:

head(survivor_words, n = 10)

# Remove English stop words from Remember The Name:
# Stop words include the, and, me , you, myself, of, etc.

survivor_words <- survivor_words %>%
  anti_join(stop_words) 

# Word Counts:

survivor_wordcounts <- survivor_words %>% count(word, sort = TRUE)

# Print top 30 words
head(survivor_wordcounts, n = 15)

print(survivor_wordcounts[16:30, ])

# Remove weird words such as iÃ¢ and Ã¢ with filter function from dplyr:
# Use ! negation with str_detect from stringr package.
# https://stackoverflow.com/questions/13043928/selecting-rows-where-a-column-has-a-string-like-hsa-partial-string-match

survivor_wordcounts <- filter(survivor_wordcounts, !str_detect(word, "Ã¢"))

# Print top 30 words:

head(survivor_wordcounts, n = 15)

print(survivor_wordcounts[16:30, ])

# Plot of Word Counts (Top 30 Words):

survivor_wordcounts[1:30, ] %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) + 
  geom_col(fill = "red") +
  coord_flip() +
  labs(x = "Word \n", y = "\n Count ", title = "Top 30 Most Common Words In \n Cancer Survivor Stories \n") +
  geom_text(aes(label = n), hjust = 1, colour = "white", fontface = "bold", size = 3.5) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.ticks.x = element_blank(),
        axis.title.x = element_text(face="bold", colour="darkblue", size = 12),
        axis.title.y = element_text(face="bold", colour="darkblue", size = 12)) 

#-------------------------------------------

### 3) Bigrams (Two-Word Phrases):

survivor_bigrams <- survivor_stories_df %>% 
  unnest_tokens(bigram, input = Text, token = "ngrams", n = 2)

# Look at the bigrams:

survivor_bigrams

# Remove stop words from bigrams with tidyr's separate function
# along with the filter() function

survivor_bigrams_sep <- survivor_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

survivor_bigrams_filt <- survivor_bigrams_sep %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# Filtered bigram counts:
survivor_bigrams_counts <- survivor_bigrams_filt %>% 
  count(word1, word2, sort = TRUE)

head(survivor_bigrams_counts, n = 15)


# Unite the words with the unite() function:

survivor_bigrams_counts <- survivor_bigrams_counts %>%
  unite(bigram, word1, word2, sep = " ")

survivor_bigrams_counts

# Remove the weird Ã£ words from counts list:

# Check each row and remove Ã£ in the america bigram and change Ã£Â¢ to ' 

for (j in (1:nrow(survivor_bigrams_counts))){
  survivor_bigrams_counts[j, "bigram"] <- gsub(survivor_bigrams_counts[j, "bigram"], pattern = "Ã£", replacement = "")
  survivor_bigrams_counts[j, "bigram"] <- gsub(survivor_bigrams_counts[j, "bigram"], pattern = "Â¢ ", replacement = "'")
}


#survivor_bigrams_counts <- filter(survivor_bigrams_counts, !str_detect(bigram, "Ã£"))

# Check again:

print(survivor_bigrams_counts[1:20, ])

# We can now make a plot of the word counts.
# ggplot2 Plot Of Top 20 Bigrams From Cancer Stories:

survivor_bigrams_counts[1:20, ] %>% 
  ggplot(aes(reorder(bigram, n), n)) + 
  geom_col(fill = "darkgray") +
  coord_flip() +
  labs(x = "Bigram \n", y = "\n Count ", title = "Top 20 Bigrams In \n Cancer Survivor Stories \n") +
  geom_text(aes(label = n), hjust = 1.2, colour = "black", fontface = "bold") +
  theme(plot.title = element_text(hjust = 0.5, colour = "darkblue", size = 14), 
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(face="bold", colour="darkred", size = 12)) 

#-------------------------------------------

# 4) Sentiment Analysis 
# Are the stories positive, negative, neutral?

# Using nrc, bing and AFINN lexicons

word_labels_nrc <- c(
  `negative` = "Negative Words",
  `positive` = "Positive Words"
)

### nrc lexicons:
# get_sentiments("nrc")

survivor_words_nrc <- survivor_wordcounts %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  filter(sentiment %in% c("positive", "negative"))

head(survivor_words_nrc)

# Sentiment Plot with nrc Lexicon (Word Count over 70)

survivor_words_nrc %>%
  filter(n > 70) %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = sentiment)) + 
  geom_bar(stat = "identity", position = "identity") +
  geom_text(aes(label = n), colour = "black", hjust = 1, fontface = "bold", size = 3) +
  facet_wrap(~sentiment, nrow = 2, scales = "free_y", labeller = as_labeller(word_labels_nrc)) +
  labs(x = "\n Word \n", y = "\n Word Count ", title = "Negative & Positive Words \n In The Cancer Survivor Stories \n With The nrc Lexicon \n") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="darkblue", size = 12),
        axis.title.y = element_text(face="bold", colour="darkblue", size = 12),
        strip.background = element_rect(fill = "#8DECC7"),
        strip.text.x = element_text(size = 11, face = "bold"),
        strip.text.y = element_text(size = 11, face = "bold")) +
  scale_fill_manual(values=c("#FF0000", "#01DF3A"), guide=FALSE) + 
  coord_flip()

### bing lexicon:
# get_sentiments("bing")

word_labels_bing <- c(
  `negative` = "Negative Words",
  `positive` = "Positive Words"
)

survivor_words_bing <- survivor_wordcounts %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  ungroup()

# Preview the words and counts:

head(survivor_words_bing, n = 15)

# Sentiment Plot with bing Lexicon

survivor_words_bing %>%
  filter(n > 50) %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = sentiment)) + 
  geom_bar(stat = "identity", position = "identity") +
  geom_text(aes(label = n), colour = "black", hjust = 1, fontface = "bold", size = 3) +
  facet_wrap(~sentiment, nrow = 2, scales = "free_y", labeller = as_labeller(word_labels_bing)) +
  labs(x = "\n Word \n", y = "\n Word Count ", title = "Negative & Positive Words \n In The Cancer Survivor Stories \n With The bing Lexicon \n") +
  theme(plot.title = element_text(hjust = 0.5, colour = "darkblue"), 
        axis.title.x = element_text(face="bold", colour="darkred", size = 11),
        axis.title.y = element_text(face="bold", colour="darkred", size = 11),
        strip.background = element_rect(fill = "#C5BFDC"),
        strip.text.x = element_text(size = 11.5, face = "bold"),
        strip.text.y = element_text(size = 11.5, face = "bold")) +
  scale_fill_manual(values=c("#F5665F", "#18EA77"), guide=FALSE) + 
  coord_flip()


### AFINN lexicon:

# Change labels 
# (Source: https://stackoverflow.com/questions/3472980/ggplot-how-to-change-facet-labels)

survivor_words_AFINN <- survivor_wordcounts %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  mutate(is_positive = score > 0) 

head(survivor_words_AFINN, n = 15)



word_labels_AFINN <- c(
  `FALSE` = "Negative Words",
  `TRUE` = "Positive Words"
)

survivor_words_AFINN %>%
  filter(n >= 50) %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = is_positive)) + 
  geom_bar(stat = "identity", position = "identity") +
  geom_text(aes(label = n), colour = "black", hjust = 1, fontface = "bold", size = 3) +
  facet_wrap(~is_positive, scales = "free_y", nrow = 2, labeller = as_labeller(word_labels_AFINN)) +
  labs(x = "\n Word \n", y = "\n Word Count ", title = "Negative & Positive Words \n In The Cancer Survivor Stories \n With The AFINN Lexicon \n",
       fill = c("Negative", "Positive")) +
  theme(plot.title = element_text(hjust = 0.5, colour = "darkred"), 
        axis.title.x = element_text(face="bold", colour="darkblue", size = 10.5),
        axis.title.y = element_text(face="bold", colour="darkblue", size = 10.5),
        strip.background = element_rect(fill = "#ECCDF0"),
        strip.text.x = element_text(size = 11.5, face = "bold"),
        strip.text.y = element_text(size = 11.5, face = "bold")) +
  scale_fill_manual(values=c("#F5665F", "#18EA77"), guide=FALSE) + 
  coord_flip()
