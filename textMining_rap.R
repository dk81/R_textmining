# Text Mining & Text Analysis On Rap Music
# Reference - Text Mining With R: A Tidy Approach by Julia Silge & David
#             Robinson [Online Book]        
# Songs

# 1) Fort Minor - Remember The Name
# 2) Eminem - Lose Yourself


# Lyrics taken from azlyrics.com
# Do word counts, sentiment analysis, bigrams, trigrams?

### Introduction

# Text mining

# Load libraries into R:

library(dplyr)
library(ggplot2)
library(tidytext)
library(tidyr)


### 1) Fort Minor - Remember The Name

# Read Fort Minor - Remember The Name lyrics into R:

remember_lyrics <- readLines("fortMinor_rememberTheName_lyrics.txt")

# Preview the lyrics:

remember_lyrics_df <- data_frame(Text = remember_lyrics) # tibble aka neater data frame

head(remember_lyrics_df, n = 10) 

# Unnest tokens: each word in the lyrics in a row:

remember_words <- remember_lyrics_df %>% 
  unnest_tokens(output = word, input = Text) 

# Preview with head() function:

head(remember_words, n = 10)


### 1a) Word Counts in Remember The Name:

# Remove English stop words from Remember The Name:
# Stop words include the, and, me , you, myself, of, etc.

remember_words <- remember_words %>%
  anti_join(stop_words) 

# Word Counts:

remember_wordcounts <- remember_words %>% count(word, sort = TRUE)

head(remember_wordcounts, n = 15)

# Plot of Word Counts (Top 15 Words):

remember_wordcounts[1:15, ] %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) + 
  geom_col(fill = "red") +
  coord_flip() +
  labs(x = "Word \n", y = "\n Count ", title = "Word Counts In \n Fort Minor - Remember The Name \n") +
  geom_text(aes(label = n), hjust = 1.2, colour = "white", fontface = "bold") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(face="bold", colour="darkblue", size = 12)) 

### 1b) Bigrams (Two-Word Phrases) in Remember The Name:

remember_bigrams <- remember_lyrics_df %>% 
  unnest_tokens(bigram, input = Text, token = "ngrams", n = 2)

# Look at the bigrams:

remember_bigrams

# Remove stop words from bigrams with tidyr's separate function
# along with the filter() function

bigrams_remember_sep <- remember_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_remember_filt <- bigrams_remember_sep %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# Filtered bigram counts:
bigrams_remember_counts <- bigrams_remember_filt %>% 
  count(word1, word2, sort = TRUE)


head(bigrams_remember_counts, n = 15)

# Unite the words with the unite() function:

remember_bigrams_counts <- bigrams_remember_counts %>%
  unite(bigram, word1, word2, sep = " ")

remember_bigrams_counts

# We can now make a plot of the word counts.


# ggplot2 Plot (Counts greater than 8)
# Bottom axis removed with element_blank()
# Counts in the bar with geom_text.

remember_bigrams_counts[1:15, ] %>% 
  ggplot(aes(reorder(bigram, n), n)) + 
  geom_col(fill = "red") +
  coord_flip() +
  labs(x = "Bigram \n", y = "\n Count ", title = "Bigrams In Fort Minor - Remember The Name \n") +
  geom_text(aes(label = n), hjust = 1.2, colour = "white", fontface = "bold") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(face="bold", colour="darkblue", size = 12)) 

### 1c) Trigrams (Three-Word Phrases) in Remember The Name:

remember_trigrams <- remember_lyrics_df %>% 
  unnest_tokens(bigram, input = Text, token = "ngrams", n = 3)

# Look at the trigrams:

remember_trigrams

# Remove stop words from the trigrams with tidyr's separate function

remember_sep_tri <- remember_trigrams %>%
  separate(bigram, c("word1", "word2", "word3"), sep = " ")

rem_tri_filtered <- remember_sep_tri %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word)

# Filtered trigram counts:
remember_trigrams_filt <- rem_tri_filtered %>% 
  count(word1, word2, word3, sort = TRUE)

head(remember_trigrams_filt, n = 15)

# Unite the words with the unite() function:

remember_trigrams_counts <- remember_trigrams_filt %>%
  unite(bigram, word1, word2, word3, sep = " ")

remember_trigrams_counts

# ggplot2 Plot (Counts greater than 8)
# Bottom axis removed with element_blank()
# Counts in the bar with geom_text.

remember_trigrams_counts[1:15, ] %>% 
  ggplot(aes(reorder(bigram, n), n)) + 
  geom_col(fill = "blue") +
  coord_flip() +
  labs(x = "Trigram \n", y = "\n Count ", title = "Trigrams In Fort Minor - Remember The Name \n") +
  geom_text(aes(label = n), hjust = 1.2, colour = "white", fontface = "bold") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(face="bold", colour="darkblue", size = 12)) 

### 1d) Sentiment Analysis:

# There are three main lexicons from the tidytext R package.
# These three are bing, AFINN and nrc.
# nrc and bing lexicons used here.

### nrc lexicons:
# get_sentiments("nrc")

remember_words_nrc <- remember_wordcounts %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  filter(sentiment %in% c("positive", "negative"))

head(remember_words_nrc)

# Sentiment Plot with nrc Lexicon

remember_words_nrc %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = sentiment)) + 
  geom_bar(stat = "identity", position = "identity") +
  geom_text(aes(label = n), colour = "black", hjust = 1, fontface = "bold", size = 3.2) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "\n Word \n", y = "\n Word Count ", title = "Negative & Positive Words \n In Fort Minor - Remember The Name \n With The nrc Lexicon \n") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="darkblue", size = 12),
        axis.title.y = element_text(face="bold", colour="darkblue", size = 12)) +
  scale_fill_manual(values=c("#FF0000", "#01DF3A"), guide=FALSE) + 
  coord_flip()

### bing lexicon:
# get_sentiments("bing")

remember_words_bing <- remember_wordcounts %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  ungroup()

head(remember_words_bing)

# Sentiment Plot with bing Lexicon

remember_words_bing %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = sentiment)) + 
  geom_bar(stat = "identity", position = "identity") +
  geom_text(aes(label = n), colour = "black", hjust = 1, fontface = "bold", size = 3.2) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "\n Word \n", y = "\n Word Count ", title = "Negative & Positive Words \n In Fort Minor - Remember The Name \n With The bing Lexicon \n") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="darkblue", size = 12),
        axis.title.y = element_text(face="bold", colour="darkblue", size = 12)) +
  scale_fill_manual(values=c("#FF0000", "#01DF3A"), guide=FALSE) + 
  coord_flip()


# ---------------------------------------------
# 2) Eminem - Lose Yourself:

# Read Eminem - Lose Yourself lyrics into R:

loseyourself_lyrics <- readLines("eminem_loseYourself.txt")

# Preview the lyrics:

loseyourself_lyrics_df <- data_frame(Text = loseyourself_lyrics ) # tibble aka neater data frame

head(loseyourself_lyrics_df, n = 10) 

# Unnest tokens: each word in the lyrics in a row:

loseyourself_words <- loseyourself_lyrics_df %>% 
  unnest_tokens(output = word, input = Text) 

# Preview with head() function:

head(loseyourself_words, n = 10)

#### 2a) Word Counts in Lose Yourself:

# Remove English stop words from Lose Yourself:
# Stop words include the, and, me , you, myself, of, etc.

loseyourself_words <- loseyourself_words %>%
  anti_join(stop_words) 

# Word Counts:

loseyourself_wordcounts <- loseyourself_words %>% count(word, sort = TRUE)

head(loseyourself_wordcounts, n = 15)

# Plot of Word Counts (Top 20 Words):

loseyourself_wordcounts[1:20, ] %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) + 
  geom_col(fill = "red") +
  coord_flip() +
  labs(x = "Word \n", y = "\n Count ", title = "Word Counts In \n Eminem - Lose Yourself \n") +
  geom_text(aes(label = n), hjust = 1.2, colour = "white", fontface = "bold") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(face="bold", colour="darkblue", size = 12)) 

### 2b) Bigrams (Two-Word Phrases) in Remember The Name:

loseyourself_bigrams <- loseyourself_lyrics_df %>% 
  unnest_tokens(bigram, input = Text, token = "ngrams", n = 2)

# Look at the bigrams:

loseyourself_bigrams

# Remove stop words from bigrams with tidyr's separate function
# along with the filter() function

bigrams_loseyourself_sep <- loseyourself_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_loseyourself_filt <- bigrams_loseyourself_sep %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# Filtered bigram counts:
bigrams_loseyourself_counts <- bigrams_loseyourself_filt %>% 
  count(word1, word2, sort = TRUE)


head(bigrams_loseyourself_counts, n = 15)

# Unite the words with the unite() function:

loseyourself_bigrams_counts <- bigrams_loseyourself_counts %>%
  unite(bigram, word1, word2, sep = " ")

loseyourself_bigrams_counts

# We can now make a plot of the word counts.


# ggplot2 Plot (Counts greater than 8)
# Bottom axis removed with element_blank()
# Counts in the bar with geom_text.

loseyourself_bigrams_counts[1:20, ] %>% 
  ggplot(aes(reorder(bigram, n), n)) + 
  geom_col(fill = "red") +
  coord_flip() +
  labs(x = "Bigram \n", y = "\n Count ", title = "Bigrams In Eminem - Lose Yourself \n") +
  geom_text(aes(label = n), hjust = 1.2, colour = "white", fontface = "bold") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(face="bold", colour="darkblue", size = 12)) 


### 2c) Sentiment Analysis:

# nrc and bing lexicons used here.


### nrc lexicons:
# get_sentiments("nrc")

loseyourself_words_nrc <- loseyourself_wordcounts %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  filter(sentiment %in% c("positive", "negative"))

head(loseyourself_words_nrc)

# Sentiment Plot with nrc Lexicon

loseyourself_words_nrc %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = sentiment)) + 
  geom_bar(stat = "identity", position = "identity") +
  geom_text(aes(label = n), colour = "black", hjust = 1, fontface = "bold", size = 3.2) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "\n Word \n", y = "\n Word Count ", title = "Negative & Positive Words \n In Eminem - Lose Yourself \n With The nrc Lexicon \n") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="darkblue", size = 12),
        axis.title.y = element_text(face="bold", colour="darkblue", size = 12)) +
  scale_fill_manual(values=c("#FF0000", "#01DF3A"), guide=FALSE) + 
  coord_flip()

### bing lexicon:
# get_sentiments("bing")

loseyourself_words_bing <- loseyourself_wordcounts %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  ungroup()

head(loseyourself_words_bing)

# Sentiment Plot with bing Lexicon

loseyourself_words_bing %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = sentiment)) + 
  geom_bar(stat = "identity", position = "identity") +
  geom_text(aes(label = n), colour = "black", hjust = 1, fontface = "bold", size = 3.2) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "\n Word \n", y = "\n Word Count ", title = "Negative & Positive Words \n In Eminem - Lose Yourself \n With The bing Lexicon \n") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="darkblue", size = 12),
        axis.title.y = element_text(face="bold", colour="darkblue", size = 12)) +
  scale_fill_manual(values=c("#FF0000", "#01DF3A"), guide=FALSE) + 
  coord_flip()

# Change labels 
# (Source: https://stackoverflow.com/questions/3472980/ggplot-how-to-change-facet-labels)

word_labels <- c(
  `FALSE` = "Negative Words",
  `TRUE` = "Positive Words"
)

meteora_words_AFINN %>%
  filter(n >= 3) %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = is_positive)) + 
  geom_bar(stat = "identity", position = "identity") +
  geom_text(aes(label = n), colour = "black", hjust = 1, fontface = "bold", size = 3.2) +
  facet_wrap(~is_positive, scales = "free_y", labeller = as_labeller(word_labels)) +
  labs(x = "\n Word \n", y = "\n Word Count ", title = "Negative & Positive Words \n With The AFINN Lexicon \n",
       fill = c("Negative", "Positive")) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="darkblue", size = 12),
        axis.title.y = element_text(face="bold", colour="darkblue", size = 12)) +
  scale_fill_manual(values=c("#FF0000", "#01DF3A"), guide=FALSE) + 
  coord_flip()



