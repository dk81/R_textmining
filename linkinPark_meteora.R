# Linkin Park - Meteora Text Analysis And Text Mining In R

# Lyrics from Meteora Album
# R Reference Book: Text Mining With R - A Tidy Approach By Julia Silge and David Robinson
# Sources:
# https://stackoverflow.com/questions/3744178/ggplot2-sorting-a-plot

# Song List In Meteora

# 1) Foreword
# 2) Don't Stay
# 3) Somewhere I Belong
# 4) Lying From You
# 5) Hit The Floor
# 6) Easier To Run
# 7) Faint
# 8) Figure.09
# 9) Breaking The Habit
# 10) From The Inside
# 11) Nobody's Listening
# 12) Session [Instrumental]
# 13) Numb

# Lyrics copied from https://www.azlyrics.com/l/linkinpark.html 
# and pasted into a .txt file

# Load libraries into R:

library(dplyr)
library(ggplot2)
library(tidytext)

# Read lyrics into R (Lyrics were copied and pasted to an offline .txt file.)

meteora_lyrics <- readLines("meteoraLyrics.txt")

# Preview all the lyrics:

meteora_lyrics_df <- data_frame(Text = meteora_lyrics) # tibble aka neater data frame

head(meteora_lyrics_df, n = 15) 

meteora_words <- meteora_lyrics_df %>% 
  unnest_tokens(output = word, input = Text) 

# ---------------------------------------------------------
# Word Counts In Meteora:

# Remove English stop words from Meteora:
# Stop words include the, and, me , myself, of and so on.

meteora_words <- meteora_words %>%
  anti_join(stop_words) 

# Word Counts:

meteora_wordcounts <- meteora_words %>% count(word, sort = TRUE)

head(meteora_wordcounts, n = 15)


# We can now make a plot of the word counts.


# ggplot2 Plot (Counts greater than 8)
# Bottom axis removed with element_blank()
# Counts in the bar with geom_text.

meteora_wordcounts %>% 
  mutate(word = reorder(word, n)) %>% 
  filter(n > 8) %>%
  ggplot(aes(word, n)) + 
  geom_col(fill = "blue") +
  coord_flip() +
  labs(x = "Word \n", y = "\n Count ", title = "Word Counts In Linkin Park - Meteora \n") +
  geom_text(aes(label = n), hjust = 1.2, colour = "white", fontface = "bold") +
  theme(plot.title = element_text(hjust = 0.5), 
  axis.title.x = element_blank(),
  axis.ticks.x = element_blank(),
  axis.text.x = element_blank(),
  axis.title.y = element_text(face="bold", colour="darkblue", size = 12)) 

# ---------------------------------------------------------
# Sentiment Analysis
# Which words are positive and negative along with word counts.

### bing sentiment
## get_sentiments("bing")

meteora_words_bing <- meteora_wordcounts %>%
                      inner_join(get_sentiments("bing"), by = "word") %>%
                      ungroup()

head(meteora_words_bing)

meteora_words_bing %>%
  filter(n > 3) %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = sentiment)) + 
  geom_bar(stat = "identity", position = "identity") +
  geom_text(aes(label = n), colour = "black", hjust = 1, fontface = "bold") +
  coord_flip() +
  labs(x = "\n Word \n", y = "Word Count \n", title = "Sentiment Scores Of Words \n Under bing Lexicon") +
  theme(plot.title = element_text(hjust = 0.5), 
  axis.title.x = element_text(face="bold", colour="darkblue", size = 12),
  axis.title.y = element_text(face="bold", colour="darkblue", size = 12)) +
  scale_fill_manual(values=c("#FF0000", "#01DF3A"), guide=FALSE)

# Separate Sentiment plots for positive and negative words (word counts > 2):

meteora_words_bing %>%
  filter(n > 2) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  geom_text(aes(label = n), colour = "black", hjust = 1, fontface = "bold") +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "\n Word \n", y = "\n Word Count ", title = "Sentiment Scores Of Words \n Under bing Lexicon") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="darkblue", size = 12),
        axis.title.y = element_text(face="bold", colour="darkblue", size = 12)) +
  scale_fill_manual(values=c("#FF0000", "#01DF3A"), guide=FALSE) +
  coord_flip()


### AFINN sentiment

meteora_words_AFINN <- meteora_wordcounts %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  mutate(is_positive = score > 0) 

head(meteora_words_AFINN)

meteora_words_AFINN %>%
  filter(n > 2) %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = is_positive)) + 
  geom_bar(stat = "identity", position = "identity") +
  geom_text(aes(label = n), colour = "black", hjust = 1, fontface = "bold", size = 3.1) +
  labs(x = "\n Word \n", y = "\n Word Count ", title = "Sentiment Scores Of Words \n With AFINN Lexicon") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="darkblue", size = 12),
        axis.title.y = element_text(face="bold", colour="darkblue", size = 12)) +
  scale_fill_manual(values=c("#FF0000", "#01DF3A"), guide=FALSE) +
  coord_flip()

# Positive and negative plots separated with facet_wrap():


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

### nrc sentiment:
# get_sentiments("nrc")

meteora_words_nrc <- meteora_wordcounts %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  filter(sentiment %in% c("positive", "negative"))

head(meteora_words_nrc)

meteora_words_nrc %>%
  filter(n >= 3) %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = sentiment)) + 
  geom_bar(stat = "identity", position = "identity") +
  geom_text(aes(label = n), colour = "black", hjust = 1, fontface = "bold", size = 3.2) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "\n Word \n", y = "\n Word Count ", title = "Negative & Positive Words \n With The nrc Lexicon \n") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="darkblue", size = 12),
        axis.title.y = element_text(face="bold", colour="darkblue", size = 12)) +
  scale_fill_manual(values=c("#FF0000", "#01DF3A"), guide=FALSE) + 
  coord_flip()

# ---------------------------------------------------------
# Bigrams - Looking at phrases with two words 
# (Trigrams looks at three word phrases.)
# Sometimes individual words are not enough, some words are linked with other words.

meteora_lyrics <- readLines("meteoraLyrics.txt")

# Preview all the lyrics:

meteora_lyrics_df <- data_frame(Text = meteora_lyrics) # tibble aka neater data frame

head(meteora_lyrics_df, n = 15) 

meteora_bigrams <- meteora_lyrics_df %>% 
  unnest_tokens(bigram, input = Text, token = "ngrams", n = 2)

# Look at the bigrams:

meteora_bigrams

# Remove stop words from bigrams with tidyr's separate function
# along with the filter() function

library(tidyr)

bigrams_separated <- meteora_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# Filtered bigram counts:
meteora_bigrams_wordcounts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)


head(meteora_bigrams_wordcounts, n = 15)

# Unite the words with the unite() function:

meteora_bigrams_wordcounts <- meteora_bigrams_wordcounts %>%
  unite(bigram, word1, word2, sep = " ")

meteora_bigrams_wordcounts



# We can now make a plot of the word counts.


# ggplot2 Plot (Counts greater than 8)
# Bottom axis removed with element_blank()
# Counts in the bar with geom_text.

meteora_bigrams_wordcounts %>% 
  filter(n > 3) %>%
  ggplot(aes(reorder(bigram, n), n)) + 
  geom_col(fill = "red") +
  coord_flip() +
  labs(x = "Bigram \n", y = "\n Count ", title = "Bigrams In Linkin Park - Meteora \n") +
  geom_text(aes(label = n), hjust = 1.2, colour = "white", fontface = "bold") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(face="bold", colour="darkblue", size = 12)) 

#---------------------
### Trigrams (results not great):

meteora_trigrams <- meteora_lyrics_df %>% 
  unnest_tokens(bigram, input = Text, token = "ngrams", n = 3)

# Look at the trigrams:

meteora_trigrams

# Remove stop words from bigrams with tidyr's separate function
# along with the filter() function

library(tidyr)

trigrams_separated <- meteora_trigrams %>%
  separate(bigram, c("word1", "word2", "word3"), sep = " ")

trigrams_filtered <- trigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>%
  filter(!word3 %in% stop_words$word)

# Filtered trigram counts:
meteora_trigrams_wordcounts <- trigrams_filtered %>% 
  count(word1, word2, word3, sort = TRUE)


head(meteora_trigrams_wordcounts, n = 15)

