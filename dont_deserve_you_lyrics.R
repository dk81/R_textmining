# Text Mining On Paul Van Dyk feat. Plumb - I Don't Deserve You

# Lyrics From https://www.azlyrics.com/lyrics/paulvandyk/idontdeserveyou.html
# Lyrics are copied and pasted into a .txt file offline.

# Load libraries into R:

library(dplyr)
library(ggplot2)
library(tidytext)
library(tidyr)

# Read the lyrics .txt file into R (Your file name may vary.)

dont_deserve_lyrics <- readLines("paulvanDyk_dont_deserveYou_lyrics.txt")

# Preview the lyrics:

dont_deserve_lyrics_df <- data_frame(Text = dont_deserve_lyrics) # tibble aka neater data frame

head(dont_deserve_lyrics_df, n = 10) 

# Unnest tokens: each word in the lyrics in a row:

dont_deserve_words <- dont_deserve_lyrics_df %>% 
  unnest_tokens(output = word, input = Text) 

# Preview with head() function:

head(dont_deserve_words, n = 10)


##### 1) Word Counts 

# Remove English stop words such as the, and, me , you, myself, of, etc.

dont_deserve_words <- dont_deserve_words %>%
  anti_join(stop_words) 

# Word Counts:

dont_deserve_wordcounts <- dont_deserve_words %>% count(word, sort = TRUE)

head(dont_deserve_wordcounts, n = 15)

# Plot of Word Counts (Top 20 Words):

dont_deserve_wordcounts[1:20, ] %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) + 
  geom_col(fill = "darkgreen") +
  coord_flip() +
  labs(x = "Word \n", y = "\n Count ", title = "Word Counts In Paul Van Dyk feat. Plumb \n I Don't Deserve You \n") +
  geom_text(aes(label = n), hjust = 1.2, colour = "white", fontface = "bold") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(face="bold", colour="darkblue", size = 12)) 


##### 2) Sentiment Analysis:

# There are three main lexicons from the tidytext R package.
# These three are bing, AFINN and nrc.

### nrc lexicons:
# get_sentiments("nrc")

dont_deserve_words_nrc <- dont_deserve_wordcounts %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  filter(sentiment %in% c("positive", "negative"))

head(dont_deserve_words_nrc)

# Sentiment Plot with nrc Lexicon

dont_deserve_words_nrc %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = sentiment)) + 
  geom_bar(stat = "identity", position = "identity") +
  geom_text(aes(label = n), colour = "black", hjust = 1, fontface = "bold", size = 3.2) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "\n Word \n", y = "\n Word Count ", title = "Negative & Positive Words In Paul Van Dyk feat. Plumb \n I Don't Deserve You \n With The nrc Lexicon \n") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="darkblue", size = 12),
        axis.title.y = element_text(face="bold", colour="darkblue", size = 12)) +
  scale_fill_manual(values=c("#FF0000", "#01DF3A"), guide=FALSE) + 
  coord_flip()

### bing lexicon:
# get_sentiments("bing")

dont_deserve_words_bing <- dont_deserve_wordcounts %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  ungroup()

head(dont_deserve_words_bing)

# Sentiment Plot with bing Lexicon

dont_deserve_words_bing %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = sentiment)) + 
  geom_bar(stat = "identity", position = "identity") +
  geom_text(aes(label = n), colour = "black", hjust = 1, fontface = "bold", size = 3.2) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(x = "\n Word \n", y = "\n Word Count ", title = "Negative & Positive Words In Paul Van Dyk feat. Plumb \n I Don't Deserve You \n With The bing Lexicon \n") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="darkblue", size = 12),
        axis.title.y = element_text(face="bold", colour="darkblue", size = 12)) +
  scale_fill_manual(values=c("#FF0000", "#01DF3A"), guide=FALSE) + 
  coord_flip()

### AFINN lexicon:
# get_sentiments("afinn")

dont_deserve_words_afinn <- dont_deserve_wordcounts %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  mutate(is_positive = score > 0)

head(dont_deserve_words_afinn)


# Change labels 
# (Source: https://stackoverflow.com/questions/3472980/ggplot-how-to-change-facet-labels)

labels <- c(
  `FALSE` = "Negative Words",
  `TRUE` = "Positive Words"
)

dont_deserve_words_afinn %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = is_positive)) + 
  geom_bar(stat = "identity", position = "identity") +
  geom_text(aes(label = n), colour = "black", hjust = 1, fontface = "bold", size = 3.2) +
  facet_wrap(~is_positive, scales = "free_y", labeller = as_labeller(labels)) +
  labs(x = "\n Word \n", y = "\n Word Count ", title = "Negative & Positive Words \n In Fort Minor - Remember The Name \n With The AFINN Lexicon \n",
       fill = c("Negative", "Positive")) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="darkblue", size = 12),
        axis.title.y = element_text(face="bold", colour="darkblue", size = 12)) +
  scale_fill_manual(values=c("#FF0000", "#01DF3A"), guide=FALSE) + 
  coord_flip()

##### 3) Bigrams Word Count 
# ---

# No filtering of stop words.

dont_deserve_bigrams <- dont_deserve_lyrics_df %>% 
  unnest_tokens(bigram, input = Text, token = "ngrams", n = 2)

# Look at the bigrams:

dont_deserve_bigrams 

# Remove stop words from bigrams with tidyr's separate function
# along with the filter() function

bigrams_dont_deserve_sep <- dont_deserve_bigrams  %>%
  separate(bigram, c("word1", "word2"), sep = " ")

# Bigram counts:
bigrams_dont_deserve_counts <- bigrams_dont_deserve_sep %>% 
  count(word1, word2, sort = TRUE)


head(bigrams_dont_deserve_counts, n = 15)

# Unite the words with the unite() function:

dont_deserve_bigrams_counts <- bigrams_dont_deserve_counts %>%
  unite(bigram, word1, word2, sep = " ")

dont_deserve_bigrams_counts
  
# Plot of Bigrams Counts (Top 15 Words):

dont_deserve_bigrams_counts[1:15, ] %>% 
  ggplot(aes(x = reorder(bigram, n), n)) + 
  geom_col(fill = "orange") +
  coord_flip() +
  labs(x = "Word \n", y = "\n Count ", title = "Bigrams Counts In Paul Van Dyk feat. Plumb \n I Don't Deserve You \n") +
  geom_text(aes(label = n), hjust = 1.2, colour = "black", fontface = "bold") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_blank(),
        axis.ticks.x = element_blank(),
        axis.text.x = element_blank(),
        axis.title.y = element_text(face="bold", colour="darkblue", size = 12)) 

