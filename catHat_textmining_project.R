# Text Mining on the Dr. Seuss - The Cat In The Hat Kids Book In R

# Text Version Of Book Source: 
# https://github.com/robertsdionne/rwet/blob/master/hw2/drseuss.txt



# 1) Wordclouds 
# 2) Word Counts 
# 3) Sentiment Analysis - nrc, bing and AFINN Lexicons



#----------------------------------

# Load libraries into R:
# Install packages with install.packages("pkg_name")

library(dplyr)
library(tidyr)
library(ggplot2)
library(tidytext)
library(wordcloud)
library(tm)


#-------------------------------------------

# 1) Wordclouds
# Reference: http://www.sthda.com/english/wiki/text-mining-and-word-cloud-fundamentals-in-r-5-simple-steps-you-should-know
# Ref 2: https://www.youtube.com/watch?v=JoArGkOpeU0

# .txt File was saved on to desktop
# Set the working directory and load the book into R offline:

catHat_book <- readLines("cat_in_the_hat_textbook.txt")

catHat_text <- Corpus(VectorSource(catHat_book))

# Clean the text up:

catHat_clean <- tm_map(catHat_text, removePunctuation)
catHat_clean <- tm_map(catHat_clean, content_transformer(tolower))
catHat_clean <- tm_map(catHat_clean, removeNumbers)
catHat_clean <- tm_map(catHat_clean, stripWhitespace)

# Remove English stopwords such as: the, and or, over, under, and so on:

#catHat_clean  <- tm_map(catHat_clean, removeWords, stopwords('english'))

# Convert to Term Document Matrix:

td_mat<- TermDocumentMatrix(catHat_clean)
matrix <- as.matrix(td_mat)
sorted <- sort(rowSums(matrix),decreasing=TRUE)
data_text <- data.frame(word = names(sorted ), freq = sorted)

#Preview data:
head(data_text, 30)


# Wordcloud with colours:

set.seed(1234)
wordcloud(words = data_text$word, freq = data_text$freq, min.freq = 5,
          max.words = 100, random.order=FALSE, rot.per=0.35, 
          colors = rainbow(30))

# Wordcloud with colours with lower max words and raise minimum frequncy:

wordcloud(words = data_text$word, freq = data_text$freq, min.freq = 15,
          max.words = 80, random.order=FALSE, rot.per=0.35, 
          colors = rainbow(30))

# Wordcounts Plot:

# ggplot2 bar plot (Top 25 Words)

data_text[1:25, ] %>% 
  mutate(word = reorder(word, freq)) %>% 
  ggplot(aes(word, freq)) + 
  geom_col(fill = "lightblue") +
  coord_flip() +
  labs(x = "Word \n", y = "\n Count ", title = "Word Counts In \n The Cat In The Hat Book \n (Top 25) \n") +
  geom_text(aes(label = freq), hjust = 1.2, colour = "black", fontface = "bold", size = 3.7) +
  theme(plot.title = element_text(hjust = 0.5, colour = "darkgreen", size = 15), 
        axis.title.x = element_text(face="bold", colour="darkblue", size = 12),
        axis.title.y = element_text(face="bold", colour="darkblue", size = 12), 
        panel.grid.major = element_blank(),
        panel.grid.minor= element_blank()) 

#-------------------------------------------

# 3) Sentiment Analysis 
# Are the stories positive, negative, neutral?

catHat_book <- readLines("cat_in_the_hat_textbook.txt")

# Preview the start of the book:

catHat_book_df <- data_frame(Text = catHat_book) # tibble aka neater data frame

head(catHat_book_df, n = 15) 

catHat_book_words <- catHat_book_df %>% 
  unnest_tokens(output = word, input = Text) 

# Retrieve word counts as set up for sentiment lexicons:

catHat_book_wordcounts <- catHat_book_words %>%
                      anti_join(stop_words)  %>% 
                      count(word, sort = TRUE)

#### Using nrc, bing and AFINN lexicons

word_labels_nrc <- c(
  `negative` = "Negative Words",
  `positive` = "Positive Words"
)

### nrc lexicons:
# get_sentiments("nrc")

catHat_book_words_nrc <- catHat_book_wordcounts %>%
  inner_join(get_sentiments("nrc"), by = "word") %>%
  filter(sentiment %in% c("positive", "negative"))

# Preview common words with sentiment label:

head(catHat_book_words_nrc, n = 12)

# Sentiment Plot with nrc Lexicon (Word Count over 5)

catHat_book_words_nrc %>%
  filter(n > 5) %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = sentiment)) + 
  geom_bar(stat = "identity", position = "identity") +
  geom_text(aes(label = n), colour = "black", hjust = 1, fontface = "bold", size = 3) +
  facet_wrap(~sentiment, nrow = 2, scales = "free_y", labeller = as_labeller(word_labels_nrc)) +
  labs(x = "\n Word \n", y = "\n Word Count ", title = "Negative & Positive Words In \n The Cat In The Hat Kids' Book \n With The nrc Lexicon \n") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="darkblue", size = 12),
        axis.title.y = element_text(face="bold", colour="darkblue", size = 12),
        strip.background = element_rect(fill = "lightblue"),
        strip.text.x = element_text(size = 10, face = "bold")) +
  scale_fill_manual(values=c("#FF0000", "#01DF3A"), guide=FALSE) + 
  coord_flip()

### bing lexicon:
# get_sentiments("bing")

word_labels_bing <- c(
  `negative` = "Negative Words",
  `positive` = "Positive Words"
)

catHat_book_words_bing <- catHat_book_wordcounts %>%
  inner_join(get_sentiments("bing"), by = "word") %>%
  ungroup()

# Preview the words and counts:

head(catHat_book_words_bing, n = 15)

# Sentiment Plot with bing Lexicon (Counts over 3):

catHat_book_words_bing %>%
  filter(n > 3) %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = sentiment)) + 
  geom_bar(stat = "identity", position = "identity") +
  geom_text(aes(label = n), colour = "black", hjust = 1, fontface = "bold", size = 3) +
  facet_wrap(~sentiment, nrow = 2, scales = "free_y", labeller = as_labeller(word_labels_bing)) +
  labs(x = "\n Word \n", y = "\n Word Count ", title = "Negative & Positive Words In \n The Cat In The Hat Kids' Book \n With The bing Lexicon  \n") +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="darkblue", size = 12),
        axis.title.y = element_text(face="bold", colour="darkblue", size = 12),
        strip.background = element_rect(fill = "#BEE1D3"),
        strip.text.x = element_text(size = 10, face = "bold", colour = "black")) +
  scale_fill_manual(values=c("#FF0000", "#01DF3A"), guide=FALSE) + 
  coord_flip()


### AFINN lexicon:

# Change labels 
# (Source: https://stackoverflow.com/questions/3472980/ggplot-how-to-change-facet-labels)

catHat_book_words_AFINN <- catHat_book_wordcounts %>%
  inner_join(get_sentiments("afinn"), by = "word") %>%
  mutate(is_positive = score > 0) 

head(catHat_book_words_AFINN, n = 15)



word_labels_AFINN <- c(
  `FALSE` = "Negative Words",
  `TRUE` = "Positive Words"
)

catHat_book_words_AFINN %>%
  filter(n > 3) %>%
  ggplot(aes(x = reorder(word, n), y = n, fill = is_positive)) + 
  geom_bar(stat = "identity", position = "identity") +
  geom_text(aes(label = n), colour = "black", hjust = 1, fontface = "bold", size = 3.2) +
  facet_wrap(~is_positive, scales = "free_y", nrow = 2, labeller = as_labeller(word_labels_AFINN)) +
  labs(x = "\n Word \n", y = "\n Word Count ", title = "Negative & Positive Words In \n The Cat In The Hat Kids' Book \n With The AFINN Lexicon  \n",
       fill = c("Negative", "Positive")) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.title.x = element_text(face="bold", colour="darkblue", size = 12),
        axis.title.y = element_text(face="bold", colour="darkblue", size = 12),
        strip.background = element_rect(fill = "#D5ADA4"),
        strip.text.x = element_text(size = 10, face = "bold", colour = "black")) +
  scale_fill_manual(values=c("#FF0000", "#01DF3A"), guide=FALSE) + 
  coord_flip()
