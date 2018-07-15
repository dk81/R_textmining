# Text Mining on the Dr. Suess - Green Eggs and Ham With R

# Text Version Of Book Source: 
# https://www.clear.rice.edu/comp200/resources/texts/Green%20Eggs%20and%20Ham.txt


# 1) Word Counts & Wordclouds The Tidy Text Way
# No bigrams



#----------------------------------

# Load libraries into R:
# Install packages with install.packages("pkg_name")

library(dplyr) # Data Manipulation
library(tidyr) # Data Wrangling
library(ggplot2) # Data Visualization
library(tidytext) # For text mining and analysis
library(wordcloud) # Wordcloud capabilities
library(gridExtra) # Multiple plots in one


#-------------------------------------------
# 1) Wordcounts in Green Eggs And Ham


greenEggs_book <- readLines("https://www.clear.rice.edu/comp200/resources/texts/Green%20Eggs%20and%20Ham.txt")

# Preview the start of the book:

greenEggs_book_df <- data_frame(Text = greenEggs_book) # tibble aka neater data frame

head(greenEggs_book_df, n = 15) 


# Unnest tokens: Have each word in a row:

greenEggs_words <- greenEggs_book_df %>% 
  unnest_tokens(output = word, input = Text) 

# Preview with head() function:

head(greenEggs_words, n = 10)

# Remove English stop words from Fox In Socks:
# Stop words include me, you, for, myself, he, she

greenEggs_words_filt <- greenEggs_words %>%
  anti_join(stop_words) 

# Word Counts in Fox In Socks (No stopwords)

greenEggs_wordcounts <- greenEggs_words %>% count(word, sort = TRUE)

# Word Counts in Fox In Socks (Stopwords removed)

greenEggs_wordcounts_filt <- greenEggs_words_filt %>% count(word, sort = TRUE)

# Print top 15 words

head(greenEggs_wordcounts, n = 15)

head(greenEggs_wordcounts_filt, n = 15)


## a) Plot & Wordcloud With StopWords

# Bar Graph (Top 15 Words):

green_wordcounts_plot <- greenEggs_wordcounts[1:15, ] %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) + 
  geom_col(fill = "#807af5") +
  coord_flip() +
  labs(x = "Word \n", y = "\n Count ", title = "The 15 Most Common Words In \n Green Eggs And Ham \n") +
  geom_text(aes(label = n), hjust = 1, colour = "white", fontface = "bold", size = 3.5) +
  theme(plot.title = element_text(hjust = 0.5), axis.ticks.x = element_blank(),
        axis.title.x = element_text(face="bold", colour="darkblue", size = 12),
        axis.title.y = element_text(face="bold", colour="darkblue", size = 12)) 

# Print plot:

green_wordcounts_plot 


# Wordcounts Wordcloud:

greenEggs_wordcounts %>%
  with(wordcloud(words = word, freq = n, min.freq = 2, max.words = 100, random.order=FALSE, rot.per=0.35, 
                 colors = rainbow(30)))

## b) Plot & Wordcloud With No StopWords

# Bar Graph (Top 15 Words):

green_wordcounts_plot_filt <- greenEggs_wordcounts_filt[1:15, ] %>% 
  mutate(word = reorder(word, n)) %>% 
  ggplot(aes(word, n)) + 
  geom_col(fill = "#d9232f") +
  coord_flip() +
  labs(x = "Word \n", y = "\n Count ", title = "The 15 Most Common Words In \n Green Eggs And Ham \n (No Stopwords) \n") +
  geom_text(aes(label = n), hjust = 1, colour = "white", fontface = "bold", size = 3.5) +
  theme(plot.title = element_text(hjust = 0.5), 
        axis.ticks.x = element_blank(),
        axis.title.x = element_text(face="bold", colour="darkblue", size = 12),
        axis.title.y = element_text(face="bold", colour="darkblue", size = 12)) 

# Print plot:

green_wordcounts_plot_filt

# Wordcounts Wordcloud:

greenEggs_wordcounts_filt %>%
  with(wordcloud(words = word, freq = n, min.freq = 2, max.words = 100, random.order=FALSE, rot.per=0.35, 
                 colors = rainbow(30)))


## Bar graphs together

grid.arrange(green_wordcounts_plot, green_wordcounts_plot_filt, ncol = 2)






