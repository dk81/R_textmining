# Twitter Mining & Wordcloud Practice Thing:

# Reference: https://www.youtube.com/watch?v=lT4Kosc_ers 

# Go to https://apps.twitter.com

# Login with a Twitter account. (Create a new Twitter if needed)

# Create new app if you do not have one.

# Create a new name, description

# Use http://test.de/ for website.

# Go to Keys and Access Tokens tab (3rd from left):

# Create access token:

# You will need your:

# 1) Consumer Key (API Key)
# 2) Consumer Secret (Consumer Secret)
# 3) Access Token
# 4) Access Token Secret

# Load libraries, Rcurl, twitteR and WordCloud

library(RCurl)
library(twitteR)
library(wordcloud)
library(tm)

consumer_key <- #your_key
consumer_secret <- #your_secret
  
access_token <- #your_access_token
access_token_secret <- #your_access_secret

# The setup_twitter_oauth() function will be used.
  
setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_token_secret)

###---------------------------
###---------------------------
#My stuff:
  
consumer_key <- "key"
consumer_secret <- "secret"
  
access_token <- "token"
access_token_secret <- "token_secret"
  
# The setup_twitter_oauth() function will be used.
  
setup_twitter_oauth(consumer_key, consumer_secret, 
                    access_token, access_token_secret)

##-----------------------------------
###---------------------------

# searchTwitter() function; type ?searchTwitter to get documentation.

math_tweets <- searchTwitter(searchString = "math", n = 500, lang = "en", resultType = "recent")

# resultType = recent returns recent results, popular returns popular results

class(math_tweets)

# Take a random sample of 10 from the 500 tweets to view.
# Results will vary unless there is a random seed placed.

sample_tweets <- sample(math_tweets, size = 10)

sample_tweets

#--------

# Convert tweets to characters:
# sapply works on lists or vectors and reutrns a vector/matrix or an array (if specified):

math_texts <- sapply(math_tweets, function(x) x$getText())

# Create corpus from vector of tweets:
# Corpus is a collection of text documents. In this case: tweets.

math_corpus <- Corpus(VectorSource(math_texts))

math_corpus 

#--------

# use tm_map() function from tm package to clean tweets:

# Can use ?tm_map for more info

math_clean <- tm_map(math_corpus, removePunctuation)

# Lower cases:

math_clean <- tm_map(math_clean, content_transformer(tolower))

# Remove english filler words such as and, the, okay using stopwords()
math_clean <- tm_map(math_clean, removeWords, stopwords("english"))

# Remove more filler, some slangs words:

math_clean <- tm_map(math_clean, removeWords, c("lol", "lmao", "rofl", "wtf", "huh"))

# Remove numbers:
math_clean <- tm_map(math_clean, removeNumbers)

# Remove any white spaces:

math_clean <- tm_map(math_clean, stripWhitespace)


## Wordcloud time: Use ?wordcloud for more info.

# Colourful wordcloud:
wordcloud(math_clean, scale = c(2, 1), min.freq = 20, colors = rainbow(50), random.color = TRUE)



