#######################################
### Hult International Business School
### MSBAN - Text Mining with R
### Business inside project- Twitter
### Due date: 5 December
### By: Amber Wang
#######################################

library(ggplot2)
library(dplyr)
library(tidytext)
library(tidyverse)
library(tidytuesdayR)
library(rtweet)
###############################################################################
#Authenticating and connecting Twitter API 

consumer_key <- "gsXILxJjRgYIzArIFwqs175J3"
consumer_secret <- "41qBCayNJk09cH55mcPe6j5fGQrqB0cl6Md23s8fwwfF4u4ofH"
access_token <- "1462537310243733506-SxTIyI2DSyAxfSZLFvOW4ceCWKUI6s"
access_secret <- "NXtgBsH2HmJdG3idpIOKM7wxmREHC3LGJSlWBxuPI7BXa"
name_of_app <- "D-power"
twitter_token <- create_token(
  app = name_of_app,
  consumer_key = consumer_key,
  consumer_secret = consumer_secret,
  access_token = access_token,
  access_secret = access_secret)
get_token()
#posting a tweet from R for fun
post_tweet("Tweeting from #R for fun")
#your tweet has been posted!

###############################################################################
#EXTRACTING TWEETS FOR RUNNING

#search for 500 tweets using the #RiskIQ hashtag
Apple_tweets <- search_tweets(q = "#Apple",
                                n = 500, lang = "en", include_rts = FALSE)
Facebook_tweets <- search_tweets(q = "#Facebook",
                              n = 500, lang = "en", include_rts = FALSE)
Google_tweets <- search_tweets(q = "#Google",
                                 n = 500, lang = "en", include_rts = FALSE)
IBM_tweets <- search_tweets(q = "#IBM",
                                 n = 500, lang = "en", include_rts = FALSE)
Amazon_tweets <- search_tweets(q = "#Amazon",
                                 n = 500, lang = "en", include_rts = FALSE)
Twitter_tweets <- search_tweets(q = "#Twitter",
                                   n = 500, lang = "en", include_rts = FALSE)


#view the first 3 rows of the dataframe
#head(RiskIQ_tweets, n = 3)

#see the length of the tweets in the running_tweets datframe
#n.tweet <- length(running_tweets)

#########################################################################################
#STARTING "SENTIMENT ANALYSIS" and CLEANING THE DATA: PROCESS EACH SET OF TWEETS INTO TIDY TEXT (OR CORPUS OBJECTS)

library(tidyr)
tweets.Apple_tweets = Apple_tweets %>% select(screen_name, text)
tweets.Apple_tweets

tweets.Facebook_tweets = Facebook_tweets %>% select(screen_name, text)
tweets.Facebook_tweets

tweets.Google_tweets = Google_tweets %>% select(screen_name, text)
tweets.Google_tweets

tweets.IBM_tweets = IBM_tweets %>% select(screen_name, text)
tweets.IBM_tweets

tweets.Twitter_tweets = Twitter_tweets %>% select(screen_name, text)
tweets.Twitter_tweets

tweets.Amazon_tweets = Amazon_tweets %>% select(screen_name, text)
tweets.Amazon_tweets

#########################################################################################
#USE PRE-PROCESSING TEXT TRANSFORMATIONS TO CLEAN UP TWEETS (changing the case, removing links, punctuations, stop words, etc)


#STEP 1) PRE-PROCESSING TEXT FOR Apple_tweets
head(tweets.Apple_tweets$text)

#removing http elements (link to web pages) manually
tweets.Apple_tweets$stripped_text1 <- gsub("http\\s+","",tweets.Apple_tweets$text)

#using unnest_tokens to convert to remove punctuation, change to lowercase, and add an id for tweets
tweets.Apple_tweets_stem <- tweets.Apple_tweets %>%
  select(stripped_text1) %>%
  unnest_tokens(word, stripped_text1)

head(tweets.Apple_tweets_stem)

#removing stop words from list of words and naming it with "cleaned_tweets"
cleaned_tweets.Apple_tweets <- tweets.Apple_tweets_stem %>%
  anti_join(stop_words)
head(cleaned_tweets.Apple_tweets)

head(tweets.Apple_tweets$text)

#STEP 2) PRE-PROCESSING TEXT FOR Facebook_tweets
head(tweets.Facebook_tweets$text)

#removing http elements (link to web pages) manually
tweets.Facebook_tweets$stripped_text1 <- gsub("http\\s+","",tweets.Facebook_tweets$text)

#using unnest_tokens to convert to remove punctuation, change to lowercase, and add an id for tweets
tweets.Facebook_tweets_stem <- tweets.Facebook_tweets %>%
  select(stripped_text1) %>%
  unnest_tokens(word, stripped_text1)

head(tweets.Facebook_tweets_stem)

#removing stop words from list of words and naming it with "cleaned_tweets"
cleaned_tweets.Facebook_tweets <- tweets.Facebook_tweets_stem %>%
  anti_join(stop_words)
head(cleaned_tweets.Facebook_tweets)

head(tweets.Facebook_tweets$text)


#STEP 3) PRE-PROCESSING TEXT FOR Google_TWEETS
head(tweets.Google_tweets$text)

#removing http elements (link to web pages) manually
tweets.Google_tweets$stripped_text1 <- gsub("http\\s+","",tweets.Google_tweets$text)

#using unnest_tokens to convert to remove punctuation, change to lowercase, and add an id for tweets
tweets.Google_tweets_stem <- tweets.Google_tweets %>%
  select(stripped_text1) %>%
  unnest_tokens(word, stripped_text1)

head(tweets.Google_tweets_stem)

#removing stop words from list of words and naming it with "cleaned_tweets"
cleaned_tweets.Google_tweets <- tweets.Google_tweets_stem %>%
  anti_join(stop_words)
head(cleaned_tweets.Google_tweets)

head(tweets.Google_tweets$text)

#STEP 4) PRE-PROCESSING TEXT FOR IBM_TWEETS
head(tweets.IBM_tweets$text)

#removing http elements (link to web pages) manually
tweets.IBM_tweets$stripped_text1 <- gsub("http\\s+","",tweets.IBM_tweets$text)

#using unnest_tokens to convert to remove punctuation, change to lowercase, and add an id for tweets
tweets.IBM_tweets_stem <- tweets.IBM_tweets %>%
  select(stripped_text1) %>%
  unnest_tokens(word, stripped_text1)

head(tweets.IBM_tweets_stem)

#removing stop words from list of words and naming it with "cleaned_tweets"
cleaned_tweets.IBM_tweets <- tweets.IBM_tweets_stem %>%
  anti_join(stop_words)
head(cleaned_tweets.IBM_tweets)

head(tweets.IBM_tweets$text)


#STEP 5) PRE-PROCESSING TEXT FOR Twitter_TWEETS
head(tweets.Twitter_tweets$text)

#removing http elements (link to web pages) manually
tweets.Twitter_tweets$stripped_text1 <- gsub("http\\s+","",tweets.Twitter_tweets$text)

#using unnest_tokens to convert to remove punctuation, change to lowercase, and add an id for tweets
tweets.Twitter_tweets_stem <- tweets.Twitter_tweets %>%
  select(stripped_text1) %>%
  unnest_tokens(word, stripped_text1)

head(tweets.Twitter_tweets_stem)

#removing stop words from list of words and naming it with "cleaned_tweets"
cleaned_tweets.Twitter_tweets <- tweets.Twitter_tweets_stem %>%
  anti_join(stop_words)
head(cleaned_tweets.Twitter_tweets)

head(tweets.Twitter_tweets$text)

#STEP 6) PRE-PROCESSING TEXT FOR Amazon_TWEETS
head(tweets.Amazon_tweets$text)

#removing http elements (link to web pages) manually
tweets.Amazon_tweets$stripped_text1 <- gsub("http\\s+","",tweets.Amazon_tweets$text)

#using unnest_tokens to convert to remove punctuation, change to lowercase, and add an id for tweets
tweets.Amazon_tweets_stem <- tweets.Amazon_tweets %>%
  select(stripped_text1) %>%
  unnest_tokens(word, stripped_text1)

head(tweets.Amazon_tweets_stem)

#removing stop words from list of words and naming it with "cleaned_tweets"
cleaned_tweets.Amazon_tweets <- tweets.Amazon_tweets_stem %>%
  anti_join(stop_words)
head(cleaned_tweets.Amazon_tweets)

head(tweets.Amazon_tweets$text)

#########################################################################################
#WORD FREQUENCY: FINDING THE TOP 10 COMMONLY USED WORDS IN EACH SET OF TWEETS


#Top 10 words in #Apple tweets
cleaned_tweets.Apple_tweets %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  theme_classic() +
  labs(x = "Unique words", 
       y = "Count",
       title = "Unique word counts found in #Apple tweets")

#Top 10 words in #Facebook tweets
cleaned_tweets.Facebook_tweets %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  theme_classic() +
  labs(x = "Unique words", 
       y = "Count",
       title = "Unique word counts found in #Facebook tweets")

#Top 10 words in #Google tweets
cleaned_tweets.Google_tweets %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  theme_classic() +
  labs(x = "Unique words", 
       y = "Count",
       title = "Unique word counts found in #Google tweets")


#Top 10 words in #IBM tweets
cleaned_tweets.IBM_tweets %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  theme_classic() +
  labs(x = "Unique words", 
       y = "Count",
       title = "Unique word counts found in #IBM tweets")


#Top 10 words in #Twitter tweets
cleaned_tweets.Twitter_tweets %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  theme_classic() +
  labs(x = "Unique words", 
       y = "Count",
       title = "Unique word counts found in #Twitter tweets")

#Top 10 words in #Google tweets
cleaned_tweets.Amazon_tweets %>%
  count(word, sort = TRUE) %>%
  top_n(10) %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(x = word, y = n)) +
  geom_col() +
  xlab(NULL) +
  coord_flip() +
  theme_classic() +
  labs(x = "Unique words", 
       y = "Count",
       title = "Unique word counts found in #Amazon tweets")

#########################################################################################
#CONTINUING SENTIMENT ANALYSIS TO EVALUATE EMOTION IN TEXT
library(textdata)

#ATTEMPTING "BING SENTIMENT ANALYSIS"
get_sentiments("bing") %>% filter(sentiment=="positive")
get_sentiments("bing") %>% filter(sentiment=="negative")

#ATTEMPTING "AFINN SENTIMENT ANALYSIS"
get_sentiments("afinn") %>% filter(value=="3")
get_sentiments("afinn") %>% filter(value=="-3")

#BING SENTIMENT ANALYSIS CONTINUED: THE RESULT RETURNS A TIBBLE FOR CLEANED RUNNING TWEETS
bing_Apple_tweets = cleaned_tweets.Apple_tweets %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

#BING SENTIMENT ANALYSIS CONTINUED: THE RESULT RETURNS A TIBBLE FOR CLEANED CYCLING TWEETS
bing_Facebook_tweets = cleaned_tweets.Facebook_tweets %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

#BING SENTIMENT ANALYSIS CONTINUED: THE RESULT RETURNS A TIBBLE FOR CLEANED SWIMMING TWEETS
bing_Google_tweets = cleaned_tweets.Google_tweets %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()


#BING SENTIMENT ANALYSIS CONTINUED: THE RESULT RETURNS A TIBBLE FOR CLEANED SWIMMING TWEETS
bing_IBM_tweets = cleaned_tweets.IBM_tweets %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

#BING SENTIMENT ANALYSIS CONTINUED: THE RESULT RETURNS A TIBBLE FOR CLEANED SWIMMING TWEETS
bing_Twitter_tweets = cleaned_tweets.Twitter_tweets %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

#BING SENTIMENT ANALYSIS CONTINUED: THE RESULT RETURNS A TIBBLE FOR CLEANED SWIMMING TWEETS
bing_Amazon_tweets = cleaned_tweets.Amazon_tweets %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort = TRUE) %>%
  ungroup()

#########################################################################################
#CREATING VISUALIZATION FOR A SIDE BY SIDE COMPARISON OF POSITVE AND NEGATIVE EMOTION IN TWEETS FOR SENTIMENT ANALYSIS

#Comparative visualization of emotions using the tweet hashtag #Apple
bing_Apple_tweets %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Tweets containing '#Apple'",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip() + theme_bw()

#Comparative visualization of emotions using the tweet hashtag #Facebook
bing_Facebook_tweets %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Tweets containing '#Facebook'",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip() + theme_bw()

#Comparative visualization of emotions using the tweet hashtag #Google
bing_Google_tweets %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Tweets containing '#Google'",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip() + theme_bw()

#Comparative visualization of emotions using the tweet hashtag #IBM
bing_IBM_tweets %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Tweets containing '#IBM'",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip() + theme_bw()

#Comparative visualization of emotions using the tweet hashtag #Twitter
bing_Twitter_tweets %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Tweets containing '#Twitter'",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip() + theme_bw()

#Comparative visualization of emotions using the tweet hashtag #Amazon
bing_Amazon_tweets %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word = reorder(word, n)) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y") +
  labs(title = "Tweets containing '#Amazon'",
       y = "Contribution to sentiment",
       x = NULL) +
  coord_flip() + theme_bw()

#########################################################################################
#N-GRAMS USING BIGRAM ANALYSIS AND NETWORK DEFINITION

#Creating a bigram for '#Apple' tweets
bi.gram.words.Apple <- tweets.Apple_tweets %>% 
  unnest_tokens(
    input = text, 
    output = bigram, 
    token = 'ngrams', 
    n = 2
  ) %>% 
  filter(! is.na(bigram))

bi.gram.words.Apple %>% 
  select(bigram) %>% 
  head(10)

#Creating a bigram for '#Facebook' tweets
bi.gram.words.Facebook <- tweets.Facebook_tweets %>% 
  unnest_tokens(
    input = text, 
    output = bigram, 
    token = 'ngrams', 
    n = 2
  ) %>% 
  filter(! is.na(bigram))

bi.gram.words.Facebook %>% 
  select(bigram) %>% 
  head(10)

#Creating a bigram for '#Google' tweets
bi.gram.words.Google <- tweets.Google_tweets %>% 
  unnest_tokens(
    input = text, 
    output = bigram, 
    token = 'ngrams', 
    n = 2
  ) %>% 
  filter(! is.na(bigram))

bi.gram.words.Google %>% 
  select(bigram) %>% 
  head(10)

#Creating a bigram for '#IBM' tweets
bi.gram.words.IBM <- tweets.IBM_tweets %>% 
  unnest_tokens(
    input = text, 
    output = bigram, 
    token = 'ngrams', 
    n = 2
  ) %>% 
  filter(! is.na(bigram))

bi.gram.words.IBM %>% 
  select(bigram) %>% 
  head(10)

#Creating a bigram for '#Twitter' tweets
bi.gram.words.Twitter <- tweets.Twitter_tweets %>% 
  unnest_tokens(
    input = text, 
    output = bigram, 
    token = 'ngrams', 
    n = 2
  ) %>% 
  filter(! is.na(bigram))

bi.gram.words.Twitter %>% 
  select(bigram) %>% 
  head(10)

#Creating a bigram for '#Amazon' tweets
bi.gram.words.Amazon <- tweets.Amazon_tweets %>% 
  unnest_tokens(
    input = text, 
    output = bigram, 
    token = 'ngrams', 
    n = 2
  ) %>% 
  filter(! is.na(bigram))

bi.gram.words.Amazon %>% 
  select(bigram) %>% 
  head(10)

#########################################################################################
#SEEING IF OUR BIGRAMS HAVE STOP WORDS SO WE CAN REMOVE THEM
bi.gram.words.Apple %>%
  count(bigram, sort = TRUE) 

bi.gram.words.Facebook %>%
  count(bigram, sort = TRUE) 

bi.gram.words.Google %>%
  count(bigram, sort = TRUE) 

bi.gram.words.IBM %>%
  count(bigram, sort = TRUE) 

bi.gram.words.Twitter %>%
  count(bigram, sort = TRUE) 

bi.gram.words.Amazon %>%
  count(bigram, sort = TRUE) 


#########################################################################################
#REMOVING STOP WORDS FROM OUR '#Apple' BIGRAM 
library(tidyr)
bigrams_separated_Apple <- bi.gram.words.Apple %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered_Apple <- bigrams_separated_Apple%>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#CREATING NEW BIGRAM WITHOUT STOP WORDS
bigram_counts_Apple <- bigrams_filtered_Apple %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
bigram_counts_Apple

#REMOVING STOP WORDS FROM OUR '#Facebook' BIGRAM 
bigrams_separated_Facebook <- bi.gram.words.Facebook %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered_Facebook <- bigrams_separated_Facebook%>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#CREATING NEW BIGRAM WITHOUT STOP WORDS
bigram_counts_Facebook <- bigrams_filtered_Facebook %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
bigram_counts_Facebook

#REMOVING STOP WORDS FROM OUR '#Google' BIGRAM 
bigrams_separated_Google <- bi.gram.words.Google %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered_Google <- bigrams_separated_Google%>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#CREATING NEW BIGRAM WITHOUT STOP WORDS
bigram_counts_Google <- bigrams_filtered_Google %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
bigram_counts_Google

#REMOVING STOP WORDS FROM OUR '#IBM' BIGRAM 
bigrams_separated_IBM <- bi.gram.words.IBM %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered_IBM <- bigrams_separated_IBM%>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#CREATING NEW BIGRAM WITHOUT STOP WORDS
bigram_counts_IBM <- bigrams_filtered_IBM %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
bigram_counts_IBM

#REMOVING STOP WORDS FROM OUR '#Twitter' BIGRAM 
bigrams_separated_Twitter <- bi.gram.words.Twitter %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered_Twitter <- bigrams_separated_Twitter%>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#CREATING NEW BIGRAM WITHOUT STOP WORDS
bigram_counts_Twitter <- bigrams_filtered_Twitter %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
bigram_counts_Twitter

#REMOVING STOP WORDS FROM OUR '#Amazon' BIGRAM 
bigrams_separated_Amazon <- bi.gram.words.Amazon %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered_Amazon <- bigrams_separated_Amazon%>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

#CREATING NEW BIGRAM WITHOUT STOP WORDS
bigram_counts_Amazon <- bigrams_filtered_Amazon %>%
  count(word1, word2, sort = TRUE)
#want to see the new bigrams
bigram_counts_Amazon

#########################################################################################
######################################################
####### VISUALISING OUR BIGRAM NETWORKS #################
######################################################
######VISUALISING OUR BIGRAM NETWORKS-Apple#####
#install.packages("igraph")
library(igraph)
bigram_graph_Apple<- bigram_counts_Apple %>%
  filter(n > 20) %>%
  graph_from_data_frame()
bigram_graph_Apple


#install.packages("ggraph")
library(ggraph)
ggraph(bigram_graph_Apple, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

######VISUALISING OUR BIGRAM NETWORKS-Facebook#####
#install.packages("igraph")
library(igraph)
bigram_graph_Facebook<- bigram_counts_Facebook %>%
  filter(n > 10) %>%
  graph_from_data_frame()
bigram_graph_Facebook


#install.packages("ggraph")
library(ggraph)
ggraph(bigram_graph_Facebook, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

######VISUALISING OUR BIGRAM NETWORKS-Google#####
#install.packages("igraph")
library(igraph)
bigram_graph_Google<- bigram_counts_Google %>%
  filter(n > 5) %>%
  graph_from_data_frame()
bigram_graph_Google


#install.packages("ggraph")
library(ggraph)
ggraph(bigram_graph_Google, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

######VISUALISING OUR BIGRAM NETWORKS-IBM#####
#install.packages("igraph")
library(igraph)
bigram_graph_IBM<- bigram_counts_IBM %>%
  filter(n > 20) %>%
  graph_from_data_frame()
bigram_graph_IBM


#install.packages("ggraph")
library(ggraph)
ggraph(bigram_graph_IBM, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

######VISUALISING OUR BIGRAM NETWORKS-Twitter#####
#install.packages("igraph")
library(igraph)
bigram_graph_Twitter<- bigram_counts_Twitter %>%
  filter(n > 20) %>%
  graph_from_data_frame()
bigram_graph_Twitter


#install.packages("ggraph")
library(ggraph)
ggraph(bigram_graph_Twitter, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

######VISUALISING OUR BIGRAM NETWORKS-Amazon#####
#install.packages("igraph")
library(igraph)
bigram_graph_Amazon<- bigram_counts_Amazon %>%
  filter(n > 20) %>%
  graph_from_data_frame()
bigram_graph_Amazon


#install.packages("ggraph")
library(ggraph)
ggraph(bigram_graph_Amazon, layout = "fr") +
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)
