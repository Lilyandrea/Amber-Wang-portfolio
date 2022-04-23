####################################################################
######################### Created by Team10 ########################
######################### MBAN1 HULT 2021 ##########################
########################## 12/12/2021 ##############################
######################### Version 1.0 ##############################
####################################################################
######################## Calling libraries #########################
library(pdftools)
library(magrittr)
library(stringr)
library(tidytext)
library(tidyverse)
library(dplyr)
library(ggplot2)
library(ggthemes)
library(tm)
library(gutenbergr)
library(tidyr)
library(topicmodels)
library(tidytuesdayR)
library(textreadr)
library(textshape)
library(twitteR)
library(scales)
library(Matrix)
library(textdata)
library(igraph)
library(ggraph)
library(widyr)
library(quanteda)
library(quanteda.textmodels)
library(RColorBrewer)
# installing and loading the mongolite library to download the Airbnb data
# install.packages("mongolite") #need to run this line of code only once and then you can comment out
library(mongolite)
library(readxl)
#installing and loading the pacman-cld2 function to dectect language
#install.packages("pacman")
pacman::p_load("cld2")
# This is the connection_string. You can get the exact url from your MongoDB cluster screen
# replace the <<user>> with your Mongo user name and <<password>> with the mongo password
# lastly, replace the <<server_name>> with your MongoDB server name
connection_string <- 'mongodb+srv://ArjunM:ajmanohar2021@cluster0.6dlb9.mongodb.net/sample_airbnb?retryWrites=true&w=majority'
airbnb_collection <- mongo(collection="listingsAndReviews", db="sample_airbnb", url=connection_string)
# # Here's how you can download all the Airbnb data from Mongo
# ## keep in mind that this is huge and you need a ton of RAM memory
airbnb_all <- airbnb_collection$find()
# #######################################################
# #if you know or want to learn MQL (MongoQueryLanguage), that is a JSON syntax, feel free to use the following:::
# ######################################################
# #1 subsetting your data based on a condition:
# airbnb_data <- airbnb_collection$find('{"bedrooms":2, "price":{"$gt":50}}')
# 
# #2 writing an analytical query on the data::
# airbnb_analytical <- airbnb_collection$aggregate('[{"$group":{"_id":"$room_type", "avg_price": {"$avg":"price"}}}]')
airbnb_data <- as.data.frame(airbnb_all)
write.csv(airbnb_data, "/Users/arjunmanohar/OneDrive - Hult Students/Hult MBAN Programme/Text Analytics/Final Project/airbnb_data.csv")
View(airbnb_data)
str(airbnb_data)
# studying/reading the data
names(airbnb_all)
names(airbnb_all$host$host_location) #location - omit due to format
names(airbnb_all$address) #address 
table(airbnb_all$address$country) # filter by North_America, Asia, Europe, Others
#creating an ID per row
airbnb_all$id_num <- rownames(airbnb_all)
# subsetting the data
airbnb_subset <- airbnb_all %>%  select(id_num, description, address, review_scores, reviews, reviews_per_month, room_type, 
                                        property_type, cancellation_policy, price, security_deposit)
str(airbnb_subset)
# Building the country variable
vector_country <- c()
for (i in 1:nrow(airbnb_subset)) {
  vector_country[i] <- airbnb_subset$address$country[i]
}
unique(df_country)
df_country <- as.data.frame(vector_country)
df_na <- df_country %>%
  filter(vector_country == "United States" | vector_country == "Canada") %>%
  mutate(region = "North America")
df_other <- df_country %>%
  filter(vector_country == "Brazil" | vector_country == "Australia") %>%
  mutate(region = "Others")
df_asia <- df_country %>%
  filter(vector_country == "China" | vector_country == "Hong Kong") %>%
  mutate(region = "Asia")
df_europe <- df_country %>%
  filter(vector_country == "Portugal" | vector_country == "Turkey" | vector_country == "Spain") %>%
  mutate(region = "Europe")
country_split <- bind_rows(df_na, df_asia, df_europe, df_other)
airbnb_subset <- cbind(airbnb_subset, country_split)
airbnb_subset <- airbnb_subset %>%  select(id_num, description, review_scores, reviews, reviews_per_month, room_type, 
                                           property_type, cancellation_policy, price, security_deposit, vector_country, region)
# building the rating variable
airbnb_subset$review_scores$review_scores_rating # location of the variable 
summary(airbnb_subset$review_scores$review_scores_rating) # mean is close to the median
cbind(lapply(lapply(airbnb_subset$review_scores$review_scores_rating, is.na), sum)) # has NAs
vector_rating <- c()
for (i in 1:nrow(airbnb_subset)) {
  vector_rating[i] <- airbnb_subset$review_scores$review_scores_rating[i]
}
df_rating <- as.data.frame(vector_rating)
mean_rating <- mean(df_rating$vector_rating, na.rm = T)
View(df_rating)
df_rating$ratings_binary <-  with(df_rating, ifelse(df_rating$vector_rating < mean_rating, "0", 
                                                    ifelse(df_rating$vector_rating > mean_rating, "1", "NA")))
# adding the rating binary to airbnb subset data
airbnb_subset <- cbind(airbnb_subset, df_rating)
View(airbnb_subset)
airbnb_subset$ratings_binary <- as.numeric(airbnb_subset$ratings_binary)
airbnb_subset <- airbnb_subset %>%  select(id_num, description, reviews, reviews_per_month, room_type, 
                                           property_type, cancellation_policy, price, security_deposit, 
                                           vector_country, region, vector_rating, ratings_binary)
# converting room_type, prop_type & cancellation policy to factors
unique(airbnb_subset$room_type)
unique(airbnb_subset$property_type)
unique(airbnb_subset$cancellation_policy)
airbnb_subset$room_type <- as.factor(airbnb_subset$room_type)
airbnb_subset$cancellation_policy <- as.factor(airbnb_subset$cancellation_policy)
# replacing missing security deposit values with 0 
airbnb_subset$security_deposit[which(is.na(airbnb_subset$security_deposit))] <- 0
which(is.na(airbnb_subset$security_deposit))
# adding sentiment (from english reviews) per property
vector <- c()
a<-1
#vector with indexes that do not have reviews
for (i in 1:nrow(airbnb_subset)){
  if(is_empty(airbnb_subset$reviews[[i]])){
    vector[a] <- i
    a <- a+1
  }
}
# Only reviews in english
vector_id <- c()
list_comments <- list()
`%!in%` <- Negate(`%in%`)
for (i in 1:nrow(airbnb_subset)){
  num <- nrow(airbnb_subset$reviews[[i]])
  m<-1
  if (num>0){
    list_comments[[i]] <- NaN*seq(1:num)
    for (j in 1:num) {
      if ((i %!in% vector)){
        if (!is.na(cld2::detect_language(airbnb_subset$reviews[[i]]$comments[j]))){
          if (cld2::detect_language(airbnb_subset$reviews[[i]]$comments[j]) == "en" ){
            vector_id[i] <- airbnb_subset$id_num[i]
            list_comments[[i]][m] <- airbnb_subset$reviews[[i]]$comments[j]
            m<-m+1
          }
        } 
      }
    }
  }
} 
get_sentiments("afinn")
sentiment_analysis <- function (n){
  df_1 <- as.data.frame(list_comments[[n]])
  colnames(df_1)[1] <- "text"
  df_1 %>%
    unnest_tokens(word, text) %>%
    anti_join(stop_words) %>%
    count(word, sort=TRUE)
  afinn_1 <- df_1 %>%
    unnest_tokens(word, text) %>%
    inner_join(get_sentiments("afinn"))%>%
    summarise(sentiment=sum(value)) %>%
    mutate(method="AFINN")
  return(afinn_1$sentiment)
}
sentiment_analysis(1)
vector_analysis <- c()
#Sentiment Analysis vector 
for(i in 1:length(vector_id)){
  if (!is.na(vector_id[i])){
    vector_analysis[i] <- sentiment_analysis(i)
    print(i)
  }
}
#Row 5555 the score df is empty
vector_analysis[5555] <- NA
# number of reviews in english
reviews_num <- c()
for (i in 1:length(list_comments)){
  num <- length(list_comments[[i]])
  m<-0
  if (num>0){
    for (j in 1:num) {
      if (list_comments[[i]][j]=="NaN"){
        m<-m+1
      }
    } 
  }
  reviews_num[i] <- num-m
}
#Row 5555 the number of reviews 0
reviews_num[5555] <- 0
# adding the vectors of sentiment analysis and number of english reviews
airbnb_subset <- cbind(airbnb_subset, vector_analysis, reviews_num)
View(airbnb_subset)
for (i in 1:nrow(airbnb_subset)){
  if (airbnb_subset$reviews_num[i]>0){
    airbnb_subset$sentiment_score[i] = airbnb_subset$vector_analysis[i]/airbnb_subset$reviews_num[i]
  } else {airbnb_subset$sentiment_score[i]=NA}
}
airbnb_subset <- airbnb_subset %>%  select(id_num, description, sentiment_score, room_type, 
                                           property_type, cancellation_policy, price, security_deposit, 
                                           vector_country, region, vector_rating, ratings_binary)
###Regression model
summary(airbnb_subset$sentiment_score) # min is -16 and max is 62
airbnb_regdata <- airbnb_subset %>%
  filter(!is.na(ratings_binary)) %>%
  filter(!is.na(sentiment_score))
View(airbnb_regdata)
for (i in 1:nrow(airbnb_regdata))
  if (airbnb_regdata$property_type[i] %!in% c("Apartment","Condominium","Guest suite","House",
                                              "Loft","Serviced apartment","Townhouse")){
    airbnb_regdata$property_type[i] = "Others"
  }
airbnb_regdata$property_type <- as.factor(airbnb_regdata$property_type)
# logistic regression
index <- sample(1:nrow(airbnb_regdata), size = 0.8*nrow(airbnb_regdata))
airbnb_train <- airbnb_regdata[index, ] #creating 80% training dataset
airbnb_test <- airbnb_regdata[-index, ] #creating 20% testing dataset
table(airbnb_train$property_type)
airbnb_train$region <- as.factor(airbnb_train$region)
airbnb_logit1 <- glm(ratings_binary~room_type+property_type+cancellation_policy+
                       price+region+security_deposit+sentiment_score, data = airbnb_train, family = "binomial")
summary(airbnb_logit1)

########################################
library(dplyr)
library(tidytext)
library(janeaustenr)

airbnb_bigrams <- airbnb_subset %>%
  unnest_tokens(bigram, description, token = "ngrams", n = 2)

airbnb_bigrams
##From bigram we can see the fantastic duplex,duplex apartment, apartment with,
#with three, three bedrooms

airbnb_bigrams %>%
  count(bigram, sort = TRUE)

library(tidyr)

bigrams_separated <- airbnb_bigrams %>%
  separate(bigram, c("word1", "word2"), sep = " ")

bigrams_filtered <- bigrams_separated %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

# new bigram counts:
bigram_counts <- bigrams_filtered %>% 
  count(word1, word2, sort = TRUE)

bigram_counts
# walking distance,2double  bed, wi fi, minute walk, minutes  walk, size bed, 
#air   conditioning, equipped kitchen, minutes,  metro station

bigrams_united <- bigrams_filtered %>%
  unite(bigram, word1, word2, sep = " ")

bigrams_united
########################
bigram_tf_idf <- bigrams_united %>%
  count(region, bigram) %>%
  bind_tf_idf(bigram, region, n) %>%
  arrange(desc(tf_idf))

bigram_tf_idf
#####
bigrams_separated %>%
  filter(word1 == "not") %>%
  count(word1, word2, sort = TRUE)

AFINN <- get_sentiments("afinn")

AFINN

not_words <- bigrams_separated %>%
  filter(word1 == "not") %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word2, value, sort = TRUE)

library(ggplot2)

not_words %>%
  mutate(contribution = n * value) %>%
  arrange(desc(abs(contribution))) %>%
  head(20) %>%
  mutate(word2 = reorder(word2, contribution)) %>%
  ggplot(aes(n * value, word2, fill = n * value > 0)) +
  geom_col(show.legend = FALSE) +
  labs(x = "Sentiment value * number of occurrences",
       y = "Words preceded by \"not\"")
#we got not shared, not recommend, not big are the to 3; not hesitate, not miss,
#not bother are negative top 3.
negation_words <- c("not", "no", "never", "without")

negated_words <- bigrams_separated %>%
  filter(word1 %in% negation_words) %>%
  inner_join(AFINN, by = c(word2 = "word")) %>%
  count(word1, word2, value, sort = TRUE)

library(igraph)

# original counts
bigram_counts

bigram_graph <- bigram_counts %>%
  filter(n > 20) %>%
  graph_from_data_frame()

bigram_graph
#
#########
library(ggraph)
set.seed(2017)

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link() +
  geom_node_point() +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1)

set.seed(2020)

a <- grid::arrow(type = "closed", length = unit(.15, "inches"))

ggraph(bigram_graph, layout = "fr") +
  geom_edge_link(aes(edge_alpha = n), show.legend = FALSE,
                 arrow = a, end_cap = circle(.07, 'inches')) +
  geom_node_point(color = "lightblue", size = 5) +
  geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
  theme_void()

count_bigrams <- function(dataset) {
  dataset %>%
    unnest_tokens(bigram, text, token = "ngrams", n = 2) %>%
    separate(bigram, c("word1", "word2"), sep = " ") %>%
    filter(!word1 %in% stop_words$word,
           !word2 %in% stop_words$word) %>%
    count(word1, word2, sort = TRUE)
}

visualize_bigrams <- function(bigrams) {
  set.seed(2016)
  a <- grid::arrow(type = "closed", length = unit(.15, "inches"))
  
  bigrams %>%
    graph_from_data_frame() %>%
    ggraph(layout = "fr") +
    geom_edge_link(aes(edge_alpha = n), show.legend = FALSE, arrow = a) +
    geom_node_point(color = "lightblue", size = 5) +
    geom_node_text(aes(label = name), vjust = 1, hjust = 1) +
    theme_void()
}
########

airbnb_cor <- airbnb_subset %>%
  mutate(textcat = textcat(x = description), cld2 = cld2::detect_language(text = text, plain_text = FALSE), 
         cld3 = cld3::detect_language(text = text)) %>%
  select(text, textcat, cld2, cld3) %>%
  filter(cld2 == "en" & cld3 == "en") %>%
  unnest_tokens(word, description) %>%
  filter(!word %in% stop_words$word)

word_cors <- airbnb_cor %>%
  group_by(word) %>%
  filter(n() >= 5) %>%
  pairwise_cor(word, region, sort=TRUE)

word_cors %>%
  filter(correlation >.9) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr")+
  geom_edge_link(aes(edge_alpha = correlation), show.legend=F)+
  geom_node_point(color = "lightgreen", size=6)+
  geom_node_text(aes(label=name), repel=T)+
  theme_void()
##########
airbnb_token <- airbnb_subset %>%
  unnest_tokens(word, description) %>%
  count(region, word, sort=TRUE) %>%
  ungroup()

total_words <- airbnb_subset %>%
  group_by(region) %>%
  summarize(total=sum(n))

netflix_words <- left_join(netflix_token, total_words)%>%
  filter(country %in% c("United States", "Mexico", "India"))

print(netflix_words)







