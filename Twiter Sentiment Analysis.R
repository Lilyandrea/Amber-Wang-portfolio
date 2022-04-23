####### Sentiment Analysis
# 1. correlograms - done
# 2. analysing correlations with bigrams
# 3. tf_idf on bigrams - done
# 4. prediction with naive bayes

library(dplyr)
library(tidytext)
library(tidyr)
library(widyr)
library(tidyverse)
# install.packages("textcat")
library(textcat)
library(cld2)

# creating a copy final data for sentiment analysis
ab_senti_data <- prediction_Data
# View(ab_senti_data)
colnames(ab_senti_data)[2] <- "text" # renaming description col as text

# filter out reviews with english
language <- c()
language <- textcat(ab_senti_data$text)
ab_senti_data <- ab_senti_data %>% filter(language == "english")

# tokenising for correlogram
tidy_airbnb <- ab_senti_data %>% unnest_tokens(word, text) %>% anti_join(stop_words) 

# plotting tokenised frequencies
tidy_airbnb %>% 
  group_by(region) %>% 
  count(word, sort = T) %>% 
  filter(n>500) %>%
  mutate(word=reorder(word,n)) %>%
  ggplot(aes(word,n,color = region)) + geom_col() + xlab(NULL) + coord_flip() + facet_wrap(~region, ncol=4)

# plotting correlograms
library(tidyr)
frequency <- tidy_airbnb %>% 
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(region, word) %>%
  group_by(region) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>%
  spread(region, proportion) %>%
  gather(region, proportion, `Asia`, `Europe`, `Others`)

# let's plot the correlograms:
library(scales)
ggplot(frequency, aes(x=proportion, y=`North America`, 
                      color = abs(`North America`- proportion)))+
  geom_abline(color="grey40", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.0001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~region, ncol=3)+
  theme(legend.position = "none")+
  labs(y= "North America", x=NULL)

cor.test(data=frequency[frequency$region == "Asia",],
         ~proportion + `North America`) # very high correlation observed
cor.test(data=frequency[frequency$region == "Europe",],
         ~proportion + `North America`) # very high correlation observed 
cor.test(data=frequency[frequency$region == "Others",],
         ~proportion + `North America`) # very high correlation observed 

# analysis of top 20 tokens across regions
summary(frequency$proportion)
frequency %>% arrange(desc(proportion)) %>% top_n(20) %>% filter(!is.na(word)) 

# analysis using bigrams
airbnb_bigrams <- ab_senti_data %>% 
  unnest_tokens(bigram, text, token = "ngrams", n = 2) %>% 
  separate(bigram, c("word1", "word2"), sep = " ") %>% 
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word)

airbnb_bigrams %>% count(word1, word2, sort = TRUE) %>% top_n(20)

# tf-idf on airbnb bigrams
library(tidytext)
airbnb_bigrams_united <- airbnb_bigrams %>% unite(bigram, word1, word2, sep = " ")

airbnb_tf_idf <- airbnb_bigrams_united %>%
  count(region, bigram) %>%
  bind_tf_idf(bigram, region, n) %>%
  arrange(desc(tf_idf))

View(airbnb_tf_idf)

# analysis of tf-idf
airbnb_tf_idf %>% arrange(desc(tf_idf)) %>% filter(region == "Asia") %>% top_n(10)
airbnb_tf_idf %>% arrange(desc(tf_idf)) %>% filter(region == "Europe") %>% top_n(10)
airbnb_tf_idf %>% arrange(desc(tf_idf)) %>% filter(region == "North America") %>% top_n(10)
airbnb_tf_idf %>% arrange(desc(tf_idf)) %>% filter(region == "Others") %>% top_n(10)

# tf_idf bigram graph
airbnb_tf_idf %>%
  arrange(desc(tf_idf)) %>%
  mutate(bigram=factor(bigram, levels=rev(unique(bigram)))) %>%
  group_by(region) %>%
  top_n(10) %>%
  ungroup %>%
  ggplot(aes(bigram, tf_idf, fill=region))+
  geom_col(show.legend=TRUE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~region, ncol=4, scales="free")+
  coord_flip()

# pairwise correlations
library(widyr)

# pairwise correlations
library(widyr)
library(ggraph)
library(igraph)
library(ggrepel)

airbnb_cor <- tidy_airbnb %>% 
  group_by(word) %>%
  filter(n() >= 5) %>% 
  pairwise_cor(word, id_num, sort=TRUE)

summary(airbnb_cor$correlation)
airbnb_cor %>% filter(correlation>0.001) %>% top_n(20)

airbnb_cor %>%
  filter(correlation >.0001) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr")+
  geom_edge_link(aes(edge_alpha = correlation), show.legend=F)+
  geom_node_point(color = "lightgreen", size=6)+
  geom_node_text(aes(label=name), repel=T)+
  geom_text_repel(max.overlaps = getOption("ggrepel.max.overlaps", default = 10))+
  theme_void()