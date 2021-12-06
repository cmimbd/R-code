####################################################################
################### Created by Cinthia Mimbela #####################
####################### MBAN2 HULT 2021 ############################
########################## 11/29/2021 ##############################
######################### Version 3.0 ##############################
####################################################################

######################## Calling libraries #########################

library(twitteR)
library(magrittr)
library(tidytext)
library(dplyr)
library(stringr)
library(tm)
library(tidyverse)
library(tidyr)
library(scales)
library(igraph)
library(ggraph)
library(ggplot2)
library(widyr)

######################## Collecting data ##########################

#Access keys and token for Twitter (current data)

consumer_key <- ''
consumer_secret <- ''
access_token <- ''
access_secret <- ''

setup_twitter_oauth(consumer_key, consumer_secret, access_token, access_secret)

#Topic: Climate Change
cc <- twitteR::searchTwitter('"#climatechange" -filter:retweets', n = 10000,retryOnRateLimit = 1e3, geocode = )
climate_change = twitteR::twListToDF(cc)

#Removing odd characters
climate_change$text <- sapply(climate_change$text,function(row) iconv(row, "latin1", "ASCII", sub="")) #removes emoticon
climate_change$text = gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", climate_change$text) #removes URL
climate_change$text = gsub('[[:punct:]]',' ',climate_change$text) #removes punctuation
climate_change$text = gsub('[[:cntrl:]]','',climate_change$text) #removes ASCII control characters
climate_change$text = gsub('\\d+','',climate_change$text) #removes decimal number
climate_change$text = gsub('\n','',climate_change$text) #removes new lines
climate_change$text = tolower(climate_change$text) #to lower case

climate_change$text

#Access old tweets from kaggle (.csv)

occ <- read.csv("D:/Hult - MBAN/5 - Text Analytics and NLP/Business Insight Report/twitter_sentiment_data.csv", comment.char="#")

colnames(occ)[2] <- "text"

#Removing odd characters
occ$text <- sapply(occ$text,function(row) iconv(row, "latin1", "ASCII", sub="")) #removes emoticon
occ$text = gsub("(f|ht)tp(s?)://(.*)[.][a-z]+", "", occ$text) #removes URL
occ$text = gsub('[[:punct:]]',' ',occ$text) #removes punctuation
occ$text = gsub('[[:cntrl:]]','',occ$text) #removes ASCII control characters
occ$text = gsub('\\d+','',occ$text) #removes decimal number
occ$text = gsub('\n','',occ$text) #removes new lines
occ$text = tolower(occ$text) #to lower case

#Filtering retweets
old_climate_change <- occ[substr(occ$text, 1, 3) !="rt ",]

old_climate_change

######################## 1. Tweets Analysis #########################

#User with more retweets
climate_change %>%
  group_by(screenName) %>%
  summarize(retweets=sum(retweetCount)) %>% 
  top_n(10) %>% 
  mutate(screenName = reorder(screenName,retweets)) %>% 
  ggplot(aes(screenName, retweets)) +
  geom_col() +
  labs(y="Retweets", x=NULL)+
  coord_flip()

#User with more favorites
climate_change %>%
  group_by(screenName) %>%
  summarize(favorites=sum(favoriteCount)) %>% 
  top_n(10) %>% 
  mutate(screenName = reorder(screenName,favorites)) %>% 
  ggplot(aes(screenName, favorites)) +
  geom_col() +
  labs(y="Favorites", x=NULL)+
  coord_flip()

#Tweet with more retweets
climate_change %>%
  group_by(text) %>%
  summarize(retweets=sum(retweetCount)) %>% 
  top_n(1) %>% 
  mutate(text = reorder(text,retweets))

#Tweet with more favorites
climate_change %>%
  group_by(text) %>%
  summarize(favorites=sum(favoriteCount)) %>% 
  top_n(1) %>% 
  mutate(text = reorder(text,favorites))

#TD-IDF Framework

#creating a tidy format for old tweets
old_tidy <- old_climate_change %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  select(word)

print(old_tidy)

#creating a tidy format for current tweets
tidy <- climate_change %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>%
  select(word)

print(tidy)

tidy_all <- bind_rows(mutate(old_tidy, time="old"),
                       mutate(tidy, time= "current"))

tidy_total <- tidy_all %>%
  count(time, word, sort=TRUE) %>%
  ungroup()

tidy_total

tidy_token <- tidy_total %>%
  count(time, word, sort=TRUE) %>%
  ungroup()

total_words <- tidy_token %>%
  group_by(time) %>%
  summarize(total=sum(n))

climate_change_words <- left_join(tidy_token, total_words)

climate_change_words

time_words <- climate_change_words %>%
  bind_tf_idf(word, time, n)

time_words %>%
  arrange(desc(tf_idf)) %>%
  mutate(word=factor(word, levels=rev(unique(word)))) %>%
  group_by(time) %>%
  top_n(15) %>%
  ungroup %>%
  ggplot(aes(word, tf_idf, fill=time))+
  geom_col(show.legend=FALSE)+
  labs(x=NULL, y="tf-idf")+
  facet_wrap(~time, ncol=2, scales="free")+
  coord_flip()

######################## 2. Words Analysis #########################
#most frequent words old tweets
old_cc_hist <- old_climate_change %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% 
  filter(word != "climate", word != "change", word != "climatechange") %>% #filtering words: climate and change
  count(word, sort=TRUE) %>% 
  filter(n>500) %>% 
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()

print(old_cc_hist)

#most frequent words current tweets
cc_hist <- climate_change %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% 
  filter(word != "climate", word != "change", word != "climatechange") %>%
  count(word, sort=TRUE) %>% 
  filter(n>300) %>% 
  mutate(word = reorder(word,n)) %>%
  ggplot(aes(word, n))+
  geom_col()+
  xlab(NULL)+
  coord_flip()

print(cc_hist)

#Bigram analysis - old tweets
old_bigrams <- old_climate_change %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>%
  separate(bigram, c("word1", "word2"), sep=" ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) 

old_bigrams

#plot (nodes)
old_bigram_graph <- old_bigrams %>%
  count(word1, word2, sort = TRUE) %>% 
  filter(n>20) %>%
  graph_from_data_frame()

ggraph(old_bigram_graph, layout = "fr") +
  theme(panel.background = element_rect(fill = "white"))+
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)


#Correlations - words (old tweets) 
old_word_cors <-  old_climate_change %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word) %>% 
  group_by(word) %>%
  filter(n() >= 5) %>%
  pairwise_cor(word, tweetid, sort=TRUE)

#Plotting these correlations

old_word_cors %>%
  filter(correlation >.8) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr")+
  geom_edge_link(aes(edge_alpha = correlation), show.legend=F)+
  geom_node_point(color = "lightblue", size=6)+
  geom_node_text(aes(label=name), repel=T)+
  theme_void()


#Bigram analysis - recent tweets
bigrams <- climate_change %>%
  unnest_tokens(bigram, text, token = "ngrams", n=2) %>%
  separate(bigram, c("word1", "word2"), sep=" ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) 

bigrams

#plot (nodes)
bigram_graph <- bigrams %>%
  count(word1, word2, sort = TRUE) %>% 
  filter(n>20) %>%
  graph_from_data_frame()

ggraph(bigram_graph, layout = "fr") +
  theme(panel.background = element_rect(fill = "white"))+
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)


#Correlations - words (recent tweets)
word_cors <-  climate_change %>%
  unnest_tokens(word, text) %>%
  filter(!word %in% stop_words$word) %>% 
  group_by(word) %>%
  filter(n() >= 5) %>%
  pairwise_cor(word, id, sort=TRUE)

#Plotting these correlations

word_cors %>%
  filter(correlation >.8) %>%
  graph_from_data_frame() %>%
  ggraph(layout = "fr")+
  geom_edge_link(aes(edge_alpha = correlation), show.legend=F)+
  geom_node_point(color = "lightblue", size=6)+
  geom_node_text(aes(label=name), repel=T)+
  theme_void()


#Trigram analysis - old tweets
old_trigrams <- old_climate_change %>%
  unnest_tokens(trigram, text, token = "ngrams", n=3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep=" ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word3 %in% stop_words$word)

old_trigrams

#plot (nodes)
old_trigram_graph <- old_trigrams %>%
  count(word1, word2, word3, sort = TRUE) %>% 
  filter(n>20) %>%
  graph_from_data_frame()

ggraph(old_trigram_graph, layout = "fr") +
  theme(panel.background = element_rect(fill = "white"))+
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)

#Trigram analysis - current tweets
trigrams <- climate_change %>%
  unnest_tokens(trigram, text, token = "ngrams", n=3) %>%
  separate(trigram, c("word1", "word2", "word3"), sep=" ") %>%
  filter(!word1 %in% stop_words$word) %>%
  filter(!word2 %in% stop_words$word) %>% 
  filter(!word3 %in% stop_words$word)

trigrams

#plot (nodes)
trigram_graph <- trigrams %>%
  count(word1, word2, word3, sort = TRUE) %>% 
  filter(n>20) %>%
  graph_from_data_frame()

ggraph(trigram_graph, layout = "fr") +
  theme(panel.background = element_rect(fill = "white"))+
  geom_edge_link()+
  geom_node_point()+
  geom_node_text(aes(label=name), vjust =1, hjust=1)


######################## 3. Correlation Analysis #########################

#creating a tidy format for old tweets
old_tidy <- old_climate_change %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% 
  select(word)

print(old_tidy)

#creating a tidy format for current tweets
tidy <- climate_change %>%
  unnest_tokens(word, text) %>%
  anti_join(stop_words) %>% 
  select(word)

print(tidy)

frequency <- bind_rows(mutate(old_tidy, time="old"),
                       mutate(tidy, time= "current")
) %>% #closing bind_rows
  mutate(word=str_extract(word, "[a-z']+")) %>%
  count(time, word) %>%
  group_by(time) %>%
  mutate(proportion = n/sum(n))%>%
  select(-n) %>% #sorting desc
  spread(time, proportion) %>% 
  gather(time, proportion, `current`)

#plot the correlograms:

ggplot(frequency, aes(x=proportion, y=`old`, 
                      color = abs(`old`- proportion)))+
  geom_abline(color="grey30", lty=2)+
  geom_jitter(alpha=.1, size=2.5, width=0.3, height=0.3)+
  geom_text(aes(label=word), check_overlap = TRUE, vjust=1.5) +
  scale_x_log10(labels = percent_format())+
  scale_y_log10(labels= percent_format())+
  scale_color_gradient(limits = c(0,0.001), low = "darkslategray4", high = "gray75")+
  facet_wrap(~time, ncol=2)+
  theme(legend.position = "none")+
  labs(y= "old", x=NULL)

#correlation values

cor.test(data=frequency[frequency$time == "current",], ~proportion + `old`)

######################## 4. Sentiment Analysis #########################

#Old tweets - Union of the three lexicons

#Current tweets - Union of the three lexicons
old_cc_token <- old_climate_change %>%
  unnest_tokens(word, text) %>% 
  select(word)

old_afinn_bing_nrc <- bind_rows(
  old_cc_token %>%
    inner_join(get_sentiments("afinn"))%>%
    summarise(sentiment=sum(value)) %>%
    mutate(method="AFINN"),
  
  old_cc_token%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al.")%>%
    count(method,  sentiment) %>%
    spread(sentiment, n, fill=0) %>%
    mutate(sentiment = positive-negative),
  
  old_cc_token %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC") %>%
    count(method,  sentiment) %>%
    spread(sentiment, n, fill=0) %>%
    mutate(sentiment = positive-negative)
  
)

old_afinn_bing_nrc %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

#most common positive and negative words

old_bing_counts <- old_cc_token %>%
  select(word) %>% 
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

old_bing_counts

old_bing_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()


#Current tweets - Union of the three lexicons
cc_token <- climate_change %>%
  unnest_tokens(word, text)

afinn_bing_nrc <- bind_rows(
  cc_token %>%
    inner_join(get_sentiments("afinn"))%>%
    summarise(sentiment=sum(value)) %>%
    mutate(method="AFINN"),
  
  cc_token%>%
    inner_join(get_sentiments("bing"))%>%
    mutate(method = "Bing et al.")%>%
    count(method,  sentiment) %>%
    spread(sentiment, n, fill=0) %>%
    mutate(sentiment = positive-negative),
  
  cc_token %>%
    inner_join(get_sentiments("nrc") %>%
                 filter(sentiment %in% c("positive", "negative"))) %>%
    mutate(method = "NRC") %>%
    count(method,  sentiment) %>%
    spread(sentiment, n, fill=0) %>%
    mutate(sentiment = positive-negative)
  
)

afinn_bing_nrc %>%
  ggplot(aes(method, sentiment, fill=method))+
  geom_col(show.legend=FALSE)+
  facet_wrap(~method, ncol =1, scales= "free_y")

#most common positive and negative words

bing_counts <- cc_token %>%
  inner_join(get_sentiments("bing")) %>%
  count(word, sentiment, sort=T) %>%
  ungroup()

bing_counts

bing_counts %>%
  group_by(sentiment) %>%
  top_n(10) %>%
  ungroup() %>%
  mutate(word=reorder(word, n)) %>%
  ggplot(aes(word, n, fill=sentiment)) +
  geom_col(show.legend = FALSE) +
  facet_wrap(~sentiment, scales = "free_y")+
  labs(y="Contribution to sentiment", x=NULL)+
  coord_flip()









