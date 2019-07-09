# Packages to install

#working with APIs
install.packages("httr")
install.packages("jsonlite")
#Twitter
install.packages("rtweet")
#Instagram - see comments
install.packages("instaR")
#Youtube - see comments
install.packages("tubeR")

#text analytics
install.packages("tidyverse")
install.packages("tidytext")
install.packages("wordcloud2")

#image analytics
install.packages("RoogleVision")

#additional
install.packages("knitr")
install.packages("plotly")

#load libraries
library(rtweet)
library(instaR)
library(tidyverse)
library(tidytext)
library(wordcloud2)
library(RoogleVision)
library(ggplot2)
library(jsonlite)

stringsAsFactors=FALSE
#Let's do our first search and locate Twitter users interested in coffee 
usrs <- search_users("#coffee", n = 1000)
head(usrs)

tw <- search_tweets(q = "cappuccino", n = 1000, lang="en")
tw_coffee <- search_tweets(q = "expresso", lang="en", n = 5000)

#plot it!
#plot1
ts_plot(tw_coffee, "hours")

#plot2
tw_coffee2 <- search_tweets(q = "cappuccino OR latte", n = 5000)

tw_coffee2 %>%
  dplyr::group_by(lang) %>%
  ts_plot("hours")

hashtags <- read.csv("hashtags.csv", encoding="UTF-8")

#Let's load tweets downloaded earlier
data <- read.csv("coffeeTweets.csv", encoding="UTF-8", stringsAsFactors=FALSE)
dim(data)

#how many unique Twitter accounts in the sample and what are the earliest tweets we have
length(unique(data$screen_name))
min(data$created_at)

#Let's see the frequencies across different languages. Who is the coffee lover?

library(lubridate)

data %>% 
mutate(timestamp = dmy_hm(created_at)) %>% 
ggplot( aes(x = timestamp, fill = lang)) +
geom_histogram(position = "identity", bins = 20, show.legend = TRUE) +
facet_wrap(~lang, ncol = 1)

## Hashtag frequency and wordcloud

head(data$hashtags)

tidy_hashtags<-data %>%
  #select(hashtags) %>%
  unnest_tokens(word, hashtags, token = "words") %>%
  group_by(word) %>%
  count() %>%
  filter(n > 30) %>%
  ungroup() %>%
  mutate(word = reorder(word,n)) %>%
  arrange(desc(word)) %>%
  rename(freq = n)

head(tidy_hashtags)

library(wordcloud2)
wordcloud2(tidy_hashtags)

#excluding used hashtags

other_hashtags<- tidy_hashtags %>%
  filter(!word %in% hashtags$hashtags)

wordcloud2(other_hashtags)

wordcloud2(other_hashtags, size = 0.7, shape="star", color = "random-light")

#We can also see how many hashtags users are using and if we can relate a higher number of `hashtags` to higher `favorite_count` and `retweet_count`

hashtagsFreq<-data %>% 
  select("user_id", "hashtags", "favorite_count", "retweet_count") %>% 
  mutate(hFreq=strsplit(as.character(hashtags), " ")) %>% 
  unnest(hFreq) %>% 
  add_count(user_id)  %>% 
  select(-hFreq)  %>%
  distinct

top_n(hashtagsFreq, 10, n)
top_n(hashtagsFreq, 10, favorite_count)
top_n(hashtagsFreq, 10, retweet_count)


ggplot(data=hashtagsFreq[1:200,], aes(x=n, y=favorite_count)) +
  geom_line()+
  geom_point()+
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold"))+
  ggplot2::labs(
      x = NULL, y = NULL,
      title = "Frequency of hashtags vs number of favourite counts",
      subtitle = "tweets are collected over June-July 2019",
      caption = "\nSource: Data collected from Twitter's REST API via rtweet"
    )

ggplot(data=hashtagsFreq[1:200,], aes(x=n, y=retweet_count)) +
  geom_line()+
  geom_point()+
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold"))+
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of hashtags vs number of retweet counts",
    subtitle = "tweets are collected over June-July 2019",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

#Profile search 
starbucks<-get_timeline("Starbucks",n = 3200)
users_data(starbucks[1,])

#frequency of tweets

coffeeCompanies <- get_timelines(c("Starbucks", "CostaCoffee", "dunkindonuts"), n = 5000)

## plot the frequency of tweets for each user over time
coffeeCompanies %>%
  dplyr::filter(created_at > "2019-07-01") %>%
  dplyr::group_by(screen_name) %>%
  ts_plot("days", trim = 1L) +
  ggplot2::geom_point() +
  ggplot2::theme_minimal() +
  ggplot2::theme(
    legend.title = ggplot2::element_blank(),
    legend.position = "bottom",
    plot.title = ggplot2::element_text(face = "bold")) +
  ggplot2::labs(
    x = NULL, y = NULL,
    title = "Frequency of tweets for Starbucks, CostaCoffee and Dunkin'",
    subtitle = "tweets are collected over June-July 2019",
    caption = "\nSource: Data collected from Twitter's REST API via rtweet"
  )

#getting the list of followers
flws<-get_followers("Starbucks")
dim(flws)


#------------------------------------------------------------------
## WORKING WITH MEDIA - googleVision

head(data[,c("screen_name", "media_url", "media_type")])

#let's download some photos from Twitter

URL ="http://pbs.twimg.com/media/D-2u7VTXsAErzLw.jpg"
download.file(URL,'test.jpg', mode = 'wb')


devtools::install_github("flovv/RoogleVision")
library(RoogleVision)
library(jsonlite) 

### set up your credentials (client id and client secret from Google Developer Console)
options("googleAuthR.client_id" = "667235664106-od6l95g7smm567do3eu2otfr9kd3uh25.apps.googleusercontent.com")
options("googleAuthR.client_secret" = "goFM_igE4N8n46GIfh0znepz")

options("googleAuthR.scopes.selected" = c("https://www.googleapis.com/auth/cloud-platform"))

googleAuthR::gar_auth() #Authentication

getGoogleVisionResponse("http://pbs.twimg.com/media/D-2u7VTXsAErzLw.jpg", feature="LABEL_DETECTION")

getGoogleVisionResponse("http://pbs.twimg.com/media/D-sY1hJXUAANqBf.jpg", feature="LABEL_DETECTION")

getGoogleVisionResponse("https://pbs.twimg.com/media/CZFKJZhVIAA4Y7m.jpg", feature="TEXT_DETECTION")

getGoogleVisionResponse("https://pbs.twimg.com/media/D-RL2xcWwAEEyeG.jpg", feature="LANDMARK_DETECTION")

#------------------------------------------------------------------
## SEMANTIC ANALYSIS - Now let's have a look at semantics of tweets
remove_reg <- "&amp;|&lt;|&gt;"
tidy_tweets <- data %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_remove_all(text, remove_reg)) %>%
  unnest_tokens(word, text, token = "tweets") %>%
  filter(!word %in% stop_words$word,
         !word %in% str_remove_all(stop_words$word, "'"),
         str_detect(word, "[a-z]"))

#Examples of dictionaries
#Loughran and McDonald lexicon 
get_sentiments("loughran")
get_sentiments("loughran") %>%
  count(sentiment, sort = TRUE)

#Bing lexicon
get_sentiments("bing") %>%
  count(sentiment, sort = TRUE)

#adding sentiment to the dataset

tweets_sentiment <- tidy_tweets %>%
  inner_join(get_sentiments("loughran"))  %>%
  group_by(sentiment) %>% # group by sentiment type
  tally %>% # counts number of rows
  arrange(desc(n))

ggpubr::ggpie(tweets_sentiment, "n", label = "sentiment", 
              fill = "sentiment", color = "white", 
              palette = "Spectral")

#or lets investigate by top words use in each sentiment
tidy_tweets %>%
  inner_join(get_sentiments("loughran"))  %>%
  count(sentiment, word) %>%
  filter(sentiment %in% c("positive", "negative", 
                          "uncertainty", "litigious")) %>%
  group_by(sentiment) %>%
  top_n(15) %>%
  ungroup %>%
  mutate(word = reorder(word, n)) %>%
  mutate(sentiment = factor(sentiment, levels = c("negative",
                                                  "positive",
                                                  "uncertainty",
                                                  "litigious"))) %>%
  ggplot(aes(word, n, fill = sentiment)) +
  geom_col(alpha = 0.8, show.legend = FALSE) +
  coord_flip() +
  scale_y_continuous(expand = c(0,0)) +
  facet_wrap(~sentiment, scales = "free") +
  labs(x = NULL, y = "Total number of occurrences",
       title = "Words driving sentiment scores in coffee",
       subtitle = "From the Loughran-McDonald lexicon")

#-----------------
#EMOJI

library(emo)

emo::ji("heart")

emo::ji("ghost")

emoji <- data %>%
  mutate( 
    emoji = ji_extract_all(text)
  ) %>%
  select(screen_name,emoji) %>% 
  unnest(emoji) 
