setwd("C:\\Users\\acer\\Desktop\\MABA\\Business Domain 2")
# install.packages("twitteR")
# install.packages("tidytext")
# install.packages("dplyr")
# install.packages("ggplot2")
# install.packages("tidyverse")
# install.packages("Rtools")
# install.packages("rtweet")
 
## install httpuv if not already
if (!requireNamespace("httpuv", quietly = TRUE)) {
  install.packages("httpuv")
}

devtools::install_github("mkearney/rtweet")

library(rtweet)
library(dplyr)
#library(twitteR)
library(tidyverse)
library(tidytext)
#library(twitteR)
library(tm)
library(syuzhet)
library(textdata)
############################# SET UP AUTHENTICATION ############################################### 
consumer_key = "oUN1MKzB8rbCqouXt7n4ljyk5"
consumer_secret = "irHYmevPNRHHBY47PkrQffpoyWeLa4ejoHqDwj0fNF1MIhkgu1"
app_name = "NCOV_scraper"
access_token = "1235768866380238848-lIKGBu3i348N9g7D9AC86laVQknkqG"
access_secret = "bzmxmLai96Ql9pCAO2ORgrVPaTB2WPtNU8BKRyXQwkaU7"

## create token
token <- create_token(app_name, consumer_key, consumer_secret,access_token, access_secret)
## print token
token

## save token to home directory
path_to_token <- file.path(path.expand("~"), ".twitter_token.rds")
saveRDS(token, path_to_token)
## create env variable TWITTER_PAT (with path to saved token)
env_var <- paste0("TWITTER_PAT=", path_to_token)
## save as .Renviron file (or append if the file already exists)
cat(env_var, file = file.path(path.expand("~"), ".Renviron"), 
    fill = TRUE, append = TRUE)

########################### SCRAPE TWEETS ########################################################
fn_twitter <- search_tweets("Duterte AND Covid Philippines",n=3000, include_rts = FALSE)
fn_twitter_df4 <- fn_twitter %>% select("text", "user_id", "status_id", "created_at", "hashtags")
fn_twitter_df4$hashtags <- vapply(fn_twitter_df4$hashtags, paste, collapse = ", ", character(1L))
write.csv(fn_twitter_df4, "fn_twitter_df4.csv")

fn_twitter_en <- search_tweets("Duterte AND Covid Philippines",n=3000,include_rts = FALSE, lang="en")
fn_twitter_en_df4 <- fn_twitter_en %>% select("text", "user_id", "status_id", "created_at", "hashtags")
fn_twitter_en_df4$hashtags <- vapply(fn_twitter_en_df4$hashtags, paste, collapse = ", ", character(1L))
write.csv(fn_twitter_en_df4, "fn_twitter_en_df4.csv")


#########################CLEAM THE TEXT COLUMN###################################################
En_tweet<-read.csv("En_tweet.csv")
En_tweet$Tweet <- gsub("http.*","",  En_tweet$Tweet)
En_tweet$Tweet <- gsub("https.*","", En_tweet$Tweet)
En_tweet$Tweet <- gsub("@.*","", En_tweet$Tweet)
En_tweet$Tweet <- gsub("#.*","", En_tweet$Tweet)
Tweet_en_stem<- En_tweet %>% unnest_tokens(word, Tweet) 
#remove stop words
cleaned_tweets_en<- Tweet_en_stem %>% anti_join(stop_words)

cleaned_tweets2_en<-



En_tweet$Tweet <- gsub("[[:space:]]","", En_tweet$Tweet)
Tweet_en<-Corpus(VectorSource(En_tweet$Tweet))
Tweet_en = tm_map(Tweet_en,stripWhitespace)
Tweet_en = tm_map(Tweet_en,removeWords,stopwords("en"))
Tweet_en = tm_map(Tweet_en,removePunctuation)
Tweet_en= tm_map(Tweet_en,removeNumbers)

Tweet_en2<-as.data.frame(unlist(as.list(Tweet_en)))
#Tweet_en3<-as.data.frame(Tweet_en2)
Tweet_en3<- unnest_tokens(word, Tweet_en)
#########################
# Tweet_sentiments <- get_sentiment(Tweet_en2,method="bing")
# 
# sentiments_tweet <- Tweet_en2 %>%  
#   inner_join(get_sentiments("bing"), by = "word") %>%
#   count(word, sentiment, sort = TRUE) %>%
#   ungroup() %>%
#   group_by(sentiment) %>%
#   top_n(10) %>%
#   ungroup() %>%
#   mutate(word = reorder(word, n)) %>%
#   ggplot(aes(word, n, fill = sentiment)) +
#   geom_col(show.legend = FALSE) +
#   scale_fill_manual(values = c("red2", "green3")) +
#   facet_wrap(~sentiment, scales = "free_y") +
#   ylim(0, 2500) +
#   labs(y = NULL, x = NULL) +
#   coord_flip() +
#   theme_minimal()

#sentiment
sentiment<-cleaned_tweets_en %>% inner_join(get_sentiments("bing")) %>% count(word, sentiment, sort=TRUE) %>%
  ungroup()

#Emotions
word.df <- as.vector(En_tweet$Tweet)
emotion<- get_nrc_sentiment(word.df)
emotion.df2 <- cbind(En_tweet, emotion) 

#Get sentiment score
sent.value <- get_sentiment(word.df)
most.positive <- word.df[sent.value == max(sent.value)]
most.negative <- word.df[sent.value <= min(sent.value)] 

positive.tweets <- word.df[sent.value > 0]
negative.tweets <- word.df[sent.value < 0]
neutral.tweets <- word.df[sent.value == 0]

################## save datasets fro PBI visualization
write.csv(sentiment,"sentiment_polarity.csv")
write.csv(emotion.df2,"tweet_emotion.csv")
write.csv(positive.tweets,"positive.tweets.csv")
write.csv(negative.tweets,"negative.tweets.csv")
write.csv(neutral.tweets,"neutral.tweets.csv")

############################# TOPIC MODELLING ######################################
