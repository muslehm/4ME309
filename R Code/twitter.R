library (rtweet)
library(dplyr)
library(tidyr)
wd <- getwd()
setwd(wd)
options(scipen = 999) 
#create an access token 
twitter_token <- create_token(
  app = "",
  consumer_key = "",
  consumer_secret = "",
  access_token = "",
  access_secret = "")

#create a query
query <- "#BlackLivesMatter lang:en"

#recent tweets (seven last days, up to 18000 tweets per 15 minutes)
tweets <- search_tweets (query, n=1000000, type = "recent", include_rts = F, parse = TRUE, retryonratelimit = TRUE, token = twitter_token)


##SAVE Tweets to CSV File
my.tweets <- data.frame(lapply(tweets, as.character), stringsAsFactors=FALSE)
#CONDUCTED on SEP 25
write.csv(my.tweets,'blm0925to0916.csv', row.names = FALSE)
#CONDUCTED on OCT 2
write.csv(my.tweets,'blm1002to0923.csv', row.names = FALSE)

#CONDUCTED on OCT 2: Read and merge the two datasets then save as csv
df1 <- read.csv('blm1002to0923.csv', header = TRUE)
df2 <- read.csv('blm0925to0916.csv', header = TRUE)
df <- rbind(df1, df2)

my.tweets <- data.frame(lapply(df, as.character), stringsAsFactors=FALSE)
write.csv(my.tweets,'blm1002to0916.csv', row.names = FALSE)

##Filter Merge Tweets
my.tweets <- read.csv('blm1002to0916.csv', header = TRUE)
colnames(my.tweets)

#Drop 42 variables
drops <- c("urls_expanded_url", "is_retweet", "screen_name", "reply_to_screen_name", "ext_media_expanded_url", "ext_media_type",
           "mentions_screen_name", "lang", "quoted_screen_name", "quoted_description", "quoted_name", "retweet_status_id",      
           "retweet_text", "retweet_created_at", "retweet_source", "retweet_favorite_count", "retweet_retweet_count", "retweet_user_id",  
           "retweet_screen_name", "retweet_name", "retweet_followers_count", "retweet_friends_count", "retweet_statuses_count",  
           "retweet_location", "retweet_description", "retweet_verified", "name", "place_url", "place_full_name", "country", 
           "geo_coords", "coords_coords", "bbox_coords", "description", "status_url", "url", "profile_url", "profile_expanded_url", 
           "account_lang", "profile_banner_url", "profile_background_url", "profile_image_url")
names(my.tweets)

my.tweets <- my.tweets[ , !(names(my.tweets) %in% drops)]
my.tweets <- data.frame(lapply(my.tweets, as.character), stringsAsFactors=FALSE)
write.csv(my.tweets,'filtered_blm1002to0916.csv', row.names = FALSE)

