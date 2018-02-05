###load req packages

library(ROAuth)
library(twitteR)
library(base64enc)
library(httr)
library(RCurl)
library(httpuv)
library(purrr)
library(dplyr)
library(tidyr)
library(lubridate)
library(ggplot2)
library(stringr)

###send API creds-redacted

api_key<-""
api_secret<-""
acess_token<-""
acess_token_secret<-""

owner_id<-""
owner<-""
setup_twitter_oauth(api_key,api_secret)

###grab tweets from @USAA_help timeline
helptweets<-userTimeline("USAA_Help", n=3000)

helptweets_df<-tbl_df(map_df(helptweets, as.data.frame))

july_helptweets<-filter(helptweets_df, month(created)==07)

###see if we got all the way to 1 july
View(tail(july_helptweets))

###see what the time distribution is for helper tweets

bytime<-july_helptweets %>% count(id, hour=hour(with_tz(created, "EST" )) )

byday<-july_helptweets %>% count(id, day=day(with_tz(created, "EST" )) )

start<-min(july_helptweets$created)
end<-max(july_helptweets$created)
                                 
g<-ggplot(bytime, aes(x=hour))+geom_histogram(binwidth =1)
g

 
g<-ggplot(byday, aes(x=day))+geom_histogram(binwidth =1, fill="blue", color="white")+scale_x_continuous(breaks = 1:31)+
        xlab("Day in July")+ylab("Tweets from @USAA_Help")
g

###theres a wild spike more on that later
mid_july_spike<-filter(july_helptweets, day(created)==14)


###Whats up with tweets between midnight and 0500?

early<-filter(july_helptweets, hour(helptweets_df$created)<=5)

nrow(early)
###182 tweets between midnight and 5 AM in july-thats service!

### by day is a bit silly give that it starts at 0 each month.  We have the end July
### and beggining of August here-maybecheck by day of week?
g1<-ggplot(byday, aes(x=day))+geom_histogram(binwidth =1)
g1

end-start
###lets see whats in the tweets
reg <- "([^A-Za-z\\d#@']|'(?![A-Za-z\\d#@]))"
###get the words for each tweet broken out as tokens
july_tweet_words<-july_helptweets %>%
        unnest_tokens(word, text, token="regex", pattern=reg) %>%
        filter(!word %in% stop_words$word,
               str_detect(word, "[a-z]"))

july_tweet_wordst<-july_tweet_words %>% select(replyToSN, created,id,word)

head(july_tweet_wordst)

t<-plyr::count(july_tweet_wordst, 'word') 
t<-arrange(t, -freq)
head(t, 20)

###looks like most instructions to direct message as dm by far the most common word
###lets see if the tweets from the 14 (spike day are different)

spike_tweet_words<-filter(july_tweet_wordst, day(created)==14)
spike_t<-plyr::count(spike_tweet_words, 'word')
spike_t<-arrange(spike_t, -freq)
head(spike_t,20)

###looks like they definatly are different as outage, access, restore and unabailale now top the list
###-dm drops way down so it looks like on this day the @USAA_account is diseminating an 
###apology/explanation rather than solving individual problems notably assist has dropped out all together 

###lets plot our top words for the two periods 

###match on ids?

g<-ggplot(head(t,20), aes(x=word, y=freq))+geom_bar(stat="identity")

g

####as interesting as the @USAA_Help tweets are....the customers question might be even better
###lets grab those as well. Looks like we cant go back that far...###seems like we are stuck at 295
customer_tweets<-searchTwitter("@USAA_help",n=300, retryOnRateLimit = 5)

###now put them in a data frame and then to data table
customer_tweets_df<-tbl_df(map_df(customer_tweets, as.data.frame))

###tokenize and remove stop words
customer_tweet_words<-customer_tweets_df %>%
        unnest_tokens(word, text, token="regex", pattern=reg) %>%
        filter(!word %in% stop_words$word,
               str_detect(word, "[a-z]"))
###shrink to the variables of iterest
customer_tweet_wordst<-customer_tweet_words %>% select(replyToSN, created,id,word,longitude,latitude)

###get counts 
c_word_counts<-plyr::count(customer_tweet_wordst, 'word') 
c_word_counts<-arrange(c_word_counts, -freq)
head(c_word_counts, 20)

