library(lubridate)
library(ggplot2)
library(dplyr)
library(readr)
library(tidyr)
library(purrr)
library(tidytext)
library(stringr)
library(scales)

#Retrieving and setting our working directory (wd)
getwd()
setwd("c:/University/Second Year/Founders Network/Big Data Educational/")

#retrieving the saved data sets
tweets_obama <- read_csv("data/ObamaTweets.csv")
tweets_trump <- read_csv("data/theRealTrumpTweet.csv")

#reformating the two data sets to match
tweets_trump[,"id_str"] = NULL

tweets_obama[, c("in_reply_to_status_id", "in_reply_to_user_id", "retweeted_status_user_id", "retweeted_status_timestamp", "expanded_urls")] = NULL
colnames(tweets_obama)[colnames(tweets_obama)=="retweeted_status_id"] <- "is_retweet"
tweets_obama$is_retweet = tweets_obama$is_retweet > 0
tweets_obama$is_retweet[is.na(tweets_obama$is_retweet)] = FALSE

#combining two data sets into one with new label called "person" to keep track of who tweeted what
tweets <- bind_rows(tweets_obama %>%  mutate(person = "Obama"),  tweets_trump %>%  mutate(person = "Trump"))


#removing twitter specific words and generic words
replace_reg <- "https://t.co/[A-Za-z\\d]+|http://[A-Za-z\\d]+|&amp;|&lt;|&gt;|RT|https"
unnest_reg <- "([^A-Za-z_\\d#@']|'(?![A-Za-z_\\d#@]))"
tidy_tweets <- tweets %>% 
  filter(!str_detect(text, "^RT")) %>%
  mutate(text = str_replace_all(text, replace_reg, "")) %>%
  unnest_tokens(word, text, token = "regex", pattern = unnest_reg) %>%
  filter(!word %in% stop_words$word,
         str_detect(word, "[a-z]"))


#frequency take 1
frequency <- tidy_tweets %>% 
  group_by(person) %>% 
  count(word, sort = TRUE) %>% 
  left_join(tidy_tweets %>% 
              group_by(person) %>% 
              summarise(total = n())) %>%
  mutate(freq = n/total)

frequency
# frequency take 2
frequency <- frequency %>% select(person, word, freq) %>% spread(person, freq) %>% arrange(Trump, Obama)

frequency

#frequency plot
ggplot(frequency, aes(Trump, Obama)) +
  geom_jitter(alpha = 0.1, size = 2.5, width = 0.25, height = 0.25) +
  geom_text(aes(label = word), check_overlap = TRUE, vjust = 1.5) +
  scale_x_log10(labels = percent_format()) +
  scale_y_log10(labels = percent_format()) +
  geom_abline(color = "red")
readRDS(file ="Trump_frequence_graph.RDS")

#Creating a word comparison
word_ratios <- tidy_tweets %>%
  filter(!str_detect(word, "^@")) %>%
  count(word, person) %>%
  filter(sum(n) >= 10) %>%
  ungroup() %>%
  spread(person, n, fill = 0) %>%
  mutate_if(is.numeric, funs((. + 1) / sum(. + 1))) %>%
  mutate(logratio = log(Trump / Obama)) %>%
  arrange(desc(logratio))

#applying a log ration to compare word usage and plotting results
word_ratios %>% 
  arrange(abs(logratio))

word_ratios %>%
  group_by(logratio < 0) %>%
  top_n(15, abs(logratio)) %>%
  ungroup() %>%
  mutate(word = reorder(word, logratio)) %>%
  ggplot(aes(word, logratio, fill = logratio < 0)) +
  geom_col(show.legend = FALSE) +
  coord_flip() +
  ylab("log odds ratio (Trump/Obama)") +
  scale_fill_discrete(name = "", labels = c("Trump", "Obama"))
readRDS("Trump_word_ratios_graph.RDS")




