library(tidyverse)
library(dplyr)
library(rtweet)
library(academictwitteR)
library(readr)
library(httr)
library(jsonlite)


###########################################################################################
# clean-up all tweets
all_tweets_motives <- readRDS("all_tweets_with_motives.rds")

####################################################################################
# topic modeling

unique_tweets_with_sentiment <- readRDS("all_tweets_sentiment.rds")

tweets_with_topics <- unique_tweets_with_sentiment[unique_tweets_with_sentiment$reply_to_status_id == "",]

writeLines(as.character(tweets_with_topics$text[[1500]]))

tweets_with_topics$text <- iconv(tweets_with_topics$text, to = "ASCII", sub = " ") 

tweets_with_topics$text <- stringr::str_replace_all(tweets_with_topics$text, "@[a-z,A-Z,_]*", "") %>%
  # remove whitespaces if tweet only consists of usernames
  stringr::str_trim()

tweets_with_topics$text <- stringr::str_replace_all(tweets_with_topics$text, "https://t.co/[a-z,A-Z,0-9]*", "") %>%
  # remove whitespaces if tweet only consists of url
  stringr::str_trim()

tweets_with_topics$text <- stringr::str_replace_all(tweets_with_topics$text, "#[a-z,A-Z,_]*", "") %>%
  # remove whitespaces if tweet only consists of hashtags
  stringr::str_trim()

#tweets_with_topics$text <- stringr::str_replace_all(tweets_with_topics$text, "[[:digit:]]", "") %>%
  # remove whitespaces if tweet only consists of digits
#stringr::str_trim()

#tweets_with_topics$text <- stringr::str_replace_all(tweets_with_topics$text, "[[:punct:]]", "") %>%
  # remove whitespaces if tweet only consists of punctuation
#  stringr::str_trim()

writeLines(as.character(tweets_with_topics$text[[1500]]))

tweets_with_topics$text = tolower(tweets_with_topics$text)
#tweets_with_topics$text = unique(tweets_with_topics$text)
writeLines(as.character(tweets_with_topics$text[[1500]]))

library(tm)

stopwords <- c(stopwords::stopwords("en"), "amp", "na", "rt", "via", "im", "just", "like", "can", "get", "now", "dont", "one", "don", "s", "time", "day", "new", "today", "let", "much", "never", "ever", "really", "make", "put", "talk", "ask", "use",
               "make", "think", "know", "want", "see", "look", "say", "need", "even", "thing", "got", "also", "way", "use", "still", "yes", "well", "said", "made", "hope", "good", "give", "youre", "thats", "says", "gonna", "going", "saying", "people")
#stopwords <- c(stopwords::stopwords("en"), "amp", "na", "rt", "via", "im", "s")
tweets_with_topics$text <- removeWords(tweets_with_topics$text, stopwords)
#tweets_with_topics$text <- tm_map(tweets_with_topics$text, removeWords, english)
tweets_with_topics$text <- removePunctuation(tweets_with_topics$text, preserve_intra_word_dashes = TRUE)
tweets_with_topics$text <- removeNumbers(tweets_with_topics$text)
#tweets_with_topics$text <- tm_map(tweets_with_topics$text, stemDocument, language = "en")
tweets_with_topics$text <- stripWhitespace(tweets_with_topics$text)
tweets_with_topics$text <- trimws(tweets_with_topics$text, which = c("both"))
tweets_with_topics <- tweets_with_topics[tweets_with_topics$text != " ",]
tweets_with_topics <- tweets_with_topics %>% filter(lapply(strsplit(text, " "), FUN = length) > 1)
tweets_with_topics$words <- strsplit(tweets_with_topics$text, " ")
tweets_with_topics$wordlengths <- lapply(tweets_with_topics$words, FUN = nchar)
tweets_with_topics$throw_out <- lapply(tweets_with_topics$wordlengths, function(x) all(x < 3))
tweets_with_topics <- tweets_with_topics[tweets_with_topics$throw_out == FALSE,]

corpus <- Corpus(VectorSource(tweets_with_topics$text))

#library(wordcloud)
#set.seed(1234)
#palet  = brewer.pal(8, 'Dark2')
#wordcloud(corpus, min.freq = 50, scale = c(4, 0.2) , random.order = TRUE, col = palet)

dtm = DocumentTermMatrix(corpus, control = list(wordLengths=c(3, Inf)))
#dtm
doc.length = apply(dtm, 1, sum)
which(doc.length <= 0)

library(topicmodels)
library(ldatuning)
#LDA model with 7 topics selected

result <- ldatuning::FindTopicsNumber(
  dtm,
  topics = seq(from = 2, to = 20, by = 1),
  metrics = c("CaoJuan2009",  "Deveaud2014"),
  method = "Gibbs",
  control = list(seed = 77),
  verbose = TRUE
)
FindTopicsNumber_plot(result)

lda_16 = LDA(dtm, k = 16, method = 'Gibbs', 
            control = list(nstart = 5, seed = list(1505,99,36,56,88), best = TRUE, 
                           thin = 500, burnin = 4000, iter = 2000, alpha = 0.4))

top10terms_16 = as.matrix(terms(lda_16,10))
top10terms_16

saveRDS(lda_16, "lda_16.rds")

tmResult <- posterior(lda_16)
theta <- tmResult$topics
beta <- tmResult$terms

# re-rank top topic terms for topic names
topicNames <- apply(lda::top.topic.words(beta, 5, by.score = T), 2, paste, collapse = " ")
# What are the most probable topics in the entire collection?
topicProportions <- colSums(theta) / nDocs(dtm)  # mean probabilities over all paragraphs
names(topicProportions) <- topicNames     # assign the topic names we created before
sort(topicProportions, decreasing = TRUE) # show summed proportions in decreased order

topics_ids <- matrix(ncol=3, nrow=nDocs(dtm))
for (i in 1:nDocs(dtm)) {
  topicsPerDoc <- theta[i, ] # select topic distribution for document i
  # get first element position from ordered list
  if (any(topicsPerDoc > 0.3)) {
    primaryTopic <- order(topicsPerDoc, decreasing = TRUE)[1:2]
    print(primaryTopic[2])
    topics_ids[i,1] <- tweets_with_topics$status_id[i]
    topics_ids[i,2] <- topicNames[primaryTopic[1]]
    topics_ids[i,3] <- topicNames[primaryTopic[2]]
  }
}

topics_ids_df <- data.frame(topics_ids) %>% drop_na()

topics_ids_df <- rename(topics_ids_df, c("status_id" = "X1", "topic1" = "X2", "topic2" = "X3"))

unique_tweets_with_topics <- left_join(all_tweets_with_sentiment, topics_ids_df, by="status_id")

saveRDS(unique_tweets_with_topics, "all_tweets_topics.rds")

###########################################################################################
# classify URLs

unique_tweets_with_topics <- readRDS("all_tweets_topics.rds")

URLS <- unique_tweets_with_topics[unique_tweets_with_topics$urls_expanded_url != "" & !(grepl("twitter.com", unique_tweets_with_sentiment$urls_expanded_url, fixed = TRUE)),]
URLS <- URLS %>% select(status_id, urls_expanded_url)
URLS$urls_expanded_url <- as.character(URLS$urls_expanded_url)
#categories <- read_csv("URL-categorization-DFE.csv")
#categories <- categories %>% select(main_category, url)

URLS$urls_expanded_url <- lapply(URLS$urls_expanded_url, function(x) strsplit(x, "/"))

for (i in 1:length(URLS$urls_expanded_url)) {
  main_url <- URLS$urls_expanded_url[[i]][[1]][3]
  main_url <- gsub('www.', '', main_url)
  URLS$urls_expanded_url[[i]] <- main_url
}

all_tweets_with_urls <- left_join(unique_tweets_with_topics, URLS, by="status_id")

saveRDS(all_tweets_with_urls, "full_dataset.rds")

###########################################################################################
# add follower relations

all_tweets_with_urls <- readRDS("full_dataset.rds")

bearer_token <- "XXX"

all_tweets_with_urls$followed_by_retweeter <- NA
all_tweets_with_urls$following_retweeter <- NA
for (i in 1:nrow(all_tweets_with_urls)){
  if (all_tweets_with_urls$is_retweet[i] == TRUE){
    print(all_tweets_with_urls$screen_name[i]); flush.console()
    print(all_tweets_with_urls$retweet_screen_name[i]); flush.console()
    connection <- lookup_friendships(all_tweets_with_urls$screen_name[i], all_tweets_with_urls$retweet_screen_name[i])
    print(connection$value["following"]); flush.console()
    if (!is.null(connection$value["following"])){
      print("worked"); flush.console()
      all_tweets_with_urls$followed_by_retweeter[i] <- connection$value["followed_by"]
      all_tweets_with_urls$following_retweeter[i] <- connection$value["following"]
    } else {
      print("waiting 15mins"); flush.console()
      Sys.sleep(900)
      print("done waiting"); flush.console()
    }
  }
}

saveRDS(all_tweets_with_urls, "full_dataset_friends.rds")

###########################################################################################
# add manually labeled URL categories

#tweets_w_motives <- public_all_tweets[!is.na(all_tweets$motive_bringattention), ]
#saveRDS(tweets_w_motives, "tweets_w_motives.rds")
#write_csv(tweets_w_motives, "tweets_w_motives.csv")
all_tweets_with_friends <- readRDS("full_dataset_friends.rds")

URL_categories <- read_csv2("URL_categories.csv", )
URL_categories <- unique(URL_categories)

all_tweets_with_friends <- all_tweets_with_friends %>% dplyr::rename("base_url" = "urls_expanded_url.y")
all_tweets_with_friends$base_url <- as.character(all_tweets_with_friends$base_url)

all_tweets_with_URL_categories <- left_join(all_tweets_with_friends, URL_categories, by="base_url")

saveRDS(all_tweets_with_URL_categories, "full_dataset_URL_cat.rds")

###########################################################################################
# chatGPT
library(httr)

all_tweets_with_friends <- readRDS("full_dataset_counts.rds")

# Then, put your API key in the quotes below: 
my_API <- "XXX"

#The "hey_chatGPT function will help you access the API and prompt GPT 
hey_chatGPT <- function(answer_my_question) {
  chat_GPT_answer <- POST(
    url = "https://api.openai.com/v1/chat/completions",
    add_headers(Authorization = paste("Bearer", my_API)),
    content_type_json(),
    encode = "json",
    body = list(
      model = "gpt-3.5-turbo-0301",
      temperature = 0,
      messages = list(
        list(
          role = "user",
          content = answer_my_question
        )
      )
    )
  )
  str_trim(content(chat_GPT_answer)$choices[[1]]$message$content)
}

# Read in your dataset
#data <- as.data.frame(all_tweets_with_friends$text[200:230])
#data <- as.data.frame(all_tweets_with_friends[!is.na(all_tweets_with_friends$tweet_motive2), ])
data <- all_tweets_with_friends

# Create a "gpt" column
data$gpt <- NA

# Run a loop over your dataset and prompt ChatGPT - an example prompt for sentiment is given
for (i in 1:nrow(data)) {
#for (i in 1:10) {
  print(i)
  #question <- "Is the sentiment of this text positive, neutral, or negative? Answer only with a number: 1 if positive, 2 if neutral, and 3 if negative. Here is the text:"
  #question <- "What is the general topic of this tweet? Answer with one word that describes a category and try using always the same categories and add (comma separated) another word for the subtopic specifying it further. And what is the sentiment of this tweet? Answer in a new line with one word (comma separated) for the sentiment and only choose between positive, neutral, and negative, also add comma separated the one most pronounced emotion distinguishing between disgust, anger, fear, joy, disgust, sadness, surpise, trust. Use NA if one of the answers is not applicable. Don't use any description or titles in your response and answer in the exact format: topic, subtopic \n sentiment, emotion. Here is the tweet:"
  question <- "What is the general topic of this tweet? Answer with one word that describes a category and try using always the same categories and add (comma separated) another word for the subtopic specifying it further, also add (comma separated) the sentiment between positive, neutral, and negative, also add comma separated the one most pronounced sentiment distinguishing between disgust, anger, fear, joy, disgust, sadness, surpise, trust. If none of those apply just say NA. Here is the tweet:"
  text <- data$text[i]
  print(text)
  concat <- paste(question, text)
  result <- hey_chatGPT(concat)
  while(length(result) == 0){
    result <- hey_chatGPT(concat)
    print(result)
  }
  print(result)
  data$gpt[i] <- result
}
#write_csv(data, "GPT_tweetcategory_test2.csv")

data <- readRDS("full_dataset_gpt.rds")

#for (i in 1:nrow(data)) {
data$category1 <- NA
data$category2 <- NA
data$sentiment <- NA
data$emotion <- NA
for (i in 1:nrow(data)) {
  print(i)
  split <- unlist(strsplit(data$gpt[i], ", "))
  print(split)
  if (length(split) == 4){
    data$category1[i] <- tolower(split[[1]])
    data$category2[i] <- tolower(split[[2]])
    data$sentiment[i] <- tolower(split[[3]])
    data$emotion[i] <- tolower(split[[4]])
    }
}
saveRDS(data, "full_dataset_categories.rds")
all_tweets_with_friends <- readRDS("full_dataset_categories.rds")

data <- all_tweets_with_friends

# Create a "gpt" column
data$outrage <- NA

# Run a loop over your dataset and prompt ChatGPT - an example prompt for sentiment is given
for (i in 1:nrow(data)) {
  #for (i in 1:10) {
  print(i)
  #question <- "Is the sentiment of this text outraging? Answer only with a number: 1 if somehow outraging, and 0 if not outraging. Here is the text:"
  question <- "Is this tweet expressing moral outrage? Answer only with a number: 1 if yes, and 0 if not. Here is the text:"
  #question <- "What is the general topic of this tweet? Answer with one word that describes a category and try using always the same categories and add (comma separated) another word for the subtopic specifying it further. And what is the sentiment of this tweet? Answer in a new line with one word (comma separated) for the sentiment and only choose between positive, neutral, and negative, also add comma separated the one most pronounced emotion distinguishing between disgust, anger, fear, joy, disgust, sadness, surpise, trust. Use NA if one of the answers is not applicable. Don't use any description or titles in your response and answer in the exact format: topic, subtopic \n sentiment, emotion. Here is the tweet:"
  #question <- "What is the general topic of this tweet? Answer with one word that describes a category and try using always the same categories and add (comma separated) another word for the subtopic specifying it further, also add (comma separated) the sentiment between positive, neutral, and negative, also add comma separated the one most pronounced sentiment distinguishing between disgust, anger, fear, joy, disgust, sadness, surpise, trust. If none of those apply just say NA. Here is the tweet:"
  text <- data$text[i]
  print(text)
  concat <- paste(question, text)
  result <- hey_chatGPT(concat)
  while(length(result) == 0){
    result <- hey_chatGPT(concat)
    print(result)
  }
  print(result)
  data$outrage[i] <- result
}

saveRDS(data, "full_dataset_outrage.rds")

###########################################################################################

all_tweets_with_friends <- readRDS("full_dataset_outrage.rds")
#saveRDS(all_tweets_with_friends, "full_dataset_categories.rds")
all_tweets_with_friends$is_reply <- ifelse(all_tweets_with_friends$reply_to_status_id != "", TRUE, FALSE)

###########################################################################################
#get a random subset for human checking

tweets <- all_tweets_with_friends %>% filter(!is.na(tweet_motive3))
org_tweets <- tweets %>% filter(is_reply == "FALSE")
random_subset <- sample_n(org_tweets, 100)
write_csv(random_subset, "test_set.csv")

test_set_gpt <- read_csv("test_set.csv")
test_set_human <- read.csv("test_set_human.csv", sep = ";")

overlap <- 0
baserate <- 0
for (i in 1:nrow(test_set_gpt)) {
  print(test_set_gpt$sentiment[i])
  print(test_set_human$Sentiment[i])
  if (!is.na(test_set_gpt$sentiment[i]) && !is.na(test_set_human$Sentiment[i])){
    baserate <- baserate + 1
    if (test_set_gpt$sentiment[i] == test_set_human$Sentiment[i]){
      print(i)
      overlap <- overlap + 1
    }
  }
}
print(overlap/baserate)

overlap <- 0
baserate <- 0
for (i in 1:nrow(test_set_gpt)) {
  print(test_set_gpt$outrage[i])
  print(test_set_human$Outrage[i])
  if (!is.na(test_set_gpt$outrage[i]) && !is.na(test_set_human$Outrage[i])){
    baserate <- baserate + 1
    if (test_set_gpt$outrage[i] == test_set_human$Outrage[i]){
      print(i)
      overlap <- overlap + 1
    }
  }
}
print(overlap/baserate)

overlap <- 0
baserate <- 0
for (i in 1:nrow(test_set_gpt)) {
  print(test_set_gpt$category1[i])
  print(test_set_human$Topic[i])
  if (!is.na(test_set_gpt$category1[i]) && !is.na(test_set_human$Topic[i]) && test_set_human$Topic[i] == "politics"){
    baserate <- baserate + 1
    if (test_set_gpt$category1[i] == test_set_human$Topic[i]){
      print(i)
      overlap <- overlap + 1
    }
  }
}
print(overlap/baserate)

test_set_human$Emotion <- tolower(test_set_human$Emotion)
overlap <- 0
baserate <- 0
for (i in 1:nrow(test_set_gpt)) {
  print(test_set_gpt$emotion[i])
  print(test_set_human$Emotion[i])
  if (!is.na(test_set_gpt$emotion[i]) && !is.na(test_set_human$Emotion[i])){
    baserate <- baserate + 1
    if (test_set_gpt$emotion[i] == test_set_human$Emotion[i]){
      print(i)
      overlap <- overlap + 1
    }
  }
}
print(overlap/baserate)

#test <- read_csv("full_dataset.csv")
###########################################################################################
# remove IDs and text and names etc for public version
#write_csv(all_tweets_with_friends, "full_dataset.csv")

public_all_tweets <- all_tweets_with_friends %>% select(created_at, is_quote, is_retweet, is_reply, display_text_width, favorite_count, retweet_count, quote_count_2, reply_count_2, hashtags, media_type, 
                                                     quoted_favorite_count, quoted_retweet_count, retweet_favorite_count, retweet_retweet_count,  retweet_verified, 
                                                     followers_count, friends_count, statuses_count, favourites_count, account_created_at, verified, session, created, 
                                                     relative_date, daynumber, tweet_retweet_count, retweet_favorite_count.x, contains("tweet_motive"), contains("retweet_motive"),
                                                     anger, disgust, fear, anticipation, joy, sadness, surprise, trust, outrage, outraging, negative, positive, topic1, base_url, url_category, following_retweeter, followed_by_retweeter, following_quoted, followed_by_quoted, category1, category2, sentiment, emotion)

public_all_tweets <- public_all_tweets %>% dplyr::rename("original_tweet_retweets_then" = "tweet_retweet_count")
public_all_tweets <- public_all_tweets %>% dplyr::rename("original_tweet_favorites_then" = "retweet_favorite_count.x")
public_all_tweets <- public_all_tweets %>% dplyr::rename("quote_count" = "quote_count_2")
public_all_tweets <- public_all_tweets %>% dplyr::rename("reply_count" = "reply_count_2")
#public_all_tweets <- public_all_tweets %>% dplyr::rename("base_url" = "urls_expanded_url.y")
#public_all_tweets$base_url <- as.character(public_all_tweets$base_url)
public_all_tweets$hashtags <- as.character(public_all_tweets$hashtags)
public_all_tweets <- public_all_tweets %>% dplyr::rename(retweet_motive_support = retweet_motive1, retweet_motive_makefunof = retweet_motive2, retweet_motive_addthoughts = retweet_motive3, retweet_motive_argue = retweet_motive4, retweet_motive_expressfeelings = retweet_motive5, retweet_motive_ridicule = retweet_motive6, retweet_motive_correct = retweet_motive7)
public_all_tweets <- public_all_tweets %>% dplyr::rename(motive_informothers = tweet_motive1, motive_entertain = tweet_motive2, motive_expressopinion = tweet_motive3, motive_provoke = tweet_motive4, motive_savecontent = tweet_motive5, motive_showemotions = tweet_motive6, motive_connectwothers = tweet_motive7, motive_showachievement = tweet_motive8, motive_showattitude = tweet_motive9, motive_deceiveothers = tweet_motive10, motive_gainattention = tweet_motive11, motive_provepoint = tweet_motive12, motive_causechaos = tweet_motive13, motive_bringattention = tweet_motive14, motive_influence = tweet_motive15, motive_surpriseothers = tweet_motive16)
public_all_tweets <- public_all_tweets %>% dplyr::rename(sentiment_anger = anger, sentiment_disgust = disgust, sentiment_fear = fear, sentiment_anticipation = anticipation, sentiment_joy = joy, sentiment_sadness = sadness, sentiment_surprise = surprise, sentiment_trust = trust, sentiment_positive = positive, sentiment_negative = negative)


saveRDS(public_all_tweets, "public_dataset.rds")
write_csv(public_all_tweets, "public_dataset.csv")

###########################################################################################
# calculate activity of participants 
public_all_tweets <- readRDS("public_dataset.rds")

public_all_tweets$active_days <- difftime(as.Date(public_all_tweets$created), as.Date(public_all_tweets$account_created_at),units='days')+35
public_all_tweets$average_activity <- as.numeric(public_all_tweets$statuses_count)/as.numeric(public_all_tweets$active_days)

saveRDS(public_all_tweets, "public_dataset.rds")
write_csv(public_all_tweets, "public_dataset.csv")
