#' ---
#' title: Reactivity
#' output:
#'   html_document:
#'     toc: true
#'     toc_depth: 3
#' ---
#'

#' # Load packages
if (!require("pacman")) install.packages("pacman"); library(pacman)

p_load(tidyverse, brms, tidybayes)

library(brms)
library(tidybayes)
library(broom.mixed)
library(bayesplot)



#################################################################################################
# load data

tweet_pre_df <- readRDS("pre_survey.rds")
tweet_pre_df <- tweet_pre_df[!(is.na(tweet_pre_df$session) | tweet_pre_df$session==""), ]
tweet_pre_df$created <- as.Date(tweet_pre_df$created)
tweet_pre_df <- filter(tweet_pre_df, created >= "2022-02-18")

all_tweets <- readRDS("public_dataset.rds")

all_tweets <- all_tweets %>% mutate_all(type.convert)
all_tweets <- all_tweets %>% 
  mutate(has_media = !is.na(media_type)) %>% 
  mutate(has_link = base_url != "NULL") %>% 
  mutate(is_regular_tweet = if_else(is_quote | is_retweet | is_reply, FALSE, TRUE)) 
# observables:
# binary/tweet level: "exists", is_quote, is_retweet, is_reply, has_media
# counts: favorite_count, retweet_count, quote_count, reply_count, word count (fehlt), display_text_width
# sentiments: sentiment_anger, sentiment_disgust, sentiment_fear, sentiment_anticipation, sentiment_joy, sentiment_sadness, sentiment_surprise, sentiment_trust, sentiment_negative, sentiment_positive
# is_quote, is_retweet, is_reply, display_text_width, favorite_count, retweet_count, quote_count, reply_count, sentiment_anger, sentiment_disgust, sentiment_fear, sentiment_anticipation, sentiment_joy, sentiment_sadness, sentiment_surprise, sentiment_trust, sentiment_negative, sentiment_positive

qplot(all_tweets$retweet_count) + scale_x_log10()

user_day_level <- all_tweets %>% group_by(session, created, relative_date)%>% 
  summarise(tweet_count = n(),
            regular_tweet_count = sum(is_regular_tweet),
            retweet_count = sum(is_retweet),
            quote_tweet_count = sum(is_quote),
            media_count = sum(has_media),
            link_count = sum(has_link),
            reply_count = sum(is_reply)) %>% ungroup()


ggplot(user_day_level %>% select(relative_date, ends_with("count")) %>% 
         pivot_longer(-relative_date) %>% 
         mutate(relative_date = as.numeric(relative_date),
                period = case_when(relative_date <= 0 ~ "pre",
                                   relative_date > 7 ~ "post",
                                   TRUE ~ "during")), aes(relative_date, value, group = period)) + 
  annotate("rect", xmin = 0, xmax = 7, ymin = -Inf, ymax = Inf, fill = "#EEDDAA99") +
  geom_smooth(method = 'glm', method.args = list(family = 'poisson')) +
  facet_wrap(~ name, scales = "free_y") +
  ggtitle("Frequency of daily tweets by user") +
  theme_bw()

cor.test(all_tweets$motive_showemotions, all_tweets$sentiment_joy)


# probability of a tweet being a QT, RT, reply
ggplot(all_tweets %>% select(relative_date, is_quote, is_retweet, is_reply) %>% 
         mutate(is_regular_tweet = if_else(is_quote | is_retweet | is_reply, FALSE, TRUE)) %>% 
         pivot_longer(-relative_date) %>% 
         mutate(relative_date = as.numeric(relative_date),
                value = if_else(value, 1, 0, NA_real_),
                period = case_when(relative_date <= 0 ~ "pre",
                                   relative_date > 7 ~ "post",
                                   TRUE ~ "during")), aes(relative_date, value, group = period)) + 
  annotate("rect", xmin = 0, xmax = 7, ymin = -Inf, ymax = Inf, fill = "#EEDDAA99") +
  geom_smooth(method = 'lm') + 
  theme_minimal() +
  facet_wrap(~ name, scales = "free_y")

ggplot(all_tweets %>% select(relative_date, starts_with("sentiment")) %>% 
         pivot_longer(-relative_date) %>% 
         mutate(relative_date = as.numeric(relative_date),
                name = str_replace(name, "sentiment_", ""),
                period = case_when(relative_date <= 0 ~ "pre",
                                   relative_date > 7 ~ "post",
                                   TRUE ~ "during")), aes(relative_date, value, group = period)) + 
  annotate("rect", xmin = 0, xmax = 7, ymin = -Inf, ymax = Inf, fill = "#EEDDAA99") +
  geom_smooth(method = 'lm') + 
  facet_wrap(~ name, scales = "free_y") + 
  ggtitle("Sentiment")(~ name, scales = "free_y")

# category2, sentiment, emotion
all_tweets <- all_tweets %>%
  mutate(category1 = recode(category1, "na" = NA_character_,
                              "arts" = "art"))
table(all_tweets$category1) %>% sort()
all_tweets$tweet_id <- 1:nrow(all_tweets)
top_cats <-  table(all_tweets$category1, exclude = NULL) %>% sort %>% tail(6) %>% names()
all_tweets %>% select(session, tweet_id, category = category1) %>% 
  filter(category %in% top_cats) %>% 
  mutate(category = factor(category, levels = rev(top_cats)),
         value = 1) %>% 
  complete(nesting(session, tweet_id), category, fill = list(value = 0)) %>% 
  left_join(all_tweets %>% select(session, tweet_id, relative_date)) %>% 
  mutate(
    relative_date = as.numeric(relative_date),
    period = case_when(relative_date <= 0 ~ "pre",
                       relative_date > 7 ~ "post",
                       TRUE ~ "during")
  ) %>% 
  arrange(session, category, relative_date) %>% 
ggplot(., aes(relative_date, value, group = period)) + 
  annotate("rect", xmin = 0, xmax = 7, ymin = -Inf, ymax = Inf, fill = "#EEDDAA99") +
  geom_smooth(method = 'lm') + 
  facet_wrap(~ category, scales = "free_y") + 
  theme_minimal() 

all_tweets <- all_tweets %>%
  mutate(category2 = recode(category2, "na" = NA_character_,
                            "arts" = "art"))
top_cats <-  table(all_tweets$category2, exclude = NULL) %>% sort %>% tail(20) %>% names()
all_tweets %>% select(session, tweet_id, category = category2) %>% 
  filter(category %in% top_cats) %>% 
  mutate(category = factor(category, levels = rev(top_cats)),
         value = 1) %>% 
  complete(nesting(session, tweet_id), category, fill = list(value = 0)) %>% 
  left_join(all_tweets %>% select(session, tweet_id, relative_date)) %>% 
  mutate(
    relative_date = as.numeric(relative_date),
    period = case_when(relative_date <= 0 ~ "pre",
                       relative_date > 7 ~ "post",
                       TRUE ~ "during")
  ) %>% 
  arrange(session, category, relative_date) %>% 
  ggplot(., aes(relative_date, value, group = period)) + 
  annotate("rect", xmin = 0, xmax = 7, ymin = -Inf, ymax = Inf, fill = "#EEDDAA99") +
  geom_smooth(method = 'lm') + 
  facet_wrap(~ category, scales = "free_y") + 
  ggtitle("Subcategory")

table(all_tweets$emotion, exclude = NULL) %>% as.data.frame() %>% View
all_tweets <- all_tweets %>%
  mutate(
    emotion_raw = emotion,
         emotion = str_replace(emotion_raw, "most pronounced sentiment:", ""),
         emotion = str_replace(emotion, "pronounced sentiment:", ""),
         emotion = str_replace(emotion, "distinguishing sentiment:", ""),
         emotion = str_extract(emotion, "[a-z]+"),
    emotion = recode(emotion, "na" = NA_character_,
                     "na." = NA_character_),
  )
top_cats <-  table(all_tweets$emotion, exclude = NULL) %>% sort %>% tail(20) %>% names()

all_tweets %>% select(session, tweet_id, category = emotion) %>% 
  filter(category %in% top_cats) %>% 
  mutate(category = factor(category, levels = rev(top_cats)),
         value = 1) %>% 
  complete(nesting(session, tweet_id), category, fill = list(value = 0)) %>% 
  left_join(all_tweets %>% select(session, tweet_id, relative_date)) %>% 
  mutate(
    relative_date = as.numeric(relative_date),
    period = case_when(relative_date <= 0 ~ "pre",
                       relative_date > 7 ~ "post",
                       TRUE ~ "during")
  ) %>% 
  arrange(session, category, relative_date) %>% 
  ggplot(., aes(relative_date, value, group = period)) + 
  annotate("rect", xmin = 0, xmax = 7, ymin = -Inf, ymax = Inf, fill = "#EEDDAA99") +
  geom_smooth(method = 'lm') + 
  facet_wrap(~ category, scales = "free_y") + 
  ggtitle("Emotion")

table(all_tweets$sentiment, exclude = NULL) %>% sort()
all_tweets <- all_tweets %>% 
  mutate(sentiment_clean = str_extract(sentiment, "(positive|negative|neutral)"))
top_cats <-  table(all_tweets$sentiment_clean, exclude = NULL) %>% sort %>% tail(5) %>% names()

all_tweets %>% select(session, tweet_id, category = sentiment_clean) %>% 
  filter(category %in% top_cats) %>% 
  mutate(category = factor(category, levels = rev(top_cats)),
         value = 1) %>% 
  complete(nesting(session, tweet_id), category, fill = list(value = 0)) %>% 
  left_join(all_tweets %>% select(session, tweet_id, relative_date)) %>% 
  mutate(
    relative_date = as.numeric(relative_date),
    period = case_when(relative_date <= 0 ~ "pre",
                       relative_date > 7 ~ "post",
                       TRUE ~ "during")
  ) %>% 
  arrange(session, category, relative_date) %>% 
  ggplot(., aes(relative_date, value, group = period)) + 
  annotate("rect", xmin = 0, xmax = 7, ymin = -Inf, ymax = Inf, fill = "#EEDDAA99") +
  geom_smooth(method = 'lm') + 
  facet_wrap(~ category, scales = "free_y") + 
  ggtitle("Sentiment")

all_tweets %>% select(session, tweet_id, relative_date, sentiment = sentiment_clean) %>% 
  mutate(sentiment = recode(sentiment, "positive" = 1, "neutral" = 0,
                            "negative" = -1)) %>% 
  mutate(
    relative_date = as.numeric(relative_date),
    period = case_when(relative_date <= 0 ~ "pre",
                       relative_date > 7 ~ "post",
                       TRUE ~ "during")
  ) %>% 
  ggplot(., aes(relative_date, sentiment, group = period)) + 
  annotate("rect", xmin = 0, xmax = 7, ymin = -Inf, ymax = Inf, fill = "#EEDDAA99") +
  geom_smooth(method = 'lm') + 
  theme_minimal() +
  ggtitle("Sentiment")

all_tweets %>% select(session, tweet_id, relative_date, outrage) %>% 
  mutate(outrage =
           case_when(outrage == 1 ~ 1,
                     TRUE ~ 0)) %>%
  mutate(
    relative_date = as.numeric(relative_date),
    period = case_when(relative_date <= 0 ~ "pre",
                       relative_date > 7 ~ "post",
                       TRUE ~ "during")
  ) %>% 
  ggplot(., aes(relative_date, outrage, group = period)) + 
  annotate("rect", xmin = 0, xmax = 7, ymin = -Inf, ymax = Inf, fill = "#EEDDAA99") +
  geom_smooth(method = 'lm') + 
  theme_minimal() +
  ggtitle("Outrage")

ggplot(all_tweets %>% select(character_count = display_text_width, starts_with("sentiment")) %>% 
         pivot_longer(-character_count), aes(character_count, value)) +
  geom_smooth()+ 
  facet_wrap(~ name, scales = "free_y")

ggplot(all_tweets %>% filter(is_regular_tweet) %>% select(relative_date, starts_with("sentiment")) %>% 
         pivot_longer(-relative_date) %>% 
         mutate(relative_date = as.numeric(relative_date),
                name = str_replace(name, "sentiment_", ""),
                period = case_when(relative_date <= 0 ~ "pre",
                                   relative_date > 7 ~ "post",
                                   TRUE ~ "during")), aes(relative_date, value, group = period)) + 
  annotate("rect", xmin = 0, xmax = 7, ymin = -Inf, ymax = Inf, fill = "#EEDDAA99") +
  geom_smooth(method = 'lm') + 
  facet_wrap(~ name, scales = "free_y") + 
  ggtitle("Sentiment", "regular tweets only")

ggplot(all_tweets %>% filter(is_regular_tweet | is_reply) %>% select(relative_date, starts_with("sentiment")) %>% 
         pivot_longer(-relative_date) %>% 
         mutate(relative_date = as.numeric(relative_date),
                name = str_replace(name, "sentiment_", ""),
                period = case_when(relative_date <= 0 ~ "pre",
                                   relative_date > 7 ~ "post",
                                   TRUE ~ "during")), aes(relative_date, value, group = period)) + 
  annotate("rect", xmin = 0, xmax = 7, ymin = -Inf, ymax = Inf, fill = "#EEDDAA99") +
  geom_smooth(method = 'lm') + 
  facet_wrap(~ name, scales = "free_y") + 
  ggtitle("Sentiment", "regular tweets or replies only")



ggplot(all_tweets %>% select(relative_date, favorite_count, retweet_count, quote_count, reply_count, quoted_favorite_count, quoted_retweet_count, retweet_favorite_count, display_text_width) %>% 
         pivot_longer(-relative_date) %>% 
         mutate(relative_date = as.numeric(relative_date),
                value = if_else(as.numeric(value) > 0, 1, 0),
                name = str_replace(name, "_count", ""),
                name = str_replace(name, "display_text_width", "has_text"),
                period = case_when(relative_date <= 0 ~ "pre",
                                   relative_date > 7 ~ "post",
                                   TRUE ~ "during")), aes(relative_date, value, group = period)) + 
  annotate("rect", xmin = 0, xmax = 7, ymin = -Inf, ymax = Inf, fill = "#EEDDAA99") +
  geom_smooth(method = 'glm', method.args = list(family = 'binomial')) + 
  facet_wrap(~ name, scales = "free_y") +
  ggtitle("Probability of non-zero values")

ggplot(all_tweets %>% select(relative_date, favorite_count, retweet_count, quote_count, reply_count, quoted_favorite_count, quoted_retweet_count, retweet_favorite_count, display_text_width) %>% 
         pivot_longer(-relative_date) %>% 
         mutate(relative_date = as.numeric(relative_date),
                value = if_else(as.numeric(value) > 0, log(value), NA_real_),
                name = str_replace(name, "display_text_width", "character_count"),
                period = case_when(relative_date <= 0 ~ "pre",
                                   relative_date > 7 ~ "post",
                                   TRUE ~ "during")), aes(relative_date, value, group = period)) + 
  annotate("rect", xmin = 0, xmax = 7, ymin = -Inf, ymax = Inf, fill = "#EEDDAA99") +
  geom_smooth(method = 'lm') + 
  scale_y_continuous(labels = function(x) { round(exp(x))}) + 
  facet_wrap(~ name, scales = "free_y") +
  ggtitle("Geometric means of non-zero values") +
  theme_bw()

ggplot(all_tweets %>% filter(is_regular_tweet)  %>% select(relative_date, favorite_count, retweet_count, quote_count, reply_count, display_text_width) %>% 
         pivot_longer(-relative_date) %>% 
         mutate(relative_date = as.numeric(relative_date),
                value = if_else(as.numeric(value) > 0, log(value), NA_real_),
                name = str_replace(name, "display_text_width", "character_count"),
                period = case_when(relative_date <= 0 ~ "pre",
                                   relative_date > 7 ~ "post",
                                   TRUE ~ "during")), aes(relative_date, value, group = period)) + 
  annotate("rect", xmin = 0, xmax = 7, ymin = -Inf, ymax = Inf, fill = "#EEDDAA99") +
  geom_smooth(method = 'lm') + 
  scale_y_continuous(labels = function(x) { round(exp(x))}) + 
  facet_wrap(~ name, scales = "free_y") +
  ggtitle("Geometric means of non-zero values", "regular tweets only") +
  theme_bw()




ggplot(all_tweets %>% select(relative_date, favorite_count, retweet_count, quote_count, reply_count, quoted_favorite_count, quoted_retweet_count, retweet_favorite_count, display_text_width) %>% 
         pivot_longer(-relative_date) %>% 
         mutate(relative_date = as.numeric(relative_date),
                value = sqrt(as.numeric(value)),
                name = str_replace(name, "display_text_width", "character_count"),
                period = case_when(relative_date <= 0 ~ "pre",
                                   relative_date > 7 ~ "post",
                                   TRUE ~ "during")) %>% 
         drop_na(), aes(relative_date, value, group = period)) + 
  annotate("rect", xmin = 0, xmax = 7, ymin = -Inf, ymax = Inf, fill = "#EEDDAA99") +
  geom_smooth(method = 'lm') + 
  scale_y_continuous(labels = function(x) { round(x^2) }) +
  geom_vline(xintercept = 0, linetype = 'dashed') +
  geom_vline(xintercept = 7, linetype = 'dashed') +
  facet_wrap(~ name, scales = "free_y")


full <- readRDS("full_dataset_nonpublic.rds")
characters <- full$text %>%  str_split(pattern = "") %>% unlist()
characters <- tibble(char = characters) %>% anti_join(tibble(char = c(0:9, letters, str_to_upper(letters))))
characters$char %>% table() %>% sort() %>% tail(100)

