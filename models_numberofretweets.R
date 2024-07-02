#' ---
#' title: Modeling Twitter sharing motivations
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
options(mc.cores = 4, 
        brms.backend = "cmdstanr",
        brms.file_refit = "on_change")
# install.packages("cmdstanr", repos = c("https://mc-stan.org/r-packages/", getOption("repos")))


#################################################################################################
# load data

tweet_pre_df <- readRDS("pre_survey.rds")
tweet_pre_df <- tweet_pre_df[!(is.na(tweet_pre_df$session) | tweet_pre_df$session==""), ]
tweet_pre_df$created <- as.Date(tweet_pre_df$created)
tweet_pre_df <- filter(tweet_pre_df, created >= "2022-02-18")
tweet_pre_df %>% group_by(session) %>% filter(n()>1)
tweet_pre_df <- tweet_pre_df %>% group_by(session) %>% filter(row_number() == 1) %>% ungroup()
length(unique(tweet_pre_df$session))

all_tweets <- readRDS("public_dataset.rds")
length(unique(all_tweets$session))
tweets_motives <- all_tweets[!is.na(all_tweets$motive_bringattention), ]
#motive_columns <- select(tweets_motives, contains("motive_") & !contains("retweet_"))
#tweets_motives <- tweets_motives %>% mutate(top_motive = names(motive_columns)[max.col(motive_columns)])
length(unique(tweets_motives$session))

# join pre- and mini-surveys
motives_with_pre <- left_join(tweets_motives, tweet_pre_df, by = "session")
length(unique(tweets_motives$session))

tweets <- left_join(all_tweets, tweet_pre_df, by = "session")
length(unique(tweets$session))
tweets$user <- tweets$session
tweets <- tweets %>% 
  rename(birtyr_num = birthyr, followers_count_num = followers_count, gender_bin = gender, education_num = education, employ_bin = employ, pid3_bin = pid3, is_retweet_bin = is_retweet, 
         is_quote_bin = is_quote, is_reply_bin = is_reply, sentiment_bin = sentiment, emotion_bin = emotion, retweet_count_num = retweet_count) %>% 
  mutate(tweet_type =
           case_when(is_retweet_bin == "TRUE" ~ "retweet",
                     is_reply_bin == "TRUE" ~ "reply",
                     is_quote_bin == "TRUE" ~ "quote",
                     TRUE ~ "original"))
tweets <- tweets %>% mutate_all(type.convert)

tweets$retweets <- tweets$retweet_count_num
tweets$log_retweets <- log(as.numeric(tweets$retweet_count_num+1))
tweets$tweet_id <- 1:nrow(tweets)
tweets$log_followers <- log(as.numeric(tweets$followers_count_num+1))
tweets$sqrt_followers <- sqrt(as.numeric(tweets$followers_count_num))

### a model to predict RTs
tweets$user <- tweets$session
tweets <- tweets %>% 
  mutate(tweet_type =
           case_when(is_retweet_bin == "TRUE" ~ "retweet",
                     is_reply_bin == "TRUE" ~ "reply",
                     is_quote_bin == "TRUE" ~ "quote",
                     TRUE ~ "original"))

tweets <- tweets %>% 
  mutate(is_original =
           case_when(tweet_type == "original" ~ "TRUE",
                     TRUE ~ "FALSE"))

tweets <- tweets %>% 
  mutate(gender =
           case_when(gender_bin != 1 & gender_bin != 2 ~ "other",
                     gender_bin == 1 ~ "male",
                     gender_bin == 2 ~ "female",
                     TRUE ~ "other"))

tweets <- tweets %>% 
  mutate(is_popular =
           case_when(retweet_count_num > 0 ~ "TRUE",
                     TRUE ~ "FALSE"))

tweets <- tweets %>% 
  mutate(education =
           case_when(education_num > 3 ~ "college or higher",
                     education_num <= 3 ~ "no college",
                     TRUE ~ "no college"))


tweets <- tweets %>% 
  mutate(employment =
           case_when(as.numeric(employ_bin) < 3 ~ "full- or part-time employed",
                     as.numeric(employ_bin) >= 3 ~ "not employed",
                     TRUE ~ "not employed"))

tweets <- tweets %>% 
  mutate(political =
           case_when(pid3_bin == 3 ~ "independent",
                     pid3_bin == 2 ~ "republican",
                     pid3_bin == 1 ~ "democrat",
                     TRUE ~ "other"))

length(unique(tweets$session))
# binarize agreement ratings
#motives_with_pre_binarized <- binarizer(motives_with_pre, contains("motive_"), thresh = 3)

################################################################################

tweets <- tweets %>% rename_with(~ str_sub(., 8), starts_with("motive_")) 
tweets <- tweets %>%
  mutate_if(grepl("_num", names(.)), as.numeric)
tweets$age_num <- 2022 - tweets$birtyr_num

tweets <- tweets %>%
  mutate(category1 = recode(category1, "na" = NA_character_,
                            "arts" = "art"))
top10 <- table(tweets$category1) %>% sort() %>% tail(5) %>% names()
tweets$top10_categories <- if_else(tweets$category1 %in% top10, tweets$category1, "other")
tweets$c_ <- relevel(factor(tweets$top10_categories %>% str_replace_all(" ", "_")), "other")
dummies <- model.matrix(~ c_, tweets) %>% as.data.frame()
dummies$tweet_id <- tweets$tweet_id
tweets <- tweets %>% full_join(dummies, by = "tweet_id")
names(dummies) %>% str_c(collapse = " + ")


tweets <- tweets %>%
  mutate(relative_date = as.numeric(relative_date),
         period = case_when(relative_date <= 0 ~ "pre",
                            relative_date > 7 ~ "post",
                            TRUE ~ "during"))

tweets <- tweets %>% 
  mutate(is_political =
           case_when(c_politics == 1 ~ "TRUE",
                     TRUE ~ "FALSE"))

tweets <- tweets %>% 
  mutate(outrage_bin =
           case_when(outrage == 1 ~ "TRUE",
                     TRUE ~ "FALSE"))

tweets <- tweets %>%
  mutate(sentiment_bin = case_when(sentiment_bin == "sentiment: negative" ~ "negative",
                                   sentiment_bin == "negative" ~ "negative",
                                   sentiment_bin == "positive" ~ "positive"
  ))


################################################################################
# how much variance of a motive can be explained by the user and how much by demographics
# do this also for tweet features?
all_tweets <- tweets

tweets <- tweets %>% filter(!is.na(expressopinion))
org_tweets <- tweets %>% filter(tweet_type != "retweet")
org_tweets <- org_tweets %>% select(retweets, expressopinion, informothers, bringattention, connectwothers, showemotions, showattitude, entertain,
                                influence, provepoint, gainattention, savecontent, surpriseothers, provoke, showachievement, causechaos, deceiveothers,
                                user, age_num, political, log_followers, employment, gender, sqrt_followers, education, tweet_type, period,
                                c_entertainment, c_politics, c_social, c_sports, c_technology, sentiment_bin, retweet_count_num, user, tweet_type, is_popular, is_original, is_reply_bin, is_quote_bin, outrage_bin, log_retweets,
                                is_political, sentiment_bin)

org_tweets <- org_tweets[complete.cases(org_tweets), ]
org_tweets$retweets <- org_tweets$retweet_count_num
################################################################################
# hurdle lognormal model to predict retweets when controling for followers
# predict from tweet type and from motives

org_tweets <- org_tweets %>% mutate(retweets = retweets - if_else(tweet_type == "retweet", 1, 0))
# hurdle log normal because RTs can be understood as a two-step process. 
# a) factors that govern whether it will be RTed at all
# b) factors that govern how often it will be RTed
# its distribution is exponential because RTs are multiplicative

m1 <- brm(
  bf(retweets ~ 
       tweet_type + sqrt_followers + (1 | user),
     hu ~ 
       tweet_type + sqrt_followers + (1 | user)),
  data = org_tweets,
  family = hurdle_lognormal(),
  seed = 12345,
  file = "predict_rt_from_tweet_type"
)
loo(m1)
################################################################################
# marginal effects

m2_simple <- brm(
  bf(retweets ~ 
       entertain + expressopinion + provoke + savecontent + showemotions + connectwothers + showachievement + showattitude + deceiveothers + gainattention + provepoint + causechaos + bringattention + influence + surpriseothers + informothers +
       (1 | user),
     hu ~ 
       entertain + expressopinion + provoke + savecontent + showemotions + connectwothers + showachievement + showattitude + deceiveothers + gainattention + provepoint + causechaos + bringattention + influence + surpriseothers + informothers +
       (1 | user)),
  data = org_tweets,
  family = hurdle_lognormal(),
  seed = 12345,
  file = "predict_rt_from_tweet_type_and_motive"
)



m2_simple
loo(m2_simple)
LOO(m1, m2_simple)
#marginaleffects::avg_comparisons(m2, dpar = "hu", type = "response")

# einfach type = "response" reicht, ohne dpar="hu"/"mu"
avg_comp_df <- as.data.frame(marginaleffects::avg_comparisons(m2_simple, type = "response", ndraws = 100))
avg_comp_df$label <- paste(avg_comp_df$term, avg_comp_df$contrast, sep = "-")
# plot

avg_comp_df$weighted_distance <- with(avg_comp_df, 
                                             abs(estimate) / (conf.high - conf.low))

ggplot(avg_comp_df, aes(x = estimate, y = label)) +
  geom_point(alpha = 0.3) +
  geom_point(aes(color = weighted_distance)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, color = weighted_distance, height =0)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_color_gradient(low = "lightgrey", high = "black") +  
  scale_y_discrete(limits = rev(c("expressopinion-+1", "informothers-+1", "bringattention-+1", "connectwothers-+1", "showemotions-+1",
                                  "showattitude-+1", "entertain-+1", "influence-+1", "provepoint-+1", "gainattention-+1", "savecontent-+1", "surpriseothers-+1", "provoke-+1", 
                                  "showachievement-+1", "causechaos-+1", "deceiveothers-+1"))) +
  #geom_text(aes(label = label), vjust = -1.2, size = 3, color = "black") +  
  #labs(x = "Average Comparison Estimate") +
  theme_classic() +
  theme(#axis.text.y=element_blank(),
    legend.position = "none",
    axis.title.x=element_blank(),
    axis.title.y=element_blank())


ggplot(avg_comp_df, aes(x = estimate, y = label)) +
  geom_point(alpha = 0.3) +
  geom_point(data = avg_comp_df %>% filter(conf.low > 0 | conf.high < 0)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0., alpha = 0.3) +
  geom_errorbarh(data = avg_comp_df %>% filter(conf.low > 0 | conf.high < 0), aes(xmin = conf.low, xmax = conf.high), height = 0.) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_y_discrete(limits = rev(c("expressopinion-+1", "informothers-+1", "bringattention-+1", "connectwothers-+1", "showemotions-+1",
                                  "showattitude-+1", "entertain-+1", "influence-+1", "provepoint-+1", "gainattention-+1", "savecontent-+1", "surpriseothers-+1", "provoke-+1", 
                                  "showachievement-+1", "causechaos-+1", "deceiveothers-+1"))) +
  #geom_text(aes(label = label), vjust = -1.2, size = 3, color = "black") +  
  #labs(x = "Average Comparison Estimate") +
  theme_classic() +
  theme(#axis.text.y=element_blank(),
    axis.title.x=element_blank(),
    axis.title.y=element_blank())
  



avg_comp_df <- as.data.frame(marginaleffects::avg_comparisons(m2_simple, dpar = "hu", type = "link"))
avg_comp_df$label <- paste(avg_comp_df$term, avg_comp_df$contrast, sep = "-")
# plot
ggplot(avg_comp_df, aes(x = estimate, y = label)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_y_discrete(limits = rev(c("expressopinion-+1", "informothers-+1", "bringattention-+1", "connectwothers-+1", "showemotions-+1",
                                  "showattitude-+1", "entertain-+1", "influence-+1", "provepoint-+1", "gainattention-+1", "savecontent-+1", "surpriseothers-+1", "provoke-+1", 
                                  "showachievement-+1", "causechaos-+1", "deceiveothers-+1"))) +
  #geom_text(aes(label = label), vjust = -1.2, size = 3, color = "black") +  
  #labs(x = "Average Comparison Estimate") +
  theme_minimal() +
  ggtitle("hurdle model") 


#marginaleffects::avg_comparisons(m2, dpar = "hu", type = "response")
avg_comp_df <- as.data.frame(marginaleffects::avg_comparisons(m2_simple, type = "link"))
avg_comp_df$label <- paste(avg_comp_df$term, avg_comp_df$contrast, sep = "-")
# plot
ggplot(avg_comp_df, aes(x = estimate, y = label)) +
  geom_point() +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_y_discrete(limits = rev(c("expressopinion-+1", "informothers-+1", "bringattention-+1", "connectwothers-+1", "showemotions-+1",
                                  "showattitude-+1", "entertain-+1", "influence-+1", "provepoint-+1", "gainattention-+1", "savecontent-+1", "surpriseothers-+1", "provoke-+1", 
                                  "showachievement-+1", "causechaos-+1", "deceiveothers-+1"))) +
  #geom_text(aes(label = label), vjust = -1.2, size = 3, color = "black") +  
  #labs(x = "Average Comparison Estimate") +
  theme_minimal() +
  ggtitle("retweet model") 

library(marginaleffects)
marginaleffects::avg_comparisons(m1, type = "link")
marginaleffects::avg_comparisons(m1, type = "response")
marginaleffects::plot_predictions(m1, condition = "tweet_type", type = "link")
marginaleffects::plot_predictions(m1, condition = "tweet_type", type = "response")
 
m2 <- brm(
  bf(retweets ~ 
       entertain + expressopinion + provoke + savecontent + showemotions + connectwothers + showachievement + showattitude + deceiveothers + gainattention + provepoint + causechaos + bringattention + influence + surpriseothers + informothers +
       log_followers + (1 | user),
     hu ~ 
       entertain + expressopinion + provoke + savecontent + showemotions + connectwothers + showachievement + showattitude + deceiveothers + gainattention + provepoint + causechaos + bringattention + influence + surpriseothers + informothers +
       log_followers + (1 | user)),
  data = org_tweets,
  family = hurdle_lognormal(),
  seed = 12345,
  file = "predict_rt_from_tweet_type_and_motive"
)

m3 <- brm(
  bf(retweets ~ 
       entertain + expressopinion + provoke + savecontent + showemotions + connectwothers + showachievement + showattitude + deceiveothers + gainattention + provepoint + causechaos + bringattention + influence + surpriseothers + informothers +
       tweet_type + log_followers + (1 | user),
     hu ~ 
       entertain + expressopinion + provoke + savecontent + showemotions + connectwothers + showachievement + showattitude + deceiveothers + gainattention + provepoint + causechaos + bringattention + influence + surpriseothers + informothers +
       tweet_type + log_followers + (1 | user)),
  data = org_tweets,
  family = hurdle_lognormal(),
  seed = 12345,
  file = "predict_rt_from_tweet_type_and_motive"
)

loo(m2)
LOO(m1, m2)
LOO(m1, m2_simple, m2)
LOO(m1, m2_simple, m2, m3)

################################################################################
# predicting sucess from content

org_tweets <- all_tweets %>% filter(tweet_type != "retweet")
org_tweets <- org_tweets %>% select(retweets, 
                                    user, age_num, political, log_followers, employment, gender, sqrt_followers, education, tweet_type, period,
                                    c_entertainment, c_politics, c_social, c_sports, c_technology, sentiment_bin, retweet_count_num, user, tweet_type, is_popular, is_original, is_reply_bin, is_quote_bin, outrage_bin, log_retweets,
                                    is_political, sentiment_bin)

org_tweets <- org_tweets[complete.cases(org_tweets), ]
org_tweets$retweets <- org_tweets$retweet_count_num

m4 <- brm(
  bf(retweets ~ 
       outrage_bin + is_political + sentiment_bin +
       tweet_type + log_followers + (1 | user),
     hu ~ 
       outrage_bin + is_political + sentiment_bin +
       tweet_type + log_followers + (1 | user)),
  data = org_tweets,
  family = hurdle_lognormal(),
  seed = 12345,
  file = "predict_rt_from_tweet_type_and_motive"
)


#LOO(m1, m2_simple, m2, m3, m4)

avg_comp_df <- as.data.frame(marginaleffects::avg_comparisons(m4, type = "response", ndraws = 100))
avg_comp_df$label <- paste(avg_comp_df$term, avg_comp_df$contrast, sep = "-")
# plot
avg_comp_df$weighted_distance <- with(avg_comp_df, 
                                      abs(estimate) )

max_weighted_measure <- max(avg_comp_df$weighted_distance, na.rm = TRUE)
min_weighted_measure <- min(avg_comp_df$weighted_distance, na.rm = TRUE)

avg_comp_df$weighted_distance <- (avg_comp_df$weighted_distance - min_weighted_measure) / (max_weighted_measure - min_weighted_measure)


ggplot(avg_comp_df, aes(x = estimate, y = label)) +
  geom_point(alpha = 0.3) +
  geom_point(aes(color = weighted_distance)) +
  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, color = weighted_distance, height =0)) +
  geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  scale_color_gradient(low = "grey", high = "black") +  # Use your preferred colors
  #geom_text(aes(label = label), vjust = -1.2, size = 3, color = "black") +  
  #labs(x = "Average Comparison Estimate") +
  theme_classic() +
  theme(#axis.text.y=element_blank(),
    legend.position = "none",
    axis.title.x=element_blank(),
    axis.title.y=element_blank())

