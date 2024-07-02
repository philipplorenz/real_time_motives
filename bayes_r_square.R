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

all_tweets <- readRDS("public_dataset.rds")
tweets_motives <- all_tweets[!is.na(all_tweets$motive_bringattention), ]
motive_columns <- select(tweets_motives, contains("motive_") & !contains("retweet_"))
tweets_motives <- tweets_motives %>% mutate(top_motive = names(motive_columns)[max.col(motive_columns)])

# join pre- and mini-surveys
motives_with_pre <- left_join(tweets_motives, tweet_pre_df, by = "session")
tweets <- left_join(all_tweets, tweet_pre_df, by = "session")
tweets$user <- tweets$session
tweets <- tweets %>% 
  rename(birtyr_num = birthyr, followers_count_num = followers_count, gender_bin = gender, education_num = education, employ_bin = employ, pid3_bin = pid3, is_retweet_bin = is_retweet, 
         is_quote_bin = is_quote, is_reply_bin = is_reply, sentiment_negative_num = sentiment_negative, sentiment_positive_num = sentiment_positive, retweet_count_num = retweet_count) %>% 
  mutate(tweet_type =
           case_when(is_retweet_bin == "TRUE" ~ "retweet",
                     is_reply_bin == "TRUE" ~ "reply",
                     is_quote_bin == "TRUE" ~ "quote",
                     TRUE ~ "original"))
tweets <- tweets %>% mutate_all(type.convert)

tweets$retweets <- tweets$retweet_count_num
tweets$tweet_id <- 1:nrow(tweets)
tweets$log_followers <- log(as.numeric(tweets$followers_count_num))
tweets$sqrt_followers <- sqrt(as.numeric(tweets$followers_count_num))
# binarize agreement ratings
#motives_with_pre_binarized <- binarizer(motives_with_pre, contains("motive_"), thresh = 3)


#################################################################################################

#' **TODO: Do we need to use a sum contrast coding for the categorical variables**

################################################################################
# real data

# TODO create new features that summarize some of those
mydata <- motives_with_pre %>% select(session, contains("motive_"), birthyr, followers_count, gender, education, employ, pid3, is_retweet, is_quote, is_reply, sentiment_negative, sentiment_positive, retweet_count, original_tweet_retweets_then)
mydata <- mydata %>% rename(birtyr_num = birthyr, followers_count_num = followers_count, gender_bin = gender, education_num = education, employ_bin = employ, pid3_bin = pid3, is_retweet_bin = is_retweet, 
                            is_quote_bin = is_quote, is_reply_bin = is_reply, sentiment_negative_num = sentiment_negative, sentiment_positive_num = sentiment_positive, retweet_count_num = retweet_count)

mydata <- mydata %>%
  mutate_if(grepl("motive_", names(.)), as.numeric)

mydata <- mydata %>%
  mutate_if(grepl("_num", names(.)), as.numeric)
mydata$age_num <- 2022 - mydata$birtyr_num

mydata <- mydata %>%
  mutate_if(grepl("_bin", names(.)), as.factor)

mydata <- drop_na(mydata)

### a model to predict RTs
mydata$user <- mydata$session
mydata <- mydata %>% 
  mutate(tweet_type =
           case_when(is_retweet_bin == "TRUE" ~ "retweet",
                     is_reply_bin == "TRUE" ~ "reply",
                     is_quote_bin == "TRUE" ~ "quote",
                     TRUE ~ "original"))
rts <- mydata %>% filter(tweet_type == "retweet")
min(rts$retweet_count_num)
min(as.numeric(rts$original_tweet_retweets_then))
cor.test(rts$retweet_count_num, as.numeric(rts$original_tweet_retweets_then))
rts %>% filter(
  retweet_count_num < as.numeric(original_tweet_retweets_then)
) %>% 
  select(retweet_count_num, original_tweet_retweets_then)
table(rts$retweet_count_num < as.numeric(rts$original_tweet_retweets_then))
mydata$retweets <- mydata$retweet_count_num #  - mydata$original_tweet_retweets_then # doesn't work, sometimes retweet_count_num is higher than original_tweet_retweets_then

table(mydata$tweet_type)
mydata_mot <- mydata %>% rename_with(~ str_sub(., 8), starts_with("motive_")) 
# names(mydata_mot) %>% str_c(collapse = ", ")
# the log of followers currently introduces -inf and doesn't work
mydata_mot$log_followers <- log(mydata$followers_count_num)
mydata_mot$sqrt_followers <- sqrt(mydata$followers_count_num)


mydata_mot <-  mydata_mot %>% 
  mutate(expression_identity = (expressopinion + showemotions + showattitude)/3, 
         informing_persuasion = (provepoint + bringattention + influence + informothers)/4,
         provokation = (provoke + causechaos + deceiveothers)/3,
         socializing_attention = (entertain + savecontent + showachievement + gainattention + surpriseothers + connectwothers)/6
  )

#motive_var <- brm(entertain ~ (1 | user), mydata_mot)
#motive_var_demo <- brm(entertain ~ tweet_type + age_num + pid3_bin + employ_bin + gender_bin + sqrt_followers + education_num + (1 | user), mydata_mot)



motive_models <- list(
  entertain = brm(entertain ~ (1|tweet_type) + age_num + pid3_bin + employ_bin + gender_bin + education_num + (1 | user), mydata_mot, control = list(adapt_delta = 0.99)),
  expressopinion = brm(expressopinion ~ (1|tweet_type) + age_num + pid3_bin + employ_bin + gender_bin + education_num + (1 | user), mydata_mot, control = list(adapt_delta = 0.99)),
  provoke = brm(provoke ~ (1|tweet_type) + age_num + pid3_bin + employ_bin + gender_bin + education_num + (1 | user), mydata_mot, control = list(adapt_delta = 0.99)),
  savecontent = brm(savecontent ~ (1|tweet_type) + age_num + pid3_bin + employ_bin + gender_bin + education_num + (1 | user), mydata_mot, control = list(adapt_delta = 0.99)),
  showemotions = brm(showemotions ~ (1|tweet_type) + age_num + pid3_bin + employ_bin + gender_bin + education_num + (1 | user), mydata_mot, control = list(adapt_delta = 0.99)),
  connectwothers = brm(connectwothers ~ (1|tweet_type) + age_num + pid3_bin + employ_bin + gender_bin + education_num + (1 | user), mydata_mot, control = list(adapt_delta = 0.99)),
  showachievement = brm(showachievement ~ (1|tweet_type) + age_num + pid3_bin + employ_bin + gender_bin + education_num + (1 | user), mydata_mot, control = list(adapt_delta = 0.99)),
  showattitude = brm(showattitude ~ (1|tweet_type) + age_num + pid3_bin + employ_bin + gender_bin + education_num + (1 | user), mydata_mot, control = list(adapt_delta = 0.99)),
  deceiveothers = brm(deceiveothers ~ (1|tweet_type) + age_num + pid3_bin + employ_bin + gender_bin + education_num + (1 | user), mydata_mot, control = list(adapt_delta = 0.99)),
  gainattention = brm(gainattention ~ (1|tweet_type) + age_num + pid3_bin + employ_bin + gender_bin + education_num + (1 | user), mydata_mot, control = list(adapt_delta = 0.99)),
  causechaos = brm(causechaos ~ (1|tweet_type) + age_num + pid3_bin + employ_bin + gender_bin + education_num + (1 | user), mydata_mot, control = list(adapt_delta = 0.99)),
  bringattention = brm(bringattention ~ (1|tweet_type) + age_num + pid3_bin + employ_bin + gender_bin + education_num + (1 | user), mydata_mot, control = list(adapt_delta = 0.99)),
  influence = brm(influence ~ (1|tweet_type) + age_num + pid3_bin + employ_bin + gender_bin + education_num + (1 | user), mydata_mot, control = list(adapt_delta = 0.99)),
  surpriseothers = brm(surpriseothers ~ (1|tweet_type) + age_num + pid3_bin + employ_bin + gender_bin + education_num + (1 | user), mydata_mot, control = list(adapt_delta = 0.99)),
  informothers = brm(informothers ~ (1|tweet_type) + age_num + pid3_bin + employ_bin + gender_bin + education_num + (1 | user), mydata_mot, control = list(adapt_delta = 0.99)),
  provepoint = brm(provepoint ~ (1|tweet_type) + age_num + pid3_bin + employ_bin + gender_bin + education_num + (1 | user), mydata_mot, control = list(adapt_delta = 0.99))
)


extract_var_components <- function(model) {
  vc <- VarCorr(model)
  vcs <- vc %>% map(~ as_tibble(.$sd ^ 2)) %>% bind_rows(.id ="param") %>% 
    column_to_rownames("param")
  br2 <- bayes_R2(model, 
                  re_formula = NA)[,"Estimate"]
  vcs$Estimate <- vcs$Estimate/sum(vcs$Estimate,na.rm=T)*(1-br2)
  tibble(within_subject_residual = 100*vcs["residual", "Estimate"],
         individual_user = 100*vcs["user", "Estimate"],
         tweet_type = 100*vcs["tweet_type", "Estimate"],
         demographics = 100*br2)
}

var_comps <- motive_models %>% map(extract_var_components) %>% bind_rows(.id = "Motive") 

ggplot(var_comps %>% pivot_longer(-Motive) %>% 
         mutate(name = factor(name, rev(c("demographics", "individual_user", "tweet_type", "within_subject_residual")))), aes(y = Motive, x = value, fill = name)) +
  geom_bar(stat = "identity") +
  scale_y_discrete(limits = rev(c('expressopinion', 'showemotions', 'showattitude', 
                                  'bringattention', 'informothers', 'provepoint', 'influence', 
                                  'connectwothers', 'entertain', 'savecontent', 'gainattention', 'surpriseothers', 'showachievement', 
                                  'provoke', 'causechaos', 'deceiveothers')), 
                   labels = rev(c('express opinion', 'show emotions', 'show attitude', 
                                  'bring attention', 'inform others', 'prove a point', 'influence others', 
                                  'connect with others', 'entertain', 'save content', 'gain attention', 'surprise others', 'show achievement', 
                                  'provoke', 'cause chaos', 'deceive others'))) +
  scale_fill_manual(values = c("demographics" = "#3e818e","individual_user" = "#006682", "tweet_type" = "lightblue", "within_subject_residual" = "#996f00")) +
  labs(title = "",
       x = "Relative Variance Explained [%]",
       y = "") +
  theme(legend.position = "none") +
  theme_minimal()
  #theme_bw()

