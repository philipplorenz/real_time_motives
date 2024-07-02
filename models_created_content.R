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
  mutate(is_entertainment =
           case_when(c_entertainment == 1 ~ "TRUE",
                     TRUE ~ "FALSE"))

tweets <- tweets %>% 
  mutate(outrage_bin =
           case_when(outrage == 1 ~ "TRUE",
                     TRUE ~ "FALSE"))

tweets <- tweets %>%
  mutate(
    emotion_raw = emotion_bin,
    emotion_bin = str_replace(emotion_raw, "most pronounced sentiment:", ""),
    emotion_bin = str_replace(emotion_bin, "pronounced sentiment:", ""),
    emotion_bin = str_replace(emotion_bin, "distinguishing sentiment:", ""),
    emotion_bin = str_extract(emotion_bin, "[a-z]+"),
    emotion_bin = recode(emotion_bin, "na" = NA_character_,
                     "na." = NA_character_),
  )
top_cats <-  table(tweets$emotion_bin, exclude = NULL) %>% sort %>% tail(5) %>% names()

tweets <- tweets %>% 
  mutate(emotion_top =
           case_when(emotion_bin == "anger" ~ "anger",
                     emotion_bin == "joy" ~ "joy",
                     emotion_bin == "disgust" ~ "disgust",
                     TRUE ~ "NA"))

tweets <- tweets %>% 
  mutate(is_anger =
           case_when(emotion_bin == "anger" ~ "TRUE",
                     TRUE ~ "FALSE"))

tweets <- tweets %>% 
  mutate(is_joy =
           case_when(emotion_bin == "joy" ~ "TRUE",
                     TRUE ~ "FALSE"))

tweets <- tweets %>% 
  mutate(is_disgust =
           case_when(emotion_bin == "disgust" ~ "TRUE",
                     TRUE ~ "FALSE"))

tweets <- tweets %>% 
  mutate(is_disgust_or_anger =
           case_when(emotion_bin == "disgust" | emotion_bin == "anger" ~ "TRUE",
                     TRUE ~ "FALSE"))


tweets <- tweets %>% 
  mutate(expression_identity = (expressopinion + showemotions + showattitude)/3, 
         informing_persuasion = (provepoint + bringattention + influence + informothers)/4,
         provocation = (provoke + causechaos + deceiveothers)/3,
         socializing_attention = (entertain + savecontent + showachievement + gainattention + surpriseothers + connectwothers)/6
  )


tweets <- tweets %>% 
  mutate(expressopinion_bin = 
           case_when(expressopinion > 3 ~ "TRUE",
                     TRUE ~ "FALSE"),
         informothers_bin = 
           case_when(informothers > 3 ~ "TRUE",
                     TRUE ~ "FALSE"),
         bringattention_bin = 
           case_when(bringattention > 3 ~ "TRUE",
                     TRUE ~ "FALSE"),
         connectwothers_bin = 
           case_when(connectwothers > 3 ~ "TRUE",
                     TRUE ~ "FALSE"),
         showemotions_bin = 
           case_when(showemotions > 3 ~ "TRUE",
                     TRUE ~ "FALSE"),
         showattitude_bin = 
           case_when(showattitude > 3 ~ "TRUE",
                     TRUE ~ "FALSE"),
         entertain_bin = 
           case_when(entertain > 3 ~ "TRUE",
                     TRUE ~ "FALSE"),
         influence_bin = 
           case_when(influence > 3 ~ "TRUE",
                     TRUE ~ "FALSE"),
         provepoint_bin = 
           case_when(provepoint > 3 ~ "TRUE",
                     TRUE ~ "FALSE"),
         gainattention_bin = 
           case_when(gainattention > 3 ~ "TRUE",
                     TRUE ~ "FALSE"),
         savecontent_bin = 
           case_when(savecontent > 3 ~ "TRUE",
                     TRUE ~ "FALSE"),
         surpriseothers_bin = 
           case_when(surpriseothers > 3 ~ "TRUE",
                     TRUE ~ "FALSE"),
         provoke_bin = 
           case_when(provoke > 3 ~ "TRUE",
                     TRUE ~ "FALSE"),
         showachievement_bin = 
           case_when(expressopinion > 3 ~ "TRUE",
                     TRUE ~ "FALSE"),
         causechaos_bin = 
           case_when(causechaos > 3 ~ "TRUE",
                     TRUE ~ "FALSE"),
         deceiveothers_bin = 
           case_when(deceiveothers > 3 ~ "TRUE",
                     TRUE ~ "FALSE")
         )


tweets <- tweets %>%
  mutate(sentiment_bin = case_when(sentiment_bin == "sentiment: negative" ~ "negative",
                                   sentiment_bin == "negative" ~ "negative",
                                   sentiment_bin == "positive" ~ "positive"
                                   ))


tweets <- tweets %>% 
  mutate(has_url =
           case_when(url_category == "Mainstream News" | url_category == "Alternative News" | url_category == "Music" | url_category == "Video" | url_category == "Entertainment" | url_category == "Social media" ~ TRUE,
                     TRUE ~ FALSE))


################################################################################
# how much variance of a motive can be explained by the user and how much by demographics
# do this also for tweet features?

tweets <- tweets %>% filter(!is.na(expressopinion))
retweets <- tweets %>% filter(tweet_type != "retweet")
retweets <- retweets %>% select(expression_identity, informing_persuasion, socializing_attention, provocation, 
                                expressopinion, informothers, bringattention, connectwothers, showemotions, showattitude, entertain,
                                influence, provepoint, gainattention, savecontent, surpriseothers, provoke, showachievement, causechaos, deceiveothers,
                                user, tweet_type, is_popular, is_entertainment, is_original, is_reply_bin, is_quote_bin, outrage_bin, log_retweets,
                                is_political, sentiment_bin, emotion_bin, is_disgust, is_joy, is_anger, has_url)

retweets <- retweets[complete.cases(retweets), ]

#motive_var <- brm(cbind(expressopinion, informothers, bringattention, connectwothers, showemotions, showattitude, entertain, influence, provepoint, gainattention, savecontent, surpriseothers, provoke, showachievement, causechaos, deceiveothers) ~ (1 | user), mydata_mot)

dependent_variables <-  c("is_political", "is_entertainment", "outrage_bin", "sentiment_bin", "is_disgust", "is_anger", "is_joy", "has_url")
#dependent_variables <-  c("is_reply_bin", "is_quote_bin", "is_original", "outrage_bin", "is_political", "sentiment_bin")
#dependent_variables <-  c("expressopinion", "informothers", "bringattention", "connectwothers", "showemotions",
#                          "showattitude", "entertain", "influence", "provepoint", "gainattention", "savecontent", "surpriseothers", "provoke", 
#                          "showachievement", "causechaos", "deceiveothers")

retweets[dependent_variables] <- lapply(retweets[dependent_variables], factor)

# Initialize an empty list to store the results
r2_comparison_list <- list()
model_list <- list()
variance_explained_list <- list()
loo_list <- list()


# Loop through the dependent variables
for (dep_var in dependent_variables) {
  # Fit the models and calculate Bayesian R2 (similar to previous example)
  #model1 <- brm(as.formula(paste0(dep_var, " ~ (1 | user)")), family = bernoulli(link = "logit"), retweets)
  #model2 <- brm(as.formula(paste0(dep_var, " ~ age_num + pid3_bin + employ_bin + gender_bin + sqrt_followers + education_num + (1 | user)")), mydata_mot)
  #model1 <- brm(as.formula(paste0(dep_var, " ~ age_num + political + employment + gender + log_followers + education + (1 | user)")), retweets)
  #model2 <- brm(as.formula(paste0(dep_var, " ~ expressopinion + informothers + bringattention + connectwothers + showemotions + showattitude + entertain + influence + provepoint + gainattention + savecontent + surpriseothers + provoke + showachievement + causechaos + deceiveothers + (1 | user)")), family = bernoulli(link = "logit"), retweets)
  model2 <- brm(as.formula(paste0(dep_var, " ~ expression_identity + informing_persuasion + provocation + socializing_attention + (1 | user)")), family = bernoulli(link = "logit"), retweets)
  #model2 <- brm(as.formula(paste0(dep_var, " ~ expressopinion_bin + informothers_bin + bringattention_bin + connectwothers_bin + showemotions_bin + showattitude_bin + entertain_bin + influence_bin + provepoint_bin + gainattention_bin + savecontent_bin + surpriseothers_bin + provoke_bin + showachievement_bin + causechaos_bin + deceiveothers_bin + (1 | user)")), family = bernoulli(link = "logit"), retweets)
  #model3 <- brm(as.formula(paste0(dep_var, " ~ tweet_type + age_num + pid3_bin + employ_bin + gender_bin + sqrt_followers + education_num + (1 | user)")), mydata_mot)
  
  #r2_model1 <- bayes_R2(model1)
  #r2_model2 <- bayes_R2(model2)
  
  # Perform LOO cross-validation for model 1
  #loo1 <- loo(model1)
  
  # Perform LOO cross-validation for model 2
  loo2 <- loo(model2)
  loo_list[[dep_var]] <- loo2
  
  # Compare the models using LOO
  #loo_compare <- loo_compare(loo1, loo2)

  model_list[[dep_var]] <- model2
  # Append the data frame to the list

}


library(sjPlot)
library(gridExtra)

avg_comp_list <- list()

for (dep_var in dependent_variables) {
  
  print(dep_var)
  avg_comp_df <- as.data.frame(marginaleffects::avg_comparisons(model_list[[dep_var]], type = "link"))
  avg_comp_df$label <- paste(avg_comp_df$term, avg_comp_df$contrast, sep = "-")
  avg_comp_list[[dep_var]] <- avg_comp_df
  
}

library(sjPlot)
library(gridExtra)
p <- list()
avg_comp_list[[dep_var]]
for (dep_var in dependent_variables) {
  
  
  avg_comp_list[[dep_var]]$weighted_distance <- with(avg_comp_list[[dep_var]], 
                                        abs(estimate) / (conf.high - conf.low))
  
  #conditional_effects(model_list[[dep_var]])
  p[[dep_var]] <- ggplot(avg_comp_list[[dep_var]], aes(x = estimate, y = label)) +
    geom_point(alpha = 0.3) +
    geom_point(aes(color = weighted_distance)) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high, color = weighted_distance, height =0)) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    scale_color_gradient(low = "lightgrey", high = "black") +  
    #scale_y_discrete(limits = rev(c("expressopinion-+1", "informothers-+1", "bringattention-+1", "connectwothers-+1", "showemotions-+1",
    #                                "showattitude-+1", "entertain-+1", "influence-+1", "provepoint-+1", "gainattention-+1", "savecontent-+1", "surpriseothers-+1", "provoke-+1", 
    #                                "showachievement-+1", "causechaos-+1", "deceiveothers-+1"))) +
    #geom_text(aes(label = label), vjust = -1.2, size = 3, color = "black") +  
    #labs(x = "Average Comparison Estimate") +
    #scale_x_continuous(limits = c(-0.7, 0.7)) +
    theme_classic() +
    theme(#axis.text.y=element_blank(),
      legend.position = "none",
      axis.title.x=element_blank(),
      axis.title.y=element_blank()) +
    ggtitle(dep_var) 
    
    
    #ggplot(avg_comp_list[[dep_var]], aes(x = estimate, y = label)) +
    #geom_point(alpha = 0.3) +
    #geom_point(data = avg_comp_list[[dep_var]] %>% filter(conf.low > 0 | conf.high < 0), size=3) +
    #geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0., alpha = 0.3) +
    #geom_errorbarh(data = avg_comp_list[[dep_var]] %>% filter(conf.low > 0 | conf.high < 0), aes(xmin = conf.low, xmax = conf.high), height = 0.) +
    #geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    #scale_y_discrete(limits = rev(c("expressopinion-+1", "informothers-+1", "bringattention-+1", "connectwothers-+1", "showemotions-+1",
    #                            "showattitude-+1", "entertain-+1", "influence-+1", "provepoint-+1", "gainattention-+1", "savecontent-+1", "surpriseothers-+1", "provoke-+1", 
    #                            "showachievement-+1", "causechaos-+1", "deceiveothers-+1"))) +
    #scale_x_continuous(limits = c(-0.7, 0.7)) +
    ##geom_text(aes(label = label), vjust = -1.2, size = 3, color = "black") +  
    #labs(x = "Average Comparison Estimate") +
    #theme_minimal() +
    #ggtitle(dep_var) 
  
}

do.call(grid.arrange, c(p, ncol=2))


################################################################################
# reaction motives

retweets <- tweets %>% filter(tweet_type == "quote")
retweets <- retweets %>% select(retweet_motive_makefunof, retweet_motive_addthoughts, retweet_motive_argue, retweet_motive_expressfeelings, 
                                retweet_motive_ridicule, retweet_motive_correct, retweet_motive_support,
                                user, tweet_type, is_popular, is_original, is_reply_bin, is_quote_bin, outrage_bin, log_retweets, following_quoted,
                                is_political, sentiment_bin)

retweets <- retweets[complete.cases(retweets), ]

#motive_var <- brm(cbind(expressopinion, informothers, bringattention, connectwothers, showemotions, showattitude, entertain, influence, provepoint, gainattention, savecontent, surpriseothers, provoke, showachievement, causechaos, deceiveothers) ~ (1 | user), mydata_mot)

dependent_variables <-  c("outrage_bin", "is_political", "sentiment_bin", "following_quoted")
#dependent_variables <-  c("is_reply_bin", "is_quote_bin", "is_original", "outrage_bin", "is_political", "sentiment_bin")
#dependent_variables <-  c("expressopinion", "informothers", "bringattention", "connectwothers", "showemotions",
#                          "showattitude", "entertain", "influence", "provepoint", "gainattention", "savecontent", "surpriseothers", "provoke", 
#                          "showachievement", "causechaos", "deceiveothers")

retweets[dependent_variables] <- lapply(retweets[dependent_variables], factor)

# Initialize an empty list to store the results
r2_comparison_list <- list()
model_list <- list()
variance_explained_list <- list()
loo_list <- list()


# Loop through the dependent variables
for (dep_var in dependent_variables) {
  # Fit the models and calculate Bayesian R2 (similar to previous example)
  #model1 <- brm(as.formula(paste0(dep_var, " ~ (1 | user)")), family = bernoulli(link = "logit"), retweets)
  #model2 <- brm(as.formula(paste0(dep_var, " ~ age_num + pid3_bin + employ_bin + gender_bin + sqrt_followers + education_num + (1 | user)")), mydata_mot)
  #model1 <- brm(as.formula(paste0(dep_var, " ~ age_num + political + employment + gender + log_followers + education + (1 | user)")), retweets)
  model2 <- brm(as.formula(paste0(dep_var, " ~ retweet_motive_makefunof + retweet_motive_addthoughts + retweet_motive_argue + retweet_motive_expressfeelings + retweet_motive_ridicule + retweet_motive_correct + retweet_motive_support + (1 | user)")), family = bernoulli(link = "logit"), retweets)
  #model2 <- brm(as.formula(paste0(dep_var, " ~ expressopinion_bin + informothers_bin + bringattention_bin + connectwothers_bin + showemotions_bin + showattitude_bin + entertain_bin + influence_bin + provepoint_bin + gainattention_bin + savecontent_bin + surpriseothers_bin + provoke_bin + showachievement_bin + causechaos_bin + deceiveothers_bin + (1 | user)")), family = bernoulli(link = "logit"), retweets)
  #model3 <- brm(as.formula(paste0(dep_var, " ~ tweet_type + age_num + pid3_bin + employ_bin + gender_bin + sqrt_followers + education_num + (1 | user)")), mydata_mot)
  
  #r2_model1 <- bayes_R2(model1)
  #r2_model2 <- bayes_R2(model2)
  
  # Perform LOO cross-validation for model 1
  #loo1 <- loo(model1)
  
  # Perform LOO cross-validation for model 2
  loo2 <- loo(model2)
  loo_list[[dep_var]] <- loo2
  
  # Compare the models using LOO
  #loo_compare <- loo_compare(loo1, loo2)
  
  model_list[[dep_var]] <- model2
  # Append the data frame to the list
  
}


# Combine the data frames from the list into a single data frame
combined_comparison <- bind_rows(r2_comparison_list)

# Plot the comparison with error bars for each dependent variable
ggplot(combined_comparison, aes(x = Dependent_Variable, y = var, fill = Model)) +
  geom_bar(stat = "identity", position = "dodge") +
  geom_errorbar(aes(ymin = var-Lower, ymax = var+Upper),
                width = 0.2, color = "black") +
  #lim(0, 1) +
  labs(x = "Model", y = "elpd_diff") +
  theme_minimal() +
  scale_x_discrete(guide = guide_axis(n.dodge = 2))

# Combine the data frames from the list into a single data frame
combined_comparison <- do.call(rbind, variance_explained_list)

table(retweets$outrage)

loo(model_list[["outrage_bin"]])

library(sjPlot)
library(gridExtra)

avg_comp_list <- list()

for (dep_var in dependent_variables) {
  
  print(dep_var)
  avg_comp_df <- as.data.frame(marginaleffects::avg_comparisons(model_list[[dep_var]], type = "link"))
  avg_comp_df$label <- paste(avg_comp_df$term, avg_comp_df$contrast, sep = "-")
  avg_comp_list[[dep_var]] <- avg_comp_df
  
}

library(sjPlot)
library(gridExtra)
p <- list()
avg_comp_list[[dep_var]]
for (dep_var in dependent_variables) {
  
  #conditional_effects(model_list[[dep_var]])
  p[[dep_var]] <- ggplot(avg_comp_list[[dep_var]], aes(x = estimate, y = label)) +
    geom_point(alpha = 0.3) +
    geom_point(data = avg_comp_list[[dep_var]] %>% filter(conf.low > 0 | conf.high < 0)) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0., alpha = 0.3) +
    geom_errorbarh(data = avg_comp_list[[dep_var]] %>% filter(conf.low > 0 | conf.high < 0), aes(xmin = conf.low, xmax = conf.high), height = 0.) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    #scale_y_discrete(limits = rev(c("expressopinion-+1", "informothers-+1", "bringattention-+1", "connectwothers-+1", "showemotions-+1",
    #                                "showattitude-+1", "entertain-+1", "influence-+1", "provepoint-+1", "gainattention-+1", "savecontent-+1", "surpriseothers-+1", "provoke-+1", 
    #                                "showachievement-+1", "causechaos-+1", "deceiveothers-+1"))) +
    scale_x_continuous(limits = c(-1, 1)) +
    #geom_text(aes(label = label), vjust = -1.2, size = 3, color = "black") +  
    #labs(x = "Average Comparison Estimate") +
    theme_classic() +
    theme(#axis.text.y=element_blank(),
      axis.title.x=element_blank(),
      axis.title.y=element_blank()) + 
    ggtitle(dep_var) 
  
  
  #ggplot(avg_comp_list[[dep_var]], aes(x = estimate, y = label)) +
  #geom_point(alpha = 0.3) +
  #geom_point(data = avg_comp_list[[dep_var]] %>% filter(conf.low > 0 | conf.high < 0), size=3) +
  #geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0., alpha = 0.3) +
  #geom_errorbarh(data = avg_comp_list[[dep_var]] %>% filter(conf.low > 0 | conf.high < 0), aes(xmin = conf.low, xmax = conf.high), height = 0.) +
  #geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
  #scale_y_discrete(limits = rev(c("expressopinion-+1", "informothers-+1", "bringattention-+1", "connectwothers-+1", "showemotions-+1",
  #                            "showattitude-+1", "entertain-+1", "influence-+1", "provepoint-+1", "gainattention-+1", "savecontent-+1", "surpriseothers-+1", "provoke-+1", 
  #                            "showachievement-+1", "causechaos-+1", "deceiveothers-+1"))) +
  #scale_x_continuous(limits = c(-0.7, 0.7)) +
  ##geom_text(aes(label = label), vjust = -1.2, size = 3, color = "black") +  
  #labs(x = "Average Comparison Estimate") +
  #theme_minimal() +
  #ggtitle(dep_var) 
  
}

do.call(grid.arrange, c(p, ncol=1))



