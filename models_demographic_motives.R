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
  mutate(gender =
           case_when(gender_bin != 1 & gender_bin != 2 ~ "other",
                     gender_bin == 1 ~ "male",
                     gender_bin == 2 ~ "female",
                     TRUE ~ "other"))

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
  mutate(generation =
           case_when(birtyr_num >= 1985 ~ "younger",
                     birtyr_num < 1985 ~ "older"))

tweets <- tweets %>%
  mutate(relative_date = as.numeric(relative_date),
         period = case_when(relative_date <= 0 ~ "pre",
                            relative_date > 7 ~ "post",
                            TRUE ~ "during"))

tweets <- tweets %>%
  mutate(sentiment_bin = case_when(sentiment_bin == "sentiment: negative" ~ "negative",
                                   sentiment_bin == "negative" ~ "negative",
                                   sentiment_bin == "positive" ~ "positive",
                                   TRUE ~ "NA"
                                   ))

tweets <- tweets %>%
  mutate(category = case_when(c_politics == 1 ~ "politics",
                              c_sports == 1 | c_entertainment == 1 | c_technology == 1 ~ "entertainment",
                              c_social == 1 ~ "social",
                                   TRUE ~ "NA"
  ))

tweets <- tweets %>% 
  mutate(url_type =
           case_when(url_category == "Mainstream News" ~ "news url",
                     url_category == "Alternative News" ~ "alternative url",
                     url_category == "Music" | url_category == "Video" | url_category == "Entertainment" | url_category == "Social media" ~ "entertainment urls",
                     TRUE ~ "NA"))


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
  mutate(content_type =
           case_when(tweet_type == "quote" | tweet_type == "original" | tweet_type == "reply" ~ "produced",
                     tweet_type == "retweet" ~ "shared",
                     TRUE ~ "NA"))

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
  mutate(follower =
           case_when(followers_count_num >= 3000 ~ 'hub',
                     followers_count_num < 3000 ~ 'not hub'))

tweets <- tweets %>% 
  mutate(activity =
           case_when(average_activity > 10 ~ 'active',
                     average_activity <= 10 ~ 'less active'))


#tweets <- tweets %>% 
#  mutate(expression_identity = (expressopinion + showattitude + showemotions)/3,
#         informing_persuasion = (informothers + bringattention + influence + provepoint)/4,
#         socializing_attention = (connectwothers + gainattention + surpriseothers + showachievement)/4,
#         #entertainment_saving = (entertain + savecontent)/2,
#         provokation = (provoke + causechaos + deceiveothers)/3
#  )

tweets <- tweets %>% 
  mutate(expression_identity = (expressopinion + showemotions + showattitude)/3, 
         informing_persuasion = (provepoint + bringattention + influence + informothers)/4,
         provokation = (provoke + causechaos + deceiveothers)/3,
         socializing_attention = (entertain + savecontent + showachievement + gainattention + surpriseothers + connectwothers)/6
  )


tweets$log_retweets <- scale(tweets$log_retweets)[,1]

levels(tweets$url_type)
tweets$url_type <- factor(tweets$url_type, levels = c("NA", "alternative url", "entertainment urls", "news url"))
tweets$category <- factor(tweets$category, levels = c("NA", "entertainment", "politics", "social"))
tweets$tweet_type <- factor(tweets$tweet_type, levels = c("retweet", "reply", "quote", "original"))
tweets$emotion_top <- factor(tweets$emotion_top, levels = c("NA", "joy", "anger", "disgust"))
#tweets$url_type <- relevel(as.factor(tweets$url_type), "no url")
################################################################################
# how much variance of a motive can be explained by the user and how much by demographics
# do this also for tweet features?
tweets$activity

tweets <- tweets %>% filter(!is.na(expressopinion))
#retweets <- tweets %>% filter(tweet_type == "retweet")
retweets <- tweets %>% select(expression_identity, informing_persuasion, socializing_attention, provokation, expressopinion, informothers, bringattention, connectwothers, showemotions, showattitude, entertain,
                                influence, provepoint, gainattention, savecontent, surpriseothers, provoke, showachievement, causechaos, deceiveothers,
                                follower, activity, tweet_type, generation, user, following_retweeter, age_num, political, employment, gender, outrage, outraging, log_followers, log_retweets, education,
                                category, sentiment_bin, retweet_count_num, url_type, emotion_top)
#retweets <- retweets[complete.cases(retweets), ]

retweets$tweet_type
#motive_var <- brm(cbind(expressopinion, informothers, bringattention, connectwothers, showemotions, showattitude, entertain, influence, provepoint, gainattention, savecontent, surpriseothers, provoke, showachievement, causechaos, deceiveothers) ~ (1 | user), mydata_mot)

dependent_variables <-  c("expression_identity", "informing_persuasion", "provokation", "socializing_attention")

#dependent_variables <-  c("expressopinion", "informothers", "bringattention", "connectwothers", "showemotions",
#                          "showattitude", "entertain", "influence", "provepoint", "gainattention", "savecontent", "surpriseothers", "provoke", 
#                          "showachievement", "causechaos", "deceiveothers")

# Initialize an empty list to store the results
r2_comparison_list <- list()
model_list <- list()
variance_explained_list <- list()


# Loop through the dependent variables
for (dep_var in dependent_variables) {
  # Fit the models and calculate Bayesian R2 (similar to previous example)
  #model1 <- brm(as.formula(paste0(dep_var, " ~ (1 | user)")), retweets)
  #model2 <- brm(as.formula(paste0(dep_var, " ~ age_num + pid3_bin + employ_bin + gender_bin + sqrt_followers + education_num + (1 | user)")), mydata_mot)
  #model1 <- brm(as.formula(paste0(dep_var, " ~ age_num + political + employment + gender + log_followers + education + (1 | user)")), retweets)
  #model1 <- brm(as.formula(paste0(dep_var, " ~ generation + political + employment + gender + education + (1 | user)")), retweets)
  model1 <- brm(as.formula(paste0(dep_var, " ~ follower + activity + generation + political + employment + gender + education + (1 | user)")), retweets)
  model2 <- brm(as.formula(paste0(dep_var, " ~ tweet_type + follower + activity + generation + political + employment + gender + education + (1 | user)")), retweets)
  #model1 <- brm(as.formula(paste0(dep_var, " ~ outrage + category + sentiment_bin + following_retweeter + log_retweets + url_type + emotion_top + (1 + url_type | user)")), retweets)
  #model3 <- brm(as.formula(paste0(dep_var, " ~ tweet_type + age_num + pid3_bin + employ_bin + gender_bin + sqrt_followers + education_num + (1 | user)")), mydata_mot)
  
  #r2_model1 <- bayes_R2(model1)
  #r2_model2 <- bayes_R2(model2)
  
  # Perform LOO cross-validation for model 1
  loo1 <- loo(model1)
  
  # Perform LOO cross-validation for model 2
  loo2 <- loo(model2)
  
  # Compare the models using LOO
  loo_compare <- loo_compare(loo1, loo2)
  print(loo_compare)
  model_list[[dep_var]] <- model2
  # Append the data frame to the list
  #variance_explained_list[[dep_var]] <- r2_comparison
  
  # Generate samples from the posterior distribution of the standard deviations
  #model1_sd_samples <- posterior_samples(model1, pars = "^sd_")[,1]
  #model2_sd_samples <- posterior_samples(model2, pars = "^sd_")[,1]
  
  # Convert these to variances
  #model1_var_samples <- model1_sd_samples^2
  #model2_var_samples <- model2_sd_samples^2
  
  # Calculate the variance explained for each pair of samples
  #var_explained_samples <- (model1_var_samples - model2_var_samples) / model1_var_samples
  
  # Calculate the median and 95% uncertainty interval
  #var_explained_median <- median(var_explained_samples)
  #var_explained_interval <- quantile(var_explained_samples, c(0.025, 0.975))
  
  #print(var_explained_interval[1])
  #print(var_explained_median)
  
  # Create a data frame with the model comparison results
  #r2_comparison <- data.frame(Model = c("including demographics"),
  #                            var = loo_compare[, "elpd_diff"],
  #                            Lower = loo_compare[, "se_diff"],
  #                            Upper = loo_compare[, "se_diff"])
  #r2_comparison$Dependent_Variable <- dep_var
  #model_list[[dep_var]] <- model2
  # Append the data frame to the list
  #r2_comparison_list[[dep_var]] <- r2_comparison
  #print(r2_comparison_list)
}


avg_comp_list <- list()

for (dep_var in dependent_variables) {
  
  print(dep_var)
  avg_comp_df <- as.data.frame(marginaleffects::avg_comparisons(model_list[[dep_var]], type = "response"))
  avg_comp_df$label <- paste(avg_comp_df$term, avg_comp_df$contrast, sep = "-")
  avg_comp_list[[dep_var]] <- avg_comp_df
  
}

library(sjPlot)
library(gridExtra)
p <- list()

for (dep_var in dependent_variables) {
  
  #significant_marginal_effects <- avg_comp_list[[dep_var]] %>%
  #  filter(conf.low > 0 | conf.high < 0)
  #marginal_effects <- avg_comp_list[[dep_var]]
  
  #marginal_effects <- marginal_effects %>%
  #  arrange(term, desc(estimate))  
  
  #p[[dep_var]] <- ggplot(marginal_effects, aes(x = estimate, y = reorder(label, estimate), color = term)) +
  #  geom_point() +
  #  geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0.2) +
  #  facet_grid(term ~ ., scales = "free", space = "free") +
  #  theme_minimal() +
  #  theme(
  #    legend.position = "none",
  #    strip.background = element_blank(),
  #    strip.text.y.left = element_text(angle = 0),
  #    axis.title.y = element_blank(),
  #    axis.text.y = element_text(size = 8),
  #    panel.spacing = unit(0.1, "lines")  # Adjust spacing between facets
  #  ) +
  #  labs(x = "Effect on probability to remove posts (%) points", y = "") +
  #  ggtitle(dep_var)
  
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
    labs(x = "average marginal contrast [agreement]") +
    #scale_x_continuous(limits = c(-2, 2)) +
    theme_classic() +
    theme(
      legend.position = "none",
      axis.title.y=element_blank()) +
    ggtitle(dep_var) 
  
}

do.call(grid.arrange,p)

#estimates <- tidy(model_list[["bringattention"]], conf.int = TRUE, conf.level = 0.95)

################################################################################
# bayes r square

motive_models <- list(
  expression_identity = brm(expression_identity ~ (1|tweet_type) + generation + political + employment + gender + education + (1 | user), retweets, control = list(adapt_delta = 0.99)),
  informing_persuasion = brm(informing_persuasion ~ (1|tweet_type) + generation + political + employment + gender + education + (1 | user), retweets, control = list(adapt_delta = 0.99)),
  provokation = brm(provokation ~ (1|tweet_type) + generation + political + employment + gender + education + (1 | user), retweets, control = list(adapt_delta = 0.99)),
  socializing_attention = brm(socializing_attention ~ (1|tweet_type) + generation + political + employment + gender + education + (1 | user), retweets, control = list(adapt_delta = 0.99))
)


################################################################################
#specific retweet motives

dependent_variables <-  c("retweet_motive_makefunof", "retweet_motive_addthoughts", "retweet_motive_argue", "retweet_motive_expressfeelings", "retweet_motive_ridicule", "retweet_motive_correct", "retweet_motive_support")

# Initialize an empty list to store the results
r2_comparison_list <- list()
model_list <- list()
variance_explained_list <- list()


# Loop through the dependent variables
for (dep_var in dependent_variables) {
  # Fit the models and calculate Bayesian R2 (similar to previous example)
  #model1 <- brm(as.formula(paste0(dep_var, " ~ (1 | user)")), retweets)
  #model2 <- brm(as.formula(paste0(dep_var, " ~ age_num + pid3_bin + employ_bin + gender_bin + sqrt_followers + education_num + (1 | user)")), mydata_mot)
  #model1 <- brm(as.formula(paste0(dep_var, " ~ age_num + political + employment + gender + log_followers + education + (1 | user)")), retweets)
  model2 <- brm(as.formula(paste0(dep_var, " ~ outrage + category + sentiment_bin + following_retweeter + url_type + (1 | user)")), retweets)
  #model3 <- brm(as.formula(paste0(dep_var, " ~ tweet_type + age_num + pid3_bin + employ_bin + gender_bin + sqrt_followers + education_num + (1 | user)")), mydata_mot)
  
  #r2_model1 <- bayes
  model_list[[dep_var]] <- model2
  # Append the data frame to the list
  
}



avg_comp_list <- list()

for (dep_var in dependent_variables) {
  
  print(dep_var)
  avg_comp_df <- as.data.frame(marginaleffects::avg_comparisons(model_list[[dep_var]], type = "response"))
  avg_comp_df$label <- paste(avg_comp_df$term, avg_comp_df$contrast, sep = "-")
  avg_comp_list[[dep_var]] <- avg_comp_df
  
}

library(sjPlot)
library(gridExtra)
p <- list()

for (dep_var in dependent_variables) {
  
  #conditional_effects(model_list[[dep_var]])
  p[[dep_var]] <- ggplot(avg_comp_list[[dep_var]], aes(x = estimate, y = label)) +
    geom_point(alpha = 0.3) +
    geom_point(data = avg_comp_list[[dep_var]] %>% filter(conf.low > 0 | conf.high < 0)) +
    geom_errorbarh(aes(xmin = conf.low, xmax = conf.high), height = 0., alpha = 0.3) +
    geom_errorbarh(data = avg_comp_list[[dep_var]] %>% filter(conf.low > 0 | conf.high < 0), aes(xmin = conf.low, xmax = conf.high), height = 0.) +
    geom_vline(xintercept = 0, linetype = "dashed", color = "black") +
    #geom_text(aes(label = label), vjust = -1.2, size = 3, color = "black") +  
    #labs(x = "Average Comparison Estimate") +
    theme_classic() +
    theme(axis.text.y=element_blank(),
          axis.title.x=element_blank(),
          axis.title.y=element_blank()) + 
    ggtitle(dep_var) 
  
}

do.call(grid.arrange, c(p, ncol=4))



