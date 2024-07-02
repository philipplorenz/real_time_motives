library(tidyverse)
library(dplyr)
library(readr)
library(httr)
library(jsonlite)


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
motive_columns <- select(tweets_motives, contains("motive_") & !contains("retweet_"))
tweets_motives <- tweets_motives %>% mutate(top_motive = names(motive_columns)[max.col(motive_columns)])

reaction_motive_columns <- select(tweets_motives, contains("retweet_motive_"))
tweets_motives <- tweets_motives %>% mutate(top_reaction_motive = names(reaction_motive_columns)[max.col(reaction_motive_columns)])

length(unique(tweets_motives$session))

# join pre- and mini-surveys
motives_with_pre <- left_join(tweets_motives, tweet_pre_df, by = "session")
length(unique(tweets_motives$session))

tweets <- motives_with_pre
#tweets <- left_join(all_tweets, tweet_pre_df, by = "session")
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
  mutate(generation =
           case_when(birtyr_num >= 1985 ~ "millenial or younger",
                     birtyr_num < 1985 ~ "older than millenial",
                     TRUE ~ "other"))

tweets <- tweets %>% 
  mutate(younger =
           case_when(birtyr_num >= 1985 ~ 1,
                     TRUE ~ 0))

tweets <- tweets %>% 
  mutate(older =
           case_when(birtyr_num < 1985 ~ 1,
                     TRUE ~ 0))

tweets <- tweets %>% 
  mutate(popular =
           case_when(retweet_count_num > 50 ~ 1,
                     TRUE ~ 0))

tweets <- tweets %>% 
  mutate(not_popular =
           case_when(retweet_count_num <= 50 ~ 1,
                     TRUE ~ 0))

tweets <- tweets %>% 
  mutate(active =
           case_when(average_activity > 10 ~ 1,
                     TRUE ~ 0))

tweets <- tweets %>% 
  mutate(not_active =
           case_when(average_activity < 10 ~ 1,
                     TRUE ~ 0))

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
  mutate(employed =
           case_when(as.numeric(employ_bin) < 3 ~ 1,
                     TRUE ~ 0))

tweets <- tweets %>% 
  mutate(not_employed =
           case_when(as.numeric(employ_bin) >= 3 ~ 1,
                     TRUE ~ 0))

tweets <- tweets %>% 
  mutate(is_hub =
           case_when(followers_count_num >= 3000 ~ 1,
                     TRUE ~ 0))

tweets <- tweets %>% 
  mutate(not_hub =
           case_when(followers_count_num < 3000 ~ 1,
                     TRUE ~ 0))
tweets <- tweets %>% 
  mutate(male =
           case_when(gender == "male" ~ 1,
                     TRUE ~ 0))

tweets <- tweets %>% 
  mutate(female =
           case_when(gender == "female" ~ 1,
                     TRUE ~ 0))

tweets <- tweets %>% 
  mutate(college =
           case_when(education == "college or higher" ~ 1,
                     TRUE ~ 0))

tweets <- tweets %>% 
  mutate(no_college =
           case_when(education == "no college" ~ 1,
                     TRUE ~ 0))

tweets <- tweets %>% 
  mutate(political =
           case_when(pid3_bin == 3 ~ "independent",
                     pid3_bin == 2 ~ "republican",
                     pid3_bin == 1 ~ "democrat",
                     TRUE ~ "other"))

tweets <- tweets %>% 
  mutate(republican =
           case_when(political == "republican" ~ 1,
                     TRUE ~ 0))

tweets <- tweets %>% 
  mutate(democrat =
           case_when(political == "democrat" ~ 1,
                     TRUE ~ 0))

tweets <- tweets %>% 
  mutate(independent =
           case_when(political == "independent" ~ 1,
                     TRUE ~ 0))

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
  mutate(following =
           case_when(following_retweeter == TRUE ~ 1,
                     TRUE ~ 0))

#tweets <- tweets %>% 
#  mutate(expressopinion = 
#           case_when(top_motive == "motive_expressopinion" ~ 1,
#                     TRUE ~ 0),
#         informothers = 
#           case_when(top_motive == "motive_informothers" ~ 1,
#                     TRUE ~ 0),
#         bringattention = 
#           case_when(top_motive == "motive_bringattention" ~ 1,
#                     TRUE ~ 0),
#         connectwothers = 
#           case_when(top_motive == "motive_connectwothers" ~ 1,
#                     TRUE ~ 0),
#         showemotions = 
#           case_when(top_motive == "motive_showemotions" ~ 1,
#                     TRUE ~ 0),
#         showattitude = 
#           case_when(top_motive == "motive_showattitude" ~ 1,
#                     TRUE ~ 0),
#         entertain = 
#           case_when(top_motive == "motive_entertain" ~ 1,
#                     TRUE ~ 0),
#         influence = 
#           case_when(top_motive == "motive_influence" ~ 1,
#                     TRUE ~ 0),
#         provepoint = 
#           case_when(top_motive == "motive_provepoint" ~ 1,
#                     TRUE ~ 0),
#         gainattention = 
#           case_when(top_motive == "motive_gainattention" ~ 1,
#                     TRUE ~ 0),
#         savecontent = 
#           case_when(top_motive == "motive_savecontent" ~ 1,
#                     TRUE ~ 0),
#         surpriseothers = 
#           case_when(top_motive == "motive_surpriseothers" ~ 1,
#                     TRUE ~ 0),
#         provoke = 
#           case_when(top_motive == "motive_provoke" ~ 1,
#                     TRUE ~ 0),
#         showachievement = 
#           case_when(top_motive == "motive_showachievement" ~ 1,
#                     TRUE ~ 0),
#         causechaos = 
#           case_when(top_motive == "motive_causechaos" ~ 1,
#                     TRUE ~ 0),
#         deceiveothers = 
#           case_when(top_motive == "motive_deceiveothers" ~ 1,
#                     TRUE ~ 0)
#  )


#tweets <- tweets %>% 
#  mutate(expression_identity = 
#           case_when(top_motive == "motive_expressopinion" |
#                     top_motive == "motive_showattitude" |
#                     top_motive == "motive_showemotions" ~ 1,
#                     TRUE ~ 0),
#         informing_persuasion = 
#           case_when(top_motive == "motive_informothers" |
#                     top_motive == "motive_bringattention" |
#                     top_motive == "motive_influence" |
#                     top_motive == "motive_provepoint" ~ 1,
#                     TRUE ~ 0),
#         socializing_attention = 
#           case_when(top_motive == "motive_connectwothers" |
#                     top_motive == "motive_gainattention" |
#                     top_motive == "motive_surpriseothers" |
#                     top_motive == "motive_showachievement" |
#                     top_motive == "motive_entertain" |
#                     top_motive == "motive_savecontent" ~ 1,
#                     TRUE ~ 0),
#         provokation = 
#           case_when(top_motive == "motive_provoke" |
#                     top_motive == "motive_causechaos" |
#                     top_motive == "motive_deceiveothers" ~ 1,
#                     TRUE ~ 0)
#  )


tweets <- tweets %>% 
  mutate(expression_identity = (expressopinion + showemotions + showattitude)/3, 
         informing_persuasion = (provepoint + bringattention + influence + informothers)/4,
         provokation = (provoke + causechaos + deceiveothers)/3,
         socializing_attention = (entertain + savecontent + showachievement + gainattention + surpriseothers + connectwothers)/6
  )

tweets <- tweets %>% 
  mutate(reaction_makefunof = 
           case_when(top_reaction_motive == "retweet_motive_makefunof" ~ 1,
                     TRUE ~ 0),
         reaction_addthoughts = 
           case_when(top_reaction_motive == "retweet_motive_addthoughts" ~ 1,
                     TRUE ~ 0),
         reaction_argue = 
           case_when(top_reaction_motive == "retweet_motive_argue" ~ 1,
                     TRUE ~ 0),
         reaction_expressfeelings = 
           case_when(top_reaction_motive == "retweet_motive_expressfeelings" ~ 1,
                     TRUE ~ 0),
         reaction_ridicule = 
           case_when(top_reaction_motive == "retweet_motive_ridicule" ~ 1,
                     TRUE ~ 0),
         reaction_correct = 
           case_when(top_reaction_motive == "retweet_motive_correct" ~ 1,
                     TRUE ~ 0),
         reaction_support = 
           case_when(top_reaction_motive == "retweet_motive_support" ~ 1,
                     TRUE ~ 0)
  )

tweets <- tweets %>%
  mutate(sentiment_bin = case_when(sentiment_bin == "sentiment: negative" ~ "negative",
                                   sentiment_bin == "negative" ~ "negative",
                                   sentiment_bin == "positive" ~ "positive"
  ))

tweets <- tweets %>% 
  mutate(negative =
           case_when(sentiment_bin == "negative" ~ 1,
                     TRUE ~ 0))

tweets <- tweets %>% 
  mutate(positive =
           case_when(sentiment_bin == "positive" ~ 1,
                     TRUE ~ 0))

tweets <- tweets %>% 
  mutate(mainstream_url =
           case_when(url_category == "Mainstream News" ~ 1,
                     TRUE ~ 0))

tweets <- tweets %>% 
  mutate(alternative_url =
           case_when(url_category == "Alternative News" ~ 1,
                     TRUE ~ 0))

tweets <- tweets %>% 
  mutate(entertainment_url =
           case_when(url_category == "Music" | url_category == "Video" | url_category == "Entertainment" | url_category == "Social media" ~ 1,
                     TRUE ~ 0))

tweets <- tweets %>% 
  mutate(political =
           case_when(c_politics == 1 ~ 1,
                     TRUE ~ 0))

tweets <- tweets %>% 
  mutate(not_political =
           case_when(c_politics != 1 ~ 1,
                     TRUE ~ 0))

tweets <- tweets %>% 
  mutate(retweet =
           case_when(tweet_type == "retweet" ~ 1,
                     TRUE ~ 0))

tweets <- tweets %>% 
  mutate(quote =
           case_when(tweet_type == "quote" ~ 1,
                     TRUE ~ 0))

tweets <- tweets %>% 
  mutate(reply =
           case_when(tweet_type == "reply" ~ 1,
                     TRUE ~ 0))

tweets <- tweets %>% 
  mutate(original =
           case_when(tweet_type == "original" ~ 1,
                     TRUE ~ 0))

tweets <- tweets %>% 
  mutate(outrage =
           case_when(outrage_bin == "TRUE" ~ 1,
                     TRUE ~ 0))


################################################################################
# select relevant variables

tweets <- tweets %>% filter(!is.na(expressopinion))
weird_cases <- tweets[(!is.na(tweets$retweet_motive_addthoughts) & tweets$original == 1),]
tweets <- tweets[!(!is.na(tweets$retweet_motive_addthoughts) & tweets$original == 1),]
tweets_red <- tweets %>% select(session, tweet_type, reaction_makefunof, reaction_addthoughts, reaction_argue, reaction_expressfeelings, 
                                reaction_ridicule, reaction_correct, reaction_support, 
                                informing_persuasion, expression_identity, socializing_attention, provokation, 
                                expressopinion, informothers, bringattention, connectwothers, showemotions, showattitude, entertain,
                                influence, provepoint, gainattention, savecontent, surpriseothers, provoke, showachievement, causechaos, deceiveothers,
                                younger, older, employed, not_employed, is_hub, not_hub, male, female, republican, democrat, independent, college, no_college, active, not_active,
                                outrage, political, not_political, negative, positive, retweet, quote, reply, original, popular, not_popular, following, alternative_url, mainstream_url, entertainment_url)
                                #outrage, political, not_political, negative, positive, quote, popular, following, alternative_url, mainstream_url, entertainment_url)


motive_vars <-c('informothers', 'bringattention', 'influence', 'provepoint', 'expressopinion', 'showemotions', 
                'showattitude', 'connectwothers', 'gainattention', 'surpriseothers', 'showachievement', 
                'entertain', 'savecontent', 'provoke', 'causechaos', 'deceiveothers')


#simple_motives <- c('informing_persuasion', 'expression_identity', 'socializing_attention', 'provokation', 'entertainment')

# Person Features
person_vars <- c('younger', 'older', 'female', 'male', 'employed', 'not_employed', 'college', 'no_college',   
                 'democrat', 'independent', 'republican', 'active', 'is_hub', 'not_hub')

# Content Features
content_vars <- c('outrage', 'political', 'not_political', 'negative', 'positive', 'entertainment_url', 'mainstream_url', 'alternative_url')

reaction_motives_var <- c('reaction_makefunof', 'reaction_addthoughts', 'reaction_argue', 'reaction_expressfeelings', 
                          'reaction_ridicule', 'reaction_correct', 'reaction_support')

post_types <- c('original', 'retweet', 'quote', 'reply')


education_employment <- c('employed', 'not_employed', 'college', 'no_college')

age <- c('younger', 'older')

gender <- c('male', 'female')

political <- c('democrat', 'independent', 'republican')

updated_df <- tweets_red

individual_means <- updated_df %>%
  group_by(`session`) %>%
  summarise(across(everything(), mean, na.rm = TRUE), .groups = 'drop')


calculate_means_and_sem <- function(df, group_vars, post_types) {
  df %>%
    select(one_of(c(group_vars, post_types))) %>%
    pivot_longer(cols = post_types, names_to = 'person feature', values_to = 'Post Type Value') %>%
    filter(`Post Type Value` == 1) %>%
    pivot_longer(cols = group_vars, names_to = 'Variable', values_to = 'Value') %>%
    group_by(`person feature`, `Variable`) %>%
    summarise(Mean = mean(`Value`, na.rm = TRUE),
              SEM = sd(`Value`, na.rm = TRUE) / sqrt(n()), .groups = 'drop') %>%
    ungroup()
}

for (group_vars in list(education_employment, age, gender, political)) {
  group_data <- calculate_means_and_sem(individual_means, motive_vars,  group_vars)
  
  # Adjust the levels of Variable to be in reverse order for plotting
  #group_data$Variable <- factor(group_data$Variable, levels = unique(group_data$Variable))
  group_data$Variable <- factor(group_data$Variable, levels = rev(motive_vars))
  
  # Plot
  #my_colors <- c("blue", "violet", "red", "green")
  p <- ggplot(group_data, aes(x = Variable, y = Mean, group = `person feature`, color = `person feature`)) +
    scale_x_discrete(limits = rev(c('expressopinion', 'showemotions', 'showattitude', 
                                    'bringattention', 'informothers', 'provepoint', 'influence', 
                                    'connectwothers', 'entertain', 'savecontent', 'gainattention', 'surpriseothers', 'showachievement', 
                                    'provoke', 'causechaos', 'deceiveothers')), 
                     labels = rev(c('express opinion', 'show emotions', 'show attitude', 
                                    'bring attention', 'inform others', 'prove a point', 'influence others', 
                                    'connect with others', 'entertain', 'save content', 'gain attention', 'surprise others', 'show achievement', 
                                    'provoke', 'cause chaos', 'deceive others'))) +
    #geom_ribbon(aes(ymin = Mean - SEM, ymax = Mean + SEM, fill = `person feature`), alpha = 0.2, color = NA) +
    geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), alpha = 0.8, width=0.2) +
    geom_line(alpha = .3, size = 0.8) +
    geom_point(alpha = .8, size = 3) +
    #scale_y_continuous(breaks = seq(1, 6, 1), limits = c(1, 6)) +
    coord_flip() +
    labs(title = "",
         y = "agreement [1 fully disagree - 6 fully agree]",
         x = "") +
    theme_minimal() +
    theme(
      axis.text.y = element_text(size = 10),
      axis.text.x = element_text(size = 10),
      #axis.title.x = element_blank(),
      legend.position = c(0.3, 0.88),
      legend.text=element_text(size=10),
      legend.title=element_blank(),
      panel.grid.major.x = element_blank()
    ) + scale_color_brewer(palette = "Set2") 
  #+ scale_color_manual(values = my_colors) 

  
  # Display the plot
  print(p)
}

individual_type_means <- updated_df %>%
  group_by(`session`, tweet_type) %>%
  summarise(across(everything(), mean, na.rm = TRUE), .groups = 'drop')

for (group_vars in list(post_types)) {
  group_data <- calculate_means_and_sem(individual_type_means, motive_vars,  group_vars)
  
  # Adjust the levels of Variable to be in reverse order for plotting
  #group_data$Variable <- factor(group_data$Variable, levels = unique(group_data$Variable))
  group_data$Variable <- factor(group_data$Variable, levels = rev(motive_vars))
  
  # Plot
  p <- ggplot(group_data, aes(x = Variable, y = Mean, group = `person feature`, color = `person feature`)) +
    scale_x_discrete(limits = rev(c('expressopinion', 'showemotions', 'showattitude', 
                                    'bringattention', 'informothers', 'provepoint', 'influence', 
                                    'connectwothers', 'entertain', 'savecontent', 'gainattention', 'surpriseothers', 'showachievement', 
                                    'provoke', 'causechaos', 'deceiveothers')), 
                     labels = rev(c('express opinion', 'show emotions', 'show attitude', 
                                    'bring attention', 'inform others', 'prove a point', 'influence others', 
                                    'connect with others', 'entertain', 'save content', 'gain attention', 'surprise others', 'show achievement', 
                                    'provoke', 'cause chaos', 'deceive others'))) +
    #geom_ribbon(aes(ymin = Mean - SEM, ymax = Mean + SEM, fill = `person feature`), alpha = 0.2, color = NA) +
    geom_errorbar(aes(ymin = Mean - SEM, ymax = Mean + SEM), alpha = 0.8, width=0.2) +
    geom_line(alpha = .3, size = 0.8) +
    geom_point(alpha = .8, size = 3) +
    #scale_y_continuous(breaks = seq(1, 6, 1), limits = c(1, 6)) +
    coord_flip() +
    labs(title = "",
         y = "agreement [1 fully disagree - 6 fully agree]",
         x = "") +
    theme_minimal() +
    scale_color_brewer(palette = "Set1") +
    scale_fill_brewer(palette = "Set1")
  
  # Display the plot
  print(p)
}
  
################################################################################

updated_df <- tweets_red

individual_means <- updated_df %>%
  group_by(`session`) %>%
  summarise(across(everything(), mean, na.rm = TRUE), .groups = 'drop')


updated_df <- mutate_all(updated_df, function(x) as.numeric(as.character(x)))


calculate_normalized_proportions <- function(df, group_vars, post_types) {
  df %>%
    select(one_of(c(group_vars, post_types))) %>%
    pivot_longer(cols = post_types, names_to = 'Post Type', values_to = 'Post Type Value') %>%
    filter(`Post Type Value` == 1) %>%
    pivot_longer(cols = group_vars, names_to = 'Variable', values_to = 'Value') %>%
    group_by(Variable, `Post Type`) %>%
    summarise(Count = sum(Variable)) %>%
    ungroup() %>%
    group_by(Variable) %>%
    mutate(Total = sum(Count)) %>%
    ungroup() %>%
    mutate(Proportion = Count / Total,  # Normalizing to sum to 100% for each Variable
           standard_error = sqrt(Proportion * (1 - Proportion) / Total))
}

# Apply the function to each group and plot
for (group_vars in list(motive_vars, person_vars, content_vars, reaction_motives_var, simple_motives)) {
  group_data <- calculate_normalized_proportions(updated_df, group_vars, post_types)
  
  # Adjust the levels of Variable to be in reverse order
  group_data$Variable <- factor(group_data$Variable, levels = rev(group_vars))
  
  # Plot
  p <- ggplot(group_data, aes(x = Variable, y = Proportion, group = `Post Type`, color = `Post Type`)) +
    geom_ribbon(aes(ymin = Proportion - standard_error, ymax = Proportion + standard_error, fill = `Post Type`), 
                alpha = 0.2, color = NA) +
    geom_point() +
    geom_line() +
    coord_flip() +
    labs(title = paste('Normalized Comparative Proportions in', ifelse(group_vars == person_vars, 'Person Features', 'Content Features'), 'Variables by Post Type'),
         x = 'Variable', y = 'Proportion of "1"') +
    theme_minimal() +
    scale_color_brewer(palette = "Set1") +
    scale_fill_brewer(palette = "Set1")
  
  # Display the plot
  print(p)
}

################################################################################
# Reversed order of variables
motive_vars <-c('informothers', 'bringattention', 'influence', 'provepoint', 'expressopinion', 'showemotions', 
                'showattitude', 'connectwothers', 'gainattention', 'surpriseothers', 'showachievement', 
                'entertain', 'savecontent', 'provoke', 'causechaos', 'deceiveothers')




calculate_normalized_proportions <- function(df, group_vars, post_types) {
  df %>%
    select(one_of(c(group_vars, post_types))) %>%
    pivot_longer(cols = post_types, names_to = 'person feature', values_to = 'Post Type Value') %>%
    filter(`Post Type Value` == 1) %>%
    pivot_longer(cols = group_vars, names_to = 'Variable', values_to = 'Value') %>%
    group_by(Variable, `person feature`) %>%
    summarise(Count = sum(Value)) %>%
    ungroup() %>%
    group_by(`person feature`) %>%
    mutate(Total = sum(Count)) %>%
    ungroup() %>%
    mutate(Proportion = Count / Total,  # Normalizing to sum to 100% for each Variable
           standard_error = sqrt(Proportion * (1 - Proportion) / Total))
}

# Apply the function to each group and plot
for (group_vars in list(education_employment, age_gender, political)) {
  group_data <- calculate_normalized_proportions(updated_df, motive_vars, group_vars)
  
  # Adjust the levels of Variable to be in reverse order
  group_data$Variable <- factor(group_data$Variable, levels = rev(motive_vars))
  
  # Plot
  p <- ggplot(group_data, aes(x = Variable, y = Proportion, group = `person feature`, color = `person feature`)) +
    geom_ribbon(aes(ymin = Proportion - standard_error, ymax = Proportion + standard_error, fill = `person feature`), 
                alpha = 0.2, color = NA) +
    geom_point() +
    geom_line() +
    coord_flip() +
    labs(title = paste('Normalized Comparative Proportions in', ifelse(motive_vars == person_vars, 'Person Features', 'Content Features'), 'Variables by Post Type'),
         x = 'Variable', y = 'Proportion of "1"') +
    theme_minimal() +
    scale_color_brewer(palette = "Set2") +
    scale_fill_brewer(palette = "Set2")
  
  # Display the plot
  print(p)
}

################################################################################

