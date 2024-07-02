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

tweets <- tweets %>% 
  mutate(expressopinion = 
           case_when(top_motive == "motive_expressopinion" ~ 1,
                     TRUE ~ 0),
         informothers = 
           case_when(top_motive == "motive_informothers" ~ 1,
                     TRUE ~ 0),
         bringattention = 
           case_when(top_motive == "motive_bringattention" ~ 1,
                     TRUE ~ 0),
         connectwothers = 
           case_when(top_motive == "motive_connectwothers" ~ 1,
                     TRUE ~ 0),
         showemotions = 
           case_when(top_motive == "motive_showemotions" ~ 1,
                     TRUE ~ 0),
         showattitude = 
           case_when(top_motive == "motive_showattitude" ~ 1,
                     TRUE ~ 0),
         entertain = 
           case_when(top_motive == "motive_entertain" ~ 1,
                     TRUE ~ 0),
         influence = 
           case_when(top_motive == "motive_influence" ~ 1,
                     TRUE ~ 0),
         provepoint = 
           case_when(top_motive == "motive_provepoint" ~ 1,
                     TRUE ~ 0),
         gainattention = 
           case_when(top_motive == "motive_gainattention" ~ 1,
                     TRUE ~ 0),
         savecontent = 
           case_when(top_motive == "motive_savecontent" ~ 1,
                     TRUE ~ 0),
         surpriseothers = 
           case_when(top_motive == "motive_surpriseothers" ~ 1,
                     TRUE ~ 0),
         provoke = 
           case_when(top_motive == "motive_provoke" ~ 1,
                     TRUE ~ 0),
         showachievement = 
           case_when(top_motive == "motive_showachievement" ~ 1,
                     TRUE ~ 0),
         causechaos = 
           case_when(top_motive == "motive_causechaos" ~ 1,
                     TRUE ~ 0),
         deceiveothers = 
           case_when(top_motive == "motive_deceiveothers" ~ 1,
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
library(igraph)

tweets <- tweets %>% filter(!is.na(expressopinion))
tweets_red <- tweets %>% select(expressopinion, informothers, bringattention, connectwothers, showemotions, showattitude, entertain,
                                influence, provepoint, gainattention, savecontent, surpriseothers, provoke, showachievement, causechaos, deceiveothers,
                                younger, older, employed, not_employed, is_hub, not_hub, male, female, republican, democrat, independent, college, no_college, active, not_active,
                                outrage, political, not_political, negative, positive, retweet, quote, reply, original, popular, not_popular, following, alternative_url, mainstream_url, entertainment_url)
                                #outrage, political, not_political, negative, positive, quote, popular, following, alternative_url, mainstream_url, entertainment_url)


################################################################################

types <- c("motive", "motive", "motive", "motive", "motive", "motive", "motive", "motive", "motive", "motive", "motive", "motive", "motive", "motive", "motive", "motive", 
           "person", "person", "person", "person", "person", "person", "person", "person", "person", "person", "person", "person", "person", "person", "person",
           "tweet", "tweet", "tweet", "tweet", "tweet", "tweet", "tweet", "tweet", "tweet", "tweet", "tweet", "tweet", "tweet", "tweet", "tweet")

cor_matrix <- cor(as.matrix(tweets_red), method = "spearman")

# Create a correlation plot using corrplot
cor_matrix[cor_matrix < 0] <- 0
graph <- graph.adjacency(cor_matrix, mode = "undirected", weighted = TRUE)
graph <- simplify(graph)
plot(graph)
# Add node attributes based on types
V(graph)$type <- types

# Define colors and shapes for different node types
type_colors <- c("motive" = "light blue", "person" = "light coral", "tweet" = "light green")
#type_shapes <- c("motive" = "circle", "person" = "square", "tweet" = "triangle")

# Assign colors and shapes based on node types
V(graph)$color <- type_colors[V(graph)$type]
#V(graph)$shape <- type_shapes[V(graph)$type]

# Customize node attributes
V(graph)$size <- degree(graph)*0.8  # Adjust the scaling factor as needed

# Layout as a bipartite graph
#coords <- layout_as_bipartite(graph, types = V(graph)$type)
#coords <- layout_(graph, with_dh())
layout <- layout_with_fr(graph, maxiter = 1000)
plot(graph, edge.width = E(graph)$weight*10, layout = layout, vertex.label.cex = 0.75, vertex.label.family = 'Helvetica', vertex.label.font = 2, vertex.frame.color = NA, vertex.label.color = "Black")  # You can adjust the scaling factor (5 in this case)

#layout = layout_with_sugiyama(graph, layers=c(2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2, 2,3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 3, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1, 1), maxiter=5000)
#plot(graph, edge.width = E(graph)$weight*10, layout = layout$layout[,2:1], vertex.label.cex = 0.7, vertex.label.family = 'Helvetica', vertex.label.font = 2, vertex.frame.color = NA, vertex.label.color = "Black")  # You can adjust the scaling factor (5 in this case)


write_graph(graph, "feature_graph.graphml", format="graphml")




