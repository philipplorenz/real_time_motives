library(dplyr)
library(tidyverse)
library(corrplot)
library(ggcorrplot)
library(GGally)
library(caret)
library(psych)
library("Hmisc")
library(formr)



#################################################################################################
# load data

tweet_pre_df <- readRDS("pre_survey.rds")
tweet_pre_df <- tweet_pre_df[!(is.na(tweet_pre_df$session) | tweet_pre_df$session==""), ]
tweet_pre_df$created <- as.Date(tweet_pre_df$created)
tweet_pre_df <- filter(tweet_pre_df, created >= "2022-02-18")
#tweet_pre_df <- tweet_pre_df %>% filter(is_retweet != TRUE)

tweet_pre_df %>% group_by(session) %>% filter(n()>1)
tweet_pre_df <- tweet_pre_df %>% group_by(session) %>% filter(row_number() == 1) %>% ungroup()
tweet_pre_df <- tweet_pre_df %>% dplyr::rename(motive_informothers = g_tweet_motive1, motive_entertain = g_tweet_motive2, motive_expressopinion = g_tweet_motive3, motive_provoke = g_tweet_motive4, motive_savecontent = g_tweet_motive5, motive_showemotions = g_tweet_motive6, motive_connectwothers = g_tweet_motive7, motive_showachievement = g_tweet_motive8, motive_showattitude = g_tweet_motive9, motive_deceiveothers = g_tweet_motive10, motive_gainattention = g_tweet_motive11, motive_provepoint = g_tweet_motive12, motive_causechaos = g_tweet_motive13, motive_bringattention = g_tweet_motive14, motive_influence = g_tweet_motive15, motive_surpriseothers = g_tweet_motive16)
make_numeric <- tweet_pre_df %>% select(c(contains("motive"))) %>% colnames()
tweet_pre_df <- tweet_pre_df %>% mutate_at(vars(make_numeric), as.numeric)

tweet_pre_df_motives <- select(tweet_pre_df, c(contains("motive_") & !contains("retweet_"), session))
tweet_pre_df_motives <- tweet_pre_df_motives[!is.na(tweet_pre_df_motives$motive_bringattention), ]

avg_by_group <- tweet_pre_df_motives %>%
  group_by(`session`) %>%
  summarise(across(everything(), mean, na.rm = TRUE), .groups = 'drop')

all_tweets <- readRDS("public_dataset.rds")
tweets_motives <- all_tweets[!is.na(all_tweets$motive_bringattention), ]

make_numeric <- tweets_motives %>% select(c(contains("motive"))) %>% colnames()
tweets_motives <- tweets_motives %>% mutate_at(vars(make_numeric), as.numeric)
tweets_motives <- tweets_motives %>% mutate(tweet_type =
                                          case_when(is_retweet == "TRUE" ~ "retweet",
                                                    is_reply == "TRUE" ~ "reply",
                                                    is_quote == "TRUE" ~ "quote",
                                                    TRUE ~ "original"))
motive_columns <- select(tweets_motives, c(contains("motive_") & !contains("retweet_"), session, tweet_type))
motive_columns <- motive_columns[!is.na(motive_columns$motive_bringattention), ]
unique(tweets_motives$session)

avg_by_group_realtime <- motive_columns %>%
  group_by(`session`) %>%
  summarise(across(everything(), mean, na.rm = TRUE), .groups = 'drop')

values_to_match <- avg_by_group_realtime[["session"]]
avg_by_group <- avg_by_group[avg_by_group[["session"]] %in% values_to_match, ]

values_to_match <- avg_by_group[["session"]]
avg_by_group_realtime <- avg_by_group_realtime[avg_by_group_realtime[["session"]] %in% values_to_match, ]

avg_by_group_realtime <- select(avg_by_group_realtime, contains("motive_"))
avg_by_group <- select(avg_by_group, contains("motive_"))

# Get the list of common variable names
common_variable_names <- intersect(names(avg_by_group), names(avg_by_group_realtime))

# Create empty vectors to store correlations and variable names
correlations <- c()
variable_pairs <- c()

# Calculate correlations for common variables
for (variable_name in common_variable_names) {
  cor_value <- cor(as.numeric(avg_by_group[[variable_name]]), as.numeric(avg_by_group_realtime[[variable_name]]), use = "complete.obs")
  correlations <- c(correlations, cor_value)
  variable_pairs <- c(variable_pairs, variable_name)
}

results <- data.frame(motive = variable_pairs, correlation = correlations)

# Create a dataframe to store the results
correlation_results <- data.frame(
  Variable_Pair = variable_pairs,
  Correlation = correlations
)

print(correlation_results)


#######################################################################
# show distribution across all motives, for demographics

sem <- function(x) sd(x)/sqrt(length(x))

motive_columns <- motive_columns %>%
  group_by(`session`) %>%
  summarise(across(everything(), mean, na.rm = TRUE), .groups = 'drop')

averages_mean <- motive_columns %>%
  select(contains("motive_") & !contains("retweet_")) %>%
  summarise(across(everything(), median))

averages_sd <- motive_columns %>%
  select(contains("motive_") & !contains("retweet_")) %>%
  summarise(across(everything(), sd))

combined_data <- bind_rows(averages_mean, averages_sd)
combined_data <- t(combined_data)
#dplyr::count(top_motive) %>%
#filter(motive_expressopinion == 1)%>%
combined_data <- as.data.frame(combined_data)
combined_data <- rownames_to_column(combined_data, "motive")
combined_data <- combined_data %>% rename(mean = V1)
combined_data <- combined_data %>% rename(sd = V2)

sum_n <- length(motive_columns$motive_informothers)

combined_data$group <- paste("all (N=",sum_n,")")

averages <- combined_data

###################


ggplot(averages, aes(motive, mean, group = group, color = group)) +
  scale_x_discrete(limits = rev(c("motive_expressopinion", "motive_connectwothers", "motive_bringattention", "motive_informothers", "motive_showemotions",
                                  "motive_showattitude", "motive_entertain", "motive_influence", "motive_provepoint", "motive_gainattention", "motive_savecontent", "motive_surpriseothers", "motive_provoke", 
                                  "motive_showachievement", "motive_causechaos", "motive_deceiveothers")), 
                   labels = rev(c("expressopinion","connectwothers", "bringattention", "informothers", "showemotions",
                                  "showattitude", "entertain", "influence", "provepoint", "gainattention", "savecontent", "surpriseothers", "provoke",
                                  "showachievement", "causechaos", "deceiveothers"))) +
  #geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd, fill = group), alpha = 0.2, color=NA) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), alpha = 0.8, width=0) +
  scale_y_continuous(breaks = seq(0, 7, 1), limits = c(0, 7)) +
  coord_flip() +
  #geom_line(alpha = .5, size = 1) +
  geom_point(size = 3) +
  geom_text(aes(y = mean + 0.3, label = round(mean, digits=1)), size = 2.5) +
  theme_minimal() +
  labs(title = "",
       y = "agreement [1 fully disagree - 6 fully agree]",
       x = "") +
  theme(
    axis.text.y = element_text(size = 12),
    axis.text.x = element_text(size = 12),
    #axis.title.x = element_blank(),
    legend.position = c(0.7, 0.1),
    legend.text=element_text(size=12),
    legend.title=element_blank(),
    panel.grid.major.x = element_blank()
  ) + scale_color_brewer(palette="Set1")





#######################################################################
# show distribution across all motives, for demographics

high_ed <- motive_columns

averages_mean <- high_ed %>%
  select(contains("motive_") & !contains("retweet_")) %>%
  summarise(across(everything(), mean))

averages_sd <- high_ed %>%
  select(contains("motive_") & !contains("retweet_")) %>%
  summarise(across(everything(), sem))

combined_data <- bind_rows(averages_mean, averages_sd)
combined_data <- t(combined_data)
#dplyr::count(top_motive) %>%
#filter(motive_expressopinion == 1)%>%
combined_data <- as.data.frame(combined_data)
combined_data <- rownames_to_column(combined_data, "motive")
combined_data <- combined_data %>% rename(mean = V1)
combined_data <- combined_data %>% rename(sd = V2)

sum_n <- length(high_ed$motive_informothers)

combined_data$group <- paste("ecological momentary assessments")
combined_data$sum_n <- sum_n

averages <- combined_data

###################

tweet_pre_df_motives <- tweet_pre_df_motives %>%
  group_by(`session`) %>%
  summarise(across(everything(), mean, na.rm = TRUE), .groups = 'drop')

#values_to_match <- avg_by_group_realtime[["session"]]
tweet_pre_df_motives <- tweet_pre_df_motives[tweet_pre_df_motives[["session"]] %in% values_to_match, ]

high_ed <- tweet_pre_df_motives

averages_mean <- high_ed %>%
  select(contains("motive_") & !contains("retweet_")) %>%
  summarise(across(everything(), mean))

averages_sd <- high_ed %>%
  select(contains("motive_") & !contains("retweet_")) %>%
  summarise(across(everything(), sem))

combined_data <- bind_rows(averages_mean, averages_sd)
combined_data <- t(combined_data)
#dplyr::count(top_motive) %>%
#filter(motive_expressopinion == 1)%>%
combined_data <- as.data.frame(combined_data)
combined_data <- rownames_to_column(combined_data, "motive")
combined_data <- combined_data %>% rename(mean = V1)
combined_data <- combined_data %>% rename(sd = V2)

sum_n <- length(high_ed$motive_informothers)

combined_data$group <- paste("onboarding survey")
combined_data$sum_n <- sum_n

averages <- rbind(averages, combined_data)

###################

ggplot(averages, aes(motive, mean, group = group, color = group)) +
  scale_x_discrete(limits = rev(c('motive_expressopinion', 'motive_showemotions', 'motive_showattitude', 
                                  'motive_bringattention', 'motive_informothers', 'motive_provepoint', 'motive_influence', 
                                  'motive_connectwothers', 'motive_entertain', 'motive_savecontent', 'motive_gainattention', 'motive_surpriseothers', 'motive_showachievement', 
                                  'motive_provoke', 'motive_causechaos', 'motive_deceiveothers')), 
                   labels = rev(c('express opinion', 'show emotions', 'show attitude', 
                                  'bring attention', 'inform others', 'prove a point', 'influence others', 
                                  'connect with others', 'entertain', 'save content', 'gain attention', 'surprise others', 'show achievement', 
                                  'provoke', 'cause chaos', 'deceive others'))) +
  #geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd, fill = group), alpha = 0.2, color=NA) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), alpha = 0.8, width=0.2) +
  scale_y_continuous(breaks = seq(1, 6, 1), limits = c(1, 6)) +
  coord_flip() +
  geom_line(alpha = .3, size = 0.8) +
  geom_point(size = 3) +
  #geom_text(aes(y = mean + 0.3, label = round(mean, digits=1)), size = 2.5) +
  theme_minimal() +
  labs(title = "",
       y = "agreement [1 fully disagree - 6 fully agree]",
       x = "") +
  theme(
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    #axis.title.x = element_blank(),
    legend.position = c(0.4, 1.04),
    legend.text=element_text(size=10),
    legend.title=element_blank(),
    panel.grid.major.x = element_blank()
  ) + scale_color_brewer(palette="Set1")

################################################################################

#######################################################################
# show distribution across all motives, for demographics

high_ed <- motive_columns %>% filter(tweet_type == "retweet")

high_ed <- high_ed %>%
  group_by(`session`) %>%
  summarise(across(everything(), mean, na.rm = TRUE), .groups = 'drop')

averages_mean <- high_ed %>%
  select(contains("motive_") & !contains("retweet_")) %>%
  summarise(across(everything(), mean))

averages_sd <- high_ed %>%
  select(contains("motive_") & !contains("retweet_")) %>%
  summarise(across(everything(), sem))

combined_data <- bind_rows(averages_mean, averages_sd)
combined_data <- t(combined_data)
#dplyr::count(top_motive) %>%
#filter(motive_expressopinion == 1)%>%
combined_data <- as.data.frame(combined_data)
combined_data <- rownames_to_column(combined_data, "motive")
combined_data <- combined_data %>% rename(mean = V1)
combined_data <- combined_data %>% rename(sd = V2)

sum_n <- length(high_ed$motive_informothers)

combined_data$group <- paste("shared content")
combined_data$sum_n <- sum_n

averages <- combined_data

###################

high_ed <- motive_columns %>% filter(tweet_type != "retweet")

averages_mean <- high_ed %>%
  select(contains("motive_") & !contains("retweet_")) %>%
  summarise(across(everything(), mean))

averages_sd <- high_ed %>%
  select(contains("motive_") & !contains("retweet_")) %>%
  summarise(across(everything(), sem))

combined_data <- bind_rows(averages_mean, averages_sd)
combined_data <- t(combined_data)
#dplyr::count(top_motive) %>%
#filter(motive_expressopinion == 1)%>%
combined_data <- as.data.frame(combined_data)
combined_data <- rownames_to_column(combined_data, "motive")
combined_data <- combined_data %>% rename(mean = V1)
combined_data <- combined_data %>% rename(sd = V2)

sum_n <- length(high_ed$motive_informothers)

combined_data$group <- paste("created content")
combined_data$sum_n <- sum_n

averages <- rbind(averages, combined_data)

###################

ggplot(averages, aes(motive, mean, group = group, color = group)) +
  scale_x_discrete(limits = rev(c('motive_expressopinion', 'motive_showemotions', 'motive_showattitude', 
                                  'motive_bringattention', 'motive_informothers', 'motive_provepoint', 'motive_influence', 
                                  'motive_connectwothers', 'motive_entertain', 'motive_savecontent', 'motive_gainattention', 'motive_surpriseothers', 'motive_showachievement', 
                                  'motive_provoke', 'motive_causechaos', 'motive_deceiveothers')), 
                   labels = rev(c('express opinion', 'show emotions', 'show attitude', 
                                  'bring attention', 'inform others', 'prove a point', 'influence others', 
                                  'connect with others', 'entertain', 'save content', 'gain attention', 'surprise others', 'show achievement', 
                                  'provoke', 'cause chaos', 'deceive others'))) +
  #geom_ribbon(aes(ymin = mean - sd, ymax = mean + sd, fill = group), alpha = 0.2, color=NA) +
  geom_errorbar(aes(ymin = mean - sd, ymax = mean + sd), alpha = 0.8, width=0.2) +
  scale_y_continuous(breaks = seq(1, 6, 1), limits = c(1, 6)) +
  coord_flip() +
  geom_line(alpha = .3, size = 0.8) +
  geom_point(size = 3) +
  #geom_text(aes(y = mean + 0.3, label = round(mean, digits=1)), size = 2.5) +
  theme_minimal() +
  labs(title = "",
       y = "agreement [1 fully disagree - 6 fully agree]",
       x = "") +
  theme(
    axis.text.y = element_text(size = 10),
    axis.text.x = element_text(size = 10),
    #axis.title.x = element_blank(),
    legend.position = c(0.4, 1.04),
    legend.text=element_text(size=10),
    legend.title=element_blank(),
    panel.grid.major.x = element_blank()
  ) + scale_color_brewer(palette="Set2")
 


