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
motive_columns <- select(tweets_motives, c(contains("motive_") & !contains("retweet_"), session))
motive_columns <- motive_columns[!is.na(motive_columns$motive_bringattention), ]
unique(tweets_motives$session)

################################################################################
# simple factor analysis for onboarding motives

tweet_pre_df_motives$session <- NULL
motives_numeric <- mutate_all(tweet_pre_df_motives, function(x) as.numeric(as.character(x)))
cor_matrix <- cor(motives_numeric, method = 'spearman')
#cor_matrix <- crossprod(as.matrix(tweets_red))
#cor_matrix[cor_matrix < 0] <- 0

variable_names <- colnames(cor_matrix)

library(corrplot)

heatmap(cor_matrix,
        Rowv = as.dendrogram(hclust(dist(cor_matrix))),
        Colv = as.dendrogram(hclust(dist(t(cor_matrix)))),
        col = colorRampPalette(c("white", "blue"))(1000),
        cexRow = 1,
        scale = "none")


pca_result <- prcomp(motives_numeric, scale. = TRUE)

plot(pca_result$sdev^2, type = "b", xlab = "Principal Component", ylab = "Eigenvalue",
     main = "Scree Plot")
abline(h = 1, col = "red", lty = 2)  # Cutoff criterion (Kaiser criterion)

fa_result <- factanal(motives_numeric, factors = 4, rotation = "varimax")
print(fa_result)

alpha(motives_numeric[, c("motive_provepoint", "motive_bringattention", "motive_influence", "motive_informothers")] )

alpha(motives_numeric[, c("motive_expressopinion", "motive_showemotions", "motive_showattitude")] )

alpha(motives_numeric[, c("motive_provoke", "motive_causechaos", "motive_deceiveothers")] )

alpha(motives_numeric[, c("motive_entertain", "motive_savecontent", "motive_showachievement", "motive_gainattention", "motive_surpriseothers", "motive_connectwothers")] )

################################################################################
# simple factor analysis for onboarding motives

#motive_columns <- motive_columns %>%
#  group_by(`session`) %>%
#  summarise(across(everything(), mean, na.rm = TRUE), .groups = 'drop')



pca_result <- prcomp(motives_numeric, scale. = TRUE)

plot(pca_result$sdev^2, type = "b", xlab = "Principal Component", ylab = "Eigenvalue",
     main = "Scree Plot")
abline(h = 1, col = "red", lty = 2)  # Cutoff criterion (Kaiser criterion)

fa_result <- factanal(motives_numeric, factors = 4, rotation = "varimax")
print(fa_result)

alpha(motives_numeric[, c("motive_provepoint", "motive_bringattention", "motive_influence", "motive_informothers")] )

alpha(motives_numeric[, c("motive_expressopinion", "motive_showemotions", "motive_showattitude")] )

alpha(motives_numeric[, c("motive_provoke", "motive_causechaos", "motive_deceiveothers")] )

alpha(motives_numeric[, c("motive_entertain", "motive_savecontent", "motive_showachievement", "motive_gainattention", "motive_surpriseothers", "motive_connectwothers")] )


################################################################################
# multilevel factor analysis using lavaan

# Load necessary packages
library(lavaan)

model <- '
  level: 1
    information_tw =~ motive_provepoint + motive_bringattention + motive_influence + motive_informothers 
    expression_tw =~ motive_expressopinion + motive_showemotions + motive_showattitude 
    #entertainment_tw =~ motive_entertain + motive_savecontent 
    provocation_tw =~ motive_provoke + motive_causechaos + motive_deceiveothers
    social_tw =~ motive_showachievement + motive_gainattention + motive_surpriseothers + motive_connectwothers + motive_entertain + motive_savecontent 
  level: 2
    information_ind =~ motive_provepoint + motive_bringattention + motive_influence + motive_informothers 
    expression_ind =~ motive_expressopinion + motive_showemotions + motive_showattitude 
    #entertainment_ind =~ motive_entertain + motive_savecontent 
    provocation_ind =~ motive_provoke + motive_causechaos + motive_deceiveothers
    social_ind =~ motive_showachievement + motive_gainattention + motive_surpriseothers + motive_connectwothers + motive_entertain + motive_savecontent 

'

# Fit the model specifying the participant ID as the grouping variable
fit <- cfa(model, data = motive_columns, cluster = "session")

# Summarize the results
summary(fit)
fitMeasures(fit, c('cfi', 'rmsea', 'rmsea.ci.upper', 'bic'))

lavInspect(fit, "icc") 
semTools::compRelSEM(fit)

modindices(fit, sort = TRUE)
mi <- modindices(fit, sort = TRUE)
mi[mi$op == "=~",]

print(fitMeasures(fit,  c("cfi", "rmsea", "srmr"), output = "text"))

