library(ggplot2)
library(tidyverse)
library(wordcloud2)
library(tm)
library(dplyr)
library(pROC)
library(reshape2)
library(ggbiplot)
library(textdata)
library(tidytext)
library(wordcloud)
library(nnet) 
library(caret)
library(randomForest)
library(sf)
library(rnaturalearth)
library(rnaturalearthdata)
library(GGally)

#importing the spotify dataset
spotify <- read.csv("Spotify.csv")
View(spotify)

#filter out tracks with 0 popularity
filterd_spotify <- spotify %>% filter(popularity != 0)
filterd_spotify
View(filterd_spotify)



#Data Visualisation INF4000



i_p_genres <- c("indie", "j-idol")

#filter them into new variable
ip_filter <- filterd_spotify %>%
  filter(track_genre %in% i_p_genres)

col_pallete2 <- c("indie" = "#00A4CCFF", "j-idol" = "#F95700FF")


#scatter Plot
ggplot(ip_filter, aes(x=energy, y=loudness, colour = track_genre))+
  geom_point() +
  scale_colour_manual(values = col_pallete2) +
  labs(title = "Scatterplot of Popularity against Energy for two genres (J-Idol and Indie)") +
    theme(plot.title = element_text(size = 16, face = "bold"),
  axis.title.x = element_text(size = 20),
  axis.title.y = element_text(size = 16),
  axis.text.x = element_text(size = 18),
  axis.text.y = element_text(size = 18),
  legend.title = element_text(size = 16),
  legend.text = element_text(size = 16)
  )




#Fourth Plot
#sentiment analysis maybe sad music names has sadder words than pop?

indie <- filterd_spotify %>% filter(track_genre %in% c("indie"))
indie <- indie %>% select(Column5 = 5)
indie <- indie %>% rename(track_name = Column5)
words_remove <- c("feat.", "feat")



#split into words
indie_t <- indie %>% unnest_tokens(word, track_name)

#clean up the data
indie_tc <- indie_t %>%
  filter(!word %in% tolower(words_remove))

#remove stopwords
indie_tc2 <- indie_tc %>%
  anti_join(stop_words, by = "word")




#same sentiment analysis but for party songs
idol <- filterd_spotify %>% filter(track_genre %in% c("j-idol"))
idol <- idol %>% select(Column5 = 5)
idol <- idol %>% rename(track_name = Column5)
words_remove <- c("feat.", "feat")

#split into words
idol_t <- idol %>% unnest_tokens(word, track_name)

#clean up the data
idol_tc <- idol_t %>%
  filter(!word %in% tolower(words_remove))

#remove stopwords
idol_tc2 <- idol_tc %>%
  anti_join(stop_words, by = "word")





#sentiment analysis using NRC
nrc_lexicon <- get_sentiments("nrc")

sentiment_analysis <- indie_tc2 %>%
  inner_join(nrc_lexicon, by = "word")



emotion_indie <- sentiment_analysis %>%
  count(sentiment) %>%
  arrange(desc(n))

#J-Idol sentiment analysis

sentiment_analysis <- idol_tc2 %>%
  inner_join(nrc_lexicon, by = "word")


emotion_idol <- sentiment_analysis %>%
  count(sentiment) %>%
  arrange(desc(n))

#count of each emotion
print(emotion_idol)




#grouped bar chart
total_sum <- sum(emotion_indie$n)
total_sum3 <- sum(emotion_idol$n)



#New collumn with percentages
emotion_indie$percentage <- (emotion_indie$n / total_sum) * 100
emotion_indie <- emotion_indie %>% select(-2)

emotion_idol$percentage <- (emotion_idol$n / total_sum3) * 100
emotion_idol <- emotion_idol %>% select(-2)



merged_em <- merge(emotion_indie, emotion_idol, by = "sentiment", suffixes = c("_indie", "_j-idol"))
colnames(merged_em) <- c("sentiment", ".indie", ".J-Idol")


#transforming the data

merged_em_long <- merged_em %>%
  pivot_longer(cols = starts_with("."),  
               names_to = "group",           
               values_to = "count")          


color_pal <- c(".J-Idol" = "#F95700FF", ".indie" = "#00A4CCFF")

ggplot(merged_em_long, aes(x = count, y = sentiment, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = color_pal, name = "Genres", labels =c(".J-Idol"="J-Idol",".indie"="Indie")) +
  theme_minimal() +
  labs(x = "Percentage of words (%)", y = "Emotion", fill = "Genre", title = "Grouped Bar Chart Sentiment of words in song titles by genre using the NRC lexicon") +
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 18),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16)
  )





col_pal_box <- c("j-idol" = "#F95700FF", "indie" = "#00A4CCFF")


#boxplot

box_filter <- ip_filter

box_filter$duration_ms <- box_filter$duration_ms/1000



ggplot(box_filter, aes(x = factor(track_genre), y = duration_ms, fill = track_genre)) +
  geom_boxplot() +
  scale_fill_manual(name = "Genres", values = col_pal_box, labels =c("j-idol"="J-Idol","indie"="Indie")) +
  labs(title = "Boxplot of Indie and J-Idol Music by Duration of songs",
       x = "Genres",
       y = "Duration (Seconds)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16)
  )

summary_stats <- box_filter %>%
  group_by(track_genre) %>%
  summarise(
    Median = median(duration_ms),
    Q1 = quantile(duration_ms, 0.25),
    Q3 = quantile(duration_ms, 0.75),
    Min = min(duration_ms),
    Max = max(duration_ms),
    IQR = IQR(duration_ms)
  )
print(summary_stats)

#density plot
ggplot(ip_filter, aes(x = popularity, fill = (track_genre))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(name = "Genres", values = col_pal_box, labels =c("j-idol"="J-Idol","indie"="Indie")) +
  labs(title = "Density Plot of Sad and J-Idol Music by Popularity of songs",
       x = "Popularity",
       y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16)
  )


#Overlapping Hisotgram
col_outline <- c("j-idol" = "black", "indie" = "black")


ggplot(ip_filter, aes(x = popularity, fill = track_genre, colour = track_genre)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 40) +
  scale_fill_manual(name = "Genres", values = col_pal_box, labels =c("j-idol"="J-Idol","indie"="Indie")) +
  scale_color_manual(values = col_outline, guide = "none") +
  labs(title = "Histogram of Indie and J-Idol Music by Popularity of songs",
       x = "Popularity",
       y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16)
  )



























































































































































































































#sad vs J-Idol
#Data Visualisation INF4000



s_p_genres <- c("sad", "j-idol")

#filter them into new variable
sp_filter <- filterd_spotify %>%
  filter(track_genre %in% s_p_genres)

col_pal <- c("sad" = "#00A4CCFF", "j-idol" = "pink")

#First Plot

heat_data <- sp_filter[, c("loudness", "energy", "acousticness", "popularity", "danceability", "speechiness", "instrumentalness",
                           "liveness", "valence", "tempo")]
cor_matrix <- cor(heat_data, use = "complete.obs")
cor_matrix[upper.tri(cor_matrix)] <- NA
cor_data <- melt(cor_matrix, na.rm = TRUE)


ggplot(cor_data, aes(x = Var1, y = Var2, fill = value)) +
  geom_tile() +
  scale_fill_gradientn(colors = c("#4575b4", "white", "#d73027", "lightgrey"),  
                       values = scales::rescale(c(-1, 0, 0.99, 1)),
                       limits = c(-0.99, 1)) +
  geom_text(aes(label = round(value, 2)), color = "black", size = 4) + 
  labs(title = "Correlation Heatmap of 17 Variables (Sad and J-idol)",
       x = "Variables",
       y = "Variables",
       fill = "Correlation") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 20, angle = 45, hjust = 1),
        axis.text.y = element_text(size = 18),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16)
  )




#second Plot
ggplot(sp_filter, aes(x=energy, y=loudness, colour = track_genre))+
  geom_point() +
  scale_colour_manual(values = col_pal) +
  labs(title = "Scatterplot of Popularity against Energy for two genres (J-Idol and Sad)") +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16)
  )



#Third Plot

sp_PCA <- prcomp(sp_filter[, c("loudness", "energy", "acousticness", "popularity", "danceability", "speechiness", "instrumentalness",
                               "liveness", "valence", "tempo")], center = TRUE, scale. = TRUE)


sp_group <- factor(sp_filter$track_genre)


#First PCA
ggbiplot(sp_PCA, 
         ellipse = TRUE,
         groups = sp_group, 
         labels = rownames(data), 
         var.axes = TRUE) +
  scale_color_manual(name = "Genre", values = col_pal) +
  ggtitle("Principle Component Analysis of Genre Variation between J-idol and Sad music Genres") +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16)
  )










#Fourth Plot
#sentiment analysis maybe sad music names has sadder words than pop?

sad <- filterd_spotify %>% filter(track_genre %in% c("sad"))
sad <- sad %>% select(Column5 = 5)
sad <- sad %>% rename(track_name = Column5)
words_remove <- c("feat.", "feat")



#split into words
sad_t <- sad %>% unnest_tokens(word, track_name)

#clean up the data
sad_tc <- sad_t %>%
  filter(!word %in% tolower(words_remove))

#remove stopwords
sad_tc2 <- sad_tc %>%
  anti_join(stop_words, by = "word")

#get sentiment, using bing
sentiment <- get_sentiments("bing")

#sentimet classification
sad_sentiment <- sad_tc2 %>%
  inner_join(sentiment, by = "word") %>%
  count(sentiment, sort = TRUE)


#same sentiment analysis but for party songs
idol <- filterd_spotify %>% filter(track_genre %in% c("j-idol"))
idol <- idol %>% select(Column5 = 5)
idol <- idol %>% rename(track_name = Column5)
words_remove <- c("feat.", "feat")

#split into words
idol_t <- idol %>% unnest_tokens(word, track_name)

#clean up the data
idol_tc <- idol_t %>%
  filter(!word %in% tolower(words_remove))

#remove stopwords
idol_tc2 <- idol_tc %>%
  anti_join(stop_words, by = "word")

#get sentiment, using bing
sentiment <- get_sentiments("bing")

#sentimet classification
idol_sentiment <- idol_tc2 %>%
  inner_join(sentiment, by = "word") %>%
  count(sentiment, sort = TRUE)


#word cloud on every word in every track name
all_tracks <- filterd_spotify %>% select(Column5 = 5)
all_tracks <- all_tracks %>% rename(track_name = Column5)

#split into words
all_text <- all_tracks %>% unnest_tokens(word, track_name)

#clean up the data
all_text_clean <- all_text %>%
  filter(!word %in% tolower(words_remove))

#remove stopwords
all_text_clean_2 <- all_text_clean %>%
  anti_join(stop_words, by = "word")

#get sentiment, using bing
sentiment <- get_sentiments("bing")

#sentimet classification
sentiment_analysis3 <- all_text_clean_2 %>%
  inner_join(sentiment, by = "word") %>%
  count(sentiment, sort = TRUE)


sad_sent <- sad_sentiment
idol_sent <- idol_sentiment
sent_3 <- sentiment_analysis3


#Clustered bar chart of all three bing sentiment analyses
total_sum <- sum(sad_sentiment$n)
total_sum2 <- sum(idol_sentiment$n)
total_sum3 <- sum(sentiment_analysis3$n)

sad_sent <- sad_sentiment
idol_sent <- idol_sentiment
sent_3 <- sentiment_analysis3


#New collumn with percentages
sad_sentiment$percentage <- (sad_sentiment$n / total_sum) * 100
sad_sentiment <- sad_sentiment %>% select(-2)
colnames(sad_sentiment)[2] <- "Sad_pecentage"


idol_sentiment$percentage <- (idol_sentiment$n / total_sum2) * 100
idol_sentiment <- idol_sentiment %>% select(-2)
colnames(idol_sentiment)[2] <- "J-Idol_percentage"

sentiment_analysis3$percentage <- (sentiment_analysis3$n / total_sum3) * 100
sentiment_analysis3 <- sentiment_analysis3 %>% select(-2)
colnames(sentiment_analysis3)[2] <- "All_percentage"

merged_sent <- merge(sad_sentiment, idol_sentiment, by = "sentiment")
merged_sent <- merge(merged_sent, sentiment_analysis3, by = "sentiment")
colnames(merged_sent) <- c("sentiment", ".Sad", ".J-Idol", ".All")


#transforming the data

merged_sent_long <- merged_sent %>%
  pivot_longer(cols = starts_with("."),  
               names_to = "group",           
               values_to = "count")          


color_pal <- c(".All" = "lightgrey", ".J-Idol" = "pink", ".Sad" = "#00A4CCFF")


ggplot(merged_sent_long, aes(x = sentiment, y = count, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = color_pal, name = "Genres", labels =c(".J-Idol"="J-Idol",".All"="All Genres",".Sad"="Sad")) +
  theme_minimal() +
  labs(x = "Sentiment", y = "Percentage of Words (%)", fill = "Genre", title = "Grouped Bar Chart showing the sentiment of words in song titles by genre using the Bing Lexicon") +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16)
  )





#more meaningfull sentiment analysis using NRC
nrc_lexicon <- get_sentiments("nrc")

sentiment_analysis <- sad_tc2 %>%
  inner_join(nrc_lexicon, by = "word")



emotion_sad <- sentiment_analysis %>%
  count(sentiment) %>%
  arrange(desc(n))

#count of each emotion
print(emotion_sad)



#versus all genre catagories

sentiment_analysis <- all_text_clean_2 %>%
  inner_join(nrc_lexicon, by = "word")


emotion_all <- sentiment_analysis %>%
  count(sentiment) %>%
  arrange(desc(n))

#count of each emotion
print(emotion_all)


sentiment_analysis <- idol_tc2 %>%
  inner_join(nrc_lexicon, by = "word")


emotion_idol <- sentiment_analysis %>%
  count(sentiment) %>%
  arrange(desc(n))

#count of each emotion
print(emotion_idol)




#grouped bar chart
total_sum <- sum(emotion_sad$n)
total_sum3 <- sum(emotion_idol$n)



#New collumn with percentages
emotion_sad$percentage <- (emotion_sad$n / total_sum) * 100
emotion_sad <- emotion_sad %>% select(-2)

emotion_idol$percentage <- (emotion_idol$n / total_sum3) * 100
emotion_idol <- emotion_idol %>% select(-2)



merged_em <- merge(emotion_sad, emotion_idol, by = "sentiment", suffixes = c("_sad", "_j-idol"))
colnames(merged_em) <- c("sentiment", ".Sad", ".J-Idol")


#transforming the data

merged_em_long <- merged_em %>%
  pivot_longer(cols = starts_with("."),  
               names_to = "group",           
               values_to = "count")          


color_pal <- c(".J-Idol" = "pink", ".Sad" = "#00A4CCFF")

ggplot(merged_em_long, aes(x = count, y = sentiment, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = color_pal, name = "Genres", labels =c(".J-Idol"="J-Idol",".Sad"="Sad")) +
  theme_minimal() +
  labs(x = "Percentage of words (%)", y = "Emotion", fill = "Genre", title = "Grouped Bar Chart Sentiment of words in song titles by genre using the NRC lexicon") +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 18),
        axis.text.y = element_text(size = 18),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16)
  )


col_pal_box <- c("j-idol" = "pink", "sad" = "#00A4CCFF")


#boxplot

box_filter <- sp_filter

box_filter$duration_ms <- box_filter$duration_ms/1000



ggplot(box_filter, aes(x = factor(track_genre), y = duration_ms, fill = track_genre)) +
  geom_boxplot() +
  scale_fill_manual(name = "Genres", values = col_pal_box, labels =c("j-idol"="J-Idol","sad"="Sad")) +
  labs(title = "Boxplot of Sad and J-Idol Music by Duration of songs",
       x = "Genres",
       y = "Duration (Seconds)") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16)
  )


#density plot
ggplot(sp_filter, aes(x = popularity, fill = (track_genre))) +
  geom_density(alpha = 0.5) +
  scale_fill_manual(name = "Genres", values = col_pal_box, labels =c("j-idol"="J-Idol","sad"="Sad")) +
  labs(title = "Density Plot of Sad and J-Idol Music by Popularity of songs",
       x = "Popularity",
       y = "Density") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16)
  )


#Overlapping Hisotgram
col_outline <- c("j-idol" = "black", "sad" = "black")


ggplot(sp_filter, aes(x = popularity, fill = track_genre, colour = track_genre)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 30) +
  scale_fill_manual(name = "Genres", values = col_pal_box, labels =c("j-idol"="J-Idol","sad"="Sad")) +
  scale_color_manual(values = col_outline, guide = "none") +
  labs(title = "Histogram of Sad and J-Idol Music by Popularity of songs",
       x = "Popularity",
       y = "Frequency") +
  theme_minimal() +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16)
  )


#misleading histogram
ggplot(sp_filter, aes(x = popularity, fill = track_genre, colour = track_genre)) +
  geom_histogram(alpha = 0.5, position = "identity", bins = 10) +
  scale_fill_manual(name = "Genres", values = col_pal_box, labels =c("j-idol"="J-Idol","sad"="Sad")) +
  scale_color_manual(values = col_outline, guide = "none") +
  labs(title = "Histogram of Sad and J-Idol Music by Popularity of songs",
       x = "Popularity",
       y = "Frequency") +
  theme_minimal() +
  coord_cartesian(xlim = c(0, 100), ylim = c(200, 10000)) +
  theme(plot.title = element_text(size = 16, face = "bold"),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 16),
        axis.text.x = element_text(size = 20),
        axis.text.y = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16)
  )

