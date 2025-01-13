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



#Data Visualization INF4000


#selecting genres to be investigated
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
  axis.title.y = element_text(size = 20),
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


#colour pallete for the chart

color_pal <- c(".J-Idol" = "#F95700FF", ".indie" = "#00A4CCFF")

#creating the clustered bar chart

ggplot(merged_em_long, aes(x = count, y = sentiment, fill = group)) +
  geom_bar(stat = "identity", position = "dodge") +
  scale_fill_manual(values = color_pal, name = "Genres", labels =c(".J-Idol"="J-Idol",".indie"="Indie")) +
  theme_minimal() +
  labs(x = "Percentage of words (%)", y = "Emotion", fill = "Genre", title = "Grouped Bar Chart Sentiment of words in song titles by genre using the NRC lexicon") +
  theme(plot.title = element_text(size = 18, face = "bold"),
        axis.title.x = element_text(size = 20),
        axis.title.y = element_text(size = 20),
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
        axis.title.y = element_text(size = 20),
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
        axis.title.y = element_text(size = 20),
        axis.text.x = element_text(size = 16),
        axis.text.y = element_text(size = 16),
        legend.title = element_text(size = 16),
        legend.text = element_text(size = 16)
  )

