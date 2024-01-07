# Load libraries
library(ggplot2)
library(tidyverse)
library(viridis)
library(ggExtra)
library(corrplot)
library(wordcloud2)

# Load relevant dataset

load(url("https://balrog.wu.ac.at/~h1613073/data_analytics/spotify_tracks.rda"))

# Remove duplicates

spotify_tracks_c <- distinct(spotify_tracks,track_id,.keep_all = TRUE)


# Checking correlation of all features to popularity-------------------------------------
# Only relevant for understandiog the analyse process 
# of search for relevant features
all_cor <- glm(popularity ~ .,data=select(spotify_tracks_c,5,6,7,8,9,10,12,13,15,16,18))
summary(all_cor)

# WARNING - COMPUTATION INTENSE (may take a few minutes)
# Shows plots of every feature correlation to popularity 
# of all entries from data set
plot(popularity ~ .,data=select(spotify_tracks_c,5,6,7,8,9,10,12,13,15,16,18))
# ---------------------------------------------------------------------------------------


# key -----------------------------------------------------------------------------------
plot(popularity ~ key,data = spotify_tracks_c)
key_corr <- glm(popularity ~ key, data = spotify_tracks_c, model = TRUE)
summary(key_corr)

# Taking a random sample
set.seed(12345)
id <- sample(1:2, nrow(spotify_tracks_c), replace = TRUE, prob = c(0.995,0.005))
key_sample <- spotify_tracks_c[id == 2, ] # 0,5% of data from set

# Color of boxplots
box_color <- c("red", "orange", "yellow", "#C8FE2E", "green", "cyan", "#58ACFA", "blue", "#8904B1", "purple", "pink", "#8000FF")

# Pretty plotting
ggplot( spotify_tracks_c, aes(y=popularity, x=key)) +
  geom_boxplot(fill = box_color) + theme_classic() + 
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(data=key_sample,color="#04B404", size=2.3, alpha=0.9) +
  # theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=4)
  ) +
  ggtitle("Correlation of popularity and keys") +
  xlab("")
# ---------------------------------------------------------------------------------------


# acousticness --------------------------------------------------------------------------
plot(popularity ~ acousticness,data = spotify_tracks_c)
acousticness_corr <- glm(popularity ~ acousticness, data = spotify_tracks_c, model = TRUE)
summary(acousticness_corr)

set.seed(12345)
id <- sample(1:2, nrow(spotify_tracks_c), replace = TRUE, prob = c(0.995,0.005))
acousticness_sample <- spotify_tracks_c[id == 2, ] # 0,5% of data from set

p <- ggplot(acousticness_sample, aes(x=popularity, y=acousticness)) +
  geom_point(size=2, color="#E87D72") + theme(legend.position="none") 
ggMarginal(p, type="density")
# ---------------------------------------------------------------------------------------


# Correlation of all features -----------------------------------------------------------
s <- cor(select(spotify_tracks_c,5,6,7,8,9,10,12,13,15,16,18))
# col1 <- colorRampPalette(c("#00007F","blue","#007FFF","cyan","white","yellow","#FF7F00", "red"))
corrplot(s, type = "full", method = "square")
# ---------------------------------------------------------------------------------------


# mode ----------------------------------------------------------------------------------
plot(popularity ~ mode,data = spotify_tracks_c)
mode_corr <- glm(popularity ~ mode, data = spotify_tracks_c, model = TRUE)
summary(mode_corr)

# Taking a random sample
set.seed(12345)
id <- sample(1:2, nrow(spotify_tracks_c), replace = TRUE, prob = c(0.995,0.005))
mode_sample <- spotify_tracks_c[id == 2, ] # 0,5% of data from set

# Color of boxplots
box_color <- c("#FF8000", "#0080FF")

# Pretty plotting
ggplot( spotify_tracks_c, aes(y=popularity, x=mode)) +
  geom_boxplot(fill = box_color) +
  scale_fill_viridis(discrete = TRUE, alpha=0.6) +
  geom_jitter(data=mode_sample,color="#04B404", size=2, alpha=0.9) + 
  # theme_ipsum() +
  theme(
    legend.position="none",
    plot.title = element_text(size=5)
  ) +
  ggtitle("Correlation of popularity and modes") +
  xlab("")
# ---------------------------------------------------------------------------------------
