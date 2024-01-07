# Install packages and load libraries
install.packages("dplyr")
library(dplyr)
library(rpart)
library(rpart.plot)

# Load relevant datasets
load(url("https://balrog.wu.ac.at/~h1613073/data_analytics/spotify_rankings_subset.rda"))
load(url("https://balrog.wu.ac.at/~h1613073/data_analytics/spotify_tracks.rda"))


# MERGING SPOTIFY RANKINGS WITH SPOTIFY_TRACKS:

## Converting title and artist strings to lowercase to increase matches between the datasets.
spotify_rankings_subset[[2]] <- tolower(spotify_rankings_subset[[2]])
spotify_rankings_subset[[3]] <- tolower(spotify_rankings_subset[[3]])
spotify_tracks[[2]] <- tolower(spotify_tracks[[2]])
spotify_tracks[[3]] <- tolower(spotify_tracks[[3]])

## Merging the datasets by artist and track name columns.
spotify_merged <- merge(spotify_rankings_subset, spotify_tracks, by.x=c("Artist","Track.Name"), by.y=c("artist_name", "track_name") )
## Success rate of finding the songs of spotify_rankings_subset in spotify_tracks: about 70%. 


# DATA PROCESSING FOR EACH COUNTRY:

## For each country, we perform the following steps:
### From the merged dataset, we select only the country-specific data rows.
### We reduce every song to appear only once per day, which eliminates duplicates caused by multiple genres.
### Then, we add a binary (yes/no) "Top 50" column which is based on the numeric "Position" column.
### Finally, we reduce the variables and keep only the Top 50 column as well as the song features.

## Austria
spotify_at <- spotify_merged[spotify_merged$Region =="at",]
spotify_at <- distinct(spotify_at, URL, Date, .keep_all = TRUE)
spotify_at$top50 <- cut(spotify_at$Position, c(0,50,200) ,c("Yes","No"))
spotify_at_subset <- select(spotify_at,"top50","genre", "acousticness", "danceability", "duration_ms", "energy", "instrumentalness", "key", "liveness","loudness","mode","speechiness","tempo","time_signature", "valence")

## Australia
spotify_au <- spotify_merged[spotify_merged$Region =="au",]
spotify_au <- distinct(spotify_au, URL, Date, .keep_all = TRUE)
spotify_au$top50 <- cut(spotify_au$Position, c(0,50,200) ,c("Yes","No"))
spotify_au_subset <- select(spotify_au,"top50","genre", "acousticness", "danceability", "duration_ms", "energy", "instrumentalness", "key", "liveness","loudness","mode","speechiness","tempo","time_signature", "valence")

## Brazil
spotify_br <- spotify_merged[spotify_merged$Region =="br",]
spotify_br <- distinct(spotify_br, URL, Date, .keep_all = TRUE)
spotify_br$top50 <- cut(spotify_br$Position, c(0,50,200) ,c("Yes","No"))
spotify_br_subset <- select(spotify_br,"top50","genre", "acousticness", "danceability", "duration_ms", "energy", "instrumentalness", "key", "liveness","loudness","mode","speechiness","tempo","time_signature", "valence")

## Japan
spotify_jp <- spotify_merged[spotify_merged$Region =="jp",]
spotify_jp <- distinct(spotify_jp, URL, Date, .keep_all = TRUE)
spotify_jp$top50 <- cut(spotify_jp$Position, c(0,50,200) ,c("Yes","No"))
spotify_jp_subset <- select(spotify_jp,"top50","genre", "acousticness", "danceability", "duration_ms", "energy", "instrumentalness", "key", "liveness","loudness","mode","speechiness","tempo","time_signature", "valence")

## United States
spotify_us <- spotify_merged[spotify_merged$Region =="us",]
spotify_us <- distinct(spotify_us, URL, Date, .keep_all = TRUE)
spotify_us$top50 <- cut(spotify_us$Position, c(0,50,200) ,c("Yes","No"))
spotify_us_subset <- select(spotify_us,"top50","genre", "acousticness", "danceability", "duration_ms", "energy", "instrumentalness", "key", "liveness","loudness","mode","speechiness","tempo","time_signature", "valence")


# FIT AND PLOT DECISION TREES (CART: CLASSIFICATION AND REGRESSION TREE):

## Austria
model_cart_at <- rpart(top50 ~ ., data = spotify_at_subset, method = "class",
                       control=rpart.control(minsplit = 500, minbucket = 500, cp=0.01))
model_cart_at
rpart.plot(model_cart_at, extra = 106, under = TRUE, roundint=FALSE)

### The decision tree for Austria shows that a song with a high valence larger than 0.93 (= happier song)
### is more likely to be in the Top 50. If the song is also not a live recording, it achieves a 
### higher chart ranking. On top of that, the key being A, B, D, D# or F also contributes in a positive way.
### Additionally, if the song is of the genres Dance, Hip-Hop, Pop and Reggaeton it is more likely to be
### ranked in the Top 50 than songs of other genres. If the song also has a tempo lower than 123 BPM, it
### is more likely to be ranked highly. If the valence is at least 0.39, the ranking also benefits.

## Australia
model_cart_au <- rpart(top50 ~ ., data = spotify_au_subset, method = "class")
model_cart_au
rpart.plot(model_cart_au, extra = 106, under = TRUE, roundint=FALSE)

### Those songs that are most likely to be in the Top 50 in Australia combine the following attributes:
### A duration under 241 seconds with the genre being either Dance, Folk, Pop, Rap or Reggaeton and the
### acousticness being low combined with:
#### either the danceability being high
#### or a high valence, the key being B, C#, D, D#, F# or G# and the speechiness being at least 0.047.

## Brazil
model_cart_br <- rpart(top50 ~ ., data = spotify_br_subset, method = "class")
model_cart_br
rpart.plot(model_cart_br, extra = 106, under = TRUE, roundint=FALSE)

### In Brazil, the songs that combine a high danceability and acousticness between 0.52 and 0.59
### are extremely likely to be in the Top 50. Those with a danceability of 0.76 and acousticness of
### at least 0.52 that also have the key A, D or G# are also top-performing songs.
### A low danceability is a characteristic of songs that do not manage to get into the Top 50 in Brazil.

## Japan
model_cart_jp <- rpart(top50 ~ ., data = spotify_jp_subset, method = "class", 
                       control=rpart.control(minsplit = 2000, minbucket = 2000, cp=0.01))
model_cart_jp
rpart.plot(model_cart_jp, extra = 106, under = TRUE, roundint=FALSE)

### In Japan, songs are especially successful if they are of the genres Dance, Pop, R&B or Rap, have
### an energy lower than 0.65, are of the key B, C#, D#, F, F# or G# and have a valence of at least 0.42.
### Therefore, happy & relaxed Dance/Pop/R&B/Rap songs are very likely to be in the Top 50.

## United States
model_cart_us <- rpart(top50 ~ ., data = spotify_us_subset, method = "class", 
                       control=rpart.control(minsplit = 3000, minbucket = 3000, cp=0.008))

model_cart_us
rpart.plot(model_cart_us, extra = 106, under = TRUE, roundint=FALSE)

### In the United States of America, a song that combines a high danceability with the key being either
### A, B, C#, D or D#, the genre being Indie, Pop or Rap and the mode being Minor is very likely to be
### in the nationwide Top 50 Spotify Rankings.
