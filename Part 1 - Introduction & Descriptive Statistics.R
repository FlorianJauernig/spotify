# CREATING A SUBSET OF THE SPOTIFY RANKINGS FOR 5 COUNTRIES & SAVING ALL DATASETS AS RDA FILES:
#
# setwd("/Users/flo/Dropbox/WU/Fächer/Data Analytics/Project/data")
# 
# spotify_rankings_worldwide <- read.csv("spotify_rankings_worldwide.csv", header = TRUE)
# save(spotify_rankings_worldwide, file = "spotify_rankings_worldwide.rda")
# 
# spotify_rankings_subset <- subset(spotify_rankings_worldwide, Region == "at" | Region == "au" | Region == "br" | Region == "jp" | Region == "us")
# save(spotify_rankings_subset, file = "spotify_rankings_subset.rda")
# 
# spotify_tracks <- read.csv("spotify_tracks.csv", header = TRUE)
# save(spotify_tracks, file = "spotify_tracks.rda")
# 
# itunes <- read.csv("itunes.csv", header = TRUE, sep=";")
# save(itunes, file="itunes.rda")
#
# We uploaded the created files to the Balrog WU server.


# DOWNLOADING THE RELEVANT DATASETS 

load(url("https://balrog.wu.ac.at/~h1613073/data_analytics/spotify_rankings_worldwide.rda"))
load(url("https://balrog.wu.ac.at/~h1613073/data_analytics/spotify_rankings_subset.rda"))
load(url("https://balrog.wu.ac.at/~h1613073/data_analytics/spotify_tracks.rda"))
load(url("https://balrog.wu.ac.at/~h1613073/data_analytics/itunes.rda"))


# INSPECTING THE DATASETS:

# Looking at the first lines of the datasets.
head(spotify_rankings_worldwide)
head(spotify_rankings_subset)
head(spotify_tracks)
head(itunes)
# Within the first two datasets, the variables relevant for addressing our problem are the position, track name, artist and region.
# In the third dataset, apart from artist and track name, the popularity and the various song features (acousticness, danceability, etc.) are relevant.
# The only variables we will need in the fourth dataset are the title, artist and "Total", which is a points score based on iTunes chart performance.

# Descriptive Statistics for the datasets.
summary(spotify_rankings_worldwide)
# Looking at the list of top artists, the most remarkable fact is that Ed Sheeran's songs can be found 127.064 times in the dataset.
# That means they made up 3% of all worldwide Spotify chart positions of an entire year!
summary(spotify_rankings_subset)
summary(spotify_tracks)
# Looking at the spotify_tracks dataset, you can see that there are about 10.000 songs per genre.
# Also, some of the artists with the most songs in the dataset are actually classical composers.
# Furthermore, the most used, most unoriginal track names are "Home", "You", "Intro", "Stay", "Wake Up" and "Closer".
summary(itunes)

# Displaying the unique number of tracks within the spotify_tracks dataset.
length(unique(c(spotify_tracks$track_name, spotify_tracks$artist_name)))
length(unique(spotify_tracks$track_id))
# There are 148615 unique title/artist combinations, but 176774 unique track IDs.
# This is because some songs exist multiple times in Spotify's song catalogue.

