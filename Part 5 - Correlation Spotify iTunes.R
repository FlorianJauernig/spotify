# Install packages and load libraries
install.packages("scales")
library(scales)

# Load relevant datasets

load(url("https://balrog.wu.ac.at/~h1613073/data_analytics/spotify_tracks.rda"))
load(url("https://balrog.wu.ac.at/~h1613073/data_analytics/itunes.rda"))


# MERGING SPOTIFY_TRACKS WITH ITUNES:

# Converting title and artist strings to lowercase to increase matches between the datasets.
spotify_tracks[[2]] <- tolower(spotify_tracks[[2]])
spotify_tracks[[3]] <- tolower(spotify_tracks[[3]])
itunes[[1]] <- tolower(itunes[[1]])
itunes[[2]] <- tolower(itunes[[2]])

# Merging the datasets by artist and title columns.
itunes_spotify_merged <- merge(spotify_tracks, itunes, by.x=c("artist_name","track_name"), by.y=c("Artist", "Title") )

# Rescale the iTunes popularity to a range from 0 to 100.
itunes_spotify_merged$itunes_popularity <- rescale(itunes_spotify_merged$Total, to = c(0, 100))

# Generate a scatter plot with a smooth curve fitted by Loess. Limit range of x and y axes to have a closer look at the curve.
scatter.smooth(x=itunes_spotify_merged$popularity, y=itunes_spotify_merged$itunes_popularity, main="Spotify ~ iTunes", xlim=c(0,100), ylim=c(0,100), xlab="Spotify Popularity", ylab="iTunes Popularity")
scatter.smooth(x=itunes_spotify_merged$popularity, y=itunes_spotify_merged$itunes_popularity, main="Spotify ~ iTunes", xlim=c(30,100), ylim=c(0,50), xlab="Spotify Popularity", ylab="iTunes Popularity")
scatter.smooth(x=itunes_spotify_merged$popularity, y=itunes_spotify_merged$itunes_popularity, main="Spotify ~ iTunes", xlim=c(40,100), ylim=c(0,20), xlab="Spotify Popularity", ylab="iTunes Popularity")
# From the plots, we can see that spotfiy popularity does not seem to have a linear correlation with iTunes popularity.
# The line in the graph is almost horizontal, it only goes up slightly after we pass a Spotify popularity of 70.  

# Fitting a linear model based on the variables of Spotify popularity and iTunes popularity.
linear_model <- lm(popularity~ itunes_popularity, data=itunes_spotify_merged)
summary(linear_model) 
# The p-value is tiny, so we are rejecting the null hypothesis that there is a correlation between Spotify popularity and iTunes popularity.