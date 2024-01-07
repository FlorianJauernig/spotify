#Libraries
library(FSelector)
library(caret)
library(dplyr)
library(data.tree)
library(caTools)
library(rpart)
library(rpart.plot)
library(rattle)
library(e1071)
library(ROCR)
library(ggplot2)
library(cowplot)

# Loading the dataset and reading the data in
load(url("https://balrog.wu.ac.at/~h1613073/data_analytics/spotify_tracks.rda"))

spotify_tracks <- distinct(spotify_tracks, track_name, artist_name, .keep_all = TRUE)

data <- spotify_tracks

data$high_popularity <- cut(data$popularity, c(-1,50,100), c("No","Yes"))

### Filtering data
Data <- subset(data, select = -c(popularity, genre, artist_name, track_name, track_id, instrumentalness))
colnames(Data)

# Further inspection o the data
head(data$high_popularity, n=100)
summary(data$high_popularity)
summary(Data)
colnames(Data)

### Splitting the data in subsets
set.seed(321)
sample <- sample.split(Data$high_popularity, SplitRatio = 0.80)
train_data <- subset(Data, sample == TRUE)
test_data <- subset(Data, sample == FALSE)

### Decision tree model

## Training decision Tree (Test data)
fit <- rpart(high_popularity ~ ., data = train_data, method = "class",
             control=rpart.control(minsplit = 1000, minbucket = 3000, cp=0.0001))

##Testing the results
result <- predict(fit, test_data, type = "class")

## Evaluating the model
confusionMatrix(result, test_data$high_popularity)

## Ploting the results
prp(fit, extra = 5)
rpart.plot(fit)
prp(fit)
fancyRpartPlot(fit, type=0)


### Logistic regression model

## Logistic regression
fit_log <- glm(high_popularity ~ ., data = Data, family = binomial())
summary(fit_log)

fit_log_step <- step(fit_log, direction = "both")

## Logistic with train data
fit_log_train <- glm(high_popularity ~ ., data = train_data, family = binomial())

## Predicting with log_reg
fit_log_test <- predict(fit_log_train, newdata = test_data, type = "response")
fit_pred <- prediction(fit_log_test, labels = test_data$high_popularity)
fit_pred

# Accuracy
tab1 <- table(predicted = (fit_log_test > 0.5) + 0,
              true = test_data$high_popularity)

acc <- sum(diag(tab1))/sum(tab1)

# Precision
prec <- (tab1[2,2]/sum(tab1[2,]))


## Ploting Logistic Regression model's performance
log_reg_data <- data.frame(probability=fit_log$fitted.values, high_popularity=data$high_popularity)

# Sorting values
log_reg_data <- log_reg_data[order(log_reg_data$probability, decreasing = FALSE),]
# Adding a rank to each sample (for visualisation)
log_reg_data$rank <- 1:nrow(log_reg_data)

# Setting nicer theme for the plots
theme_set(theme_cowplot()) 

# Plotting
ggplot(data = log_reg_data, aes(x=rank, y=probability))+
   geom_point(aes(color=high_popularity), alpha=1, shape=4, stroke=2)+
  geom_hline(yintercept = 0.5, linetype="dashed", color="red", size=2)+
   xlab("Index")+
   ylab("Predicted probability of high popularity")
