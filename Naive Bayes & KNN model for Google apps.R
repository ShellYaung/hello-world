#This file contains Apps from Google store, along with their attribution 
google <- read.csv('google_play_store.csv')
df <- google[-c(1,3,11,12,13,14)]
library(tidyr)
df <- df %>% drop_na(Rating)
table(df$Content.Rating)
which(df$Content.Rating=='Adults only 18+'|df$Content.Rating=='Unrated')
df_new = df[-c(288,2905,5949,7342),]
dim(df_new)
table(df_new$Content.Rating)
df_new$Content.Rating <- as.factor(df_new$Content.Rating)
df_new$Content.Rating <- droplevels(df_new$Content.Rating)
table(df_new$Content.Rating)


str(df_new)
df_new$Size <- as.numeric(df_new$Size)
str(df_new)
#

#Create a fictional app for prediction: I'ma name my App 'shellsmen'
rating <- runif(1,3.5, 5)
reviews <- runif(1,1,6002)
size <- runif(1,200,460)
table(df_new$Type)
x = 647/(647+8715)
type <- rbinom(1,1,x)
table(df$Price)
s <- sd(table(df$Price))
m <- mean(table(df$Price))
Price <- rnorm(1,m,s)
#raing = 4.2; reviews = 524; type = 0 (free); Price = 0; 
#create dummy var
library(caret)
dmy <- dummyVars("~Type", data = df_new)
Newlist <- data.frame(predict(dmy, newdata = df_new))
df_new <- cbind(df_new, Newlist)

dmy <- dummyVars("~Content.Rating", data = df_new)
Newlist <- data.frame(predict(dmy, newdata = df_new))
df_new <- cbind(df_new, Newlist)

df_new <- df_new[-c(10,11,16)]
#data partition
library(dplyr)
df_new_final <- df_new[c(1,2,3,4,7,9,10,11,12,5)]
set.seed(410)
df_new <- sample_n(df_new_final, 9362)
df_train <- slice(df_new_final, 1:5618)
df_valid <- slice(df_new_final,5618:9362)
df_new_norm <- df_new_final
df_train_norm <- df_train
df_valid_norm <- df_valid
#create my data frame
#I'ma make 'shellsmen' Mature 17+
#raing = 4.2; reviews = 524; type = 0 (free); Price = 0; 
shellsmen <- data.frame(App = 'shellsmen', Rating=4.2,Reviews=524, Size=313, Price=0,
                        Type.Free=1,Content.Rating.Everyone=0,Content.Rating.Everyone.10.=0,
                        Content.Rating.Mature.17.=1)
#data normalization
norm_value <- preProcess(df_train[, 2:9], method=c("center", "scale"))
df_train_norm[,2:9] <- predict(norm_value, df_train[,2:9])
df_valid_norm[,2:9] <- predict(norm_value, df_valid[,2:9])
df_new_norm[, 2:9] <- predict(norm_value, df_new_final[, 2:9])
shellsmen_norm <- predict(norm_value, shellsmen)

#KNN function: my App falls into 'good' level
df_new$Reviews <- as.numeric(df_new$Reviews)
library(FNN)
df_knn <- df_new
df_knn <- knn(train = df_train_norm[, 2:9], test = shellsmen_norm[,2:9], 
                  cl = df_train_norm[, 10], k = 7)
df_knn

#define k
dim(df_train)
sqrt(5618)
accuracy.df <- data.frame(k=seq(1,75,1), accuracy = rep(0,75))
library(e1071)
for(i in 1:75) {
  knn.pred <- knn(df_train_norm[, 2:9], df_valid_norm[, 2:9], 
                  cl = df_train_norm[, 10], k = i)
  accuracy.df[i, 2] <- confusionMatrix(knn.pred, df_valid_norm[, 10])$overall[1] 
}
accuracy.df

# k = 6
arrange(accuracy.df, desc(accuracy))
knn.pred.new <- knn(train = df_train_norm[, 2:9], test= shellsmen_norm[,2:9], 
                    cl = df_train_norm[, 10], k = 6)
knn.pred.new

#Model 2:naive bayes
kickstart_origin <- read.csv('kickstart_project.csv')
# I will pick goal for group A
# category, main_category, launched and country for group B
summary(kickstart_origin$goal)
kickstart <- kickstart_origin
kickstart$goal <- cut(kickstart$goal, breaks=c(0,2000,5000,15000,100000000), labels=c("D","C","B","A"))
summary(kickstart$goal)

set.seed(410)
library(dplyr)
df_kick <- sample_n(kickstart, 322994)
df_kick_train <- slice(df_kick, 1:193796)
df_kick_valid <- slice(df_kick,193796:322994)

# build a naive bayes model
library(e1071)
nb_state <- naiveBayes(state ~ .,data=df_kick_train)
nb_state
# against training data
library(caret)
compar <- predict(nb_state, newdata = df_kick_train)
confusionMatrix(compar, df_kick_train$state)

# validation data
compar2 <- predict(nb_state, newdata = df_kick_valid)
confusionMatrix(compar2, df_kick_valid$state)

#
library(dplyr)
hello <- group_by(kickstart_origin, state)
hello2 <- summarize(hello, avggoal = mean(goal))
View(hello2)
library(ggplot2)
ggplot(hello2, aes(x=state, y=avggoal)) + geom_bar(stat="identity", fill=rainbow(n=6)) + 
  ggtitle("State VS Goal")
