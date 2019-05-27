# Data Preparation and Exploration
airbnb = read.csv("airbnbtrain.csv")
library(dplyr)
NYC = filter(airbnb, city == "NYC")
NYC = NYC[-c(20,21,26)]
NYC$property_type = droplevels(NYC$property_type)

# I (first replace empty cells with NA and dropped them)
anyNA(NYC)
library(tidyr)
NYC[NYC == ""] = NA
NYC = drop_na(NYC)
# Delete the listing with log_price = 0 which is undefined and free listing which is not possible
min(NYC$log_price)
library(dplyr)
NYC = subset(NYC, log_price != "0")
min(NYC$log_price)

# II variables chosen are log_price, accomodates, host response rate, number of reviews, 
# review_scores rating. In case for Association rules we can use bathroom, bedroom, beds
summary(NYC$log_price)
sd(NYC$log_price)
range(NYC$log_price)
summary(NYC$neighbourhood)

# III Visualization
# boxplot with #bedrooms and log price
top10Neighbor <- filter(NYC, neighbourhood %in% c("Williamsburg", "Bedford-Stuyvesant", "Bushwick", "Hell's Kitchen", "Harlem", "Upper West Side", "Upper East Side", "Upper East Side", "Crown Heights", "Astoria", "East Harlem"))
library(ggplot2)
ggplot(top10Neighbor, aes(x=neighbourhood, y = log_price, fill = neighbourhood)) + geom_boxplot(outlier.colour = "red") + labs(title = "Prices per Neighbourhood") + theme(plot.title = element_text(hjust = 0.5))
boxplot(NYC$log_price~NYC$bedrooms, xlab = "Bedrooms", ylab = "Log_Price")

# neighborhood and price barplot
top10 = sort(table(NYC$neighbourhood), decreasing = TRUE)[1:10]
View(top10)
class(top10)
top10 = data.frame(top10)
names(top10)[1] = "Top10Neighbourhoods"
ggplot(top10, aes(x=Top10Neighbourhoods, y=Freq,fill = Top10Neighbourhoods)) + geom_bar(stat = "identity") + labs(title = "Neighbourhoods", x = "Top 10 Neighbourhoods") + theme(plot.title = element_text(hjust = 0.5))

# Violin 
NYC$accommodates = as.factor(NYC$accommodates)
ggplot(NYC, aes(x=accommodates, y= log_price)) +geom_violin()

# Histogram 
summary(NYC$property_type)
propertytop10 = sort(table(NYC$property_type), decreasing = TRUE)[1:10]
class(propertytop10)
propertytop10 = data.frame(propertytop10)
names(propertytop10)[1] = "PropertyType"
ggplot(propertytop10, aes(x=PropertyType, y = Freq, fill=PropertyType)) + geom_histogram(binwidth = 5, stat = "identity") + labs(x = "Property Type")

# Scatterplot
ggplot(top10Neighbor, aes(x=neighbourhood, y=log_price)) + geom_point()

#Barplot for avg log_price and top 10 neighbourhood
avgpricetop10 = group_by(top10Neighbor, neighbourhood)
avgpricetop10 = summarize(avgpricetop10, avg=mean(log_price))
ggplot(avgpricetop10, aes(x=neighbourhood, y = avg, fill=neighbourhood)) + geom_bar(stat = "identity") + labs(title = "Average Price per Neighbourhood") + theme(plot.title = element_text(hjust = 0.5))

# II dont forget to change accommodates to numeric 
# A. Independent varriables: accommodates, review_scores_rating, number_of_reviews, property_type, room_type
# first we started with beds, bedrooms, bathrooms, accommodates, review score, number of reviews, property and room type. Then
# we use ggpairs and saw high correlation between beds, bathrooms, bedrooms so we took them out to avoid
# multicollineariry and then we filter our dataset to analyze only apartments because it entails around 80%
# and also NYC is mostly composed of apartments and the other ones are not very significant in impacting the
# log_price because there is not a high frequency of these other property types.
# Then we create dummy variables for room_type and we also saw that no multicollinearity so we took this
# variable into consideration. Then we run the regression model and backward elimination and saw that 
# number of reviews has a high p-value > 0.05 so not significant so took it out.
new_NYC <- filter(NYC, property_type == "Apartment")
library(caret)
dmy <- dummyVars("~room_type", data = new_NYC, fullRank = TRUE)
transform = data.frame(predict(dmy, newdata = new_NYC))
new_NYC1 = cbind(new_NYC, transform)
View(new_NYC1)
new_NYC1 = new_NYC1[-4]
colnames(new_NYC1)

library(GGally)
new_NYC1$accommodates = as.numeric(new_NYC1$accommodates)
IndVariables = new_NYC1[c(5,21,22,26,27)]
ggpairs(IndVariables)

# B. 
# Partitioning 
set.seed(20)
new_NYC1 = sample_n(new_NYC1, 15690)
training = slice(new_NYC1, 1:9576)
validation = slice(new_NYC1, 9577:15690)

fitall = lm(formula = log_price~accommodates + number_of_reviews + review_scores_rating + room_type.Private.room + room_type.Shared.room, data = training)
fitall
summary(fitall)
mymodel = step(fitall, direction = "backward")

# new model without number of reviews R-square
fitall1 = lm(log_price~accommodates + review_scores_rating + room_type.Private.room + room_type.Shared.room, data = training)
fitall1
summary(fitall1)

# Prediction and RMSE
library(forecast)
pred1 = predict(fitall1, training)
accuracy(pred1, training$log_price)
pred2 = predict(fitall1, validation)
accuracy(pred2, validation$log_price)

#3
#I. K-nn
table(new_NYC1$cleaning_fee)
myRental = data.frame(id = "000", log_price = 2, accommodates= 2, number_of_reviews = 2, review_scores_rating = 40, room_type.Private.room = 1, room_type.Shared.room = 0)
NYCknn = new_NYC1[c(1,2,5,21,22,26,27,9)]
trainknn = training[c(1,2,5,21,22,26,27,9)]
validknn = validation[c(1,2,5,21,22,26,27,9)]
NYCknn.norm = NYCknn
train.norm = trainknn
valid.norm = validknn
library(caret)
norm.values = preProcess(train.norm[,2:7], method = c("center", "scale"))
train.norm[,2:7] = predict(norm.values, trainknn[,2:7])
valid.norm[,2:7] = predict(norm.values, validknn[,2:7])
NYCknn.norm[,2:7] = predict(norm.values, NYCknn[,2:7])
new.norm = predict(norm.values, myRental)

#Optimal value for K
library(FNN)
nn = knn(train = train.norm[,2:7], test = new.norm[,2:7], cl = train.norm[,8], k = 92)
row.names(train.norm)[attr(nn, "nn.index")]
nn
levels(new_NYC1$neighbourhood) #620 Neighbourhoods
sqrt(9576) #98 neighbourhoods for analysis
library(caret)
accuracy.df = data.frame(k = seq(1, 98, 1), accuracy = rep(0, 98))
for(i in 1:98) {
  knn.pred = knn(train.norm[,2:7], valid.norm[,2:7], cl = train.norm[,8], k=i)
  accuracy.df[i,2] = confusionMatrix(knn.pred, valid.norm[,8])$overall[1]
}
accuracy.df
max(accuracy.df$accuracy) # max value for accuracy neighbour 92

#II. Naive Bayes
NYCnaive = new_NYC1
summary(new_NYC1$log_price)
NYCnaive$log_price = as.numeric(NYCnaive$log_price)
NYCnaive$log_price = cut(NYCnaive$log_price, breaks = c(1.6,4.2,4.7,5.1,7.4), labels = c("Student Budget", "Below Average", "Above Average", "Pricey Digs"))
summary(NYCnaive$log_price)
set.seed(20)
selected.var = c(2,5,21,22,26,27)
train.index <- sample(c(1:dim(NYCnaive)[1]), dim(NYCnaive)[1]*0.6)
train.df <- NYCnaive[train.index, selected.var]
valid.df <- NYCnaive[-train.index, selected.var]
View(train.df)

library(e1071)
price.nb <- naiveBayes(log_price~., data=train.df)
price.nb
pred.class = predict(price.nb, newdata = train.df)
confusionMatrix(pred.class, train.df$log_price)
pred.class1 = predict(price.nb, newdata = valid.df)
confusionMatrix(pred.class1, valid.df$log_price)
summary(NYCnaive$log_price)
4551/15690

pred3 = predict(fitall1, training, type = "class")
confusionMatrix(pred3, training$log_price)
pred4 = predict(fitall1, validation, type = "class")
confusionMatrix(pred4, validation$log_price)

#III. Decision tree
new_NYC2 = new_NYC[c(2,4,6,22,23,9)]
View(new_NYC2)
which(new_NYC2$cancellation_policy == "super_strict_30" | new_NYC2$cancellation_policy == "super_strict_60")
new_NYC2 <- new_NYC2[-c(395,14325), ]
dim(new_NYC2)
table(new_NYC2$cancellation_policy)
new_NYC2$cancellation_policy <- as.factor(new_NYC2$cancellation_policy)
new_NYC2$cancellation_policy <- droplevels(new_NYC2$cancellation_policy)
table(new_NYC2$cancellation_policy)

set.seed(20)
new_NYC2 = sample_n(new_NYC2, 15690)
trainingTree = slice(new_NYC2, 1:9576)
validationTree = slice(new_NYC2, 9577:15690)
install.packages("rpart")
install.packages("rpart.plot")
library(rpart)
library(rpart.plot)
str(trainingTree)
summary(trainingTree)

#Cross-validation
df.cv <- rpart(cancellation_policy~., data=trainingTree, method='class',cp=0.00001,minsplit=5)
printcp(df.cv)
pruned.ct <- prune(df.cv,cp=df.cv$cptable[which.min(df.cv$cptable[,'xerror']),'CP'])
length(pruned.ct$frame$var[pruned.ct$frame$var=='<leaf>'])
rpart.plot(pruned.ct,type=4,extra=101,split.font=50,varlen=-10)

#Confusion Metric
df.pred <- predict(pruned.ct,validationTree,type='class')
confusionMatrix(df.pred,validationTree$cancellation_policy)
df.pred1 <- predict(pruned.ct,trainingTree,type='class')
confusionMatrix(df.pred1,trainingTree$cancellation_policy)