#load necessary packages
library(caTools)
library(MASS)
library(ggord)

#load the dataset
wine <- read.csv("https://github.com/natalie-elliot/activity-4/blob/main/Wine.csv")

#view data structure
str(wine)

#view statistical summary
summary(wine)

#subset the data
wine2 <- wine[, c(1, 3, 7, 8, 9, 10, 11, 12, 14)]

#verify subsetting by examining first 6 rows of data
head(wine2)

#convert data type to factor
wine2$Customer_Segment <- as.factor(wine2$Customer_Segment)

#confirm conversion by examining data structure
str(wine2)

set.seed(5623) #for randomization

#split the data 70:30
split <- sample.split(wine2$Customer_Segment, SplitRatio=0.7)
train <- subset(wine2, split==T)
test <- subset(wine2, split==F)

#run LDA using training dataset
discrim <- lda(Customer_Segment~., train)
discrim
attributes(discrim)
discrim$scaling #coefficient to transform observations to discriminant functions
discrim$counts # no. of observations per group
discrim$N #total no. of observations
discrim$svd #group standard deviation per LD function

#develop a predictive model using the training dataset
pre <- predict(discrim, train)
attributes(pre)
pre$class
pre$posterior #probability of belonging to a particular class
pre$x #calculated using the coefficient of all the variables

#histograms for LDA1 and LDA2
ldahist(data = pre$x[,1], train$Customer_Segment)
ldahist(data = pre$x[,2], train$Customer_Segment)

#plot the LDA model using the training model
ggord(discrim, train$Customer_Segment, xlim = c(-5, 7), ylim = c(-6,5))

#calculate the accuracy rate for the training set
pre1 <- predict(discrim, train)$class
tab <- table(Predicted = pre1, Actual = train$Customer_Segment)
tab
sum(diag(tab))/sum(tab)

#calculate the accuracy for the testing set
pre2 <- predict(discrim, test)$class
tab1 <- table(Predicted = pre2, Actual = test$Customer_Segment)
tab1
sum(diag(tab1))/sum(tab1)
