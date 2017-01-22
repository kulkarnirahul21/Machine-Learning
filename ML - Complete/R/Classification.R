setwd("C:/Users/kulka/Downloads/Machine-Learning/ML - Complete/R/Data")

data <- read.csv("flags.csv")
data <- data[,-1]
index <- createDataPartition(data$landmass,p = 0.9,list = F)
trainSet <- data[index,]
testSet <- data[-index,]
predictVar <- "religion"
predictors<-names(trainSet)[!names(trainSet) %in% predictVar]
set.seed(22)
library(caret)
model_decision_trees<-train(trainSet[,predictors],trainSet[,predictVar],method='lm')

#####################################################
data <- read.csv("germancredit.csv")
summary(data)
index <- createDataPartition(data$GoodCredit,p = 0.8,list = F)
trainSet <- data[index,]
testSet <- data[-index,]
#########################################################
## decision trees to predict Good Credit #######
#########################################################
library(rpart)
dmodel1 <- rpart(GoodCredit ~ .,data = trainSet, method = "class")

dmodel2 <- rpart(GoodCredit ~ .,
                cp = 0.001,   # Set complexity parameter
                maxdepth = 6, # Set maximum tree depth
                minbucket = 5,
                method = "class",
                data = trainSet)
summary(dmodel2)
plot(dmodel1)
text(dmodel1)
# To find the variable importance
dmodel1$variable.importance

### C 5.0
library(C50)
trainSet$GoodCredit <- as.factor(trainSet$GoodCredit)
dmodel3 <- C5.0(GoodCredit ~ .,
                 trials = 10,
                 data = trainSet)


# there are few beautiful packages to plot the same
library(rattle)
library(rpart.plot)
library(RColorBrewer)

fancyRpartPlot(dmodel1)

testSet$predicted <- predict(dmodel3, testSet, type = "class")
confusionMatrix(testSet$GoodCredit,testSet$predicted)

######################################################################
## Random Forest ####################################################
#####################################################################

library(randomForest)
rfmodel <- randomForest(GoodCredit ~.,
                        data = trainSet,
                        ntree=1000,            # Number of trees to grow
                        mtry=3)                # Number of branch variables
summary(rfmodel)
rfmodel
rfmodel$classes
rfmodel$type
rfmodel$forest

  
testSet$predicted <- predict(rfmodel, testSet, type = "class")
confusionMatrix(testSet$GoodCredit,testSet$predicted)



# Support vector machines in R ################
library("e1071")
svmmodel1 <- svm(GoodCredit ~.,data = trainSet)
svmmodel2 <- svm(GoodCredit ~.,
                 kernel = "radial",
                 gamma = 1,
                 data = trainSet)
testSet$predicted <- predict(rfmodel, testSet, type = "class")
confusionMatrix(testSet$GoodCredit,testSet$predicted)

#### KNN #######################################
library(class)
data <- read.csv("germancredit.csv")
summary(data)
index <- createDataPartition(data$GoodCredit,p = 0.8,list = F)
trainSet <- data[index,]
testSet <- data[-index,]


prc_train_labels <- trainSet$GoodCredit
prc_test_labels <- testSet$GoodCredit

knnmodel <- knn(train = trainSet, test = testSet,cl = prc_train_labels, k=10)
# you will get an error, conver all that to numeric and run the same code.