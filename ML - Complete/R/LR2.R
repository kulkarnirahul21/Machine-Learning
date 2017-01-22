setwd("C:/Machine-Learning/ML - Complete/1 - Linear Regression/Assignment")
data <- read.csv("train.csv")
data <- data[1:40000,]
summary(data)
###############
data1 <- data
data1 <- na.omit(data1)
rm(data1)
####################### Data cleanng ##########
meanOfPC2 <- mean(data$Product_Category_2,na.rm = T)
meanOfPC3 <- mean(data$Product_Category_3,na.rm = T)
a <- which(is.na(data$Product_Category_2))
b <- which(is.na(data$Product_Category_3))
data$Product_Category_2[a] <- meanOfPC2
data$Product_Category_3[b] <- meanOfPC3
summary(data)
rm(a,b,meanOfPC3,meanOfPC2)
############################ EDA ##################
unique(data$User_ID)
data[data$User_ID == 1000001,]
length(unique(data$Product_ID))
unique(data$City_Category)
unique(data$Stay_In_Current_City_Years)
str(data)
##################################################
library(caret)
set.seed(22)
trainIndex <- createDataPartition(data$User_ID,p=0.8,list = F)
train <- data[trainIndex,]
test <- data[-trainIndex,]
rm(data,trainIndex)
names(train)
model <- lm(Purchase ~ User_ID + Product_ID + Gender + Age + Occupation + City_Category + Stay_In_Current_City_Years + Marital_Status +
              Product_Category_1 + Product_Category_2 + Product_Category_3, data = train)
summary(model)
model1 <- lm(Purchase ~ Gender + Age + Occupation + City_Category + Stay_In_Current_City_Years + Marital_Status +
              Product_Category_1 + Product_Category_2 + Product_Category_3, data = train)
summary(model1)

model2 <- lm(Purchase ~ Gender + Age + Occupation + City_Category + Marital_Status +
               Product_Category_1 + Product_Category_2 + Product_Category_3, data = train)
summary(model2)

#######################################
train$Age <- ifelse(train$Age == "0-17", 10,
            ifelse(train$Age == "18-25", 21,
                   ifelse(train$Age == "26-35",30, 
                          ifelse(train$Age == "36-45", 40,
                                 ifelse(train$Age == "46-50", 48,
                                        ifelse(train$Age == "55+", 57,0))))))
train$Age <- as.numeric(train$Age)


model3 <- lm(Purchase ~ Gender + Age + Occupation + City_Category + Marital_Status +
               Product_Category_1 + Product_Category_2 + Product_Category_3, data = train)
summary(model3)

model5 <- lm(Purchase ~ Gender  + City_Category + 
               Product_Category_1 + Product_Category_2 + Product_Category_3, data = train)
summary(model5)

plot(model4)

step(model2, direction="forward")

anova(model2,model3)

test$User_ID <- NULL 
test$Product_ID <- NULL
test$Age <- NULL
test$Stay_In_Current_City_Years <- NULL
test$Marital_Status <- NULL

test$Mpurchase <- predict(model4,newdata = test)
mape = abs(actual - predicted)/actual