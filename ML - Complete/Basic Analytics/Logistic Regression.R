rm(list=ls(all=TRUE))
setwd("C:/Users/Rahul/Desktop/")
mydata <- read.csv("http://www.ats.ucla.edu/stat/data/binary.csv")
head(mydata)
summary(mydata)
attach(mydata)
str(mydata)
mydata$rank <- factor(mydata$rank)
mylogit1 <- glm(admit ~ gre + gpa + rank, 
                data = mydata, 
                family = "binomial")
mylogit2 <- glm(admit ~ gre + gpa, 
               data = mydata, 
               family = "binomial")

summary(mylogit1)
summary(mylogit2)
#We can get odds ratio by exponentiating

exp(coef(mylogit1))

#predicting the probabilities on the same data
mydata$pred <- predict(mylogit, 
                       newdata = mydata, 
                       type = "response")
#Logit on bank

univ <- read.table("univComp.csv", sep=",", header=T)
univ <- subset(univ, select=-c(1,3))
summary(univ)

univS <- univ[,c(1:8,17)]
train <- univS[1:3800,]
test <- univS[3801:4986,]
mylogit <- glm(loan~., 
               data = train, 
               family = "binomial")

summary(mylogit)
anova(mylogit, test="Chisq")

pred <- predict(mylogit, 
                     newdata = test, 
                     type = "response")

pred[pred>0.5]=1
pred[pred<=0.5]=0

pred

table(test$loan,pred)

library(ROCR)
pr <- prediction(pred, univ$loan)
prf <- performance(pr, measure = "tpr", x.measure = "fpr")
plot(prf)
auc <- performance(pr, measure = "auc")
auc <- auc@y.values[[1]]
auc
