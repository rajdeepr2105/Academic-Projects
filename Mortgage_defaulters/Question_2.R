getwd()




library(rpart)
library(rpart.plot)
library(e1071)
library(randomForest)
library(readxl)




MortgageDefaulters.df <- read_excel("MortgageDefaulters-Thouin.xlsx", sheet="MortgageDefaulters")
View(MortgageDefaulters.df)



MortgageDefaulters.df$OUTCOME <- factor(MortgageDefaulters.df$OUTCOME)



nrows <- nrow(MortgageDefaulters.df)



set.seed(22)
train.index <- sample(nrows, nrows*0.6)
train.df <- MortgageDefaulters.df[train.index, ]
valid.df <- MortgageDefaulters.df[-train.index, ]
View(train.df)
View(valid.df)
summary(train.df)





.ct <- rpart(OUTCOME ~ ., data = train.df, method = "class", cp = 0, maxdepth = 4, minsplit = 1)




printcp(.ct)
prp(.ct, type = 1, extra = 1, under = FALSE, split.font = 1, varlen = -10)






ct.pred <- predict(.ct, valid.df, type = "class")
confusionMatrix(ct.pred, as.factor(valid.df$OUTCOME))







library(caret)





MortgageDefaulters3.df <- upSample(MortgageDefaulters.df[,-c(9)], MortgageDefaulters.df$OUTCOME, list = FALSE, yname = "Class")
View(MortgageDefaulters3.df)
summary(MortgageDefaulters3.df)





nrows <- nrow(MortgageDefaulters3.df)





set.seed(22)
train.index <- sample(nrows, nrows*0.6)
train.df <- MortgageDefaulters3.df[train.index, ]
valid.df <- MortgageDefaulters3.df[-train.index, ]
View(train.df)
View(valid.df)
summary(train.df)
print(train.df[1:10,])






.ct2 <- rpart(Class ~ ., data = train.df, method = "class", cp = 0, maxdepth = 4, minsplit = 1)





prp(.ct2, type = 1, extra = 1, under = FALSE, split.font = 1, varlen = -10)
printcp(.ct2)






ct.pred2 <- predict(.ct2, valid.df, type = "class")
confusionMatrix(ct.pred2, as.factor(valid.df$Class))







rf<- randomForest(as.factor(Class) ~ ., data = train.df,
                  ntree = 500, mtry = 6 , nodesize = 1, importance = TRUE, sampsize = 1250)





varImpPlot(rf, type = 1)





valid.df$Class <- factor(valid.df$Class)
rf.pred <- predict(rf, valid.df)
confusionMatrix(rf.pred, valid.df$Class)
