#loading the necessary libraries
library(gains)
library(caret)
library(ROCR)
library(readxl)
library(dplyr)
library(rpart)
library(rpart.plot)


#reading excel
airlines.df <- read_excel("test.xlsx")
View(airlines.df)


#data cleansing
airlines1.df <-  airlines.df[ , -c(1,8,9,10,11,12,13,14,15,16,17,18,19,20,21,22,23)]
View(airlines1.df)


#converting to factors
sapply(airlines1.df,class)

cols<-c('Gender','Customer Type','Type of Travel','Class','satisfaction')
airlines1.df[cols]<-lapply(airlines1.df[cols], factor)

sapply(airlines1.df,class)

new_col_names<-c('Gender','Customer_Type','Age','Type_of_Travel','Class','Flight_Distance',
                 'satisfaction')
colnames(airlines1.df)<-new_col_names
View(airlines1.df)


#partitioning the data
numberOfRows <- nrow(airlines1.df)
set.seed(1)
train.index <- sample(numberOfRows, numberOfRows*0.6)
train.df <- airlines1.df[train.index, ]
valid.df <- airlines1.df[-train.index, ]
View(train.df)
View(valid.df)


#classification tree

.ct <- rpart(satisfaction ~., data = train.df, method = "class", cp = 0.000583601, maxdepth = 30, minsplit = 10)
printcp(.ct)
prp(.ct, type = 1, extra = 1, under = FALSE, split.font = 1, varlen = -10)


#predicting using training data
ct.pred <- predict(.ct, valid.df, type = "class")
ct.pred

confusionMatrix(ct.pred, as.factor(valid.df$satisfaction))
test_case<-read_excel('test_case.xlsx')
test_case.df<-data.frame(test_case)
class(test_case.df)
View(test_case.df)
ct.pred_1 <- predict(.ct,test_frame, type = "class")
ct.pred_1


test_frame<-data.frame(Gender='Male',Customer_Type='Loyal Customer',Age=43,Type_of_Travel='Personal Travel',
                       Class='Business',Flight_Distance=1235)

View(test_frame)

cols_1<-c('Gender','Customer_Type','Type_of_Travel','Class')
test_frame[cols_1]<-lapply(test_frame[cols_1], factor)

sapply(test_frame,class)

ct.pred_1 <- predict(.ct,test_frame, type = "class")
ct.pred_1

#Confusion matrix
confusionMatrix(ct.pred, as.factor(valid.df$satisfaction))


#random forest
rf <- randomForest(satisfaction ~ ., data = train.df, 
                    ntree = 5000, mtry = 1, nodesize = 1, importance = TRUE, sampsize = 2000) 

#plot the variables by order of importance
varImpPlot(rf, type = 1)

#create a confusion matrix

rf.pred <- predict(rf, valid.df)
rf.pred
confusionMatrix(rf.pred, valid.df$satisfaction)


cv.ct <- rpart(satisfaction ~ ., data = train.df, method = "class", 
               control = rpart.control(cp = 0.00000005, minsplit = 5, xval = 5))

# use printcp() to print the table. 
printcp(cv.ct)
prp(cv.ct, type = 1, extra = 1, split.font = 1, varlen = -10)  


library(adabag)
library(rpart)
library(caret)

boost <- boosting(satisfaction ~ ., data = train.df)
pred <- predict(boost, valid.df)
confusionMatrix(as.factor(pred$class), valid.df$satisfaction)


#==========================================================================

#preparing the data for logistic regression
airlines1.df$satisfied <- ifelse(airlines1.df$satisfaction == "satisfied", 1,0)
View(airlines1.df)
airlines2.df<-airlines1.df[-7]
View(airlines2.df)


set.seed(19)
numberOfRows2 <- nrow(airlines2.df)
train2.index <- sample(numberOfRows2, numberOfRows2*0.6)
train2.df <- airlines2.df[train2.index, ]
validation2.df <- airlines2.df[-train2.index, ]
View(validation2.df)
#Logistic regression
airlines1.df.glm <- glm(satisfied ~., data = train2.df, family = "binomial")
summary(airlines1.df.glm)
options(scipen=999)

confusionMatrix(table(predict(airlines1.df.glm, newdata = validation2.df, 
                              type="response") >= 0.1, validation2.df$satisfied == 1))

anova(airlines1.df.glm)
#predicting
logit.reg.pred <- predict(airlines1.df.glm, validation2.df[, -7], type = "response") 
logit.reg.pred

test_frame<-data.frame(Gender='Male',Customer_Type = 'Loyal Customer',Age = 45,
                       Type_of_Travel = 'Business travel',
                       Class = 'Eco',
                       Flight_Distance = 800)
View(test_frame)

logit.reg.pred_1<- predict(airlines1.df.glm, test_frame, type = "response") 
logit.reg.pred_1

summary(airlines.df$Age)
boxplot(airlines.df$Age)
summary(airlines.df$`Flight Distance`)
boxplot(airlines.df$`Flight Distance`)

boxplot(airlines.df$`Flight Distance`, ylab='miles',main='Boxplot for Flight_Distance')
boxplot(airlines.df$Age, ylab='Age',main='Boxplot for Age')


plot(airlines1.df$`Flight_Distance`,airlines1.df$`Cu`, main = "Main title",
     xlab = "X axis title", ylab = "Y axis title")
sapply(airlines1.df,class)

cor(airlines1.df$'Age',airlines1.df$Flight_Distance)

View(is.na(airlines.df))
na.omit(airlines.df)
