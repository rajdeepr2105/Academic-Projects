#a.Import the file to R and convert the 
#following variables to factors: CARRIER, DEST, ORIGIN, Weather, 
#DAY_WEEK, and Flight_Status
library(rpart) 
library(rpart.plot)
library(caret)
library(e1071)
library(magrittr)
library(dplyr)

FlightDelaysSpring2022<-read_excel('FlightDelaysSpring2022.xlsx')
flight_data_1<-data.frame(FlightDelaysSpring2022)
View(flight_data_1)
cols <- c("CARRIER", "DEST", "ORIGIN", "Weather","Flight_Status")

flight_data_1 %<>%
  mutate_each_(funs(factor(.)),cols)


flight_data_1
sapply(flight_data_1,class)

#converting day_week into factor.

flight_data_1$DAY_WEEK <- factor(flight_data_1$DAY_WEEK, levels=1:7,
                      labels=c("Monday", "Tuesday", "Wednesday",
                               "Thursday", "Friday", "Saturday","Sunday"))
flight_data_1$DAY_WEEK
class(flight_data_1$DAY_WEEK)
View(flight_data_1)

library(dplyr)

#BinningOfData
flight_data_2<-flight_data_1 %>% mutate(CRS_DEP_TIME_BIN = ntile(CRS_DEP_TIME,n=8))


View(flight_data_2)

numberOfRows <- nrow(flight_data_2)
set.seed(1)
training.ind <- sample(numberOfRows, numberOfRows*0.7)

print(training.ind)
training.data <- flight_data_2[training.ind, ]
validation.data <- flight_data_2[-training.ind, ]
View(training.data)
View(validation.data)


#.ct <- rpart(Flight_Status ~ CRS_DEP_TIME_BIN+CARRIER+DEST+ORIGIN+
             #Weather+DAY_WEEK, data = training.data, 
             #method = "class", cp = 0.00001, maxdepth = 2, minsplit = 20)
#drafting a classification tree
#.ct <- rpart(Flight_Status ~ ., data = training.data %>% select(1,2,4,8,9,10,13), 
             #method = "class", cp = 0.00001, maxdepth = 2, minsplit = 20)

.ct <- rpart(Flight_Status ~ CRS_DEP_TIME_BIN+CARRIER+DEST+ORIGIN+
              Weather+DAY_WEEK, data = training.data, 
            method = "class", cp = 0.00001, maxdepth = 2, minsplit = 20)

printcp(.ct)
prp(.ct, type = 1, extra = 1, under = FALSE, split.font = 1, varlen = -10)

ct.pred <- predict(.ct, validation.data, type = "class")
ct.pred

confusionMatrix(ct.pred, as.factor(validation.data$Flight_Status))


.ct5 <- rpart(Flight_Status ~ CRS_DEP_TIME_BIN+CARRIER+DEST+ORIGIN+
                Weather+DAY_WEEK, data = training.data, 
              method = "class", cp = 0.00001, maxdepth = 5, minsplit = 20)

printcp(.ct5)

prp(.ct5, type = 1, extra = 1, under = FALSE, split.font = 1, varlen = -10)

ct.pred_5 <- predict(.ct5, validation.data, type = "class")
confusionMatrix(ct.pred_5, as.factor(validation.data$Flight_Status))
printcp(.ct5)

library(randomForest)


rf_1 <- randomForest(Flight_Status ~ ., data = training.data, 
                   ntree = 500, mtry = 12, nodesize = 1, importance = TRUE, sampsize = 1500) 

varImpPlot(rf_1, type = 1)

rf.pred <- predict(rf_1, validation.data)
confusionMatrix(rf.pred, validation.data$Flight_Status)
