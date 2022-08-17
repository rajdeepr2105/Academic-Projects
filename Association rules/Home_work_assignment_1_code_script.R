#PARTA Data exploration



getwd()


#
University.df <- read.csv("Universities2022.csv")



library(tidyverse)
#v <- sapply(University.df, is.numeric)
#v



df <- University.df[,sapply(University.df, is.numeric)]








summary(University.df)

# Used University.df instead of df

University_list <- as.list(University.df)
str(University_list)





str(University.df)



University_stats <- data.frame(mean=sapply(df, mean, na.rm = TRUE),
                               median=sapply(df, median, na.rm = TRUE),
                               sd=sapply(df, sd, na.rm = TRUE),
                               min=sapply(df, min, na.rm = TRUE),
                               max=sapply(df, max, na.rm = TRUE),
                               count=sapply(df, length),
                               miss.val=sapply(df, function(x)
                                 sum(length(which(is.na(x)))))
)





#we can remove count funcation. Not asked in question
#we can use as integer to remove exponential values 


View(University_stats)






#view function is more formatted and summarized version.

#PARTB
University.df$AdmitRate <- University.df$Admits/University.df$Applicants * 100

University.df








#PARTC
hist(University.df$Applicants)



#PARTD
hist(University.df$AdmitRate)




#PARTE
boxplot(log(University.df$Enrolled) ~ University.df$Type, xlab = "Type", ylab = "Enrolled")

#Create a Box plot of Enrolled using Type as the by variable
boxplot(University.df$Enrolled ~ University.df$Type,
        col="orange", 
        main=" Enrolled BY Type", 
        ylab="Number of Students Enrolled", 
        xlab="Type",
        outline=FALSE)


#PARTF
plot(University.df$AdmitRate ~ University.df$GraduationRate, ylab = "Admit Rate", xlab = "Graduatio Rate")



#Association Rules



#PARTA



library(Matrix)
library(arules)



getwd()



Cosmetics.df <- read.csv("Cosmetics.csv")
View(Cosmetics.df)



Cosmetics.df <- Cosmetics.df[1:15]# Why did we do this ?
Cosmetics.df


Cosmetics.num <- ifelse(Cosmetics.df > 0, 1, 0)
View(Cosmetics.num)
class(Cosmetics.num)
str(Cosmetics.num)



  Cosmetics.trans <- as(Cosmetics.num, "transactions")
 

#A vector or Data frame must be in transaction form to mine the data

#PARTB



rules_a <- apriori(Cosmetics.trans,
                   parameter = list(supp= 0.03, conf = 0.25, maxlen = 2, target = "rules"))



inspect(sort(rules_a, by = "lift"))




#PARTD



rules_b <- apriori(Cosmetics.trans,
                   parameter = list(supp= 0.03, conf = 0.25, maxlen = 3, target = "rules"))



inspect(sort(rules_b, by = "lift"))

library(arules)
library(Matrix)
library(readxl)

Coursetopics.df <- read.csv("Coursetopics.csv")
View(Coursetopics.df)


Coursetopics.num <- ifelse(Coursetopics.df > 0, 1, 0)

Coursetopics.trans<-as(Coursetopics.num,"transactions")

rules_1 <- apriori(Coursetopics.trans, 
                   parameter = list(supp=0.000002, conf=0.05, maxlen=2, target="rules"))

inspect(sort(rules_1, by = "lift"))



