path <- "C:/Users/sayan/Desktop/DATA_ANALYSIS/New folder/Projects/titanic"
setwd(path)
getwd()

Traning_Data = read.csv("train.csv")
Testing_Data = read.csv("test.csv")


Testing_Data[Testing_Data == ""] = NA
Traning_Data[Traning_Data == ""] = NA

colSums(is.na(Traning_Data))
colSums(is.na(Testing_Data))


str(Traning_Data)
summary(Traning_Data)

####

Traning_Data$Survived = as.factor(Traning_Data$Survived)
Traning_Data$Sex = as.factor(Traning_Data$Sex)
Traning_Data$Pclass = as.factor(Traning_Data$Pclass)
Traning_Data$Embarked = as.factor(Traning_Data$Embarked)
Traning_Data$Parch = as.factor(Traning_Data$Parch)
Traning_Data$SibSp = as.factor(Traning_Data$SibSp)

####

Testing_Data$Pclass = as.factor(Testing_Data$Pclass)
Testing_Data$Sex = as.factor(Testing_Data$Sex)
Testing_Data$SibSp = as.factor(Testing_Data$SibSp)
Testing_Data$Parch = as.factor(Testing_Data$Parch)
Testing_Data$Embarked = as.factor(Testing_Data$Embarked)

####

New_DATA = merge(Traning_Data, Testing_Data, all = TRUE)
str(New_DATA)

colSums(is.na(New_DATA))

library(mice)
simple = New_DATA[c("Age", "Cabin", "Embarked","Survived")]
summary(simple)
set.seed(144)
imputed = complete(mice(simple))
summary(imputed)
New_DATA$Age = imputed$Age
New_DATA$Embarked = imputed$Embarked
New_DATA$Survived = imputed$Survived

New_DATA$Cabin = NULL

####

New_DATA$PassengerId = NULL
New_DATA$Name = NULL
New_DATA$Fare = NULL
New_DATA$Ticket = NULL

ML = glm(Survived~.,data = New_DATA, family = binomial())

summary(ML)


ML01 = glm(Survived~I(Pclass == "2") + I(Sex == "male") + Age + I(Embarked == "S"), data = New_DATA, family = binomial())
summary(ML01)

vif(ML01)

prediction <- predict(ML01,newdata = New_DATA,type="response")

prediction

#### write.csv(prediction,"pred.csv")

rocCurve   <- roc(response = New_DATA$Survived, predictor = factor(prediction, ordered =TRUE), 
                  levels = rev(levels(New_DATA$Survived)))
New_DATA$Survived <- as.factor(New_DATA$Survived)

threshold<-as.numeric(coords(rocCurve,"best")[1])

predclass <-ifelse(prediction>threshold,1,0)

Confusion <- table(Predicted = predclass,Actual = New_DATA$Survived)
AccuracyRate <- sum(diag(Confusion))/sum(Confusion)

New_DATA01 = merge(Traning_Data, Testing_Data, all = TRUE)

P = cbind(New_DATA01, predclass)

submission = read.csv("gender_submission.csv")

gender_submission_New3 = merge(submission,P, by = "PassengerId")

gender_submission_New3[,2:12] = NULL

write.csv(gender_submission_New3,"gender_submission_New3.csv", row.names = FALSE)
