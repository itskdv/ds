#Fetching the data
titanic.train <- read.csv(file = "train.csv",stringsAsFactors = FALSE, header = TRUE)
titanic.test <- read.csv(file = "test.csv",stringsAsFactors = FALSE, header = TRUE)
#survived~ Pclass+Sex+Age+Sibsp+Parch+Fare+Embarked

#Adding new col for seperating after data cleaning
titanic.test$newcol <- FALSE
titanic.train$newcol <- TRUE

#Adding new col in test data to make it same as traonong data
titanic.test$Survived <- NA

#Checking the structure
str(titanic.test)
str(titanic.train)
 
#Removable of missing data (DATA Cleaning)
table(titanic.test$Sex)
#Combining the test and train data
titanic.full <- rbind(titanic.train,titanic.test)

#String Missing Value
sum(is.na(titanic.full$Embarked))
titanic.full[titanic.full$Embarked=='',"Embarked"] <- 'S'
#Integer missing value
median_age <- median(titanic.full$Age,na.rm = TRUE)
titanic.full[is.na(titanic.full$Age),"Age"] <- median_age
sum(is.na(titanic.full$Age))
sum(is.na(titanic.full$Fare))
median_fare<-median(titanic.full$Fare,na.rm = TRUE)
titanic.full[is.na(titanic.full$Fare),"Fare"] <- median_fare

#Catagorical classification
titanic.full$Pclass<- as.factor(titanic.full$Pclass)
titanic.full$Sex<-as.factor(titanic.full$Sex)
titanic.full$Embarked<- as.factor(titanic.full$Embarked)
str(titanic.full)
tail(titanic.full$Sex)
#Spliting the data back to Training and test
titanic.train <- titanic.full[titanic.full$newcol==TRUE,]
titanic.test <- titanic.full[titanic.full$newcol==FALSE,]

#CAtagorical classification of Survived after Spliting the data
titanic.train$Survived <- as.factor(titanic.train$Survived)


#Spliting the Traning data in 70 30 ration to check the Model
set.seed(2)
id<- sample(2,nrow(titanic.train),prob = c(0.7,0.3), replace = TRUE)
titanic.train_train <- titanic.train[id==1,]
titanic.train_test <- titanic.train[id==2,]


#Applying the Random forest
survived_modeleq <- as.formula("Survived  ~ Pclass+Sex+Age+SibSp+Parch+Fare+Embarked")
library(randomForest)
titanic.model <- randomForest(formula = survived_modeleq,data = titanic.train_train, ntree=500,mtry=3,node=0.01*nrow(titanic.train_train))
titanic.model
importance(titanic.model)
varImpPlot(titanic.model)

#Applying model on training.test data
Predict.model <- predict(titanic.model,newdata = titanic.train_test,type = "class")
Predict.model
install.packages('caret')
install.packages('e1071')
library(caret)
confusionMatrix(table(Predict.model,titanic.train_test$Survived))


Survived <- predict(titanic.model,newdata = titanic.test)
PassengerId <- titanic.test$PassengerId
output.df<- as.data.frame(PassengerId)
output.df$Survived<- Survived
write.csv(output.df,file = "Kaggle_submission.csv", row.names = FALSE)
