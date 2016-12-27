
#Data extraction and data wrangling

train = read.csv("train.csv")
test  = read.csv("test.csv")

# converting Survied coloum to factor

# train$Survived = as.factor(train$Survived)
# train$Pclass = as.factor(train$Pclass)


# NA's correspond to 20% of the data. Hence, removing NA entries would lead to huge data loss.
# So, we shall impute the missing vlaues usign "mice" package

library(mice)

init_train = mice(train, maxit = 0)
init_test = mice(test, maxit = 0)

predM_train = init_train$predictorMatrix
predM_test = init_test$predictorMatrix

predM_train[,c("PassengerId", "Name","Ticket","Cabin")] = 0
predM_test[,c("PassengerId", "Name","Ticket","Cabin")] = 0

imp_train = mice(train , m = 5 , predictorMatrix = predM_train)
imp_test = mice(test , m = 5 , predictorMatrix = predM_test)

train = complete(imp_train)
test = complete(imp_test)

# Split Train data into test and train data using sample.split

library("caTools")
train_split = sample.split(train, SplitRatio = 2/3)
train_new = subset(train, train_split == TRUE)
test_new = subset(train, train_split == FALSE)

#creatign a logistic regression model using all variables in train_new

predict_log1 = glm(Survived ~ Pclass + Sex + Age + SibSp + Parch + Fare , data = train_new)

#summary, identifying the significant factors and looking at AIC

summary(predict_log1)
#AIC = 480.05

#predicting on test data 
test_new_predict = predict(predict_log1, newdata = test_new, type = "response")

#testing the out of sample accuracy of the model using table function
table(test_new$Survived, test_new_predict > 0.5)

#accuracy = 80.53% , sensitivity = 71.30% , specificity = 86.33%

#creating a new logistic regression model by removing variables Parch, Fare
predict_log2 = glm(Survived ~ Pclass + Sex + Age + SibSp , data = train_new)
summary(predict_log2)

#AIC = 478.3

#predicting on test data 
test_new_predict = predict(predict_log2, newdata = test_new, type = "response")

#testing the out of sample accuracy of the model using table function
table(test_new$Survived, test_new_predict > 0.5)

#Accuracy = 81.20% , sensitivity = 73.04% , specificity = 86.33%

# WE WILL CHOOSE "predict_log2" AS OUR MODEL AND USE ON TEST DATA TO CREATE THEIR PREDICTIONS

test_predict = predict(predict_log2, newdata = test, type = "response")

#creating a new coloum in the test data frame which tells us if the passenger survived or not

survive <- function(pred1){
  vec = integer()
  for (i in 1:418) {
    if(is.na(pred1[i]) == T){
      vec[i] = NA
    }
    else if(pred1[i] > 0.5){
      vec[i] = 1
    }
    else{
      vec[i] = 0
     }
  }
  return(vec)
}

# Including the predicted suivided coloum in test data frame

test$Survived = survive(test_predict)


