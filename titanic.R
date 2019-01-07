library(plyr)
library(ggplot2)
library(caret)

# load training and test sets
train = read.csv("~/Downloads/all/train.csv")
test = read.csv("~/Downloads/all/test.csv")

# data summary
survival_count = count(train, vars="Survived")

## deal w/ missing values
apply(train,2, function(x) mean(is.na(x)))
# tried getting rid of rows without Age values. It increased the validation accuracy but it didn't increase the test accuracy.
median_age = median(train$Age[!is.na(train$Age)])
train$Age[is.na(train$Age)] = median_age

## summarize some existing features
# proportion of each gender that survived and died
prop.table(table(train$Sex,train$Survived),1)

### create new features and test for correlation
train['fam_size'] = train['SibSp'] + train['Parch']
by_fam_size = ggplot() + geom_bar(aes(x=fam_size,fill=Survived), train, position="fill")

train['fare_per_person'] = train['Fare'] / (train['fam_size'] + 1)
by_fare_pp = ggplot(data = train,aes(x=Fare,fill=as.factor(Survived))) + geom_histogram(binwidth=20,position="fill")
# the higher the fare, the more likely the passenger is to survive

# which floor of the boat their cabin was on
train['cabin'] = lapply(train['Cabin'],as.character)
train['deck'] = sapply(train[['Cabin']], substr, 1,1)

# get name prefix
train['Name'] = as.character(train[['Name']])
train['prefix'] = regmatches(train[['Name']], regexpr("[[:alpha:]]+[.]", train[['Name']]))
unique(train$prefix)
train$prefix[train$prefix == "Lady."] = "Miss."
train$prefix[train$prefix == "Mlle."] = "Miss."
train$prefix[train$prefix == "Countess."] = "Miss."
train$prefix[train$prefix == "Ms."] = "Miss."
train$prefix[train$prefix == "Mme."] = "Mrs."

train$prefix[train$prefix == "Master."] = "Mr."
train$prefix[train$prefix == "Don."] = "Mr."
train$prefix[train$prefix == "Rev."] = "Mr."
train$prefix[train$prefix == "Col."] = "Mr."
train$prefix[train$prefix == "Capt."] = "Mr."
train$prefix[train$prefix == "Jonkheer."] = "Mr."
train$prefix[train$prefix == "Sir."] = "Mr."
train$prefix[train$prefix == "Major."] = "Mr."

by_prefix = ggplot(data=train,aes(x=prefix,fill=as.factor(Survived))) + geom_bar(position="fill")
# those with prefix Mr. are less likely to survive compared to those with prefixes Miss. or Mrs.

by_age = ggplot(data=train,aes(x=Age,fill=as.factor(Survived))) + geom_histogram(position="fill",binwidth =10)
# the younger the passenger, the more likely they survived

# drop unnecessary columns
to_drop = c("Ticket", "Cabin")
train = train[,!(names(train) %in% to_drop)]
test = test[,!(names(test) %in% to_drop)]

train[,2] = sapply(train[,2], as.factor)
test[,2] = sapply(test[,2], as.factor)

# convert some columns to factor
factor_train = match(c("Pclass","Sex","Embarked"),colnames(train))
for (i in factor_train) {
  train[,i] = sapply(train[,i], as.factor)
}
factor_test = match(c("Pclass","Sex","Embarked"),colnames(test))
for (i in factor_test) {
  test[,i] = sapply(test[,i], as.factor)
}

# age by bucket
# train$Age[which(train$Age > 0 & train$Age < 12)] = "0-11"
# train$Age[which(train$Age >= 12 & train$Age < 18)] = "12-17"
# train$Age[which(train$Age >= 18 & train$Age < 25)] = "18-24"
# train$Age[which(train$Age >= 25 & train$Age < 40)] = "25-39"
# train$Age[which(train$Age >= 40 & train$Age < 65)] = "40-64"
# train$Age[which(train$Age >= 65)] = "65+"
# train$Age[which(is.na(train$Age))] = "25-39"
# 
# by_age = ggplot() + geom_bar(aes(x=age, fill=survived),data=train)



### initial plots
by_sex = ggplot() + geom_bar(aes(x=Survived, fill=Sex), data=train)
by_class = ggplot() + geom_bar(aes(x=Survived, fill=as.factor(train$Pclass)), data=train)
by_title = ggplot() + geom_bar(aes(x=as.factor(prefix), fill=survived), data=train)


### fit and predict model
to_test = c("Pclass","Sex","SibSp","Parch","Embarked","Age")
new_train = train[,c("Survived",to_test)]

# basic glm model
base_model = glm(Survived ~ ., family = binomial(link="logit"), new_train)
summary(base_model)

# embarked, parch, sibsp are more statistically insignificant
# age seems significant but should impute or figure out how to deal with missing values
# for each additional male passenger, the log odds of that passenger surviving is -2.64
# for each addiitonal passenger, being first-class changes the log odds of that passenger surviving by 2.49

# split data into training and validation sets
sample <- sample.int(n = nrow(train), size = floor(.80*nrow(train)), replace = F)
train_set <- new_train[sample,]
val_set <- new_train[-sample,]

# get accuracy on validation sett
predictions = predict(base_model, val_set)
fitted.results <- ifelse(predictions > 0.5,1,0)
misClasificError = mean(fitted.results != val_set$Survived)
print(paste('Accuracy',1-misClasificError))

# predict model on test set
predictions = predict(base_model, test, na.action = na.pass)
fitted.results <- ifelse(predictions > 0.5,1,0)

results = cbind(test[,1],fitted.results)

colnames(results) = c("PassengerId","Survived")
write.csv(results, "~/Desktop/titanic/predictions.csv", row.names=F)

# glm model w/ 10-fold cv
#### k-fold cross validation ####
train_control <- trainControl(method="cv", number=10, savePredictions = TRUE)

folds  <- cut(seq(1,nrow(new_train)),breaks=10,labels=FALSE)
result <- list()
acc <- list()
for(i in 1:10){
  testIndexes <- which(folds==i,arr.ind=TRUE)
  testData    <- new_train[testIndexes, ]
  trainData   <- new_train[-testIndexes, ]
  model       <- glm(Survived ~ .,family=binomial,data=trainData)
  predictions = predict(model, val_set,type='response')
  fitted.results <- ifelse(predictions > 0.5,1,0)
  result[[i]] <- fitted.results
  misClasificError = mean(predictions != testData$Survived)
  acc[[i]] <- 1-misClasificError
}
result
acc

# fit and predict model with cross-val (without Age variable)
cv_model = train(Survived ~ ., family="binomial", method="glm",data = new_train, trControl=train_control, tuneLength=5)
summary(cv_model)
predictions = predict(cv_model, test)

results = data.frame(test[,1],predictions)

# write results to a csv in the right format
colnames(results) = c("PassengerId","Survived")
write.csv(results, "~/Desktop/titanic/predictions.csv", row.names=F)
  
