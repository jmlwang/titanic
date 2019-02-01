library(plyr)
library(ggplot2)
library(caret)
library(mice)

# load training and test sets
# train = read.csv("~/Desktop/projects/titanic2/train.csv")
# test = read.csv("~/Desktop/projects/titanic2/test.csv")
train = read.csv("~/Desktop/titanic/train.csv")
test = read.csv("~/Desktop/titanic/test.csv")

# data summary
summary(train)
lapply(train, class)

# how many survived
survival_count = count(train, vars="Survived")
survival_count
# proportion of each gender that survived and died
prop.table(table(train$Sex,train$Survived),1)

# replace empty vals in factor columns with NA
levels(train$Embarked)[nchar(levels(train$Embarked))==0] = NA
levels(train$Cabin)[nchar(levels(train$Cabin))==0] = NA
levels(train$Ticket)[nchar(levels(train$Ticket))==0] = NA
levels(train$Name)[nchar(levels(train$Name))==0] = NA
levels(train$Sex)[nchar(levels(train$Sex))==0] = NA

## deal w/ missing values
apply(train,2, function(x) mean(is.na(x)))
# drop cabin b/c 78% is missing 
# view counts of values in Embarked
count(train$Embarked)
# replace missing values with most common value (S)
train$Embarked[is.na(train$Embarked)] = "S"
test$Embarked[is.na(test$Embarked)] = "S"

### initial plots
ggplot() + geom_bar(aes(x=Survived, fill=Sex), data=train) # by_sex 
ggplot() + geom_bar(aes(x=Survived, fill=as.factor(train$Pclass)), data=train) # by_class 
ggplot(data=train,aes(x=Age,fill=as.factor(Survived))) + geom_histogram(position="fill",binwidth =10) # by_age
# the younger the passenger, the more likely they survived

# tried getting rid of rows without Age values. It increased the validation accuracy but it didn't increase the test accuracy.
median_age <- function(data) {
  median_age = median(data$Age[!is.na(data$Age)])
  data$Age[is.na(data$Age)] = median_age
  return(data)
}
# train = median_age(train)
# test = median_age(test)

# age by bucket
# bucket_ages <- function(data) {
#   data$Age[which(data$Age > 0 & data$Age < 12)] = "0-11"
#   data$Age[which(data$Age >= 12 & data$Age < 18)] = "12-17"
#   data$Age[which(data$Age >= 18 & data$Age < 25)] = "18-24"
#   data$Age[which(data$Age >= 25 & data$Age < 40)] = "25-39"
#   data$Age[which(data$Age >= 40 & data$Age < 65)] = "40-64"
#   data$Age[which(data$Age >= 65)] = "65+"
#   data$Age[which(is.na(data$Age))] = "25-39"
#   return(data)
# }
# train = bucket_ages(train)
# test = bucket_ages(test)
# by_age = ggplot() + geom_bar(aes(x=Age, fill=Survived),data=train)

### create new features and test for correlation
train['fam_size'] = train['SibSp'] + train['Parch']
by_fam_size = ggplot() + geom_bar(aes(x=fam_size,fill=Survived), train, position="fill")

train['fare_per_person'] = train['Fare'] / (train['fam_size'] + 1)
by_fare_pp = ggplot(data = train,aes(x=Fare,fill=as.factor(Survived))) + geom_histogram(binwidth=20,position="fill")
# the higher the fare, the more likely the passenger is to survive

# which floor of the boat their cabin was on
train['Cabin'] = lapply(train['Cabin'],as.character)
train['deck'] = sapply(train[['Cabin']], substr, 1,1)

# get name prefix
get_prefix <- function(data) {
  data['Name'] = as.character(data[['Name']])
  data['prefix'] = regmatches(data[['Name']], regexpr("[[:alpha:]]+[.]", data[['Name']]))
  unique(data$prefix)
  data$prefix[data$prefix == "Lady."] = "Miss."
  data$prefix[data$prefix == "Mlle."] = "Miss."
  data$prefix[data$prefix == "Countess."] = "Miss." #should be Mrs.
  data$prefix[data$prefix == "Ms."] = "Miss."
  data$prefix[data$prefix == "Mme."] = "Mrs."
  data$prefix[data$prefix == "Dona."] = "Mrs."
  
  data$prefix[data$prefix == "Master."] = "Mr."
  data$prefix[data$prefix == "Don."] = "Mr."
  data$prefix[data$prefix == "Rev."] = "Mr."
  data$prefix[data$prefix == "Col."] = "Mr."
  data$prefix[data$prefix == "Capt."] = "Mr."
  data$prefix[data$prefix == "Jonkheer."] = "Mr."
  data$prefix[data$prefix == "Sir."] = "Mr."
  data$prefix[data$prefix == "Major."] = "Mr."
  return(data)
}
train = get_prefix(train)
test = get_prefix(test)

by_prefix = ggplot(data=train,aes(x=prefix,fill=as.factor(Survived))) + geom_bar(position="fill")
# those with prefix Mr. are less likely to survive compared to those with prefixes Miss. or Mrs.



impute_data <- function(data) {
  # make a copy before imputing 
  original_df = data
  
  # we want to remove categorical variables before we impute
  # transform/one-hot some categorical variables
  to_drop = c("PassengerId","Name","Cabin","deck","Ticket")
  temp_dataset = data[,-which(names(data) %in% to_drop)]
  
  # convert to levels - 0=C, 1=Q, 2=S
  temp_dataset$Embarked = as.factor(temp_dataset$Embarked)
  levels(temp_dataset$Embarked) = c(0,1,2)
  # convert to levels - 0=F, 1=M
  temp_dataset$Sex = as.factor(temp_dataset$Sex)
  levels(temp_dataset$Sex) = c(0,1)
  # convert to levels - 0=Dr.,1=Miss,2=Mr.,3=Mrs.
  temp_dataset$prefix = as.factor(temp_dataset$prefix)
  levels(temp_dataset$prefix) = c(0,1,2,3)
  
  # try imputing Age values using mice package
  # md.pattern(temp_dataset)
  
  imputed_train = mice(temp_dataset, m=5, method = 'pmm', seed = 500)
  # # using predictive mean matching
  # summary(imputed_train)
  # check imputed data
  # imputed_train$imp$Age
  # replace missing values with imputed values
  temp_dataset = complete(imputed_train,1)
  
  # add back some dropped columns
  temp_dataset$PassengerId = original_df$PassengerId
  temp_dataset$Name = original_df$Name
  
  return(temp_dataset)
  }
train = impute_data(train)
test = impute_data(test)

# drop unnecessary columns
# to_drop = c("Ticket", "Cabin")
# train = train[,!(names(train) %in% to_drop)]
# test = test[,!(names(test) %in% to_drop)]

# convert to factor
train$Pclass = as.factor(train$Pclass)
test$Pclass = as.factor(test$Pclass)


# # convert some columns to factor
# factor_train = match(c("Pclass","Sex","Embarked"),colnames(train))
# for (i in factor_train) {
#   train[,i] = sapply(train[,i], as.factor)
# }
# factor_test = match(c("Pclass","Sex","Embarked"),colnames(test))
# for (i in factor_test) {
#   test[,i] = sapply(test[,i], as.factor)
# }

### fit and predict model
to_test = c("Pclass","Sex","SibSp","Parch","Embarked","Age","prefix")
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

# get accuracy on validation set
# predict model on test set - gives .82123 score w/ age buckets
predictions = predict(base_model, val_set)
fitted.results <- ifelse(predictions > 0.5,1,0)
misClasificError = mean(fitted.results != val_set$Survived)
print(paste('Accuracy',1-misClasificError))

# run on test set 
predictions = predict(base_model, test, na.action = na.pass)
fitted.results <- ifelse(predictions > 0.5,1,0)

results = cbind(test$PassengerId,fitted.results)

colnames(results) = c("PassengerId","Survived")
write.csv(results, "~/Desktop/titanic/predictions.csv", row.names=F)


