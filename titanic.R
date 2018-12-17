library(plyr)
library(ggplot2)

train = read.csv("~/Downloads/all/train.csv")
test = read.csv("~/Downloads/all/test.csv")

# colnames(train) = c("id","survived","class","name","sex","age","sib_sp","par_ch","ticket_num","fare","cabin","embark_loc")

# data summary
survival_count = count(train, vars="Survived")

### create features
train['fam_size'] = train['SibSp'] + train['Parch']

train['fare_per_person'] = train['Fare'] / (train['fam_size'] + 1)

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
train$prefix[train$prefix == "Mme."] = "Mrs."

train$prefix[train$prefix == "Master."] = "Mr."
train$prefix[train$prefix == "Don."] = "Mr."
train$prefix[train$prefix == "Rev."] = "Mr."
train$prefix[train$prefix == "Col."] = "Mr."
train$prefix[train$prefix == "Capt."] = "Mr."
train$prefix[train$prefix == "Jonkheer."] = "Mr."
train$prefix[train$prefix == "Sir."] = "Mr."
train$prefix[train$prefix == "Major."] = "Mr."


# drop unnecessary columns
to_drop = c("Fare","Ticket")
train = train[,!(names(train) %in% to_drop)]

# convert some columns to factor
factor_cols = c(2,3,5,10,13,14)
for (i in factor_cols) {
  train[,i] = sapply(train[,i], as.factor)
}

# age by bucket
# train$age[which(train$age > 0 & train$age < 12)] = "0-11"
# train$age[which(train$age >= 12 & train$age < 18)] = "12-17"
# train$age[which(train$age >= 18 & train$age < 25)] = "18-24"
# train$age[which(train$age >= 25 & train$age < 40)] = "25-39"
# train$age[which(train$age >= 40 & train$age < 65)] = "40-64"
# train$age[which(train$age >= 65)] = "65+"
# train$age[which(is.na(train$age))] = "NA"
# 
# by_age = ggplot() + geom_bar(aes(x=age, fill=survived),data=train)



### initial plots
by_sex = ggplot() + geom_bar(aes(x=survived, fill=sex), data=train)
by_class = ggplot() + geom_bar(aes(x=survived, fill=as.factor(train$Pclass)), data=train)
by_title = ggplot() + geom_bar(aes(x=as.factor(prefix), fill=survived), data=train)

to_test = c("Pclass","Sex","Age","SibSp","Parch","Embarked")

model = glm(Survived ~ ., family = binomial, train[,c("Survived",to_test)])
summary(model)
