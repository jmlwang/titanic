library(plyr)

train = read.csv("~/Downloads/all/train.csv")
colnames(train) = c("id","survived","class","name","sex","age","sib_sp","par_ch","ticket_num","fare","cabin","embark_loc")

train['fam_size'] = train['sib_sp'] + train['par_ch']

train['fare_per_person'] = train['fare'] / (train['fam_size'] + 1)

# which floor of the boat their cabin was on
train['cabin'] = lapply(train['cabin'],as.character)
train['deck'] = sapply(train[['cabin']], substr, 1,1)

# get name prefix
train['name'] = as.character(train[['name']])
train['prefix'] = regmatches(train[['name']], regexpr("[[:alpha:]]+[.]", train[['name']]))

x = lm(survived ~ fam_size, train)
summary(x)
