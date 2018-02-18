library(mice)
library(lubridate)
library(dplyr)
library(caret)
library(MASS)
library(readr)
library(rpart)
library(rpart.plot)
# library(ranger)
library(speedglm)
train = read.csv("train.csv")
test = read.csv("test.csv")
#sample=read.csv("sampleSubmission.csv")
# str(train)
# table(is.na(train))
# mice(train)
# str(test)
# str(sample)
test$Category = ""
test$Descript = ""
test$Resolution = ""
nrow(train)
train$Id = -1
#train$ID=c(884263:884253+878049)
data = bind_rows(train, test)
###no  NA values
# Dates - timestamp of the crime incident
# Category - category of the crime incident (only in train.csv). This is the target variable you are going to predict.
# Descript - detailed description of the crime incident (only in train.csv)
# DayOfWeek - the day of the week
# PdDistrict - name of the Police Department District
# Resolution - how the crime incident was resolved (only in train.csv)
# Address - the approximate street address of the crime incident
# X - Longitude
# Y - Latitude
str(data)
nrow(data)
data$year = year(data$Dates)
data$hour = hour(data$Dates)
data$month = month(data$Dates)
plot(density(data$hour))
freq = data.frame(table(data$hour))
h = rpart(Freq ~ Var1, freq, control = rpart.control(
  minsplit = 1,
  cp = 1e-3,
  maxdepth = 3
))
printcp(h)
rpart.plot(h)
## tianjia lisan bianliang

data$freq = 1
data$freq[data$hour %in% c(1:7)] = 0
data$freq[data$hour %in% c(0, 12, 15, 16, 17, 18, 19, 229)] = 2
data$freq = as.factor(data$freq)
data$Descript = as.factor(data$Descript)
data$month = as.factor(data$month)
data$Category = as.factor(data$Category)
data$year = as.factor(data$year)
data$hour = as.factor(data$hour)
data$loc = as.factor(paste(round(data$X, 2), round(data$Y, 2), sep = " "))
# 因变量为连续型数据的回归为线性回归，因变量为二分类变量的回归为Logistic回归，
# 因变量服从泊松分布的回归为泊松回归，这里选择Logistic回归。但源数据是多分类问题，
# 一共有39个分类，需要将多分类问题转为二分类问题，方法是创建一个新的数据框命名为response，
# 行数与源数据相同，并将源数据的类别变量赋值到response的新列cat列，且将这39个类变成response的39个新列，
# 每一列列名为类名，与列名相同的那一行，将该行赋值为1。以下展示部分response数据框的数据：

#logistic回归是分析因变量取某个值的概率与自变量的关系
# set.seed(123)
train = data[data$Id == -1, ]
# nrow(train)
test = data[data$Id != -1, ]
# p1=ranger(Category~Descript+DayOfWeek+PdDistrict+Address+X+Y+year+month+hour+freq,train,num.trees=3000,num.threads=8,classification=T,treetype='classification')
logistic_regression= function(train, test) {
  submission= data.frame(Id = test$Id)
  response=
    data.frame(cat = train$Category)#将犯罪类型作为response数据框的第一列
  crime= as.character(unique(train$Category))# 去重犯罪类型
  crime= sort(crime)#排序
    for (i in crime) {
      #这个循环的目的是将多分类问题转为二分类问题，即将每一个类别作为一列，类名就是列名。
      response[, i]= 0 #初始化每一类的值为0
      
      response[which(response$cat == i), i]=1#将属于某类的那行的值重新赋值为1，这样每一列的类别与列名相同的值为1，否则为0，这样就转换成了二分类问题，每一列都是一个二分类问题。
       fit=
         speedglm (
          response[, i] ~ PdDistrict  + DayOfWeek + year + month + hour +freq+loc,
         data = train,
          family = binomial(link = "logit")
        )#防止属于同一纬度类别过多，所以用x:y
      pre= predict(fit, test, type = "response")#type为response表示类别的概率。
      submission[, i]= 1
    }
   return(submission)
}
submission_final= logistic_regression(train, test)
View(submission_final)
write(submission_final,"sub1.csv")
