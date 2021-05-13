# Installing Packages
#install.packages("e1071")
#install.packages("caTools")
#install.packages("caret")

# Loading package
library(klaR)
library(rsample)
library(e1071)
library(caTools)
library(caret)
library(dplyr)
# Splitting data into train
# and test data
#covidteliko=read_xlsx("covidteliko.xlsx")
attach(covidteliko)
glimpse(covidteliko)

covidteliko$pneumonia=as.factor(covidteliko$pneumonia)
class(covidteliko$pneumonia)
covidteliko$sex=as.factor(covidteliko$sex)
covidteliko$diabetes=as.factor(covidteliko$diabetes)
covidteliko$copd=as.factor(covidteliko$asthma)
covidteliko$inmsupr=as.factor(covidteliko$inmsupr)
covidteliko$hypertension=as.factor(covidteliko$hypertension)
covidteliko$other_disease=as.factor(covidteliko$other_disease)
covidteliko$cardiovascular=as.factor(covidteliko$cardiovascular)
covidteliko$obesity=as.factor(covidteliko$obesity)
covidteliko$renal_chronic=as.factor(covidteliko$renal_chronic)
covidteliko$tobacco=as.factor(covidteliko$tobacco)
covidteliko$icu=as.factor(covidteliko$icu)
covidteliko$asthma=as.factor(covidteliko$asthma)


levels(covidteliko$icu)[levels(covidteliko$icu)=="1"]="yes"
levels(covidteliko$icu)[levels(covidteliko$icu)=="2"]="no"

glimpse(covidteliko)
# counting missing values

covidteliko %>%
  summarise(count = sum(is.na(sex)))
covidteliko %>%
  summarise(count = sum(is.na(pneumonia)))
covidteliko %>%
  summarise(count = sum(is.na(age)))
covidteliko %>%
  summarise(count = sum(is.na(diabetes)))
covidteliko %>%
  summarise(count = sum(is.na(copd)))
covidteliko %>%
  summarise(count = sum(is.na(asthma)))
covidteliko %>%
  summarise(count = sum(is.na(inmsupr)))
covidteliko %>%
  summarise(count = sum(is.na(hypertension)))
covidteliko %>%
  summarise(count = sum(is.na(other_disease)))
covidteliko %>%
  summarise(count = sum(is.na(cardiovascular)))
covidteliko %>%
  summarise(count = sum(is.na(obesity)))
covidteliko %>%
  summarise(count = sum(is.na(renal_chronic)))
covidteliko %>%
  summarise(count = sum(is.na(tobacco)))
covidteliko %>%
  summarise(count = sum(is.na(icu)))


set.seed(221)
split <- sample.split(covidteliko$icu, SplitRatio = 0.8)
train_cl <- subset(covidteliko, split == "TRUE")
test_cl <- subset(covidteliko, split == "FALSE")
a=nrow(train_cl)/18000
a
# Feature Scaling
#train_scale <- scale(train_cl[, 1:4])
#test_scale <- scale(test_cl[, 1:4])

# Fitting Naive Bayes Model
# to training dataset
set.seed(120) # Setting Seed
classifier_cl <- naiveBayes(icu ~ ., data = train_cl)
classifier_cl

# Predicting on test data'
y_pred <- predict(classifier_cl, newdata = test_cl)

# Confusion Matrix
cm <- table(test_cl$icu, y_pred)
cm

# Model Evauation
confusionMatrix(cm)

###############################################################################
########################  what if it wasn't       ###########
##############################################################################

# Histogram with density plot
df=as.data.frame(covidteliko$age)
ggplot(df, aes(x=age)) + 
  geom_histogram(aes(y=..density..), colour="black", fill="white")+
  geom_density(alpha=.2, fill="#FF6666") 

ks.test(covidteliko$age, "pnorm", mean=mean(covidteliko$age), sd=sd(covidteliko$age))



# create response and feature data

x <- covidteliko[,1:13]
y <- covidteliko$icu

train_control <- trainControl(
  method = "cv", 
  number = 10
)
search_grid <- expand.grid(
  usekernel = c(TRUE, FALSE),
  fL = 0,
  adjust = 0.2
)

model1=train(x,y,'nb',trControl=train_control,tuneGrid=search_grid)
model1

# Predicting on test data'
y_pred <- predict(model1, newdata = test_cl)

# Confusion Matrix
cm <- table(test_cl$icu, y_pred)
cm
confusionMatrix(cm)
#############################################################################
######################   Laplace Correction       #################
############################################################################

classifier_cl=naiveBayes(icu ~ .,laplace = 1,data = train_cl, na.action = na.pass)
classifier_cl

# Predicting on test data'
y_pred <- predict(classifier_cl, newdata = test_cl)

# Confusion Matrix
cm <- table(test_cl$icu, y_pred)
cm
confusionMatrix(cm)

##########both laplace and kernel###########################
# Model Evauation


search_grid <- expand.grid(
  .usekernel = c(TRUE, FALSE),
  .fL = 1,
  .adjust = 0.2
)

model1=train(x,y,'nb',trControl=train_control,tuneGrid=search_grid)
model1

# Predicting on test data'
y_pred <- predict(model1, newdata = test_cl)

# Confusion Matrix
cm <- table(test_cl$icu, y_pred)
cm
confusionMatrix(cm)
#############################################################################
###################    Attribute weighted naive bayes classifiers#####################
##############################################################################
library(bnclassify)

train_control <- trainControl(
  method = "cv", 
  number = 10,classProb=TRUE
)


covidteliko$age=as.factor(covidteliko$age)
glimpse(covidteliko)
x$age=as.factor(x$age)

set.seed(221)
split <- sample.split(covidteliko$icu, SplitRatio = 0.8)
train_cl <- subset(covidteliko, split == "TRUE")
test_cl <- subset(covidteliko, split == "FALSE")

search_grid <- expand.grid(
  .usekernel = FALSE,
  .fL = 1,
  .adjust = 0
)

covidtelikoq=covidteliko[,-3]
w=list()
for(a in 1:12){
  w[a]=0
}
w=as.data.frame(w)
str(w)
for(j in 1:12){
for(i in 1:18000){
  if((covidtelikoq[i,j]=='1')&((covidtelikoq[i,13]=='yes'))){
  w[j]=w[j]+1
  }
  if((covidtelikoq[i,j]=='2')&((covidtelikoq[i,13]=='no'))){
  w[j]=w[j]+1
  }
}
}
w
covidteliko=covidteliko[,c(3,2,8,4,11,13,10,9,12,7,5,6,1,14)]
modelLookup('awnb')
levels(covidteliko$icu)
model1 = train(x,y, method='awnb',trControl = train_control,metric = 'ROC')#tuneGrid = search_grid)
model1

# Predicting on test data'
y_pred <- predict(model1, newdata = test_cl)
# Confusion Matrix
cm <- table(test_cl$icu, y_pred)
cm

confusionMatrix(cm)





