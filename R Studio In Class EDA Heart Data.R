# Installing tidyverse

library(tidyverse)
data=read.csv("heart.data.csv")

# Viewing the data

head(data)
view(data)
glimpse(data)
names(data)
summary(data)

# Checking for missing values

colSums(is.na(data))

ggplot(data=data,
       aes(biking))+
  geom_histogram()

# Fixing missing values
# Biking

biking_median=median(data$biking, na.rm=TRUE)
data$biking=ifelse(is.na(data$biking),
                   biking_median,
                   data$biking)
colSums(is.na(data))

# Smoking

ggplot(data=data,
       aes(smoking))+
  geom_histogram()

smoking_median=median(data$smoking, na.rm=TRUE)
data$smoking=ifelse(is.na(data$smoking),
                   smoking_median,
                   data$smoking)
colSums(is.na(data))

# Heart.disease

ggplot(data=data,
       aes(heart.disease))+
  geom_histogram()

heart.disease_mean=mean(data$heart.disease, na.rm=TRUE)
data$heart.disease=ifelse(is.na(data$heart.disease),
                    heart.disease_mean,
                    data$heart.disease)
colSums(is.na(data))

# Splitting the data into two datasets
library(caTools)
set.seed(23)
split=sample.split(data$heart.disease, SplitRatio = 0.8) # 80% training, 20% testing
training_set=subset(data, split=TRUE)
test_set=subset(data, split=FALSE)

# Multiple Linear Regression Check

names(data)

MLR=lm(formula=heart.disease~. ,
       data=training_set)
summary(MLR)

# The Multiple R-Squared is 0.9765 or 97.65%, so it is a good model to predict the actual value

# The MLR Equation is: 14.96 + (-0.20) (biking) + 0.18 (smoking)

# Mean Square Error
sum=summary(MLR)
MSE=(mean(sum$residuals^2))
paste("Mean Square Error: ", MSE)

# The MSE is less than 0.5, so the variable is statiscally significant

# Test set prediction

y_pred=predict(MLR, newdata=test_set)
data=data.frame(test_set$heart.disease, y_pred)
head(data)

# Validation

new=read.csv("Val.csv")
head(new)
new_x=new[c(1:2)]
new_x
data.frame(new[c(3)], predict(MLR, newdata=new_x))
