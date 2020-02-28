## Upvotes analysis ##

if(!require(dplyr)) {istall.packages("dplyr")} else {library(dplyr)}
if(!require(ggplot2)) {install.packages("ggplot2")} else {library(ggplot2)}
if(!require(magrittr)) {install.packages("magrittr")} else {library(magrittr)} # for pipe
if(!require(caTools)) {install.packages("caTools")} else {library(caTools)} # for data partition
if(!require(ModelMetrics)) {install.packages("ModelMetrics")} else {library(ModelMetrics)} # for rmse
if(!require(corrplot)) {install.packages("corrplot")} else {library(corrplot)} # correlation matrix
if(!require(knitr)) {install.packages("knitr")} else {library(knitr)} # for floating tables 

train_data = read.csv('train_data.csv')

train_data <- train_data %>%
  select(Upvotes, everything())

glimpse(train_data)

summary(train_data)

sum(is.na(train_data))

levels(train_data$Tag)

train_data <- train_data %>%
  select(-ID -Username)

## Linear Regresion Model ##

set.seed(seed = 2019)
split = sample.split(train_data, SplitRatio = 0.7)#SplitRatio indicates the size of the training set
train_set = subset(train_data, split == TRUE)
test_set = subset(train_data, split == FALSE)

lmmodel <- lm(Upvotes ~ +Tag +Reputation +Answers +Views, 
              data = train_set, singular.ok = TRUE)
anova(lmmodel)
summary(lmmodel)

test_set$predict <- predict(lmmodel, newdata =  test_set, 
                            type = "response",
                            interval = "confidence",
                            family = poisson(link = ))

rmse_lm <- rmse(test_set$Upvotes, test_set$predict)

## Non-linear Regresion Model ##