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
  select( -Username)

## Linear Regresion Model ##

set.seed(seed = 2019)
split = sample.split(train_data, SplitRatio = 0.7)#SplitRatio indicates the size of the training set
train_set = subset(train_data, split == TRUE)
test_set = subset(train_data, split == FALSE)

lmmodel <- lm(Upvotes ~ +Tag +Reputation +Answers +Views, 
              data = train_set, singular.ok = TRUE)

predict_lm <- predict(lmmodel, newdata =  test_set, 
                            type = "response",
                            interval = "confidence",
                            family = poisson(link = ))

predict <- as.data.frame(predict_lm)

test_set$predict <- predict$fit

rmse_lm <- as.integer(rmse(test_set$Upvotes, test_set$predict))

ggplot()+
  geom_point(data=test_set, aes(x=ID, y=Upvotes), fill="red") +
  geom_point(data=test_set, aes(x=ID, y=predict), fill="blue", colour="darkblue")

rm(lmmodel,split,test_set,train_set, predict_lm, predict)

## Non-linear Regresion Model ##

set.seed(seed = 2019)
split = sample.split(train_data, SplitRatio = 0.7)
train_set = subset(train_data, split == TRUE)
test_set = subset(train_data, split == FALSE)

glmmodel <- glm(Upvotes ~ +Tag +Reputation +Answers +Views, 
                train_set, family = quasi(link = "identity"))

test_set$predict <- predict.glm(glmmodel, newdata =  test_set,
                                type = "response",
                                family = poisson())

rmse_nlm <- as.integer(rmse(test_set$Upvotes, test_set$predict))

ggplot()+
  geom_point(data=test_set, aes(x=ID, y=Upvotes), fill="red") +
  geom_point(data=test_set, aes(x=ID, y=predict), fill="blue", colour="darkblue")+
  theme_bw()+
  ylab("Frequency")+
  xlab("TAG")

rm(glmmodel,split,test_set,train_set)

## XGBoost model ##201888

if(!require (xgboost)) {install.packages("xgboost")} else {library(xgboost)}
if(!require (Matrix)) {install.packages("Matrix")} else {library(Matrix)}
if(!require (Ckmeans.1d.dp)) {install.packages("Ckmeans.1d.dp")} else {library(Ckmeans.1d.dp)}

number <- 201888
set.seed(seed = number)
split = sample.split(train_data, SplitRatio = 0.7)
train_set = subset(train_data, split == TRUE)
test_set = subset(train_data, split == FALSE)

traindumb <- sparse.model.matrix(Upvotes ~. -1 , data = train_set)

train_label <- train_set[,"Upvotes"]

mtrain_set <- xgb.DMatrix(data = as.matrix(traindumb), label = train_label)

testdumb <- sparse.model.matrix(Upvotes ~. -1, data = test_set)

test_label <- test_set[,"Upvotes"]

mtest_set <- xgb.DMatrix(data = as.matrix(testdumb), label = test_label)

xgb_param <- list("booster" = "gblinear", 
                  "lambda L2" = 1,
                  "objective" = "reg:logistic", 
                  "eval_metric" = "rmsle")

watchlist <- list(train = mtrain_set, test = mtest_set)

xgbmodel <- xgb.train(parameters = xgb_param, 
                      data = mtrain_set, 
                      max.depth = 10,
                      nrounds = 60,
                      watchlist = watchlist,
                      eta = 0.1)

eval <- data.frame(xgbmodel$evaluation_log)
plot(eval$iter, eval$test_rmse, col = 'blue')

pred <- predict(xgbmodel, newdata = mtest_set)

result <- as.data.frame(cbind(test_set$Upvotes, pred))

rmse_xgb <- as.integer(rmse(test_set$Upvotes, pred))

rm(split, test_label,test_set, train_label, train_set, testdumb, traindumb,
   watchlist,xgbmodel, xgb_param, result, eval, mtest_set, mtrain_set)

## Keras Model ##

