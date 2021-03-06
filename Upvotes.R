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

#train_split <- initial_split(train_data, prop = 0.7)
#train_set <- training(train_split)
#test_set  <- testing(train_split)

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
  geom_point(data=test_set, aes(x=ID, y=predict), fill="blue", colour="darkblue")+
  theme_bw()+
  ylab("Predict, Upvotes")+
  xlab("ID")

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
  ylab("Predict, Upvotes")+
  xlab("ID")

rm(glmmodel,split,test_set,train_set)

## Neural Net model ##

train_nnet <- train_data %<>% mutate_if(is.factor, as.numeric)

nn_model <- neuralnet(Upvotes)
## XGBoost model ##

if(!require (xgboost)) {install.packages("xgboost")} else {library(xgboost)}
if(!require (Matrix)) {install.packages("Matrix")} else {library(Matrix)}
if(!require (Ckmeans.1d.dp)) {install.packages("Ckmeans.1d.dp")} else {library(Ckmeans.1d.dp)}
library(keras)
train_data = read.csv('train_data.csv')

train_data <- train_data %>%
  select(Upvotes, everything())

#train_data <- train_data %>%
#  select( -Username, -ID)

train_data %<>% mutate_if(is.factor, as.numeric)

#train_data <- train_data %>% filter(train_data$Views < 100000)
normalize <- function(x) {return ((x - min(x)) / (max(x) - min(x)))}

maxmin_data <- as.data.frame(lapply(train_data, normalize))

maxmin_data<-as.matrix(maxmin_data)

ind <- sample(2, nrow(maxmin_data), replace = T, prob = c(.7, .3))
train_set <- maxmin_data[ind==1, 2:7]
test_set <- maxmin_data[ind==2, 2:7]

train_target <- maxmin_data[ind==1, 1]
test_target <- maxmin_data[ind==2, 1]


k_model <- keras_model_sequential() 
k_model %>% 
  layer_dense(units = 18, activation = 'relu', 
              kernel_initializer='RandomNormal', 
              input_shape = c(6)) %>% 
  layer_dense(units = 12, activation = 'relu') %>%
  layer_dense(units = 1, activation = 'linear')

#summary(k_model)

k_model %>% compile(
  loss = 'mean_squared_error',
  optimizer = 'adam',
  metrics = c('mae')
)

history <- k_model %>% fit(
  train_set, train_target, 
  epochs = 2, batch_size = 50, 
  validation_split = 0.1
)
#epoch = 7 - ~1400 epoch = 2 - ~2100

k_model %>% evaluate(test_set, test_target)

pred <- data.frame(y = predict(k_model, as.matrix(test_set)))
predicted=pred$y * abs(diff(range(train_data$Upvotes))) + min(train_data$Upvotes)
actual=test_target * abs(diff(range(train_data$Upvotes))) + min(train_data$Upvotes)
result <- data.frame(predicted,actual)

rmse_keras <- as.integer(rmse(result$actual, result$predicted))

rm(actual, history, ind, k_model, maxmin_data, normalize, pred, predicted, result,
   test_set, test_target, train_set, train_target)


data_matrix <- as.matrix(train_data)
dimnames(data_matrix) <- NULL

set.seed(2020)
ind <- sample(2, nrow(data_matrix), replace = T, prob = c(.7, .3))
train_set <- data_matrix[ind==1, 2:6]
test_set <- data_matrix[ind==2, 2:6]

train_target <- data_matrix[ind==1, 1]
test_target <- data_matrix[ind==2, 1]

m <- colMeans(train_set)
s <- apply(train_set, 2, sd)

train_set <- scale(train_set, center = m, scale = s)
test_set <- scale(test_set, center = m, scale = s)

keras_mdl <- keras_model_sequential()
keras_mdl %>%
              layer_dense(units = 5, 
                          activation = 'relu',
                          input_shape = c(5)) %>%
              layer_dense(units = 1)

keras_mdl %>% compile(loss = 'mse',
                      optimizer = 'rmsprop',
                      metrics = 'mae')

ker_model <- keras_mdl %>% 
            fit(train_set,
                train_target,
                epochs = 5,
                batch_size = 16,
                validation_split = 0.2)

keras_mdl %>% evaluate(test_set, test_target)

k_pred <- keras_mdl %>% predict(test_set)
mean((test_target-k_pred)^2)
plot(test_target, k_pred)



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
                      watchlist = watchlist,
                      nrounds = 40,
                      eta = 0.1)

eval <- data.frame(xgbmodel$evaluation_log)
plot(eval$iter, eval$test_rmse, col = 'blue')

pred <- predict(xgbmodel, newdata = mtest_set)

xgb_rmse <- as.integer(rmse(test_set$Upvotes, pred))

rm(split, test_label,test_set, train_label, train_set, testdumb, traindumb,
   watchlist,xgbmodel, xgb_param, eval, mtest_set, mtrain_set)

## Keras Model ##

library(keras)
library(lime)
library(tidyquant)
library(rsample)
library(recipes)
library(yardstick)
library(corrr)

train_data %<>% mutate_if(is.factor, as.numeric)

set.seed(2019)
train_split <- initial_split(train_data, prop = 0.7)

train_set <- training(train_split)
test_set  <- testing(train_split)

# Building our Artificial Neural Network
model_keras <- keras_model_sequential()

model_keras %>% 
  
  # First hidden layer
  layer_dense(
    units              = 26, 
    kernel_initializer = "uniform", 
    activation         = "relu", 
    input_shape        = ncol(x_train_set)) %>% 
  
  # Dropout to prevent overfitting
  layer_dropout(rate = 0.1) %>%
  
  # Second hidden layer
  layer_dense(
    units              = 26, 
    kernel_initializer = "uniform", 
    activation         = "relu") %>% 
  
  # Dropout to prevent overfitting
  layer_dropout(rate = 0.1) %>%
  
  # Output layer
  layer_dense(
    units              = 1, 
    kernel_initializer = "uniform", 
    activation         = "sigmoid") %>% 
  
  # Compile ANN
  compile(
    optimizer = optimizer_rmsprop(),
    loss      = 'mse',
    metrics   = list("mean_absolute_error")
  )

history <- fit(
  object           = model_keras, 
  x                = as.matrix(x_train_set), 
  y                = train_set$Upvotes,
  batch_size       = 30, 
  epochs           = 2
)

print(history)

#plot(history)

keras_class_vec <- predict_classes(object = model_keras, x = as.matrix(x_test_set)) %>%
  as.vector()

#table(x_test_set$Upvotes,keras_class_vec)
keras_rmse <- as.integer(rmse(test_set$Upvotes, keras_class_vec))

library(caret)

estimates_keras_tbl <- tibble(
  truth      = as.factor(test_tbl$is_promoted),
  estimate   = as.factor(keras_class_vec)
)

F_meas(truth, estimate, beta = 1)

estimates_keras_tbl

options(yardstick.event_first = FALSE)

# Confusion Table
estimates_keras_tbl %>% conf_mat(truth, estimate)

estimates_keras_tbl %>% metrics(truth, estimate)

estimates_keras_tbl %>% roc_auc(truth, class_vec)

library(caTools)

colAUC(test_tbl$is_promoted,keras_class_vec, plotROC=T)

# F1-Statistic
estimates_keras_tbl %>% f_meas(truth, estimate, beta = 1)
