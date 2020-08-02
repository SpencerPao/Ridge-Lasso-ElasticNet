# Clear the workspace
rm(list = ls())

# Ridge Regression Example
#setwd('')
# Link of where data came from
#http://www.cs.toronto.edu/~delve/data/boston/bostonDetail.html

# Setwd with dataset

data = read.csv('Boston_Housing.csv')
str(data)
summary(data)

data <- na.omit(data)


data_scaled <- cbind(scale(data[,1:13]),data[,14])

# Train/set 80/20
set.seed(123)

size <- floor(0.8 *  nrow(data_scaled))

train_ind <- sample(seq_len(nrow(data_scaled)), size = size)

train <- data_scaled[train_ind, ]
xtrain <- train[,1:13]
ytrain <- train[,14]

# Create the values that were 'not' chosen
# Test values
test <- data_scaled[-train_ind,]
xtest <- test[,1:13]
ytest <- test[,14]

lambda.array <- seq(from = 0.01, to = 100, by = 0.01)


library(glmnet)
ridgeFit <- glmnet(xtrain,ytrain, alpha = 0, lambda = lambda.array)
summary(ridgeFit)

# As lambda becomes larger, this will start decreasing the sign. of the coefficients
plot(ridgeFit, xvar = 'lambda', label = T)


# Goodness of fit
plot(ridgeFit, xvar = 'dev', label = T)

# Predicted Values
y_predicted <- predict(ridgeFit, s = min(lambda.array), newx = xtest)
# Coefficients
predict(ridgeFit, s = min(lambda.array), newx = xtest, type = 'coefficients')


# SST SSE
sst <- sum((ytest - mean(ytest))^2)
sse <- sum((y_predicted - ytest)^2)

rsquare <- 1 - (sse/sst)


# MSE
MSE_ridge = (sum((y_predicted - ytest)^2) / length(y_predicted))
MSE_ridge

rmse_ridge <- sqrt(MSE_ridge)

plot(ytest, y_predicted, main = 'Predicted price vs Actual price (MEDV)')


library(glmnet)

lassoFit <- glmnet(xtrain,ytrain, alpha=1, lambda=lambda.array)
summary(lassoFit)

# Lambdas in relation to the coefficients
plot(lassoFit, xvar = 'lambda', label=T)

# Goodness of fit
plot(lassoFit, xvar = 'dev', label = T)

# Predicted Values
y_predicted_lasso <- predict(lassoFit, s=min(lambda.array), newx = xtest)

# SSE, SST
sst <- sum((ytest - mean(ytest))^2)
sse <- sum((y_predicted_lasso - ytest)^2)

rsquare_lasso <- 1 - (sse/sst)
# MSE
MSE_lasso = (sum((y_predicted_lasso - ytest)^2) / length(y_predicted_lasso))
MSE_lasso

rmse_lasso <- sqrt(MSE_lasso)
