# Clear the workspace
rm(list = ls())

#setwd('C:/Users/Spenc/Documents/Youtube/Machine Learning/R/Cross Validation Example')

# Link of where data came from
#http://www.cs.toronto.edu/~delve/data/boston/bostonDetail.html


# Libraries needed to install
library(caret)

# Checking out what caret is
a <- getModelInfo()
# a$...

# Set the seed to follow results.
set.seed(1)


data = read.csv('Boston_Housing.csv')
str(data)
#summary(data)
# Clean data of NA.
#data <- na.omit(data)

data <- cbind(as.data.frame(scale(data[,1:13])),data[,14])
colnames(data)[14] = "MEDV"



trainParameters <- trainControl(method = "cv", number = 4)

# For all intensive purposes, we will split the data into a Train and Test.
# We will use the Test data to see how well our model performs.


# Splitting the data into 80 20
trainIndex <- createDataPartition(data$CRIM, p = 0.8, list = FALSE, times = 1)

dataTrain <- data_scaled[trainIndex,]
dataTest <- data_scaled[-trainIndex,]

elastic_net_cv_0 <- train(MEDV ~ .,
                          data = dataTrain,
                          method = "glmnet",
                          trControl = trainParameters
)





# Setting the parameters for the machine learning model to train.
# N-Fold validation is used to estimate expected error (variance)
trainParameters <- trainControl(method = "cv", number = 4)

# For all intensive purposes, we will split the data into a Train and Test.
# We will use the Test data to see how well our model performs.


# Splitting the data into 80 20
trainIndex <- createDataPartition(data$CRIM, p = 0.8, list = FALSE, times = 1)

dataTrain <- data[trainIndex,]
dataTest <- data[-trainIndex,]

elastic_net_cv_0 <- train(MEDV ~ .,
                          data = dataTrain,
                          method = "glmnet",
                          trControl = trainParameters
)

elastic_net_cv_0
# Saving the R Squared VALUE of the best model
rsquare_elastic_cv_0 <- 0.7047931

# Retrieving the lambda value
elastic_net_cv_0$bestTune$lambda

# Retrieving optimal model
coef(elastic_net_cv_0$finalModel, elastic_net_cv_0$bestTune$lambda)

# Utilize this model to predict new incoming data. (our test)
elastic_net_cv_0_pred <- predict(elastic_net_cv_0, dataTest)

# Calculating the RMSE of our new found predictions.
rmse_elastic_net_0_cv <- RMSE(elastic_net_cv_0_pred, dataTest$MEDV)



# Let's see if we can improve this model?
# Using the same training and testing (and the same seed #)

# Since this is a penalization function, we don't need to address the 
# potential of overfitness. Thus, we can append values or combination of values
# There are two additional values that we will utilize.
# The .^2 value (this is just placing relationships among the features)
# i.e interactions
# And TuneLength. TuneLength is a form of hypertuning where we will find
# the X number of alpha and lambda values.
elastic_net_cv_1 <- train(MEDV ~ .^2,
                          data = dataTrain,
                          method = "glmnet",
                          trControl = trainParameters,
                          tuneLength = 10
)

# Taking a look
elastic_net_cv_1
rsquare_elastic_cv_1 <- 0.002916378

elastic_net_cv_1$bestTune$lambda

# Best model coefficients
coef(elastic_net_cv_1$finalModel, elastic_net_cv_1$bestTune$lambda)

# predictions
elastic_net_cv_1_pred <- predict(elastic_net_cv_1, dataTest)

# Calculating the RMSE of our new found predictions.
rmse_elastic_net_1_cv <- RMSE(elastic_net_cv_1_pred, dataTest$MEDV)


# At the end print out the values to neatly order them.
NAME <- c("Linear Regression",
          "Ridge Regression",
          "Lasso Regression",
          "Elastic Regression",
          "Elastic Regression CV 0",
          "Elastic Regression CV 1"
)
RSQUARED <- c(rsquare_linear,
              rsquare_ridge,
              rsquare_lasso,
              rsquare_elastic,
              rsquare_elastic_cv_0,
              rsquare_elastic_cv_1
)
RMSE_Values <- c(rmse_linear,
                 rmse_ridge,
                 rmse_lasso,
                 rmse_elastic,
                 rmse_elastic_net_0_cv,
                 rmse_elastic_net_1_cv
)
cbind(NAME,RSQUARED,RMSE_Values)
