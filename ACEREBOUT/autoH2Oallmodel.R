#autoH2O for all auto models
library(h2o)

h2o.init()

# Import a sample binary outcome train/test set into H2O
train <- as.h2o(training)
test <- as.h2o(testing)

# Identify predictors and response
y <- "V1"
x <- setdiff(names(train), y)

# For binary classification, response should be a factor
#train[,y] <- as.factor(train[,y])
#test[,y] <- as.factor(test[,y])

aml <- h2o.automl(x = x, y = y,
                  training_frame = train )

# View the AutoML Leaderboard, keep_cross_validation_models=FALSE
lb <- aml@leaderboard
print(lb)


aml@leader

# If you need to generate predictions on a test set, you can make
# predictions directly on the `"H2OAutoML"` object, or on the leader
# model object directly

pred <- h2o.predict(aml, test)  # predict(aml, test) also works

# or:
pred <- h2o.predict(aml@leader, test)