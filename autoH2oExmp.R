# on R version 3.4.4, win 8.1 32-bit, 2GB RAM  x64 
library(h2o)

h2o.init()

# Import a sample binary outcome train/test set into H2O
train <- h2o.importFile("https://s3.amazonaws.com/erin-data/higgs/higgs_train_10k.csv")
test <- h2o.importFile("https://s3.amazonaws.com/erin-data/higgs/higgs_test_5k.csv")

# Identify predictors and response
y <- "response"
x <- setdiff(names(train), y)

# For binary classification, response should be a factor
train[,y] <- as.factor(train[,y])
test[,y] <- as.factor(test[,y])

aml <- h2o.automl(x = x, y = y,
                  training_frame = train,
                  max_runtime_secs = 30)

# View the AutoML Leaderboard, keep_cross_validation_models=FALSE
lb <- aml@leaderboard
lb

#                                                model_id       auc   logloss
# 1    StackedEnsemble_AllModels_0_AutoML_20180503_085035 0.7816995 0.5603380
# 2 StackedEnsemble_BestOfFamily_0_AutoML_20180503_085035 0.7780683 0.5636519
# 3             GBM_grid_0_AutoML_20180503_085035_model_1 0.7742967 0.5656552
# 4             GBM_grid_0_AutoML_20180503_085035_model_0 0.7736082 0.5667454
# 5             GBM_grid_0_AutoML_20180503_085035_model_2 0.7704520 0.5695492
# 6             GBM_grid_0_AutoML_20180503_085035_model_3 0.7662087 0.5759679
#  mean_per_class_error      rmse       mse
# 1            0.3250067 0.4361930 0.1902644
# 2            0.3261921 0.4377744 0.1916464
# 3            0.3233579 0.4390083 0.1927283
# 4            0.3196441 0.4394696 0.1931335
# 5            0.3443406 0.4411033 0.1945721
# 6            0.3348417 0.4439429 0.1970853

# [9 rows x 6 columns]

# The leader model is stored here
aml@leader

# If you need to generate predictions on a test set, you can make
# predictions directly on the `"H2OAutoML"` object, or on the leader
# model object directly

pred <- h2o.predict(aml, test)  # predict(aml, test) also works

# or:
pred <- h2o.predict(aml@leader, test)
