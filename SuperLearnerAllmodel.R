#superlearner example
#install.packages("SuperLearner")
#install.packages(c( "RhpcBLASctl"))
#For XGBoost we need to tweak the install command a bit; Windows users may need to install Rtools first.
#install.packages("xgboost", repos=c("http://dmlc.ml/drat/", getOption("repos")), type="source")
library(SuperLearner)
SuperLearner::listWrappers()



############################
# Setup example dataset.


set.seed(seed = seed.var)

# Identify predictors and response
y <- "V1"
x <- setdiff(names(training), y)

# X is our training sample.
X_train = training[,x]

# Create a holdout set for evaluating model performance.
# Note: cross-validation is even better than a single holdout sample.
X_holdout = testing[,x]

Y_train = training[,y]
Y_holdout = testing[,y]

# Review the outcome variable distribution.
table(Y_train, useNA = "ifany")

######

# Set the seed for reproducibility.
set.seed(22)

#Let’s fit 2 separate models: lasso (sparse, penalized OLS) and randomForest. We specify family = binomial() because we are predicting a binary outcome, aka classification. With a continuous outcome we would specify family = gaussian().
# Fit lasso model.
sl_lasso = SuperLearner(Y = Y_train, X = X_train, family = binomial(),
                        SL.library = "SL.glmnet")

print(sl_lasso)
# Review the elements in the SuperLearner object.
names(sl_lasso)

# Here is the risk of the best model (discrete SuperLearner winner).
sl_lasso$cvRisk[which.min(sl_lasso$cvRisk)]

## SL.glmnet_All 
##     0.1330516

# Here is the raw glmnet result object:
str(sl_lasso$fitLibrary$SL.glmnet_All$object, max.level = 1)



#SuperLearner is using cross-validation to estimate the risk on future data. By default it uses 10 folds; use the cvControl argument to customize.
######stack ensemble?
set.seed(1)
sl = SuperLearner(Y = Y_train, X = X_train, family = binomial(),
                  SL.library = c("SL.mean", "SL.glmnet", "SL.randomForest"))
print(sl)
# Review how long it took to run the SuperLearner:
sl$times$everything


######predict
# Predict back on the holdout dataset.
# onlySL is set to TRUE so we don't fit algorithms that had weight = 0, saving computation.
pred = predict(sl, X_holdout, onlySL = T)

# Check the structure of this prediction object.
str(pred)

## List of 2
##  $ pred           : num [1:356, 1] 0.95 0.693 0.931 0.957 0.952 ...
##  $ library.predict: num [1:356, 1:3] 0 0 0 0 0 0 0 0 0 0 ...

# Review the columns of $library.predict.
summary(pred$library.predict)

##        V1          V2                  V3        
##  Min.   :0   Min.   :0.0003921   Min.   :0.0090  
##  1st Qu.:0   1st Qu.:0.1382133   1st Qu.:0.1400  
##  Median :0   Median :0.4268453   Median :0.4190  
##  Mean   :0   Mean   :0.4743975   Mean   :0.4907  
##  3rd Qu.:0   3rd Qu.:0.8353414   3rd Qu.:0.8668  
##  Max.   :0   Max.   :0.9922678   Max.   :0.9990

# Histogram of our predicted values.
library(ggplot2)

## 
## Attaching package: 'ggplot2'

## The following object is masked from 'package:randomForest':
## 
##     margin

qplot(pred$pred[, 1]) + theme_minimal()

## `stat_bin()` using `bins = 30`. Pick better value with `binwidth`.

# Scatterplot of original values (0, 1) and predicted values.
# Ideally we would use jitter or slight transparency to deal with overlap.
qplot(Y_holdout, pred$pred[, 1]) + theme_minimal()

# Review AUC - Area Under Curve
pred_rocr = ROCR::prediction(pred$pred, Y_holdout)
auc = ROCR::performance(pred_rocr, measure = "auc", x.measure = "cutoff")@y.values[[1]]
auc

## [1] 0.9659829
