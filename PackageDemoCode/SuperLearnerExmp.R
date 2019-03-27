#superlearner example
#install.packages("SuperLearner")
#install.packages(c( "RhpcBLASctl"))
#For XGBoost we need to tweak the install command a bit; Windows users may need to install Rtools first.
#install.packages("xgboost", repos=c("http://dmlc.ml/drat/", getOption("repos")), type="source")
library(SuperLearner)
SuperLearner::listWrappers()



#############Boston Data###############
# Setup example dataset.

# Load a dataset from the MASS package.
data(Boston, package = "MASS")

# Review info on the Boston dataset.
?Boston

## No documentation for 'Boston' in specified packages and libraries:
## you could try '??Boston'

# Check for any missing data - looks like we don't have any.
colSums(is.na(Boston))

##    crim      zn   indus    chas     nox      rm     age     dis     rad 
##       0       0       0       0       0       0       0       0       0 
##     tax ptratio   black   lstat    medv 
##       0       0       0       0       0

# Extract our outcome variable from the dataframe.
outcome = Boston$medv

# Create a dataframe to contain our explanatory variables.
data = subset(Boston, select = -medv)

# Check structure of our dataframe.
str(data)

## 'data.frame':    506 obs. of  13 variables:
##  $ crim   : num  0.00632 0.02731 0.02729 0.03237 0.06905 ...
##  $ zn     : num  18 0 0 0 0 0 12.5 12.5 12.5 12.5 ...
##  $ indus  : num  2.31 7.07 7.07 2.18 2.18 2.18 7.87 7.87 7.87 7.87 ...
##  ...

# If we had factor variables we would use model.matrix() to convert to numerics.

# Review our dimensions.
dim(data)

## [1] 506  13

# Set a seed for reproducibility in this random sampling.
set.seed(1)

# Reduce to a dataset of 150 observations to speed up model fitting.
train_obs = sample(nrow(data), 150)

# X is our training sample.
X_train = data[train_obs, ]

# Create a holdout set for evaluating model performance.
# Note: cross-validation is even better than a single holdout sample.
X_holdout = data[-train_obs, ]

# Create a binary outcome variable: towns in which median home value is > 22,000.
outcome_bin = as.numeric(outcome > 22)

Y_train = outcome_bin[train_obs]
Y_holdout = outcome_bin[-train_obs]

# Review the outcome variable distribution.
table(Y_train, useNA = "ifany")

########### Run simpler SL ######

# Set the seed for reproducibility.
set.seed(1)

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

#############hyp parameters
# Customize the defaults for randomForest.
learners = create.Learner("SL.randomForest", params = list(ntree = 3000))

# Look at the object.
learners

## $grid
## NULL
## 
## $names
## [1] "SL.randomForest_1"
## 
## $base_learner
## [1] "SL.randomForest"
## 
## $params
## $params$ntree
## [1] 3000

# List the functions that were created
learners$names

## [1] "SL.randomForest_1"

# Review the code that was automatically generated for the function.
# Notice that it's exactly the same as the function we made manually.
SL.randomForest_1

## function (...) 
## SL.randomForest(..., ntree = 3000)

set.seed(1)

# Fit the CV.SuperLearner.
# We use V = 3 to save computation time; for a real analysis use V = 10 or 20.
cv_sl = CV.SuperLearner(Y = Y_train, X = X_train, family = binomial(), V = 3,
                        SL.library = c("SL.mean", "SL.glmnet", learners$names, "SL.randomForest"))

# Review results.
summary(cv_sl)

## 
## Call:  
## CV.SuperLearner(Y = Y_train, X = X_train, V = 3, family = binomial(),  
##     SL.library = c("SL.mean", "SL.glmnet", learners$names, "SL.randomForest")) 
## 
## 
## Risk is based on: Mean Squared Error
## 
## All risk estimates are based on V =  3 
## 
##              Algorithm     Ave        se     Min     Max
##          Super Learner 0.15634 0.0177517 0.12509 0.20573
##            Discrete SL 0.16400 0.0189549 0.12602 0.22289
##            SL.mean_All 0.25140 0.0027616 0.24970 0.25450
##          SL.glmnet_All 0.15741 0.0186996 0.11518 0.22289
##  SL.randomForest_1_All 0.14433 0.0167944 0.12602 0.16387
##    SL.randomForest_All 0.14418 0.0168854 0.12920 0.16286
#############XGBOOS W HYPERS
# 3 * 4 * 3 = 36 different configurations.
tune = list(ntrees = c(100, 200, 500),
            max_depth = 1:4,
            shrinkage = c(0.001, 0.01, 0.1))

# Set detailed names = T so we can see the configuration for each function.
# Also shorten the name prefix.
learners = create.Learner("SL.xgboost", tune = tune, detailed_names = T, name_prefix = "xgb")

# 36 configurations - not too shabby.
length(learners$names)

## [1] 36

learners$names

##  [1] "xgb_100_1_0.001" "xgb_200_1_0.001" "xgb_500_1_0.001"
##  [4] "xgb_100_2_0.001" "xgb_200_2_0.001" "xgb_500_2_0.001"
##  [7] "xgb_100_3_0.001" "xgb_200_3_0.001" "xgb_500_3_0.001"
## [10] "xgb_100_4_0.001" "xgb_200_4_0.001" "xgb_500_4_0.001"
## [13] "xgb_100_1_0.01"  "xgb_200_1_0.01"  "xgb_500_1_0.01" 
## [16] "xgb_100_2_0.01"  "xgb_200_2_0.01"  "xgb_500_2_0.01" 
## [19] "xgb_100_3_0.01"  "xgb_200_3_0.01"  "xgb_500_3_0.01" 
## [22] "xgb_100_4_0.01"  "xgb_200_4_0.01"  "xgb_500_4_0.01" 
## [25] "xgb_100_1_0.1"   "xgb_200_1_0.1"   "xgb_500_1_0.1"  
## [28] "xgb_100_2_0.1"   "xgb_200_2_0.1"   "xgb_500_2_0.1"  
## [31] "xgb_100_3_0.1"   "xgb_200_3_0.1"   "xgb_500_3_0.1"  
## [34] "xgb_100_4_0.1"   "xgb_200_4_0.1"   "xgb_500_4_0.1"

# Confirm we have multiple cores configured. This should be > 1.
getOption("mc.cores")

## [1] 2

# Remember to set multicore-compatible seed.
set.seed(1, "L'Ecuyer-CMRG")

# Fit the CV.SuperLearner. This will take 1-2 minutes.
# We use V = 3 to save computation time; for a real analysis use V = 10 or 20.
system.time({
  cv_sl = CV.SuperLearner(Y = Y_train, X = X_train, family = binomial(),
                          V = 3, parallel = "multicore",
                          SL.library = c("SL.mean", "SL.glmnet", learners$names, "SL.randomForest"))
})

##    user  system elapsed 
##  22.620   0.844  34.234

# Review results.
summary(cv_sl)

## 
## Call:  
## CV.SuperLearner(Y = Y_train, X = X_train, V = 3, family = binomial(),  
##     SL.library = c("SL.mean", "SL.glmnet", learners$names, "SL.randomForest"),  
##     parallel = "multicore") 
## 
## Risk is based on: Mean Squared Error
## 
## All risk estimates are based on V =  3 
## 
##            Algorithm     Ave        se     Min     Max
##        Super Learner 0.12519 0.0147340 0.12367 0.12773
##          Discrete SL 0.12289 0.0143333 0.11559 0.12745
##          SL.mean_All 0.25273 0.0030641 0.25160 0.25450
##        SL.glmnet_All 0.13671 0.0170438 0.12583 0.15021
##  xgb_100_1_0.001_All 0.23808 0.0017966 0.23717 0.23881
##  xgb_200_1_0.001_All 0.22814 0.0032803 0.22688 0.22949
##  xgb_500_1_0.001_All 0.20557 0.0059835 0.20278 0.20747
##  xgb_100_2_0.001_All 0.23808 0.0017966 0.23717 0.23881
##  xgb_200_2_0.001_All 0.22814 0.0032803 0.22688 0.22949
##  xgb_500_2_0.001_All 0.20557 0.0059835 0.20278 0.20747
##  xgb_100_3_0.001_All 0.23808 0.0017966 0.23717 0.23881
##  xgb_200_3_0.001_All 0.22814 0.0032803 0.22688 0.22949
##  xgb_500_3_0.001_All 0.20557 0.0059835 0.20278 0.20747
##  xgb_100_4_0.001_All 0.23808 0.0017966 0.23717 0.23881
##  xgb_200_4_0.001_All 0.22814 0.0032803 0.22688 0.22949
##  xgb_500_4_0.001_All 0.20557 0.0059835 0.20278 0.20747
##   xgb_100_1_0.01_All 0.18151 0.0089164 0.17312 0.18686
##   xgb_200_1_0.01_All 0.16256 0.0121780 0.15084 0.17339
##   xgb_500_1_0.01_All 0.16116 0.0126982 0.14789 0.17352
##   xgb_100_2_0.01_All 0.18151 0.0089164 0.17312 0.18686
##   xgb_200_2_0.01_All 0.16256 0.0121780 0.15084 0.17339
##   xgb_500_2_0.01_All 0.16116 0.0126982 0.14789 0.17352
##   xgb_100_3_0.01_All 0.18151 0.0089164 0.17312 0.18686
##   xgb_200_3_0.01_All 0.16256 0.0121780 0.15084 0.17339
##   xgb_500_3_0.01_All 0.16116 0.0126982 0.14789 0.17352
##   xgb_100_4_0.01_All 0.18151 0.0089164 0.17312 0.18686
##   xgb_200_4_0.01_All 0.16256 0.0121780 0.15084 0.17339
##   xgb_500_4_0.01_All 0.16116 0.0126982 0.14789 0.17352
##    xgb_100_1_0.1_All 0.16119 0.0127952 0.15240 0.17263
##    xgb_200_1_0.1_All 0.16119 0.0127952 0.15240 0.17263
##    xgb_500_1_0.1_All 0.16119 0.0127952 0.15240 0.17263
##    xgb_100_2_0.1_All 0.16119 0.0127952 0.15240 0.17263
##    xgb_200_2_0.1_All 0.16119 0.0127952 0.15240 0.17263
##    xgb_500_2_0.1_All 0.16119 0.0127952 0.15240 0.17263
##    xgb_100_3_0.1_All 0.16119 0.0127952 0.15240 0.17263
##    xgb_200_3_0.1_All 0.16119 0.0127952 0.15240 0.17263
##    xgb_500_3_0.1_All 0.16119 0.0127952 0.15240 0.17263
##    xgb_100_4_0.1_All 0.16119 0.0127952 0.15240 0.17263
##    xgb_200_4_0.1_All 0.16119 0.0127952 0.15240 0.17263
##    xgb_500_4_0.1_All 0.16119 0.0127952 0.15240 0.17263
##  SL.randomForest_All 0.12289 0.0143333 0.11559 0.12745

review_weights(cv_sl)

##                     mean(weight)        sd       min       max
## SL.randomForest_All    0.7256498 0.1754881 0.5257817 0.8544914
## SL.glmnet_All          0.2743502 0.1754881 0.1455086 0.4742183
## SL.mean_All            0.0000000 0.0000000 0.0000000 0.0000000
## xgb_100_1_0.001_All    0.0000000 0.0000000 0.0000000 0.0000000
## xgb_200_1_0.001_All    0.0000000 0.0000000 0.0000000 0.0000000
## xgb_500_1_0.001_All    0.0000000 0.0000000 0.0000000 0.0000000
## xgb_100_2_0.001_All    0.0000000 0.0000000 0.0000000 0.0000000
## xgb_200_2_0.001_All    0.0000000 0.0000000 0.0000000 0.0000000
## xgb_500_2_0.001_All    0.0000000 0.0000000 0.0000000 0.0000000
## xgb_100_3_0.001_All    0.0000000 0.0000000 0.0000000 0.0000000
## xgb_200_3_0.001_All    0.0000000 0.0000000 0.0000000 0.0000000
## xgb_500_3_0.001_All    0.0000000 0.0000000 0.0000000 0.0000000
## xgb_100_4_0.001_All    0.0000000 0.0000000 0.0000000 0.0000000
## xgb_200_4_0.001_All    0.0000000 0.0000000 0.0000000 0.0000000
## xgb_500_4_0.001_All    0.0000000 0.0000000 0.0000000 0.0000000
## xgb_100_1_0.01_All     0.0000000 0.0000000 0.0000000 0.0000000
## xgb_200_1_0.01_All     0.0000000 0.0000000 0.0000000 0.0000000
## xgb_500_1_0.01_All     0.0000000 0.0000000 0.0000000 0.0000000
## xgb_100_2_0.01_All     0.0000000 0.0000000 0.0000000 0.0000000
## xgb_200_2_0.01_All     0.0000000 0.0000000 0.0000000 0.0000000
## xgb_500_2_0.01_All     0.0000000 0.0000000 0.0000000 0.0000000
## xgb_100_3_0.01_All     0.0000000 0.0000000 0.0000000 0.0000000
## xgb_200_3_0.01_All     0.0000000 0.0000000 0.0000000 0.0000000
## xgb_500_3_0.01_All     0.0000000 0.0000000 0.0000000 0.0000000
## xgb_100_4_0.01_All     0.0000000 0.0000000 0.0000000 0.0000000
## xgb_200_4_0.01_All     0.0000000 0.0000000 0.0000000 0.0000000
## xgb_500_4_0.01_All     0.0000000 0.0000000 0.0000000 0.0000000
## xgb_100_1_0.1_All      0.0000000 0.0000000 0.0000000 0.0000000
## xgb_200_1_0.1_All      0.0000000 0.0000000 0.0000000 0.0000000
## xgb_500_1_0.1_All      0.0000000 0.0000000 0.0000000 0.0000000
## xgb_100_2_0.1_All      0.0000000 0.0000000 0.0000000 0.0000000
## xgb_200_2_0.1_All      0.0000000 0.0000000 0.0000000 0.0000000
## xgb_500_2_0.1_All      0.0000000 0.0000000 0.0000000 0.0000000
## xgb_100_3_0.1_All      0.0000000 0.0000000 0.0000000 0.0000000
## xgb_200_3_0.1_All      0.0000000 0.0000000 0.0000000 0.0000000
## xgb_500_3_0.1_All      0.0000000 0.0000000 0.0000000 0.0000000
## xgb_100_4_0.1_All      0.0000000 0.0000000 0.0000000 0.0000000
## xgb_200_4_0.1_All      0.0000000 0.0000000 0.0000000 0.0000000
## xgb_500_4_0.1_All      0.0000000 0.0000000 0.0000000 0.0000000

#We can see how stochastic the weights are for each individual execution of SuperLearner.

#Finally, plot the performance for the different settings.

plot(cv_sl) + theme_bw()

##### recalc #####
# Binary outcome example adapted from SuperLearner examples

set.seed(1)
N <- 200
X <- matrix(rnorm(N*10), N, 10)
X <- as.data.frame(X)
Y <- rbinom(N, 1, plogis(.2*X[, 1] + .1*X[, 2] - .2*X[, 3] + 
                           .1*X[, 3]*X[, 4] - .2*abs(X[, 4])))

SL.library <- c("SL.glmnet", "SL.glm", "SL.knn", "SL.gam", "SL.mean")

# least squares loss function
set.seed(1) # for reproducibility
fit_nnls <- SuperLearner(Y = Y, X = X, SL.library = SL.library, 
                         verbose = TRUE, method = "method.NNLS", family = binomial())
fit_nnls
#                    Risk       Coef
# SL.glmnet_All 0.2439433 0.01293059
# SL.glm_All    0.2461245 0.08408060
# SL.knn_All    0.2604000 0.09600353
# SL.gam_All    0.2471651 0.40761918
# SL.mean_All   0.2486049 0.39936611


# negative log binomial likelihood loss function
fit_nnloglik <- recombineSL(fit_nnls, Y = Y, method = "method.NNloglik")
fit_nnloglik
#                    Risk      Coef
# SL.glmnet_All 0.6815911 0.1577228
# SL.glm_All    0.6918926 0.0000000
# SL.knn_All          Inf 0.0000000
# SL.gam_All    0.6935383 0.6292881
# SL.mean_All   0.6904050 0.2129891

# If we use the same seed as the original `fit_nnls`, then
# the recombineSL and SuperLearner results will be identical
# however, the recombineSL version will be much faster since
# it doesn't have to re-fit all the base learners.
set.seed(1)
fit_nnloglik2 <- SuperLearner(Y = Y, X = X, SL.library = SL.library,
                              verbose = TRUE, method = "method.NNloglik", family = binomial())
fit_nnloglik2
#                    Risk      Coef
# SL.glmnet_All 0.6815911 0.1577228
# SL.glm_All    0.6918926 0.0000000
# SL.knn_All          Inf 0.0000000
# SL.gam_All    0.6935383 0.6292881
# SL.mean_All   0.6904050 0.2129891
##### recalc with CV #####
# Binary outcome example adapted from SuperLearner examples

set.seed(1)
N <- 200
X <- matrix(rnorm(N*10), N, 10)
X <- as.data.frame(X)
Y <- rbinom(N, 1, plogis(.2*X[, 1] + .1*X[, 2] - .2*X[, 3] + 
                           .1*X[, 3]*X[, 4] - .2*abs(X[, 4])))

SL.library <- c("SL.glmnet", "SL.glm", "SL.knn", "SL.gam", "SL.mean")

# least squares loss function
set.seed(1) # for reproducibility
cvfit_nnls <- CV.SuperLearner(Y = Y, X = X, V = 10, SL.library = SL.library, 
                              verbose = TRUE, method = "method.NNLS", family = binomial())
cvfit_nnls$coef
#    SL.glmnet_All SL.glm_All  SL.knn_All SL.gam_All SL.mean_All
# 1      0.0000000 0.00000000 0.000000000  0.4143862   0.5856138
# 2      0.0000000 0.00000000 0.304802397  0.3047478   0.3904498
# 3      0.0000000 0.00000000 0.002897533  0.5544075   0.4426950
# 4      0.0000000 0.20322642 0.000000000  0.1121891   0.6845845
# 5      0.1743973 0.00000000 0.032471026  0.3580624   0.4350693
# 6      0.0000000 0.00000000 0.099881535  0.3662309   0.5338876
# 7      0.0000000 0.00000000 0.234876082  0.2942472   0.4708767
# 8      0.0000000 0.06424676 0.113988158  0.5600208   0.2617443
# 9      0.0000000 0.00000000 0.338030342  0.2762604   0.3857093
# 10     0.3022442 0.00000000 0.294226204  0.1394534   0.2640762


# negative log binomial likelihood loss function
cvfit_nnloglik <- recombineCVSL(cvfit_nnls, method = "method.NNloglik")
cvfit_nnloglik$coef
#    SL.glmnet_All SL.glm_All SL.knn_All SL.gam_All SL.mean_All
# 1      0.0000000  0.0000000 0.00000000  0.5974799  0.40252010
# 2      0.0000000  0.0000000 0.31177345  0.6882266  0.00000000
# 3      0.0000000  0.0000000 0.01377469  0.8544238  0.13180152
# 4      0.0000000  0.1644188 0.00000000  0.2387919  0.59678930
# 5      0.2142254  0.0000000 0.00000000  0.3729426  0.41283197
# 6      0.0000000  0.0000000 0.00000000  0.5847150  0.41528502
# 7      0.0000000  0.0000000 0.47538172  0.5080311  0.01658722
# 8      0.0000000  0.0000000 0.00000000  1.0000000  0.00000000
# 9      0.0000000  0.0000000 0.45384961  0.2923480  0.25380243
# 10     0.3977816  0.0000000 0.27927906  0.1606384  0.16230097

# If we use the same seed as the original `cvfit_nnls`, then
# the recombineCVSL and CV.SuperLearner results will be identical
# however, the recombineCVSL version will be much faster since
# it doesn't have to re-fit all the base learners, V times each.
set.seed(1)
cvfit_nnloglik2 <- CV.SuperLearner(Y = Y, X = X, V = 10, SL.library = SL.library,
                                   verbose = TRUE, method = "method.NNloglik", family = binomial())
cvfit_nnloglik2$coef
#    SL.glmnet_All SL.glm_All SL.knn_All SL.gam_All SL.mean_All
# 1      0.0000000  0.0000000 0.00000000  0.5974799  0.40252010
# 2      0.0000000  0.0000000 0.31177345  0.6882266  0.00000000
# 3      0.0000000  0.0000000 0.01377469  0.8544238  0.13180152
# 4      0.0000000  0.1644188 0.00000000  0.2387919  0.59678930
# 5      0.2142254  0.0000000 0.00000000  0.3729426  0.41283197
# 6      0.0000000  0.0000000 0.00000000  0.5847150  0.41528502
# 7      0.0000000  0.0000000 0.47538172  0.5080311  0.01658722
# 8      0.0000000  0.0000000 0.00000000  1.0000000  0.00000000
# 9      0.0000000  0.0000000 0.45384961  0.2923480  0.25380243
# 10     0.3977816  0.0000000 0.27927906  0.1606384  0.16230097

#We run summary on the cv_sl object rather than simply printing the object.
cv_sl<-fit_nnls
summary(cv_sl)


# Review the distribution of the best single learner as external CV folds.
table(simplify2array(cv_sl$whichDiscreteSL))

# Plot the performance with 95% CIs (use a better ggplot theme).
plot(cv_sl) + theme_bw()

# Save plot to a file.
ggsave("SuperLearner.png")


inCV<-c(3,5,10)
for(intCVitr in inCV){
  for(itr in methodsz){
    # Review the outcome variable distribution.
    table(Y_train, useNA = "ifany")
    allmodel <- paste0(itr,intCVitr)
    if(CrashNRep(allmodel)) {next()}
    
    
    fail.try.main <- T 
    try({
      # Set the seed for reproducibility.
      set.seed(seed = seed.var)
      
      #Let’s fit 2 separate models: lasso (sparse, penalized OLS) and randomForest. We specify family = binomial() because we are predicting a binary outcome, aka classification. With a continuous outcome we would specify family = gaussian().
      # Fit lasso model.
      when<-proc.time()
      
      #cvControl	
      #A list of parameters to control the outer cross-validation process. The outer cross-validation is the sample spliting for evaluating the SuperLearner. Parameters include V, stratifyCV, shuffle and validRows. See SuperLearner.CV.control for details.
      #innerCvControl	
      #A list of lists of parameters to control the inner cross-validation process. It should have V elements in the list, each a valid cvControl list. If only a single value, then replicated across all folds. The inner cross-validation are the values passed to each of the V SuperLearner calls. Parameters include V, stratifyCV, shuffle and validRows. See SuperLearner.CV.control for details.
      
      if(itr=="method.NNLS"){
        fit_nnls <- CV.SuperLearner(Y = Y_train, X = X_train,  SL.library = super, 
                                    verbose = TRUE, method = "method.NNLS",
                                    control = list(saveFitLibrary = TRUE),
                                    innerCvControl = list(list(V = intCVitr)),
                                    V = cv.iters)
      } else {
        fit_nnls<- recombineCVSL(fit_nnls, Y = Y_train, method = itr)
      }
      
      summary(fit_nnls)
      print(fit_nnls)
      fit_nnls$coef
      fit_nnls$control
      fit_nnls$cvControl
      names(fit_nnls)
      fit_nnls$SL.predict
      table(simplify2array(fit_nnls$whichDiscreteSL))
      predictions<-predict(fit_nnls$AllSL, X_holdout)
      for(itt in names(predictions)) print(RMSE(Y_holdout,predictions[,c(as.character(itt))]))
      #, onlySL = T
      predics<- predict(fit_nnls, X_holdout)$pred
      trainpred<- predict(fit_nnls, X_train, onlySL = T)$pred 
      
      printPredMets(predicted.outcomes=predics,trainpred =trainpred ,hypercount="none")
      fail.try.main<-F  
    })
    
    if(fail.try.main){    
      print(c("failed","failed",date(),datasource,missingdata,withextra,norming,which.computer,task.subject,allmodel))
      write.table(paste("Fail","Fail","Fail","Fail","Fail",date(),allmodel,column.to.predict,trans.y,datasource,missingdata,withextra,norming,which.computer,task.subject,FN,high.fold,.Random.seed[1],.Random.seed[2],seed.var,round(proc.time()[3]-when[3]),  sep = ","),
                  file = out.file, append =TRUE, quote = F, sep = ",",
                  eol = "\n", na = "NA", dec = ".", row.names = F,
                  col.names = F, qmethod = "double")
    }
  }
}
