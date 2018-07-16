#https://pbiecek.github.io/DALEX/
#https://github.com/pbiecek/DALEX
#install.packages("DALEX")
library(DALEX)
library(mlr)
library(breakDown)

data(apartments)
head(apartments)

set.seed(123)
regr_task <- makeRegrTask(id = "ap", data = apartments, target = "m2.price")
regr_lrn_rf <- makeLearner("regr.randomForest")
regr_lrn_nn <- makeLearner("regr.nnet")
regr_lrn_gbm <- makeLearner("regr.gbm", par.vals = list(n.trees = 500))


regr_lrn_nn <- setHyperPars(regr_lrn_nn, par.vals = list(maxit=500, size=2))
regr_lrn_nn <- makePreprocWrapperCaret(regr_lrn_nn, ppc.scale=TRUE, ppc.center=TRUE)


regr_rf <- train(regr_lrn_rf, regr_task)
regr_nn <- train(regr_lrn_nn, regr_task)
regr_gbm <- train(regr_lrn_gbm, regr_task)

data(apartmentsTest)
custom_predict <- function(object, newdata) {pred <- predict(object, newdata=newdata)
response <- pred$data$response
return(response)}

testpredict<-custom_predict(regr_rf,apartmentsTest)
str(apartmentsTest)
RMSE(testpredict,apartmentsTest$m2.price)
RMSE(mean(apartmentsTest$m2.price),apartmentsTest$m2.price)
RMSE(testpredict,mean(testpredict))

explainer_regr_rf <- DALEX::explain(regr_rf, data=apartmentsTest, y=apartmentsTest$m2.price, predict_function = custom_predict, label="rf")
explainer_regr_nn <- DALEX::explain(regr_nn, data=apartmentsTest, y=apartmentsTest$m2.price,
                                    predict_function = custom_predict, label="nn")
explainer_regr_gbm <- DALEX::explain(regr_gbm, data=apartmentsTest, y=apartmentsTest$m2.price,
                                     predict_function = custom_predict, label="gbm")

mp_regr_rf <- model_performance(explainer_regr_rf)
mp_regr_gbm <- model_performance(explainer_regr_gbm)
mp_regr_nn <- model_performance(explainer_regr_nn)

mp_regr_rf

plot(mp_regr_rf, mp_regr_nn, mp_regr_gbm)

plot(mp_regr_rf, mp_regr_nn, mp_regr_gbm, geom = "boxplot")

vi_regr_rf <- variable_importance(explainer_regr_rf, loss_function = loss_root_mean_square)
vi_regr_gbm <- variable_importance(explainer_regr_gbm, loss_function = loss_root_mean_square)
vi_regr_nn <- variable_importance(explainer_regr_nn, loss_function = loss_root_mean_square)
str(vi_regr_nn)
vig<-vi_regr_nn
plot(vi_regr_rf, vi_regr_gbm, vi_regr_nn)

vi_regr_rf <- variable_importance(explainer_regr_rf, loss_function = loss_root_mean_square, type="difference")
vi_regr_gbm <- variable_importance(explainer_regr_gbm, loss_function = loss_root_mean_square, type="difference")
vi_regr_nn <- variable_importance(explainer_regr_nn, loss_function = loss_root_mean_square, type="difference")

plot(vi_regr_rf, vi_regr_gbm, vi_regr_nn)

pdp_regr_rf  <- variable_response(explainer_regr_rf, variable =  "construction.year", type = "pdp")
pdp_regr_gbm  <- variable_response(explainer_regr_gbm, variable =  "construction.year", type = "pdp")
pdp_regr_nn  <- variable_response(explainer_regr_nn, variable =  "construction.year", type = "pdp")

plot(pdp_regr_rf, pdp_regr_gbm, pdp_regr_nn)


ale_regr_rf  <- variable_response(explainer_regr_rf, variable =  "construction.year", type = "ale")
ale_regr_gbm  <- variable_response(explainer_regr_gbm, variable =  "construction.year", type = "ale")
ale_regr_nn  <- variable_response(explainer_regr_nn, variable =  "construction.year", type = "ale")

plot(ale_regr_rf, ale_regr_gbm, ale_regr_nn)


mpp_regr_rf  <- variable_response(explainer_regr_rf, variable =  "district", type = "factor")
mpp_regr_gbm  <- variable_response(explainer_regr_gbm, variable =  "district", type = "factor")
mpp_regr_nn  <- variable_response(explainer_regr_nn, variable =  "district", type = "factor")

plot(mpp_regr_rf, mpp_regr_gbm, mpp_regr_nn)
##### varimp ####
## Not run: 
library("breakDown")
library("randomForest")
HR_rf_model <- randomForest(left~., data = breakDown::HR_data, ntree = 100)
explainer_rf  <- explain(HR_rf_model, data = HR_data, y = HR_data$left)
vd_rf <- variable_importance(explainer_rf, type = "raw")
vd_rf

HR_glm_model <- glm(left~., data = breakDown::HR_data, family = "binomial")
explainer_glm <- explain(HR_glm_model, data = HR_data, y = HR_data$left)
logit <- function(x) exp(x)/(1+exp(x))
vd_glm <- variable_importance(explainer_glm, type = "raw",
                              loss_function = function(observed, predicted)
                                sum((observed - logit(predicted))^2))
vd_glm

library("xgboost")
model_martix_train <- model.matrix(left~.-1, breakDown::HR_data)
data_train <- xgb.DMatrix(model_martix_train, label = breakDown::HR_data$left)
param <- list(max_depth = 2, eta = 1, silent = 1, nthread = 2,
              objective = "binary:logistic", eval_metric = "auc")
HR_xgb_model <- xgb.train(param, data_train, nrounds = 50)
explainer_xgb <- explain(HR_xgb_model, data = model_martix_train,
                         y = HR_data$left, label = "xgboost")
vd_xgb <- variable_importance(explainer_xgb, type = "raw")
vd_xgb
plot(vd_xgb)

## End(Not run)

##### caret #####
