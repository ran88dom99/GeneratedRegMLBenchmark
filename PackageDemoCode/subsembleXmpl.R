 #example from documentation

#install.packages("subsemble")


list.of.packages<-c("subsemble","SuperLearner","RhpcBLASctl","biglasso","dbarts","sva","LogicReg","speedglm","KernelKnn")
new.packages <- list.of.packages[!(list.of.packages %in% installed.packages()[,"Package"])]
if(length(new.packages)) install.packages(new.packages, dep = TRUE)
#For XGBoost we need to tweak the install command a bit; Windows users may need to install Rtools first.
#install.packages("xgboost", repos=c("http://dmlc.ml/drat/", getOption("repos")), type="source")
library(SuperLearner)
library(subsemble)


library(cvAUC)
data(admissions)
head(admissions)

# Training data.
x <- subset(admissions, select=-c(Y))[1:400,]
y <- admissions$Y[1:400]
head(x)
head(y)

# Test data.
newx <- subset(admissions, select=-c(Y))[401:500,]
newy <- admissions$Y[401:500]


# Set up the Subsemble.

learner <- c("SL.randomForest", "SL.glm")
metalearner <- c("SL.glm")
subsets <- 2


# Train and test the model.
# With learnControl$multiType="crossprod" (the default), 
# we ensemble 4 models (2 subsets x 2 learners).

fit <- subsemble(x=x, y=y, newx=newx, family=binomial(), 
                 learner = learner, metalearner = metalearner,
                 subsets = subsets)


# Evaulate the model by calculating AUC on the test set.

auc <- cvAUC(predictions=fit$pred, labels=newy)$cvAUC
print(auc)  # Test set AUC is: 0.937 No .93064


# We can also use the predict method to generate predictions on new data afterwards.

pred <- predict(fit, newx)
auc <- cvAUC(predictions=pred$pred, labels=newy)$cvAUC
print(auc)  # Test set AUC is: 0.937 No. Also .93064


# Modify the learnControl argument and train/eval a new Subsemble.
# With learnControl$multiType="divisor", 
# we ensemble only 2 models (one for each subset).

fit <- subsemble(x=x, y=y, newx=newx, family=binomial(), 
                 learner = learner, metalearner = metalearner,
                 subsets = subsets,
                 learnControl = list(multiType="divisor"))

auc <- cvAUC(predictions=fit$pred, labels=newy)$cvAUC
print(auc)  # Test set AUC is: 0.922 no .931


# An example using a single learner.
# In this case there are 3 subsets and 1 learner,
# for a total of 3 models in the ensemble.

learner <- c("SL.randomForest")
metalearner <- c("SL.glmnet")
subsets <- 3

fit <- subsemble(x=x, y=y, newx=newx, family=binomial(),
                 learner = learner, metalearner = metalearner,
                 subsets = subsets)

auc <- cvAUC(predictions=fit$pred, labels=newy)$cvAUC
print(auc)  # Test set AUC is: 0.925 no .927


# An example using the full data (i.e. subsets = 1).  
# Here, we have an ensemble of 2 models (one for each of the 2 learners).
# This is equivalent to the Super Learner algorithm.

learner <- c("SL.randomForest", "SL.glm")
metalearner <- c("SL.glm")
subsets <- 1

fit <- subsemble(x=x, y=y, newx=newx, family=binomial(), 
                 learner = learner, metalearner = metalearner,
                 subsets = subsets)

auc <- cvAUC(predictions=fit$pred, labels=newY)$cvAUC
print(auc)  # Test set AUC is: 0.935 #no .927

