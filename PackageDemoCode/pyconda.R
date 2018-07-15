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
# Reduce to a dataset of 150 observations to speed up model fitting.
train_obs = sample(nrow(data), 400)

# X is our training sample.
X_train = data[train_obs, ]

# Create a holdout set for evaluating model performance.
# Note: cross-validation is even better than a single holdout sample.
X_holdout = data[-train_obs, ]

# Create a binary outcome variable: towns in which median home value is > 22,000.

Y_train = outcome[train_obs]
Y_holdout = outcome[-train_obs]

#http://proceedings.mlr.press/v64/olson_tpot_2016.pdf
library(reticulate)

when <- proc.time()


if(T){
  generationcount = r_to_py(as.integer(3))
  retainpopulation = r_to_py(as.integer(50))
  offspring_size = r_to_py(as.integer(100))
  cv = r_to_py(as.integer(10))
  random_state = r_to_py(as.integer(222))
  early_stop = r_to_py(as.integer(3))
  mins_onapipe = r_to_py(as.integer(15))
  checkpoint_folder = r_to_py("tpot")
  pipefile = r_to_py('tpot_boston_pipeline.py')
}

tpot <- import("tpot")
ztpot<-tpot$TPOTRegressor(generations=generationcount, population_size=retainpopulation,
                   offspring_size=offspring_size,early_stop=early_stop,max_eval_time_mins=mins_onapipe,
                   periodic_checkpoint_folder=checkpoint_folder,verbosity=2)

X_train<-r_to_py(X_train)
Y_train<-r_to_py(Y_train)
ztpot$fit(X_train, Y_train)
prde<-ztpot$predict(X_train)
SE <- RMSE(prde,py_to_r(Y_train))

X_holdout<-r_to_py(X_holdout)
Y_holdout<-r_to_py(Y_holdout)
predictions<-ztpot$predict(X_holdout)

predicttt <- RMSE(predictions,py_to_r(Y_holdout))
print(predicttt)
print(oveRMSE)
getwd()
setwd(base.folder)

if(F){ 
# install SciPy
conda_install(c( "scipy", "numpy", "scikit-learn", "pandas"))

# indicate that we want to use a specific condaenv

use_condaenv("base")
use_condaenv("ANACONDA")
conda_list()
}