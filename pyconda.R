#http://proceedings.mlr.press/v64/olson_tpot_2016.pdf
library(reticulate)

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


when <- proc.time()



if(T){
  generationcount = r_to_py(3)
  retainpopulation = r_to_py(50)
  offspring_size = r_to_py(100)
  cv = r_to_py(10)
  random_state = r_to_py(222)
  early_stop = r_to_py(3)
  mins_onapipe = r_to_py(15)
  checkpoint_folder = r_to_py("tpot")
  pipefile = r_to_py('tpot_boston_pipeline.py')
}



tpot <- import("tpot")
ztpot<-tpot$TPOTRegressor(generations=generationcount, population_size=retainpopulation,
                   offspring_size=offspring_size,early_stop=early_stop,max_eval_time_mins=mins_onapipe,
                   periodic_checkpoint_folder=checkpoint_folder,verbosity=2)

data<-r_to_py(data)
outcome<-r_to_py(outcome)
ztpot$fit(data, outcome)
prde<-ztpot$predict(data)
RMSE(prde,py_to_r(outcome))
SE <- RMSE(trainpred,y_train)
predicttt <- RMSE(predictions,y_test)
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