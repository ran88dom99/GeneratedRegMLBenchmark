#http://proceedings.mlr.press/v64/olson_tpot_2016.pdf
library(reticulate)

when <- proc.time()
dummy<-"dummy"
generationcount<-r_to_py(generationcount)
dummy<-r_to_py(dummy)
source_python("tpot xmpl2.py")

oveRMSE <- RMSE(trainpred,y_train)
predicttt <- RMSE(predictions,y_test)
print(predicttt)
print(oveRMSE)
getwd()
setwd(base.folder)
tpot <- import("tpot")
ztpot<-tpot$TPOTRegressor(generations=generationcount, population_size=retainpopulation,
                   offspring_size=offspring_size,early_stop=early_stop,max_eval_time_mins=mins_onapipe,
                   periodic_checkpoint_folder=checkpoint_folder,verbosity=2)

data<-r_to_py(data)
outcome<-r_to_py(outcome)
ztpot$fit(data, outcome)

if(F){
# create a new environment 
conda_create("r-reticulate")

# install SciPy
conda_install("r-reticulate",c( "scipy", "numpy", "scikit-learn", "pandas"))
use_condaenv("r-reticulate")

py_list_attributes(pip)

#py_install(c( "deap", "update_checker", "tqdm", "stopit", "xgboost"))
#conda_install("r-reticulate",c( "scikit-mdr", "skrebate"))
#conda_install("r-reticulate","tpot")

#curl https://raw.githubusercontent.com/automl/auto-sklearn/master/requirements.txt | xargs -n 1 -L 1 pip install
#pip install auto-sklearn
#conda install gxx_linux-64 gcc_linux-64 swig
}

if(T){
  generationcount = as.integer(3)
  retainpopulation = as.integer(50)
  offspring_size = as.integer(100)
  cv = as.integer(10)
  random_state = as.integer(222)
  early_stop = as.integer(3)
  mins_onapipe = as.integer(15)
  checkpoint_folder = "tpot"
  pipefile = 'tpot_boston_pipeline.py'
}


library(reticulate)
os <- import("os")
pip <- import("pip")
#os$chdir("tests")
os$getcwd()
main <- py_run_string("x = 10")
main$x
py_run_string("x = 10")


# indicate that we want to use a specific condaenv
use_condaenv("base")
use_condaenv("ANACONDA")
conda_list()

#conda_remove("r-reticulate")

# import SciPy (will use "r-reticulate" as per call to use_condaenv)
scipy <- import("scipy")
tpot <- import("tpot")
