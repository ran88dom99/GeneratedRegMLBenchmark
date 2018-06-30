
 ### how has time worked in all previous small Rs?

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

library(reticulate)
os <- import("os")
pip <- import("pip")
#os$chdir("tests")
os$getcwd()
main <- py_run_string("x = 10")
main$x
py_run_string("x = 10")
source_python("tpot xmpl.py")
library(reticulate)

# indicate that we want to use a specific condaenv
use_condaenv("base")
use_condaenv("ANACONDA")
conda_list()

#conda_remove("r-reticulate")

# import SciPy (will use "r-reticulate" as per call to use_condaenv)
scipy <- import("scipy")
tpot <- import("tpot")
