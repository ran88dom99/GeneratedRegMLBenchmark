library(reticulate)
os <- import("os")
pip <- import("pip")
#os$chdir("tests")
os$getcwd()
reticulate::py_install("scipy")
library(reticulate)
py_install("scipy")
library(reticulate)

# create a new environment 
conda_create("r-reticulate")

# install SciPy
conda_install("r-reticulate",c( "scipy", "numpy", "scikit-learn", "pandas"))
use_condaenv("r-reticulate")

py_list_attributes(pip)
pip$install()

source_python("tpotinstal.py")

py_install(c( "deap", "update_checker", "tqdm", "stopit", "xgboost"))
conda_install("r-reticulate",c( "scikit-mdr", "skrebate"))
conda_install("r-reticulate","tpot")

#curl https://raw.githubusercontent.com/automl/auto-sklearn/master/requirements.txt | xargs -n 1 -L 1 pip install
#pip install auto-sklearn
#conda install gxx_linux-64 gcc_linux-64 swig

main <- py_run_string("x = 10")
main$x
main<- py_run_string("python -m ensurepip --default-pip")
py_run_string("x = 10")
# import SciPy (it will be automatically discovered in "r-reticulate")
scipy <- import("scipy")
main<- py_run_string("pip install see")
library(reticulate)

# indicate that we want to use a specific condaenv
use_condaenv("r-reticulate")

# import SciPy (will use "r-reticulate" as per call to use_condaenv)
scipy <- import("scipy")