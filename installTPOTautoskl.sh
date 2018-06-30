 #!/bin/bash

python -m pip install --upgrade pip
 
pip install deap update_checker tqdm stopit
pip install xgboost
pip install scikit-mdr skrebate
pip install tpot

curl https://raw.githubusercontent.com/automl/auto-sklearn/master/requirements.txt | xargs -n 1 -L 1 pip install
pip install auto-sklearn
conda install gxx_linux-64 gcc_linux-64 swig
 source activate r-reticulate