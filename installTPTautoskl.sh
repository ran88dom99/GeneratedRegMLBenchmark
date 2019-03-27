 #!/bin/bash

python -m pip install --upgrade pip

conda install numpy scipy scikit-learn pandas

pip install deap update_checker tqdm stopit
pip install xgboost
pip install scikit-mdr skrebate
pip install tpot

pip install auto_ml
pip install stacked_generalization
pip install mlens

cd C:/xgboost
git clone --recursive https://github.com/dmlc/xgboost 
cd python-package

 python setup.py develop --user
 python setup.py install