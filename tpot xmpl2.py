from tpot import TPOTRegressor
from sklearn.datasets import load_boston
from sklearn.model_selection import train_test_split
import time  # This is required to include time module.
import os

print(dummy)
housing = load_boston()
X_train, X_test, y_train, y_test = train_test_split(housing.data, housing.target,
                                                    train_size=0.75, test_size=0.25)

generationcountz = 3
retainpopulationz = 50
offspring_sizez = 100
cvz = 10
random_statez = 222
early_stop = 3
mins_onapipe = 15
checkpoint_folder = "tpot"
pipefile = 'tpot_boston_pipeline.py'

ticks = time.time()

tpot = TPOTRegressor(generations=generationcount, population_size=retainpopulation,
                     offspring_size=offspring_size,early_stop=early_stop,max_eval_time_mins=mins_onapipe,
                     periodic_checkpoint_folder=checkpoint_folder,verbosity=2)
tpot.fit(X_train, y_train)
print(tpot.score(X_test, y_test))

ticks2 = ticks - time.time()
print(ticks2)

oveRMSE = tpot.score(X_train, y_train)
trainpred = tpot.predict(X_train)
print(oveRMSE)
print(trainpred)


predictions = tpot.predict(X_test)
print(predictions)

os.chdir("tpot")
tpot.export(pipefile)