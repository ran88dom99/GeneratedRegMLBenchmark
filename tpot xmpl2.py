from tpot import TPOTRegressor
from sklearn.datasets import load_boston
from sklearn.model_selection import train_test_split
import time  # This is required to include time module.

housing = load_boston()
X_train, X_test, y_train, y_test = train_test_split(housing.data, housing.target,
                                                    train_size=0.75, test_size=0.25)



ticks = time.time()

tpot = TPOTRegressor(generations=20, population_size=20, verbosity=2)
tpot.fit(X_train, y_train)
print(tpot.score(X_test, y_test))

print(tpot.score(X_train, y_train))

ticks2 = ticks - time.time()
print(ticks2)
os.chdir("tpot")
tpot.export('tpot_boston_pipeline.py')