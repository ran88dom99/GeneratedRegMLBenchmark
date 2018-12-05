import numpy as np
import pandas as pd
from sklearn.ensemble import RandomForestRegressor
from sklearn.linear_model import RidgeCV
from sklearn.model_selection import train_test_split
from sklearn.pipeline import make_pipeline, make_union
from sklearn.preprocessing import MaxAbsScaler
from sklearn.svm import LinearSVR
from tpot.builtins import StackingEstimator

# NOTE: Make sure that the class is labeled 'target' in the data file
tpot_data = pd.read_csv('PATH/TO/DATA/FILE', sep='COLUMN_SEPARATOR', dtype=np.float64)
features = tpot_data.drop('target', axis=1).values
training_features, testing_features, training_target, testing_target = \
            train_test_split(features, tpot_data['target'].values, random_state=42)

# Score on the training set was:-0.5532507922479704
exported_pipeline = make_pipeline(
    MaxAbsScaler(),
    StackingEstimator(estimator=RandomForestRegressor(bootstrap=False, max_features=0.4, min_samples_leaf=15, min_samples_split=9, n_estimators=100)),
    StackingEstimator(estimator=RandomForestRegressor(bootstrap=False, max_features=0.4, min_samples_leaf=15, min_samples_split=9, n_estimators=100)),
    StackingEstimator(estimator=LinearSVR(C=0.5, dual=False, epsilon=0.001, loss="squared_epsilon_insensitive", tol=0.01)),
    StackingEstimator(estimator=RidgeCV()),
    StackingEstimator(estimator=RandomForestRegressor(bootstrap=False, max_features=0.8, min_samples_leaf=6, min_samples_split=9, n_estimators=100)),
    LinearSVR(C=0.5, dual=True, epsilon=0.001, loss="epsilon_insensitive", tol=0.0001)
)

exported_pipeline.fit(training_features, training_target)
results = exported_pipeline.predict(testing_features)
