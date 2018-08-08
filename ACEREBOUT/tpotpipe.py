import numpy as np
import pandas as pd
from sklearn.ensemble import GradientBoostingRegressor
from sklearn.feature_selection import SelectPercentile, f_regression
from sklearn.model_selection import train_test_split
from sklearn.pipeline import make_pipeline, make_union
from sklearn.svm import LinearSVR
from sklearn.tree import DecisionTreeRegressor
from tpot.builtins import StackingEstimator, ZeroCount

# NOTE: Make sure that the class is labeled 'target' in the data file
tpot_data = pd.read_csv('PATH/TO/DATA/FILE', sep='COLUMN_SEPARATOR', dtype=np.float64)
features = tpot_data.drop('target', axis=1).values
training_features, testing_features, training_target, testing_target = \
            train_test_split(features, tpot_data['target'].values, random_state=42)

# Score on the training set was:-0.6839663716401483
exported_pipeline = make_pipeline(
    StackingEstimator(estimator=GradientBoostingRegressor(alpha=0.85, learning_rate=0.01, loss="huber", max_depth=10, max_features=0.5, min_samples_leaf=4, min_samples_split=2, n_estimators=100, subsample=0.9000000000000001)),
    ZeroCount(),
    ZeroCount(),
    StackingEstimator(estimator=DecisionTreeRegressor(max_depth=8, min_samples_leaf=1, min_samples_split=4)),
    StackingEstimator(estimator=GradientBoostingRegressor(alpha=0.9, learning_rate=0.01, loss="huber", max_depth=8, max_features=0.6000000000000001, min_samples_leaf=15, min_samples_split=19, n_estimators=100, subsample=0.9000000000000001)),
    SelectPercentile(score_func=f_regression, percentile=51),
    LinearSVR(C=1.0, dual=False, epsilon=0.0001, loss="squared_epsilon_insensitive", tol=0.001)
)

exported_pipeline.fit(training_features, training_target)
results = exported_pipeline.predict(testing_features)
