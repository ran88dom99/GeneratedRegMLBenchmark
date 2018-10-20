import numpy as np
import pandas as pd
from sklearn.ensemble import GradientBoostingRegressor, RandomForestRegressor
from sklearn.feature_selection import SelectPercentile, f_regression
from sklearn.model_selection import train_test_split
from sklearn.pipeline import make_pipeline, make_union
from tpot.builtins import StackingEstimator

# NOTE: Make sure that the class is labeled 'target' in the data file
tpot_data = pd.read_csv('PATH/TO/DATA/FILE', sep='COLUMN_SEPARATOR', dtype=np.float64)
features = tpot_data.drop('target', axis=1).values
training_features, testing_features, training_target, testing_target = \
            train_test_split(features, tpot_data['target'].values, random_state=42)

# Score on the training set was:-0.6709974510892307
exported_pipeline = make_pipeline(
    SelectPercentile(score_func=f_regression, percentile=61),
    StackingEstimator(estimator=GradientBoostingRegressor(alpha=0.85, learning_rate=0.01, loss="huber", max_depth=6, max_features=1.0, min_samples_leaf=6, min_samples_split=18, n_estimators=100, subsample=0.8500000000000001)),
    RandomForestRegressor(bootstrap=False, max_features=0.3, min_samples_leaf=11, min_samples_split=6, n_estimators=100)
)

exported_pipeline.fit(training_features, training_target)
results = exported_pipeline.predict(testing_features)
