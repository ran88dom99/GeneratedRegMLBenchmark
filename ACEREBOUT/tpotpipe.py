import numpy as np
import pandas as pd
from sklearn.ensemble import GradientBoostingRegressor, RandomForestRegressor
from sklearn.linear_model import ElasticNetCV, LassoLarsCV
from sklearn.model_selection import train_test_split
from sklearn.pipeline import make_pipeline, make_union
from sklearn.preprocessing import RobustScaler
from tpot.builtins import StackingEstimator

# NOTE: Make sure that the class is labeled 'target' in the data file
tpot_data = pd.read_csv('PATH/TO/DATA/FILE', sep='COLUMN_SEPARATOR', dtype=np.float64)
features = tpot_data.drop('target', axis=1).values
training_features, testing_features, training_target, testing_target = \
            train_test_split(features, tpot_data['target'].values, random_state=42)

# Score on the training set was:-0.6892357723577235
exported_pipeline = make_pipeline(
    StackingEstimator(estimator=GradientBoostingRegressor(alpha=0.75, learning_rate=0.01, loss="huber", max_depth=10, max_features=0.8, min_samples_leaf=6, min_samples_split=13, n_estimators=100, subsample=0.8500000000000001)),
    RobustScaler(),
    StackingEstimator(estimator=LassoLarsCV(normalize=True)),
    StackingEstimator(estimator=ElasticNetCV(l1_ratio=0.75, tol=0.001)),
    StackingEstimator(estimator=GradientBoostingRegressor(alpha=0.95, learning_rate=0.01, loss="huber", max_depth=7, max_features=0.8, min_samples_leaf=6, min_samples_split=13, n_estimators=100, subsample=0.8500000000000001)),
    RandomForestRegressor(bootstrap=False, max_features=0.15000000000000002, min_samples_leaf=1, min_samples_split=3, n_estimators=100)
)

exported_pipeline.fit(training_features, training_target)
results = exported_pipeline.predict(testing_features)
