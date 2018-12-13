import numpy as np
import pandas as pd
from sklearn.ensemble import GradientBoostingRegressor
from sklearn.linear_model import ElasticNetCV
from sklearn.model_selection import train_test_split
from sklearn.pipeline import make_pipeline, make_union
from sklearn.preprocessing import MaxAbsScaler
from tpot.builtins import StackingEstimator

# NOTE: Make sure that the class is labeled 'target' in the data file
tpot_data = pd.read_csv('PATH/TO/DATA/FILE', sep='COLUMN_SEPARATOR', dtype=np.float64)
features = tpot_data.drop('target', axis=1).values
training_features, testing_features, training_target, testing_target = \
            train_test_split(features, tpot_data['target'].values, random_state=42)

# Score on the training set was:-0.6575483554933482
exported_pipeline = make_pipeline(
    StackingEstimator(estimator=GradientBoostingRegressor(alpha=0.85, learning_rate=0.01, loss="huber", max_depth=10, max_features=0.45, min_samples_leaf=4, min_samples_split=19, n_estimators=100, subsample=1.0)),
    StackingEstimator(estimator=GradientBoostingRegressor(alpha=0.85, learning_rate=0.01, loss="huber", max_depth=10, max_features=0.4, min_samples_leaf=4, min_samples_split=2, n_estimators=100, subsample=1.0)),
    MaxAbsScaler(),
    ElasticNetCV(l1_ratio=0.55, tol=0.0001)
)

exported_pipeline.fit(training_features, training_target)
results = exported_pipeline.predict(testing_features)
