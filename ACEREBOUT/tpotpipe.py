import numpy as np
import pandas as pd
from sklearn.decomposition import PCA
from sklearn.ensemble import GradientBoostingRegressor
from sklearn.feature_selection import SelectPercentile, f_regression
from sklearn.linear_model import ElasticNetCV
from sklearn.model_selection import train_test_split
from sklearn.pipeline import make_pipeline, make_union
from sklearn.preprocessing import MaxAbsScaler
from tpot.builtins import StackingEstimator
from xgboost import XGBRegressor

# NOTE: Make sure that the class is labeled 'target' in the data file
tpot_data = pd.read_csv('PATH/TO/DATA/FILE', sep='COLUMN_SEPARATOR', dtype=np.float64)
features = tpot_data.drop('target', axis=1).values
training_features, testing_features, training_target, testing_target = \
            train_test_split(features, tpot_data['target'].values, random_state=42)

# Score on the training set was:-0.6706978765636953
exported_pipeline = make_pipeline(
    SelectPercentile(score_func=f_regression, percentile=40),
    StackingEstimator(estimator=GradientBoostingRegressor(alpha=0.75, learning_rate=0.01, loss="huber", max_depth=9, max_features=0.3, min_samples_leaf=3, min_samples_split=6, n_estimators=100, subsample=0.9000000000000001)),
    StackingEstimator(estimator=XGBRegressor(learning_rate=0.5, max_depth=10, min_child_weight=10, n_estimators=100, nthread=1, subsample=0.4)),
    StackingEstimator(estimator=GradientBoostingRegressor(alpha=0.75, learning_rate=0.01, loss="huber", max_depth=9, max_features=0.3, min_samples_leaf=3, min_samples_split=6, n_estimators=100, subsample=0.9000000000000001)),
    MaxAbsScaler(),
    PCA(iterated_power=3, svd_solver="randomized"),
    ElasticNetCV(l1_ratio=0.4, tol=0.01)
)

exported_pipeline.fit(training_features, training_target)
results = exported_pipeline.predict(testing_features)
