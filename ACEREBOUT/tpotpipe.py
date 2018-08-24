import numpy as np
import pandas as pd
from sklearn.decomposition import FastICA
from sklearn.ensemble import GradientBoostingRegressor
from sklearn.feature_selection import SelectPercentile, VarianceThreshold, f_regression
from sklearn.linear_model import ElasticNetCV
from sklearn.model_selection import train_test_split
from sklearn.neighbors import KNeighborsRegressor
from sklearn.pipeline import make_pipeline, make_union
from sklearn.preprocessing import RobustScaler
from tpot.builtins import StackingEstimator, ZeroCount

# NOTE: Make sure that the class is labeled 'target' in the data file
tpot_data = pd.read_csv('PATH/TO/DATA/FILE', sep='COLUMN_SEPARATOR', dtype=np.float64)
features = tpot_data.drop('target', axis=1).values
training_features, testing_features, training_target, testing_target = \
            train_test_split(features, tpot_data['target'].values, random_state=42)

# Score on the training set was:-0.5545738526873115
exported_pipeline = make_pipeline(
    make_union(
        VarianceThreshold(threshold=0.001),
        make_pipeline(
            RobustScaler(),
            StackingEstimator(estimator=GradientBoostingRegressor(alpha=0.75, learning_rate=0.01, loss="huber", max_depth=6, max_features=0.7000000000000001, min_samples_leaf=6, min_samples_split=13, n_estimators=100, subsample=0.8500000000000001)),
            StackingEstimator(estimator=KNeighborsRegressor(n_neighbors=37, p=2, weights="uniform")),
            ZeroCount(),
            StackingEstimator(estimator=ElasticNetCV(l1_ratio=0.7000000000000001, tol=0.1)),
            SelectPercentile(score_func=f_regression, percentile=10),
            RobustScaler(),
            StackingEstimator(estimator=GradientBoostingRegressor(alpha=0.75, learning_rate=0.001, loss="huber", max_depth=10, max_features=0.35000000000000003, min_samples_leaf=6, min_samples_split=13, n_estimators=100, subsample=0.8500000000000001)),
            FastICA(tol=0.7000000000000001)
        )
    ),
    ElasticNetCV(l1_ratio=0.1, tol=0.1)
)

exported_pipeline.fit(training_features, training_target)
results = exported_pipeline.predict(testing_features)
