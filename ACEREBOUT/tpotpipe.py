import numpy as np
import pandas as pd
from sklearn.decomposition import FastICA
from sklearn.feature_selection import SelectPercentile, VarianceThreshold, f_regression
from sklearn.linear_model import LassoLarsCV
from sklearn.model_selection import train_test_split
from sklearn.neighbors import KNeighborsRegressor
from sklearn.pipeline import make_pipeline, make_union
from sklearn.preprocessing import Normalizer
from sklearn.svm import LinearSVR
from tpot.builtins import StackingEstimator, ZeroCount
from xgboost import XGBRegressor
from sklearn.preprocessing import FunctionTransformer
from copy import copy

# NOTE: Make sure that the class is labeled 'target' in the data file
tpot_data = pd.read_csv('PATH/TO/DATA/FILE', sep='COLUMN_SEPARATOR', dtype=np.float64)
features = tpot_data.drop('target', axis=1).values
training_features, testing_features, training_target, testing_target = \
            train_test_split(features, tpot_data['target'].values, random_state=42)

# Score on the training set was:-0.5810950598438567
exported_pipeline = make_pipeline(
    make_union(
        make_pipeline(
            Normalizer(norm="max"),
            SelectPercentile(score_func=f_regression, percentile=55),
            StackingEstimator(estimator=KNeighborsRegressor(n_neighbors=26, p=2, weights="distance")),
            Normalizer(norm="max"),
            SelectPercentile(score_func=f_regression, percentile=38),
            Normalizer(norm="l1"),
            FastICA(tol=0.15000000000000002),
            StackingEstimator(estimator=XGBRegressor(learning_rate=0.001, max_depth=5, min_child_weight=13, n_estimators=100, nthread=1, subsample=0.7500000000000001)),
            StackingEstimator(estimator=LassoLarsCV(normalize=False)),
            VarianceThreshold(threshold=0.01),
            ZeroCount()
        ),
        FunctionTransformer(copy)
    ),
    SelectPercentile(score_func=f_regression, percentile=59),
    StackingEstimator(estimator=XGBRegressor(learning_rate=0.001, max_depth=5, min_child_weight=7, n_estimators=100, nthread=1, subsample=0.45)),
    LinearSVR(C=15.0, dual=True, epsilon=1.0, loss="epsilon_insensitive", tol=1e-05)
)

exported_pipeline.fit(training_features, training_target)
results = exported_pipeline.predict(testing_features)
