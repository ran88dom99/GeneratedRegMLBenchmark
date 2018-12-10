import numpy as np
import pandas as pd
from sklearn.ensemble import ExtraTreesRegressor
from sklearn.feature_selection import SelectPercentile, f_regression
from sklearn.linear_model import ElasticNetCV, RidgeCV
from sklearn.model_selection import train_test_split
from sklearn.pipeline import make_pipeline, make_union
from sklearn.svm import LinearSVR
from sklearn.tree import DecisionTreeRegressor
from tpot.builtins import StackingEstimator
from xgboost import XGBRegressor

# NOTE: Make sure that the class is labeled 'target' in the data file
tpot_data = pd.read_csv('PATH/TO/DATA/FILE', sep='COLUMN_SEPARATOR', dtype=np.float64)
features = tpot_data.drop('target', axis=1).values
training_features, testing_features, training_target, testing_target = \
            train_test_split(features, tpot_data['target'].values, random_state=42)

# Score on the training set was:-0.6967993877645704
exported_pipeline = make_pipeline(
    StackingEstimator(estimator=LinearSVR(C=0.1, dual=False, epsilon=0.0001, loss="squared_epsilon_insensitive", tol=0.001)),
    SelectPercentile(score_func=f_regression, percentile=39),
    StackingEstimator(estimator=ExtraTreesRegressor(bootstrap=True, max_features=0.7500000000000001, min_samples_leaf=7, min_samples_split=8, n_estimators=100)),
    StackingEstimator(estimator=XGBRegressor(learning_rate=0.01, max_depth=6, min_child_weight=8, n_estimators=100, nthread=1, subsample=0.35000000000000003)),
    StackingEstimator(estimator=DecisionTreeRegressor(max_depth=2, min_samples_leaf=8, min_samples_split=11)),
    StackingEstimator(estimator=ExtraTreesRegressor(bootstrap=False, max_features=0.1, min_samples_leaf=17, min_samples_split=3, n_estimators=100)),
    StackingEstimator(estimator=ElasticNetCV(l1_ratio=0.15000000000000002, tol=0.1)),
    StackingEstimator(estimator=RidgeCV()),
    LinearSVR(C=20.0, dual=False, epsilon=0.0001, loss="squared_epsilon_insensitive", tol=0.0001)
)

exported_pipeline.fit(training_features, training_target)
results = exported_pipeline.predict(testing_features)
