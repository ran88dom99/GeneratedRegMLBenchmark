import numpy as np
import pandas as pd
from sklearn.feature_selection import SelectPercentile, f_regression
from sklearn.linear_model import LassoLarsCV
from sklearn.model_selection import train_test_split
from sklearn.pipeline import make_pipeline, make_union
from sklearn.preprocessing import Normalizer, StandardScaler
from tpot.builtins import StackingEstimator, ZeroCount
from xgboost import XGBRegressor

# NOTE: Make sure that the class is labeled 'target' in the data file
tpot_data = pd.read_csv('PATH/TO/DATA/FILE', sep='COLUMN_SEPARATOR', dtype=np.float64)
features = tpot_data.drop('target', axis=1).values
training_features, testing_features, training_target, testing_target = \
            train_test_split(features, tpot_data['target'].values, random_state=42)

# Score on the training set was:-0.7293122922662544
exported_pipeline = make_pipeline(
    Normalizer(norm="l2"),
    ZeroCount(),
    StackingEstimator(estimator=XGBRegressor(learning_rate=1.0, max_depth=1, min_child_weight=6, n_estimators=100, nthread=1, subsample=0.05)),
    Normalizer(norm="l1"),
    SelectPercentile(score_func=f_regression, percentile=27),
    StandardScaler(),
    SelectPercentile(score_func=f_regression, percentile=75),
    LassoLarsCV(normalize=False)
)

exported_pipeline.fit(training_features, training_target)
results = exported_pipeline.predict(testing_features)
