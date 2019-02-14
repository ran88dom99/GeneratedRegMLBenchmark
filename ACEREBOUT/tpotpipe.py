import numpy as np
import pandas as pd
from sklearn.ensemble import GradientBoostingRegressor
from sklearn.feature_selection import SelectPercentile, f_regression
from sklearn.linear_model import RidgeCV
from sklearn.model_selection import train_test_split
from sklearn.pipeline import make_pipeline, make_union
from tpot.builtins import StackingEstimator, ZeroCount

# NOTE: Make sure that the class is labeled 'target' in the data file
tpot_data = pd.read_csv('PATH/TO/DATA/FILE', sep='COLUMN_SEPARATOR', dtype=np.float64)
features = tpot_data.drop('target', axis=1).values
training_features, testing_features, training_target, testing_target = \
            train_test_split(features, tpot_data['target'].values, random_state=42)

# Score on the training set was:-0.05082607312547181
exported_pipeline = make_pipeline(
    ZeroCount(),
    SelectPercentile(score_func=f_regression, percentile=47),
    StackingEstimator(estimator=GradientBoostingRegressor(alpha=0.75, learning_rate=0.01, loss="huber", max_depth=2, max_features=0.6000000000000001, min_samples_leaf=18, min_samples_split=11, n_estimators=100, subsample=1.0)),
    RidgeCV()
)

exported_pipeline.fit(training_features, training_target)
results = exported_pipeline.predict(testing_features)
