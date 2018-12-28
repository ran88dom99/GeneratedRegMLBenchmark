import numpy as np
import pandas as pd
from sklearn.ensemble import ExtraTreesRegressor, RandomForestRegressor
from sklearn.model_selection import train_test_split
from sklearn.pipeline import make_pipeline, make_union
from sklearn.preprocessing import RobustScaler
from sklearn.tree import DecisionTreeRegressor
from tpot.builtins import StackingEstimator

# NOTE: Make sure that the class is labeled 'target' in the data file
tpot_data = pd.read_csv('PATH/TO/DATA/FILE', sep='COLUMN_SEPARATOR', dtype=np.float64)
features = tpot_data.drop('target', axis=1).values
training_features, testing_features, training_target, testing_target = \
            train_test_split(features, tpot_data['target'].values, random_state=42)

# Score on the training set was:-0.6560553622554219
exported_pipeline = make_pipeline(
    StackingEstimator(estimator=DecisionTreeRegressor(max_depth=4, min_samples_leaf=20, min_samples_split=14)),
    StackingEstimator(estimator=ExtraTreesRegressor(bootstrap=True, max_features=0.8, min_samples_leaf=17, min_samples_split=14, n_estimators=100)),
    RobustScaler(),
    StackingEstimator(estimator=DecisionTreeRegressor(max_depth=8, min_samples_leaf=20, min_samples_split=16)),
    RandomForestRegressor(bootstrap=True, max_features=0.3, min_samples_leaf=1, min_samples_split=6, n_estimators=100)
)

exported_pipeline.fit(training_features, training_target)
results = exported_pipeline.predict(testing_features)
