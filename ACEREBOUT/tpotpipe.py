import numpy as np
import pandas as pd
from sklearn.ensemble import GradientBoostingRegressor, RandomForestRegressor
from sklearn.linear_model import LassoLarsCV, RidgeCV
from sklearn.model_selection import train_test_split
from sklearn.neighbors import KNeighborsRegressor
from sklearn.pipeline import make_pipeline, make_union
from sklearn.preprocessing import PolynomialFeatures
from tpot.builtins import StackingEstimator

# NOTE: Make sure that the class is labeled 'target' in the data file
tpot_data = pd.read_csv('PATH/TO/DATA/FILE', sep='COLUMN_SEPARATOR', dtype=np.float64)
features = tpot_data.drop('target', axis=1).values
training_features, testing_features, training_target, testing_target = \
            train_test_split(features, tpot_data['target'].values, random_state=42)

# Score on the training set was:-0.5605951688301226
exported_pipeline = make_pipeline(
    make_union(
        make_pipeline(
            StackingEstimator(estimator=GradientBoostingRegressor(alpha=0.85, learning_rate=0.001, loss="lad", max_depth=9, max_features=0.9500000000000001, min_samples_leaf=7, min_samples_split=13, n_estimators=100, subsample=0.6000000000000001)),
            StackingEstimator(estimator=RidgeCV()),
            StackingEstimator(estimator=RandomForestRegressor(bootstrap=False, max_features=0.7500000000000001, min_samples_leaf=19, min_samples_split=4, n_estimators=100)),
            StackingEstimator(estimator=KNeighborsRegressor(n_neighbors=4, p=1, weights="uniform")),
            StackingEstimator(estimator=KNeighborsRegressor(n_neighbors=20, p=1, weights="uniform")),
            PolynomialFeatures(degree=2, include_bias=False, interaction_only=False)
        ),
        StackingEstimator(estimator=RandomForestRegressor(bootstrap=False, max_features=0.7000000000000001, min_samples_leaf=16, min_samples_split=17, n_estimators=100))
    ),
    StackingEstimator(estimator=LassoLarsCV(normalize=True)),
    RandomForestRegressor(bootstrap=False, max_features=0.7500000000000001, min_samples_leaf=6, min_samples_split=14, n_estimators=100)
)

exported_pipeline.fit(training_features, training_target)
results = exported_pipeline.predict(testing_features)
