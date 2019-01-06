import numpy as np
import pandas as pd
from sklearn.ensemble import GradientBoostingRegressor
from sklearn.linear_model import LassoLarsCV
from sklearn.model_selection import train_test_split
from sklearn.pipeline import make_pipeline, make_union
from sklearn.preprocessing import MaxAbsScaler, RobustScaler
from sklearn.tree import DecisionTreeRegressor
from tpot.builtins import OneHotEncoder, StackingEstimator
from sklearn.preprocessing import FunctionTransformer
from copy import copy

# NOTE: Make sure that the class is labeled 'target' in the data file
tpot_data = pd.read_csv('PATH/TO/DATA/FILE', sep='COLUMN_SEPARATOR', dtype=np.float64)
features = tpot_data.drop('target', axis=1).values
training_features, testing_features, training_target, testing_target = \
            train_test_split(features, tpot_data['target'].values, random_state=42)

# Score on the training set was:-0.698408827904663
exported_pipeline = make_pipeline(
    make_union(
        StackingEstimator(estimator=DecisionTreeRegressor(max_depth=1, min_samples_leaf=6, min_samples_split=10)),
        make_pipeline(
            make_union(
                FunctionTransformer(copy),
                StackingEstimator(estimator=make_pipeline(
                    MaxAbsScaler(),
                    LassoLarsCV(normalize=False)
                ))
            ),
            OneHotEncoder(minimum_fraction=0.1, sparse=False),
            RobustScaler()
        )
    ),
    GradientBoostingRegressor(alpha=0.9, learning_rate=0.1, loss="huber", max_depth=10, max_features=0.6000000000000001, min_samples_leaf=6, min_samples_split=10, n_estimators=100, subsample=1.0)
)

exported_pipeline.fit(training_features, training_target)
results = exported_pipeline.predict(testing_features)
