import numpy as np
import pandas as pd
from sklearn.feature_selection import SelectPercentile, f_regression
from sklearn.linear_model import ElasticNetCV
from sklearn.model_selection import train_test_split
from sklearn.pipeline import make_pipeline

# NOTE: Make sure that the class is labeled 'target' in the data file
tpot_data = pd.read_csv('PATH/TO/DATA/FILE', sep='COLUMN_SEPARATOR', dtype=np.float64)
features = tpot_data.drop('target', axis=1).values
training_features, testing_features, training_target, testing_target = \
            train_test_split(features, tpot_data['target'].values, random_state=42)

# Score on the training set was:-0.8446251536078663
exported_pipeline = make_pipeline(
    SelectPercentile(score_func=f_regression, percentile=63),
    ElasticNetCV(l1_ratio=0.6000000000000001, tol=0.001)
)

exported_pipeline.fit(training_features, training_target)
results = exported_pipeline.predict(testing_features)
