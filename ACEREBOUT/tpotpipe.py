import numpy as np
import pandas as pd
from sklearn.decomposition import FastICA
from sklearn.feature_selection import SelectPercentile, f_regression
from sklearn.model_selection import train_test_split
from sklearn.pipeline import make_pipeline
from sklearn.preprocessing import MaxAbsScaler, Normalizer
from sklearn.svm import LinearSVR

# NOTE: Make sure that the class is labeled 'target' in the data file
tpot_data = pd.read_csv('PATH/TO/DATA/FILE', sep='COLUMN_SEPARATOR', dtype=np.float64)
features = tpot_data.drop('target', axis=1).values
training_features, testing_features, training_target, testing_target = \
            train_test_split(features, tpot_data['target'].values, random_state=42)

# Score on the training set was:-0.6917938525345682
exported_pipeline = make_pipeline(
    MaxAbsScaler(),
    Normalizer(norm="l2"),
    SelectPercentile(score_func=f_regression, percentile=22),
    FastICA(tol=0.9),
    LinearSVR(C=5.0, dual=True, epsilon=0.1, loss="squared_epsilon_insensitive", tol=1e-05)
)

exported_pipeline.fit(training_features, training_target)
results = exported_pipeline.predict(testing_features)
