#does not seem that great for tabular data
#install.packages("lime")
library(caret)
library(lime)


# Split up the data set
iris_test <- iris[1:5, 1:4]
iris_train <- iris[-(1:5), 1:4]
iris_lab <- iris[[5]][-(1:5)]

# Create Random Forest model on iris data
model <- train(iris_train, iris_lab, method = 'rf')

# Create an explainer object
explainer <- lime(iris_train, model)
str(explainer)
# Explain new observation
explanation <- explain(iris_test, explainer, n_labels = 1, n_features = 2)

# The output is provided in a consistent tabular format and includes the
# output from the model.
head(explanation)
#>       model_type case  label label_prob  model_r2 model_intercept
#> 1 classification    1 setosa          1 0.3450777       0.2758681
#> 2 classification    1 setosa          1 0.3450777       0.2758681
#> 3 classification    2 setosa          1 0.3385815       0.2695084
#> 4 classification    2 setosa          1 0.3385815       0.2695084
#> 5 classification    3 setosa          1 0.3538444       0.2628305
#> 6 classification    3 setosa          1 0.3538444       0.2628305
#>   model_prediction      feature feature_value feature_weight
#> 1        0.6949928  Sepal.Width           3.5   -0.013810492
#> 2        0.6949928 Petal.Length           1.4    0.432935221
#> 3        0.7031763  Sepal.Width           3.0    0.008755776
#> 4        0.7031763 Petal.Length           1.4    0.424912088
#> 5        0.6992043 Sepal.Length           4.7   -0.001167966
#> 6        0.6992043 Petal.Length           1.3    0.437541810
#>               feature_desc               data prediction
#> 1        3.3 < Sepal.Width 5.1, 3.5, 1.4, 0.2    1, 0, 0
#> 2      Petal.Length <= 1.6 5.1, 3.5, 1.4, 0.2    1, 0, 0
#> 3 2.8 < Sepal.Width <= 3.0 4.9, 3.0, 1.4, 0.2    1, 0, 0
#> 4      Petal.Length <= 1.6 4.9, 3.0, 1.4, 0.2    1, 0, 0
#> 5      Sepal.Length <= 5.2 4.7, 3.2, 1.3, 0.2    1, 0, 0
#> 6      Petal.Length <= 1.6 4.7, 3.2, 1.3, 0.2    1, 0, 0

# And can be visualised directly
plot_features(explanation)
explanation
explanation$feature_desc
