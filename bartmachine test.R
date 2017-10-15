library(bartMachine)
options(java.parameters = "-Xmx500m")
library("bartMachine")
set_bart_machine_num_cores(1)

data("automobile", package = "bartMachine")
automobile <- na.omit(automobile)
y <- automobile$log_price
X <- automobile; X$log_price <- NULL

bart_machine <- bartMachine(X, y)
