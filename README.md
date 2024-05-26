This project is licensed under the terms of the GNU AFFERO GENERAL PUBLIC LICENSE license.
# GeneratedRegMLBenchmark
Testing what the machine learning algorithms of R-caret can detect.

For results see wiki and folders containing [number]th. Files named minnrec ods, power png especially. 
Some patterns, like 3 variables multiplied to produce target variable and nested if statments can barely be reconstructed by any algorithm.

First run: R version 3.3.2., 100 datapoints, 3 Xvalidated repeated 5 times, 16 random hyper-parameters tested each time. 
 bagEarth & cubist are best on at detecting most patterns with svmLinear2 & cforest for support.  xyf and SBC
42 models always fail. 

Warning! Installs each model's package without prompt.
Run "multiple generators" to generate data then "model tester" to see if any of caret's modeling algorithms can detect each generated pattern.
If a specific test is in "test out.csv" it will not be run by the program. 
there are 2 additional forloops (6 extra itinerations total) besides for each model and pattern generated.

To regress on your own data add your data with target column 1 to Generats folder and name of file to More Generators.R. Then run Multiple Generators.R. Look up number of your file in gensnames and make that the only number in ModelTesterAllAuto.R 's line 51. There are any options and no documentation soo good luck. 

First two columns of test out.csv are % of RMSE generated by mean resolved by algorithm and % of MAE.
