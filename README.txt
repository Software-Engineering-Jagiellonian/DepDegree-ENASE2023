script.R - main script for calculating:
  * correlation analysis
  * feature importance
  * within-project experiment with 2 variants (with AutoSpearman and with fixed set of metrics)
  * cross-project experimnt with 2 variants

github-UBD.csv - source data on bugs in nine GitHub open-source projcts, with DD and DDD metrics added

printResults.R - script for generating results (Tables III, IV, V, VI in the papr) from the csv files described below

results.csv - results of the within-project experiment, variant 1 (AutoSpearman metrics selected in each iteration)
results-VarImpVars.csv - variant 2 (fixed set of 10 metrics with the highest Gini index used)

results-cross.csv - results of the cross-project experiment, variant 1
results-cross-VarImpVars.csv - variant 2

table.txt - data for Table III (result of printRsults.R)
table-VarImpVars.txt - data for table IV (result of printResults.R; to generate it, change the input and output filename in the printResults.R script)
table-cross.txt - data for Table V (result of printResults.R)
table-cross-VarImpVars.txt - data for Table VI (result of printResults.R; to generate it, change the input and output filename in the printResults.R script)

OpenStaticAnalyzer-Metrics.txt - a textfile with the description of the metrics used in the study (taken from https://github.com/sed-inf-u-szeged/OpenStaticAnalyzer)

