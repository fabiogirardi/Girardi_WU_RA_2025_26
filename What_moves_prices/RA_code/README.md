File 1 contains the Fama–French code you provided, translated into R with a few minor modifications (in CRSP block we added cfacpr to later on calculated the split adjusted Price). We use this file to compute portfolio weights and assign each firm to a portfolio. APIs and WRDS Password and Username have to still be inserted.



File 2 includes our code for constructing new variables, winsorizing them, calculating 12-month rolling averages, and aggregating the results. This file produces the variables that are later used in the analysis. We create the 5 fama-french Portfolios. This R code uses the csv file of WRDS ratios generated in 1\_2\_import\_valuation\_ratios.py, ccm4 file generated in File 1 and the eps\_dps\_data.csv from the 1\_2\_data folder.



File 3 contains the main analysis code, with minor adjustments to integrate our data and variable definitions.



File *data\_preparation2* includes the data\_preparation function adapted to our dataset (Because we do not have price data, the code had to be changed).

