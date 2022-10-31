# algotrader



All data is scraped from Yahoo Finance from the top 250 most active stocks. Each stock's weekly average is calculated and aggregated, excluding those with excessive NAs. This main dataset provides training data for each stock which is individually modeled with both ETS and ARIMA models. The perfomance of both models are compared and whichever has the best test-set performance is selected. A dataset is then populated with forecasted weekly average prices and appended to the origional dataset. 

From this new dataset, the current code goes six months into the past and two months into the future. Each stock is then modeled by polynomial models of different order to create a momentum score from differences in their fit lines. Further parameter tuning is required to acheive better stock selections. 

Run each script in the order it's presented. 
