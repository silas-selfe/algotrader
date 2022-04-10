# 12:09

import time
import datetime
import requests
import pandas as pd
import pymongo
from pymongo import MongoClient
from pandas.tseries.offsets import BDay
from bs4 import BeautifulSoup
from collections import defaultdict



# pulls tickers for up to 250 stocks on the 'most active' list
headers = {'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36'}
url = 'https://finance.yahoo.com/most-active?offset=0&count=250'

response = requests.get(url,headers=headers)
soup = BeautifulSoup(response.content,'lxml')

l = []
mylist = []
for item in soup.select('.simpTblRow'):
  l = item.select('[aria-label=Symbol]')[0].get_text()
  mylist.append(l)
  



 # pulls data for individual tickers
def data_receive(ticker):
  period1 = int(time.mktime(datetime.datetime(2020, 1, 1, 00, 00).timetuple())) # 23, 59

  period2_d = datetime.datetime.today()
  period2 = int(datetime.datetime.utcnow().timestamp())
  if datetime.date.weekday(period2_d) == 5: # Saturday
    period2 = period2 - datetime.timedelta(days = 1).total_seconds()
  elif datetime.date.weekday(period2_d) == 6: # Sunday
    period2 = period2 - datetime.timedelta(days = 2).total_seconds()
  else:
    period2 = int(datetime.datetime.utcnow().timestamp());
    
  interval = '1d' # 1wk, 1d, 1m
  query_string = f'https://query1.finance.yahoo.com/v7/finance/download/{ticker}?period1={period1}&period2={period2}&interval={interval}&events=history&includeAdjustedClose=true'
  df = pd.read_csv(query_string)
  return df




cluster = MongoClient("mongodb+srv://<username>:<password>@cluster0.kj9sq.mongodb.net/myFirstDatabase?retryWrites=true&w=majority")
db = cluster["test2"]
collection = db["test2"]




# this works -- current 'test2' db was uploaded with this. 
# need to add more info, ie, 'stock #1' or something of the sort. 
# need to come up with way to add new data to current db
count = 0
post = defaultdict(list)
for i in range(len(mylist)):
	info = data_receive(mylist[i])
	for r in range(len(info)):
	  post =  {"_id": count,"Ticker":mylist[i],"Date":info.loc[r][0],"Open":info.loc[r][1], "Close":info.loc[r][4]}
	  collection.insert_one(post)
	  count = count + 1








#####################################################################################
for i in range(len(mylist)):
	info = data_receive(mylist[i])
	post = {"_id": i,"Ticker":mylist[i],"Date":info.loc[i][0],"Open":info.loc[i][1], "Close":info.loc[i][4]}#, "Volume":info.loc[i][6]}
	collection.insert_one(post)





for i in len(mylist):
	info = data_receive(mylist[i])
	#post = {"_id": i,"Ticker":mylist[i], "Date":info[0],
	#"Open":info[1], "Close":info[4], "Volume":info[6]}

	#collection.insert_one(post)


f = 1

for i in 5:
	info = data_receive(mylist.loc[i])
	date = data.loc[:,"Date"]
	
	post = {"_id": i,"Ticker":mylist[i], "Date":info[0],
	"Open":info[1], "Close":info[4], "Volume":info[6]}

	collection.insert_one(post)



for i in range(len(mylist)):
	info = data_receive(mylist[i])
	post = {"_id": i,"Ticker":mylist[i], "Date":info[0],
	"Open":info[1], "Close":info[4], "Volume":info[6]}
	
	#collection.insert_one(post)
	
	




