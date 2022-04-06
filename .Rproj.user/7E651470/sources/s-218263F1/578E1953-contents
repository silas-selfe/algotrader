
import time
import datetime
import requests
import pandas as pd
from pandas.tseries.offsets import BDay
from bs4 import BeautifulSoup





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






headers = {'User-Agent': 'Mozilla/5.0 (Windows NT 10.0; Win64; x64) AppleWebKit/537.36 (KHTML, like Gecko) Chrome/70.0.3538.77 Safari/537.36'}
url = 'https://finance.yahoo.com/most-active?offset=0&count=250'

response = requests.get(url,headers=headers)
soup = BeautifulSoup(response.content,'lxml')

l = []
mylist = []
for item in soup.select('.simpTblRow'):
  l = item.select('[aria-label=Symbol]')[0].get_text()
  mylist.append(l)













