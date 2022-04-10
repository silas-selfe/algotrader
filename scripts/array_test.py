import pymongo
from pymongo import MongoClient



list1 = ["AAPL", "TSLA", "NIO"]
list2 = [12, 15, 87]



cluster = MongoClient("mongodb+srv://<username>:<password>@cluster0.kj9sq.mongodb.net/myFirstDatabase?retryWrites=true&w=majority")
db = cluster["test"]
collection = db["test"]



post = {"_id": 7, "Ticker":list1[0], "Close":list2[0] }

collection.insert_one(post)
