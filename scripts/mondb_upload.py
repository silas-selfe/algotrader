#
# upload to mongodb
#


# https://www.youtube.com/watch?v=rE_bJl2GAY8&ab_channel=TechWithTim

import pymongo
from pymongo import MongoClient



cluster = MongoClient("mongodb+srv://<username><password>@cluster0.kj9sq.mongodb.net/myFirstDatabase?retryWrites=true&w=majority")
db = cluster["test"]
collection = db["test"]


#post = {"_id": 0, "name": "tim", "score": 5}

#collection.insert_one(post)

post1 = {"_id":5, "name":"joe"}
post2 = {"_id":6, "name":"bill"}
post3 = {"_id":7, "name":"mama"}

#collection.insert_many([post1,post2,post3])
  
results = collection.find({"name":"bill"})
print(results)

  
for result in results:
  print(result)


for result in results:
  print(result["_id"])
  
  
  
  
        
