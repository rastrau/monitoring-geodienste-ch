# Python 3.9, env geodienste-ch
from urllib.request import urlopen
import pandas as pd
import json
import csv
import datetime
import collections

json_url = "https://www.geodienste.ch/info/services.json?restricted_topics=false&language=de"

# Retrieve JSON from geodienste.ch
response = urlopen(json_url)
data = json.loads(response.read())

timestamp = datetime.datetime.strftime(datetime.date.today(), "%Y-%m-%d")

# Convert JSON to dataframe and select relevant attributes
df = pd.json_normalize(data, record_path=['services'])
df.head()
df = df[["topic_title", "topic", "version", "canton", 
         "publication_data", "contract_required_data", 
         "publication_wms", "contract_required_wms"]]
         
# Postprocess the data, because cantons don't indicate for many topics 
# that they do not distribute associated data via geodienste.ch
cantons = ["AG", "AI", "AR", "BE", "BL", "BS", "FL", "FR", "GE", "GL",
           "GR", "JU", "LU", "NE", "NW", "OW", "SG", "SH", "SO", "SZ",
           "TG", "TI", "UR", "VD", "VS", "ZG", "ZH"]

missing_data = []
topic_titles = set(df["topic_title"])
for topic_title in topic_titles:
  subset = df[df["topic_title"] == topic_title]
  for canton in cantons:
    if not canton in subset["canton"].unique():
      missing_data.append((topic_title, canton))

topic_title_list = [e[0] for e in missing_data]
canton_list = [e[1] for e in missing_data]
additional_records = pd.DataFrame({
    "topic_title": topic_title_list, 
    "canton": canton_list,
    "publication_data": "Keine Daten",
    "publication_wms": "Keine Daten",
    "comment": "Record inferred (no information on this record was provided by geodienste.ch)"}
  )

df = pd.concat([df, additional_records], ignore_index = True)

df["updated"] = timestamp
    
# Export dataframe to current CSV file
df.to_csv("data/geodienste-ch.csv", 
          index = False, 
          encoding = "utf-8",
          quoting = csv.QUOTE_NONNUMERIC,
          sep = ";")

# Export dataframe to timestamped CSV file for archival purposes
df.to_csv("data/%s-geodienste-ch.csv" % timestamp, 
          index = False, 
          encoding = "utf-8",
          quoting = csv.QUOTE_NONNUMERIC,
          sep = ";")
