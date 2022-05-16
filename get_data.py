#!/usr/bin/python3
import requests
import re
import os.path
from tqdm import tqdm

# Important urls
base_url = "https://s3.amazonaws.com/tripdata/index.html"
xml_url = "https://s3.amazonaws.com/tripdata"
download_url = "https://s3.amazonaws.com/tripdata/"

# Seting up regex for needed files
key_regex = r"<Key>([\d\w\-\.]+.zip)</Key>"
key_matcher = re.compile(key_regex)

# Starting up a http session
with requests.Session() as sess:

    # Authentication request
    res = sess.get(base_url)

    # Headeres nessesary for getting xml file
    xml_req_headers = {
        "Content-Type": "application/xml",
        "Server": res.headers.get("Server"),
        "x-amz-bucket-region": res.headers.get("x-amz-bucket-region"),
        "x-amz-id-2": res.headers.get("x-amz-id-2"),
        "x-amz-request-id": res.headers.get("x-amz-request-id")
    }
    
    # Getting xml file
    xml_res = sess.get(url=xml_url,
                       headers=xml_req_headers)
    # Extracting content
    xml_content = str(xml_res.content)[2:-1]

    print("Download starts...")
    # Going throught all uniqe matches of regex and downloading the files found
    for key in tqdm(set(key_matcher.findall(xml_content))):
        city_code = "JC" if key.startswith("JC") else "NC"
        filepath = "./data/" + city_code + "/" + key

        # Checking if a file doesn't already exists
        if not os.path.isfile(filepath):
            raw_data = requests.get(download_url + key)

            with open(filepath, 'wb') as f:
                f.write(raw_data.content)

            print(f"File: {key} downloaded")

    print("Download finished")
