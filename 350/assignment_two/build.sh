#!/usr/bin/env bash

wget https://chromedriver.storage.googleapis.com/2.45/chromedriver_linux64.zip
unzip chromedriver_linux64.zip
export PATH=$PATH:./chromedriver

ls -alh

pip install -r requirements.txt

# Let'er run for 5 minutes 
python benchmark.py --total-time=$((5 * 60)) --increase-size-by=50000 --verbose
