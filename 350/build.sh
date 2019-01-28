wget https://chromedriver.storage.googleapis.com/2.45/chromedriver_linux64.zip
unzip chromedriver_linux64.zip

ls -alh

pip install -r requirements.txt

python benchmark.py --max-time=60
