#!/usr/bin/env bash

ls -alh

python -m unittest discover

python main.py --file city_pairs.txt --cumulative
