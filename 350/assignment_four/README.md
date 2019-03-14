## CS 350 - Winter 2019
### David Lu
### Portland State University 

[![Build Status](https://travis-ci.com/samgomena/Classwork.svg?branch=develop)](https://travis-ci.com/samgomena/Classwork)

See the [rundown](RUNDOWN.md) for an analysis of the algorithms used here.

### Usage 

All of the code is dependent on python 3.6 or greater.
The main entry point into the application is `main.py`.

See the [build](./build.sh) script for commonly used parameters 

```bash
$ python main.py --help

usage: main.py [-h] [-f GRAPH_FILE] [-c]

Calculate minimum spanning trees with different algorithms

optional arguments:
  -h, --help            show this help message and exit
  -f GRAPH_FILE, --file GRAPH_FILE
                        A file containing a textual representation of a graph
  -c, --cumulative      Show cumulative distance travelled for each step

```