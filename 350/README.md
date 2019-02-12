## CS 350 - Winter 2019
### David Lu
### Portland State University 

[![Build Status](https://travis-ci.com/samgomena/Classwork.svg?branch=develop)](https://travis-ci.com/samgomena/Classwork)

See the [rundown](./RUNDOWN.md) for an analysis of the algorithms used here.

### Usage 

See the [build](./build.sh) script for commonly used parameters 

```bash
usage: benchmark.py [-h] [--min-arr-size MIN_ARR_SIZE]
                    [--max-arr-size MAX_ARR_SIZE] [-t TOTAL_TIME]
                    [-m MAX_TIME] [-i INCREASE_BY] [-v]

Benchmark sorting algorithms.

optional arguments:
  -h, --help            show this help message and exit
  --min-arr-size MIN_ARR_SIZE
                        Minimum size of an array that will be sorted
  --max-arr-size MAX_ARR_SIZE
                        Maximum size of an array that will be sorted
  -t TOTAL_TIME, --total-time TOTAL_TIME
                        Maximum number of seconds to let each sort algo run
                        for
  -m MAX_TIME, --max-time MAX_TIME
                        Maximum number of seconds to let each individual run
                        on the sort algo run for
  -i INCREASE_BY, --increase-size-by INCREASE_BY
                        Amount to increase the array by on each subsequent run
  -v, --verbose         Print extended output to stdout
```