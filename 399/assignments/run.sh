#! /bin/bash

an="$1"

cobc -x -o bin/assignment"$an" -std=default -Wall -debug assignment"$an".cbl && ./bin/assignment"$an"
