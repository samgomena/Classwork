#! /bin/bash

# Declare cities that we want to download data for
# Note: The city name _must_ be in the same order as the data url below and must be a valid (POSIX) directory name
declare -a CITIES
CITIES=(
    "los-angeles"
    "san-diego"
    "portland"
    "salem"
)

# See: http://insideairbnb.com/get-the-data.html for more info on the data
# Note: Data is purged from the site after 12 months so depending on when you're reading this, the data may no longer exist!
declare -a DATA
DATA=( 
    "http://data.insideairbnb.com/united-states/ca/los-angeles/2021-04-07"
    "http://data.insideairbnb.com/united-states/ca/san-diego/2021-03-17"
    "http://data.insideairbnb.com/united-states/or/portland/2021-03-13"
    "http://data.insideairbnb.com/united-states/or/salem-or/2021-03-19"
)

declare -a FILES
FILES=( listings.csv.gz calendar.csv.gz reviews.csv.gz )

# Put all of the data in a data folder
# pushd data || exit

len="${#CITIES[@]}"
for (( i=0; i<len; i++ )); do
    dir="${CITIES[$i]}"
    data_url="${DATA[$i]}"

    # Create directory and clean up any additional files
    mkdir -p "$dir"
    rm -f "$dir/*.{csv,gz,json}"
    pushd "$dir" || exit
    for file in "${FILES[@]}"; do
        echo "NOTICE: Downloading data for $dir"
        wget "$data_url/data/$file"
        gzip -d "$file"
    done
    echo "$data_url/visualisations/neighbourhoods.csv"
    popd || exit
done


# TODO: Load data into mongo with something like dis
# mongoimport -d 488 -c listings --file listings.json --type=json --jsonArray

# See: https://docs.google.com/spreadsheets/d/1iWCNJcSutYqpULSQHlNyGInUvHg2BoUGoNRIGa6Szc4/edit#gid=982310896
# For information on what's contained in the data