# Queries and Solutions

Before running any of these queries it's recommended that you create indexes on the fields commonly accessed in them.

From the `Mongo DB Shell`:

```javascript
db.listings.createIndex({ city: 1, id: 1 });
db.reviews.createIndex({ city: 1, date: 1, reviewer_id: 1, listing_id: 1 });
db.neighbourhoods.createIndex({ city: 1 });
db.calendar.createIndex({ city: 1, date: 1, available: 1, listing_id: 1 });
```

## Query 1

> Display the listings available for a particular two-day period for Portland, OR with details: name, neighborhood, room type, how many guests it accommodates, property type and amenities, and per night’s cost, in descending order of average rating?

Query performed with: `Mongo DB Shell`

Query code:

```javascript
pipeline = [
  {
    $match: {
      city: "portland",
    },
  },
  {
    $lookup: {
      from: "calendar",
      localField: "id",
      foreignField: "listing_id",
      as: "dates",
    },
  },
  {
    $unwind: "$dates",
  },
  {
    $match: {
      "dates.available": "t",
      "dates.city": "portland",
      "dates.date": {
        $gte: ISODate("2021-03-20"),
        $lte: ISODate("2021-03-22"),
      },
    },
  },
  {
    $sort: { review_scores_rating: -1 },
  },
  {
    $project: {
      name: 1,
      neighborhood: 1,
      room_type: 1,
      accommodates: 1,
      property_type: 1,
      amenities: 1,
      price: 1,
    },
  },
];

db.listings.aggregate(pipeline).pretty();
```

## Query 2

> What neighborhoods in any of the cities have no listings for a given month?

Query performed with: `Mongo DB Shell`

Query code:

```javascript
pipeline = [
  {
    $project: {
      neighbourhood: 1,
    },
  },
  {
    $lookup: {
      from: "listings",
      localField: "neighbourhood",
      foreignField: "neighbourhood_cleansed",
      as: "neighbourhoodMatch",
    },
  },
  {
    $unwind: "$neighbourhoodMatch",
  },
  {
    $project: {
      neighbourhood: 1,
      neighbourhoodMatch: {
        id: 1,
      },
    },
  },
  {
    $lookup: {
      from: "calendar",
      localField: "neighbourhoodMatch.id",
      foreignField: "listing_id",
      as: "availability",
    },
  },
  {
    $project: {
      neighbourhood: 1,
      neighbourhoodMatch: 1,
      availability: {
        date: 1,
      },
    },
  },
  {
    $addFields: {
      numListings: {
        $size: {
          $filter: {
            input: "$availability",
            as: "day",
            cond: {
              $and: [
                {
                  $gte: ["$$day.date", ISODate("2021-04-01")],
                },
                {
                  $lte: ["$$day.date", ISODate("2021-05-01")],
                },
              ],
            },
          },
        },
      },
    },
  },
  {
    $group: {
      _id: "$neighbourhood",
      totalListings: {
        $sum: "$numListings",
      },
    },
  },
  {
    $match: {
      totalListings: {
        $eq: 0,
      },
    },
  },
];

db.neighbourhoods.aggregate(pipeline).pretty();
```

## Query 3

> For each “Entire home/apt” type listings in Salem, provide its availability periods for a particular month - which chunks of time are bookable? Display the listing’s name, month, availability “from - to” date and minimum nights.

Query performed with: `Python`

Query code:

```python
from datetime import datetime
from functools import reduce
import pymongo

client = pymongo.MongoClient("localhost", 27017)
db = client.CS488


def run():
    pipeline = [
        {"$match": {"city": "salem", "room_type": "Entire home/apt"}},
        {
            "$lookup": {
                "from": "calendar",
                "localField": "id",
                "foreignField": "listing_id",
                "as": "available_dates",
            }
        },
        {"$unwind": "$available_dates"},
        # Filter by availability in a paritcular month which is march
        {
            "$match": {
                "available_dates.available": "t",
                "available_dates.date": {
                    "$gte": datetime(2021, 3, 1),
                    "$lte": datetime(2021, 3, 31),
                },
            }
        },
        {"$sort": {"available_dates.date": 1}},
        {"$project": {"name": 1, "available_dates.date": 1, "minimum_nights": 1}},
    ]

    # Flatten nested date field to get rid of nested available_dates field from `$unwind` operation
    aggregates = list(
        map(
            lambda x: {
                "_id": x["_id"],
                "name": x["name"],
                "minimum_nights": x["minimum_nights"],
                "date": x["available_dates"]["date"],
            },
            db.listings.aggregate(pipeline),
        )
    )

    def reducer(acc, curr):
        """
        Reduce the unwound list into "groups" where each key pertains to a listings
        `_id` and it's value is a list of the listings available for that listings id.

        Note: The values list is ordered because of sorting performed by mongo
            and the sequential nature of pythons reduce operation.
        """
        # "pluck" the `_id` field out of the document, as its held in the key the array each document is associated with
        doc = {
            "name": curr["name"],
            "minimum_nights": curr["minimum_nights"],
            "date": curr["date"],
        }
        try:
            acc[curr["_id"]].append(doc)
        except (KeyError, TypeError):
            acc[curr["_id"]] = [doc]

        return acc

    groups = reduce(reducer, aggregates, {})

    # Filter out listings that are ineligible based on the minimum number of nights requirement
    for dates in groups.values():
        try:
            dates = list(filter(lambda x: len(dates) > int(x["minimum_nights"]), dates))
            # Ignore entries that don't meet the minimum night requirements
            if dates != []:
                _from = dates[0]["date"].strftime("%b %d, %Y")
                _to = dates[-1]["date"].strftime("%b %d, %Y")
                print(
                    f"Name: {dates[0]['name']}\nMinimum nights: {dates[0]['minimum_nights']}\nAvailable from-to: {_from} - {_to}\n"
                )
        except KeyError:
            continue


if __name__ == "__main__":
    run()
```

Note: This requires importing the `pymongo` package. This can be done with the following command:

```sh
python -m pip install pymongo
```

## Query 4

> For “Entire home/apt” type listings in Portland provide the total number of available nights (as per Query 3) for each month March through August of a given year.

Query performed with: `Mongo DB Shell`

Query code:

```javascript
pipeline = [
  {
    $match: {
      city: "portland",
      room_type: "Entire home/apt",
    },
  },
  {
    $lookup: {
      from: "calendar",
      localField: "id",
      foreignField: "listing_id",
      as: "availableDates",
    },
  },
  {
    $unwind: "$availableDates",
  },
  {
    $match: {
      "availableDates.date": {
        $gte: ISODate("2021-03-01"),
        $lte: ISODate("2021-08-31"),
      },
    },
  },
  {
    $group: {
      _id: {
        month: {
          $month: "$availableDates.date",
        },
      },
      days: {
        $sum: 1,
      },
    },
  },
];

db.listings.aggregate(pipeline).pretty();
```

## Query 5

> For each city, how many reviews are received for December of each year?

Query performed with: `Mongo DB Shell`

Query code:

```javascript
pipeline = [
  {
    $match: {
      date: {
        $gte: ISODate("2021-03-01"),
        $lte: ISODate("2021-03-31"),
      },
    },
  },
  {
    $group: {
      _id: "$city",
      numReviews: {
        $sum: 1,
      },
    },
  },
];

db.reviews.aggregate(pipeline).pretty();
```

## Query 6

> Are there any listings that a reviewer has reviewed more than once that is also available in the same month as they posted a review previously? Also include any other listings by the same host in the same city. Display the listings name, url, description, host’s name, reviewer name, whether previously booked, month and minimum and maximum nights allowed.
