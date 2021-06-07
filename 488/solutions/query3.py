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
