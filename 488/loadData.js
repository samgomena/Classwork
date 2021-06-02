const { MongoClient } = require("mongodb");
const { parse } = require("papaparse");
const { omit } = require("underscore");
const fs = require("fs");

const uri = "mongodb://localhost:27017";

const omittedFromCalendar = ["adjusted_price"];

// Fields to throw away from listings entries
const omittedFromListings = [
  "scrape_id",
  "last_scraped",
  "listing_url",
  "picture_url",
  "neighborhood_overview",
  "host_url",
  "host_about",
  "host_response_time",
  "host_response_rate",
  "host_acceptance_rate",
  "host_thumbnail_url",
  "host_is_superhost",
  "host_picture_url",
  "host_neighbourhood",
  "host_verifications",
  "host_has_profile_pic",
  "host_identity_verified",
  "amenities",
  "latitude",
  "longitude",
  "bathrooms",
  "bathrooms_text",
  "bedrooms",
  "beds",
  "minimum_minimum_nights",
  "maximum_minimum_nights",
  "minimum_maximum_nights",
  "maximum_maximum_nights",
  "minimum_nights_avg_ntm",
  "maximum_nights_avg_ntm",
  "availability_30",
  "availability_60",
  "availability_90",
  "availability_365",
  "calendar_last_scraped",
  "calculated_host_listings_count",
  "calculated_host_listings_count_entire_homes",
  "calculated_host_listings_count_private_rooms",
  "calculated_host_listings_count_shared_rooms",
  "number_of_reviews_ltm",
  "number_of_reviews_l30d",
  "review_scores_rating",
  "review_scores_accuracy",
  "review_scores_cleanliness",
  "review_scores_checkin",
  "review_scores_communication",
  "review_scores_location",
  "review_scores_value",
  "license",
  "reviews_per_month",
];

const omittedFromReviews = ["comments"];

let client;

const dataDirPath = `${__dirname}/data`;
const dataDirs = ["los-angeles", "portland", "salem", "san-diego"];
const dataFiles = [
  "calendar.csv",
  "listings.csv",
  "neighbourhoods.csv",
  "reviews.csv",
];

async function run() {
  try {
    client = await MongoClient.connect(uri, {
      useNewUrlParser: true,
      useUnifiedTopology: true,
    });

    const database = client.db("CS488");
    let step = 1;
    let totalSteps = dataDirs.length * dataFiles.length;
    for (dir of dataDirs) {
      for (file of dataFiles) {
        const path = `${dataDirPath}/${dir}/${file}`;
        const collectionName = file.split(".csv")[0];
        const collection = database.collection(collectionName);

        parse(fs.createReadStream(path), {
          delimiter: ",",
          header: true,
          skipEmptyLines: true,
          chunk: async (results, parser) => {
            const [_, city] = parser.streamer._input.path.split("/").reverse();
            let data;
            switch (collection.s.namespace.collection) {
              case "listings":
                data = results.data.map((result) => ({
                  ...omit(result, omittedFromListings),
                  city,
                }));
                break;
              case "calendar":
                data = results.data.map((result) => ({
                  ...omit(result, omittedFromCalendar),
                  city,
                }));
                break;
              case "reviews":
                data = results.data.map((result) => ({
                  ...omit(result, omittedFromReviews),
                  city,
                }));
                break;
              case "neighbourhoods":
                data = results.data.map((result) => ({ ...result, city }));
                break;
            }
            await collection
              .insertMany(data, { ordered: true })
              .catch((err) =>
                console.error(`THERE WAS AN ERROR WRITING DATA TO MONGO ${err}`)
              );
          },
          complete: async (_, filename) => {
            const [file, city] = filename.path.split("/").reverse();
            console.info(
              `[${step}/${totalSteps}] inserted ${
                file.split(".csv")[0]
              } data for ${city}`
            );

            if (step === totalSteps) {
              console.log("Done processing data; press CTRL-C to exit");
              // TODO: Unsure if this works; taking too long to test rn
              await client.close();
            }
            step++;
          },
          error: (err) =>
            console.error(`THERE WAS AN ERROR PARSING DATA ${err}`),
        });
      }
    }
  } catch (err) {
    console.error(`THERE WAS AN UNKNOWN ERROR WHILE PROCESSING DATA ${err}`);
  }
  // TODO: This runs (I think) immediately after we step in to `parse` b/c it's callback based
  // TODO: A more robust solution might be to wrap parsing in an async function and await it's response
  // finally {
  //   await client?.close();
  // }
}
run().catch(console.dir);

process.on("SIGINT", async () => {
  console.log("Closing connection to database");
  await client?.close();
  process.exit(0);
});
