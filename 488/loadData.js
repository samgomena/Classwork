const { MongoClient } = require("mongodb");
const { parse } = require("papaparse");
const fs = require("fs");

const uri = "mongodb://localhost:27017";

// const client = new MongoClient(uri, {
//   useNewUrlParser: true,
//   useUnifiedTopology: true,
// });

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

    const database = client.db("488");
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
          chunk: async (results, parser) => {
            const [_, city] = parser.streamer._input.path.split("/").reverse();
            await collection
              .insertMany(
                results.data.map((result) => ({ ...result, city })),
                { ordered: true }
              )
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
              // await client.close();
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
