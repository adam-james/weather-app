const mongoose = require("mongoose");
const City = require("../models/city");
const jsonCities = require("./open-weather-map.city.list.json");

mongoose.connect("mongodb://localhost/weather-app-dev", {
  useNewUrlParser: true
});

mongoose.connection.on(
  "error",
  console.error.bind(console, "connection error:")
);

mongoose.connection.once("open", () => {
  console.log("db connected");
});

main();

async function main() {
  try {
    const aCity = await City.findOne();
    if (aCity != null) {
      console.log("DB already seeded.");
    } else {
      console.log("Seeding cities");
      await City.collection.insert(jsonCities);
    }
  } catch (error) {
    console.log(error);
  } finally {
    console.log("Done.");
    process.exit(0);
  }
}
