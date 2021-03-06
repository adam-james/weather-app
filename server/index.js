require("dotenv").config();

const app = require("express")();
const fetch = require("node-fetch");
const mongoose = require("mongoose");
const cors = require("cors");
const City = require("./models/city");
const port = 5000;

const { API_KEY } = process.env;
const baseUrl = "https://api.openweathermap.org/data/2.5";
const cityId = 4463523; // Denver

mongoose.connect("mongodb://localhost/weather-app-dev", {
  useNewUrlParser: true
});

function getUrl(endpoint, cityId, units) {
  return baseUrl + endpoint + `?id=${cityId}&APPID=${API_KEY}&units=${units}`;
}

function handleError(res, err) {
  console.log(err);
  res.status(500).json({ error: "An error occurred" });
}

app.use(cors());

// Forecast

app.get("/forecast", async (req, res) => {
  const { cityId, units } = req.query;

  if (cityId == null) {
    return res.status(422).json({ errors: { name: "City ID required" } });
  }

  const endpoint = "/forecast";
  const url = getUrl(endpoint, cityId, units);

  try {
    const resp = await fetch(url);

    if (resp.ok) {
      const json = await resp.json();
      res.json(json);
    } else {
      throw new Error("Response is not OK.");
    }
  } catch (err) {
    handleError(res, err);
  }
});

// Current Weather

app.get("/current-weather", async (req, res) => {
  const { cityId } = req.query;

  if (cityId == null) {
    return res.status(422).json({ errors: { name: "City ID required" } });
  }

  const endpoint = "/weather";
  const url = getUrl(endpoint, cityId);

  try {
    const resp = await fetch(url);

    if (resp.ok) {
      const json = await resp.json();
      res.json(json);
    } else {
      throw new Error("Response is not OK.");
    }
  } catch (err) {
    handleError(res, err);
  }
});

// CITY

app.get("/cities", async (req, res) => {
  const { name } = req.query;

  if (name == null) {
    return res.status(422).json({ errors: { name: "Name required" } });
  }

  try {
    const regexp = new RegExp(`^${name}`, "i");
    const cities = await City.find({ name: regexp }).limit(5);
    res.json(cities);
  } catch (err) {
    handleError(res, err);
  }
});

app.listen(port, () => console.log(`App listening on ${port}`));
